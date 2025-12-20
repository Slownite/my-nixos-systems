;;; core-python.el --- Python (TS, Eglot + basedpyright, uv-first, format, helpers) -*- lexical-binding: t; -*-
;;; Commentary:
;; This file aims to “just work” across projects, while being great for uv-based repos.
;;
;; What you get:
;; 1) Python mode:
;;    - Uses python-ts-mode when Tree-sitter is available.
;;
;; 2) LSP:
;;    - Uses Eglot.
;;    - Uses basedpyright as the LSP server.
;;    - If the project looks like a uv project, the server is started with:
;;        uv run -- basedpyright-langserver --stdio
;;      so it sees the same dependencies as the project.
;;
;; 3) Environment (REPL):
;;    - In uv projects, `M-x run-python` uses `uv run python`.
;;
;; 4) Formatting (async, never blocks Emacs):
;;    - Detects formatter from project config:
;;        - If Ruff is configured → runs ruff format
;;        - Else if Black is configured → runs black
;;        - Else → does nothing (no surprises)
;;    - Runs on save (can be disabled per project via .dir-locals.el).
;;
;; 5) uv commands (async):
;;    - M-x je/python-uv-sync
;;    - M-x je/python-uv-test
;;    - M-x je/python-uv-ruff-check
;;    - M-x je/python-uv-ruff-format
;;    - M-x je/python-uv-run (prompts for any command)
;;
;; Recommended for your big uv repo:
;; Put a .dir-locals.el in the repo root:
;;
;;   ((python-mode . ((je/python-prefer-uv . t)
;;                   (je/python-format-on-save . t)))
;;    (python-ts-mode . ((je/python-prefer-uv . t)
;;                       (je/python-format-on-save . t))))
;;
;;; Code:

(require 'cl-lib)
(require 'seq)

;; -------------------------------------------------------------------
;; 0) Tree-sitter: remap python-mode -> python-ts-mode when available
;; -------------------------------------------------------------------
(with-eval-after-load 'treesit
  (when (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))))

;; -------------------------------------------------------------------
;; 1) User options (safe to override globally or via .dir-locals.el)
;; -------------------------------------------------------------------
(defgroup je/python nil "Python setup." :group 'languages)

(defcustom je/python-prefer-uv t
  "If non-nil, prefer uv when a project looks like a uv project."
  :type 'boolean
  :safe #'booleanp)

(defcustom je/python-format-on-save t
  "If non-nil, format Python buffers on save if a project formatter is detected."
  :type 'boolean
  :safe #'booleanp)

(defcustom je/python-auto-restart-eglot-after-uv-sync nil
  "If non-nil, restart Eglot in the current buffer after a successful `uv sync`."
  :type 'boolean
  :safe #'booleanp)

;; -------------------------------------------------------------------
;; 2) Project detection helpers
;; -------------------------------------------------------------------
(defun je/python--project-root ()
  "Return a best-effort project root."
  (or (when-let ((proj (project-current nil)))
        (car (project-roots proj)))
      (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun je/python--uv-project-p (root)
  "Return non-nil if ROOT looks like a uv project."
  (or (file-exists-p (expand-file-name "uv.lock" root))
      (and (file-exists-p (expand-file-name "pyproject.toml" root))
           (executable-find "uv"))))

(defun je/python--has-pyproject-p (root)
  (file-exists-p (expand-file-name "pyproject.toml" root)))

;; -------------------------------------------------------------------
;; 3) REPL environment: prefer uv run python in uv projects
;; -------------------------------------------------------------------
(defun je/python--use-uv-repl ()
  "Configure buffer-local Python REPL to use uv."
  (setq-local python-shell-interpreter "uv"
              python-shell-interpreter-args "run python"))

(defun je/python-activate-project-env ()
  "Pick environment for this buffer (uv-first).
Currently only sets REPL command for uv projects."
  (let ((root (je/python--project-root)))
    (when (and je/python-prefer-uv
               (je/python--uv-project-p root))
      (je/python--use-uv-repl)
      'uv)))

;; -------------------------------------------------------------------
;; 4) Eglot + basedpyright
;;    We choose the server command dynamically (uv vs non-uv).
;; -------------------------------------------------------------------
(with-eval-after-load 'eglot
  (defun je/python--eglot-basedpyright-command ()
    "Return the command Eglot should use to start basedpyright."
    (let* ((root (je/python--project-root))
           (use-uv (and je/python-prefer-uv
                        (executable-find "uv")
                        (je/python--uv-project-p root))))
      (if use-uv
          '("uv" "run" "--" "basedpyright-langserver" "--stdio")
        '("basedpyright-langserver" "--stdio"))))

  ;; Register server for both python-mode and python-ts-mode.
  (setf (alist-get 'python-mode eglot-server-programs) #'je/python--eglot-basedpyright-command)
  (setf (alist-get 'python-ts-mode eglot-server-programs) #'je/python--eglot-basedpyright-command)

  ;; Optional: default LSP settings. Keep conservative.
  ;; You can tune this later without breaking anything.
  (setq-default eglot-workspace-configuration
                '((:basedpyright
                   (:python
                    (:analysis
                     (:typeCheckingMode "basic"
                      :diagnosticMode "openFilesOnly")))))))

;; -------------------------------------------------------------------
;; 5) Formatter detection (Ruff / Black) based on project config
;;    - If [tool.ruff] exists → ruff format
;;    - else if [tool.black] exists → black
;;    - else do nothing
;; -------------------------------------------------------------------
(defun je/python--file-contains-section-p (file section-regexp &optional max-bytes)
  "Return non-nil if FILE contains SECTION-REGEXP.
We only read the first MAX-BYTES bytes for speed (default ~30KB)."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 (or max-bytes 30000))
      (goto-char (point-min))
      (re-search-forward section-regexp nil t))))

(defun je/python--project-uses-ruff-p (root)
  "Detect Ruff config in ROOT."
  (or (file-exists-p (expand-file-name "ruff.toml" root))
      (je/python--file-contains-section-p
       (expand-file-name "pyproject.toml" root)
       "^[[:space:]]*\\[tool\\.ruff\\]")))

(defun je/python--project-uses-black-p (root)
  "Detect Black config in ROOT."
  (je/python--file-contains-section-p
   (expand-file-name "pyproject.toml" root)
   "^[[:space:]]*\\[tool\\.black\\]"))

(defun je/python--format-command (root file)
  "Return a (PROGRAM ARGS...) list to format FILE, or nil if none detected."
  (let ((use-uv (and je/python-prefer-uv (je/python--uv-project-p root))))
    (cond
     ((je/python--project-uses-ruff-p root)
      (if use-uv
          (list "uv" "run" "--" "ruff" "format" file)
        (list "ruff" "format" file)))
     ((je/python--project-uses-black-p root)
      (if use-uv
          (list "uv" "run" "--" "black" "--quiet" file)
        (list "black" "--quiet" file)))
     (t nil))))

;; -------------------------------------------------------------------
;; 6) Generic async runner used by format + uv commands
;; -------------------------------------------------------------------
(defun je/python--run-async (name buffer command &optional default-directory sentinel)
  "Run COMMAND asynchronously, streaming output to BUFFER.

NAME is a short process name.
DEFAULT-DIRECTORY is the working directory (use project root).
SENTINEL is called when the process exits."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "$ %s\n\n" (mapconcat #'identity command " ")))
      (special-mode))
    (display-buffer buf)
    (make-process
     :name name
     :buffer buf
     :command command
     :noquery t
     :sentinel (or sentinel
                   (lambda (p event)
                     (when (memq (process-status p) '(exit signal))
                       (let ((exit (process-exit-status p)))
                         (with-current-buffer (process-buffer p)
                           (read-only-mode -1)
                           (goto-char (point-max))
                           (insert (format "\n\n[%s] %s (exit %d)\n"
                                           name (string-trim event) exit))
                           (read-only-mode 1))
                         (if (= exit 0)
                             (message "%s done" name)
                           (message "%s failed (see %s)" name buffer)))))))
     :file-handler t
     :coding 'utf-8
     ;; Workdir:
     :connection-type 'pipe
     :stderr buffer
     :stdin nil
     :command command
     ;; set workdir via let-binding
     ))

;; Note: make-process doesn't take :default-directory directly; we let-bind.
(defun je/python--run-async-in-dir (dir name buffer command &optional sentinel)
  (let ((default-directory dir))
    (je/python--run-async name buffer command dir sentinel)))

;; -------------------------------------------------------------------
;; 7) Format current file (async) + format-on-save hook
;; -------------------------------------------------------------------
(defun je/python-format-buffer-async ()
  "Format the current Python file if a project formatter is detected.
Runs asynchronously (does not freeze Emacs)."
  (interactive)
  (let* ((root (je/python--project-root))
         (file (buffer-file-name))
         (cmd (and file (je/python--format-command root file))))
    (cond
     ((not file)
      (message "No file on disk; skipping format"))
     ((not cmd)
      (message "No project formatter detected; skipping"))
     (t
      (je/python--run-async-in-dir
       root
       "python-format" "*python-fmt*" cmd
       (lambda (p event)
         (when (memq (process-status p) '(exit signal))
           (let ((exit (process-exit-status p))
                 (buf (process-buffer p)))
             (with-current-buffer buf
               (read-only-mode -1)
               (goto-char (point-max))
               (insert (format "\n\n[python-format] %s (exit %d)\n"
                               (string-trim event) exit))
               (read-only-mode 1))
             (if (= exit 0)
                 ;; Reload the file content after formatting.
                 (progn
                   (when (buffer-live-p (get-file-buffer file))
                     (with-current-buffer (get-file-buffer file)
                       (revert-buffer :ignore-auto :noconfirm)))
                   (message "Formatted (%s)" (car cmd)))
               (display-buffer buf)
               (message "Format failed (see *python-fmt*)"))))))))))

(defun je/python--maybe-format-on-save ()
  "Format on save if enabled and formatter exists."
  (when (and je/python-format-on-save
             (derived-mode-p 'python-mode 'python-ts-mode)
             (buffer-file-name))
    (let* ((root (je/python--project-root))
           (cmd (je/python--format-command root (buffer-file-name))))
      (when cmd
        (je/python-format-buffer-async)))))

;; -------------------------------------------------------------------
;; 8) uv commands (async) for your big uv project(s)
;; -------------------------------------------------------------------
(defun je/python--require-uv-project ()
  (unless (executable-find "uv")
    (user-error "uv not found in PATH"))
  (let ((root (je/python--project-root)))
    (unless (je/python--uv-project-p root)
      (user-error "Not a uv project (no uv.lock / pyproject.toml)"))
    root))

(defun je/python-uv-sync ()
  "Run `uv sync` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-sync" "*uv*" '("uv" "sync")
     (lambda (p event)
       (when (memq (process-status p) '(exit signal))
         (let ((exit (process-exit-status p))
               (buf (process-buffer p)))
           (with-current-buffer buf
             (read-only-mode -1)
             (goto-char (point-max))
             (insert (format "\n\n[uv-sync] %s (exit %d)\n"
                             (string-trim event) exit))
             (read-only-mode 1))
           (if (= exit 0)
               (progn
                 (message "uv sync done")
                 ;; Optional: restart Eglot so it sees new deps.
                 (when (and je/python-auto-restart-eglot-after-uv-sync
                            (derived-mode-p 'python-mode 'python-ts-mode)
                            (fboundp 'eglot-reconnect))
                   (eglot-reconnect)))
             (display-buffer buf)
             (message "uv sync failed (see *uv*)"))))))))

(defun je/python-uv-test ()
  "Run `uv run -- pytest -q` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-test" "*uv*" '("uv" "run" "--" "pytest" "-q"))))

(defun je/python-uv-ruff-check ()
  "Run `uv run -- ruff check .` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-ruff-check" "*uv*" '("uv" "run" "--" "ruff" "check" "."))))

(defun je/python-uv-ruff-check-fix ()
  "Run `uv run -- ruff check . --fix` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-ruff-fix" "*uv*" '("uv" "run" "--" "ruff" "check" "." "--fix"))))

(defun je/python-uv-ruff-format ()
  "Run `uv run -- ruff format .` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-ruff-format" "*uv*" '("uv" "run" "--" "ruff" "format" "."))))

(defun je/python-uv-black ()
  "Run `uv run -- black .` asynchronously in the current project."
  (interactive)
  (let ((root (je/python--require-uv-project)))
    (je/python--run-async-in-dir
     root "uv-black" "*uv*" '("uv" "run" "--" "black" "."))))

(defun je/python-uv-run (cmdline)
  "Prompt for a command and run it as `uv run -- CMDLINE` asynchronously.

Examples to type at the prompt:
  pytest -q
  ruff check . --fix
  ruff format .
  python -m yourmodule
"
  (interactive (list (read-shell-command "uv run -- ")))
  (let ((root (je/python--require-uv-project)))
    (let* ((args (split-string-and-unquote cmdline))
           (command (append '("uv" "run" "--") args)))
      (je/python--run-async-in-dir root "uv-run" "*uv*" command))))

;; -------------------------------------------------------------------
;; 9) One setup function hooked into python-mode/python-ts-mode
;; -------------------------------------------------------------------
(defun je/python-setup ()
  "Setup Python buffer: env (uv-first), Eglot, and format-on-save."
  (je/python-activate-project-env)
  (eglot-ensure)
  ;; Buffer-local hook so it only affects Python buffers.
  (add-hook 'after-save-hook #'je/python--maybe-format-on-save nil t))

(add-hook 'python-mode-hook #'je/python-setup)
(add-hook 'python-ts-mode-hook #'je/python-setup)

;; -------------------------------------------------------------------
;; 10) Optional keybindings (uncomment if you want them)
;; -------------------------------------------------------------------
;; (global-set-key (kbd "C-c u s") #'je/python-uv-sync)
;; (global-set-key (kbd "C-c u t") #'je/python-uv-test)
;; (global-set-key (kbd "C-c u r") #'je/python-uv-run)
;; (global-set-key (kbd "C-c u c") #'je/python-uv-ruff-check)
;; (global-set-key (kbd "C-c u f") #'je/python-uv-ruff-check-fix)
;; (global-set-key (kbd "C-c u F") #'je/python-uv-ruff-format)

(provide 'core-python)
;;; core-python.el ends here
