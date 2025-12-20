;;; core-js.el --- JS/TS (TS, Eglot, Prettier/ESLint async) -*- lexical-binding: t; -*-
;;; Commentary:
;; This module aims to work across many JS/TS repos without knowing their toolchain.
;;
;; Features:
;; - Tree-sitter modes when available:
;;   - js-mode -> js-ts-mode
;;   - typescript-mode -> typescript-ts-mode (if you use typescript-mode)
;; - Eglot with typescript-language-server if available
;; - Async format-on-save (never blocks Emacs):
;;   - Prefer Prettier when project config exists
;;   - Else ESLint --fix when project config exists
;;   - Else do nothing
;; - Uses project-local tools via: npx --no-install <tool>
;;
;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup je/js nil "JS/TS setup." :group 'languages)

(defcustom je/js-format-on-save t
  "Format JS/TS buffers on save when a formatter is detected."
  :type 'boolean
  :safe #'booleanp)

(defcustom je/js-prefer-project-tools t
  "If non-nil, prefer project-local tools via `npx --no-install`."
  :type 'boolean
  :safe #'booleanp)

;; ------------------------------------------------------------
;; 1) Tree-sitter remaps
;; ------------------------------------------------------------
(with-eval-after-load 'treesit
  (when (treesit-language-available-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    ;; Some people still use js2-mode; you can remap it too if installed:
    ;; (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
    )
  (when (treesit-language-available-p 'typescript)
    ;; If you use typescript-mode package, it defines typescript-mode:
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))))

;; ------------------------------------------------------------
;; 2) Project helpers
;; ------------------------------------------------------------
(defun je/js--project-root ()
  "Return a best-effort project root."
  (or (when-let ((proj (project-current nil)))
        (car (project-roots proj)))
      (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun je/js--has-file-anywhere-up (root files)
  "Return first matching filename from FILES found in ROOT, else nil."
  (seq-find (lambda (f) (file-exists-p (expand-file-name f root))) files))

(defun je/js--package-json-mentions-p (root regexp)
  "Return non-nil if package.json under ROOT contains REGEXP (small scan)."
  (let ((pkg (expand-file-name "package.json" root)))
    (when (file-exists-p pkg)
      (with-temp-buffer
        (insert-file-contents pkg nil 0 40000)
        (goto-char (point-min))
        (re-search-forward regexp nil t)))))

(defun je/js--prettier-config-p (root)
  "Heuristic: does this project look like it uses Prettier?"
  (or (je/js--has-file-anywhere-up
       root
       '(".prettierrc" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml"
         ".prettierrc.js" ".prettierrc.cjs" "prettier.config.js" "prettier.config.cjs"
         ".prettierignore"))
      (je/js--package-json-mentions-p root "\"prettier\"")))

(defun je/js--eslint-config-p (root)
  "Heuristic: does this project look like it uses ESLint?"
  (or (je/js--has-file-anywhere-up
       root
       '(".eslintrc" ".eslintrc.json" ".eslintrc.js" ".eslintrc.cjs" ".eslintrc.yml"
         ".eslintrc.yaml" "eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"))
      (je/js--package-json-mentions-p root "\"eslint\"")))

;; ------------------------------------------------------------
;; 3) Async runner (shared)
;; ------------------------------------------------------------
(defun je/js--run-async-in-dir (dir name buffer command &optional sentinel)
  "Run COMMAND asynchronously in DIR, streaming output to BUFFER."
  (let ((default-directory dir)
        (buf (get-buffer-create buffer)))
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
                           (message "%s failed (see %s)" name buffer)))))))))

;; ------------------------------------------------------------
;; 4) Formatter command selection (Prettier > ESLint > none)
;; ------------------------------------------------------------
(defun je/js--npx-command (tool &rest args)
  "Return a command list using npx --no-install TOOL ARGS."
  (append (list "npx" "--no-install" tool) args))

(defun je/js--format-command (root file)
  "Return a (PROGRAM ARGS...) list to format FILE, or nil."
  (cond
   ;; Prefer Prettier if configured
   ((je/js--prettier-config-p root)
    (cond
     ((and je/js-prefer-project-tools (executable-find "npx"))
      (je/js--npx-command "prettier" "--write" file))
     ((executable-find "prettier")
      (list "prettier" "--write" file))
     (t nil)))

   ;; Else ESLint --fix if configured
   ((je/js--eslint-config-p root)
    (cond
     ((and je/js-prefer-project-tools (executable-find "npx"))
      (je/js--npx-command "eslint" "--fix" file))
     ((executable-find "eslint")
      (list "eslint" "--fix" file))
     (t nil)))

   (t nil)))

(defun je/js-format-buffer-async ()
  "Format current file if project formatter is detected (async)."
  (interactive)
  (let* ((root (je/js--project-root))
         (file (buffer-file-name))
         (cmd (and file (je/js--format-command root file))))
    (cond
     ((not file) (message "No file on disk; skipping format"))
     ((not cmd)  (message "No project formatter detected; skipping"))
     (t
      (je/js--run-async-in-dir
       root "js-format" "*js-fmt*" cmd
       (lambda (p event)
         (when (memq (process-status p) '(exit signal))
           (let ((exit (process-exit-status p))
                 (buf (process-buffer p)))
             (with-current-buffer buf
               (read-only-mode -1)
               (goto-char (point-max))
               (insert (format "\n\n[js-format] %s (exit %d)\n"
                               (string-trim event) exit))
               (read-only-mode 1))
             (if (= exit 0)
                 (progn
                   ;; Reload file content after formatter changes it on disk
                   (when-let ((fb (get-file-buffer file)))
                     (with-current-buffer fb
                       (revert-buffer :ignore-auto :noconfirm)))
                   (message "Formatted (%s)" (car cmd)))
               (display-buffer buf)
               (message "Format failed (see *js-fmt*)"))))))))))

(defun je/js--maybe-format-on-save ()
  "Run format-on-save if enabled and formatter exists."
  (when (and je/js-format-on-save
             (buffer-file-name))
    (let* ((root (je/js--project-root))
           (cmd (je/js--format-command root (buffer-file-name))))
      (when cmd
        (je/js-format-buffer-async)))))

;; ------------------------------------------------------------
;; 5) Eglot (typescript-language-server)
;; ------------------------------------------------------------
(with-eval-after-load 'eglot
  ;; Use typescript-language-server if available.
  ;; If not installed, Eglot will prompt / fail gracefully.
  (when (executable-find "typescript-language-server")
    (dolist (mode '(js-mode js-ts-mode typescript-mode typescript-ts-mode))
      (setf (alist-get mode eglot-server-programs)
            '("typescript-language-server" "--stdio")))))

(defun je/js-setup ()
  "Setup JS/TS buffer: start Eglot and install format-on-save hook."
  (eglot-ensure)
  (add-hook 'after-save-hook #'je/js--maybe-format-on-save nil t))

;; Hooks for common JS/TS modes
(add-hook 'js-mode-hook #'je/js-setup)
(add-hook 'js-ts-mode-hook #'je/js-setup)
(add-hook 'typescript-mode-hook #'je/js-setup)
(add-hook 'typescript-ts-mode-hook #'je/js-setup)

(provide 'core-js)
;;; core-js.el ends here
