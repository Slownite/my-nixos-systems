;;; core-vue.el --- Vue (vue-mode, Eglot + Volar, Prettier/ESLint async) -*- lexical-binding: t; -*-
;;; Commentary:
;; Vue setup for vue-mode:
;; - Open .vue files with vue-mode
;; - Start Eglot automatically
;; - Use Volar (vue-language-server) if available:
;;   Prefer project-local: npx --no-install vue-language-server --stdio
;;   Fallback: global vue-language-server --stdio
;; - Format on save (async) using core-js formatter logic:
;;   Prettier (if configured) > ESLint --fix (if configured) > do nothing
;;
;; Requirements:
;; - vue-mode installed
;; - eglot installed
;; - Volar installed either:
;;     - in the project (recommended): npm/pnpm/yarn dev dep @vue/language-server
;;       (exposes vue-language-server)
;;     - or globally (less ideal)
;;
;;; Code:

(require 'cl-lib)
(require 'core-js nil t)  ;; reuse formatter/project helpers from core-js if present

(defgroup je/vue nil "Vue setup." :group 'languages)

(defcustom je/vue-format-on-save t
  "Format Vue buffers on save when project formatter is detected."
  :type 'boolean
  :safe #'booleanp)

(defcustom je/vue-prefer-project-volar t
  "Prefer running Volar via project-local npx --no-install when possible."
  :type 'boolean
  :safe #'booleanp)

;; ------------------------------------------------------------
;; 1) File association: .vue -> vue-mode
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; ------------------------------------------------------------
;; 2) Project root helper (reuse core-js if loaded)
;; ------------------------------------------------------------
(defun je/vue--project-root ()
  (if (fboundp 'je/js--project-root)
      (je/js--project-root)
    (or (when-let ((proj (project-current nil)))
          (car (project-roots proj)))
        (locate-dominating-file default-directory "package.json")
        (locate-dominating-file default-directory ".git")
        default-directory)))

;; ------------------------------------------------------------
;; 3) Formatting: delegate to core-js (recommended)
;; ------------------------------------------------------------
(defun je/vue-format-buffer-async ()
  "Format current Vue file asynchronously using the core-js formatter.
If core-js isn't loaded, do nothing."
  (interactive)
  (if (fboundp 'je/js-format-buffer-async)
      (je/js-format-buffer-async)
    (message "core-js not loaded; Vue formatting disabled (load core-js)")))

(defun je/vue--maybe-format-on-save ()
  (when (and je/vue-format-on-save (buffer-file-name))
    ;; core-js already does the “detect formatter then run async” logic
    (je/vue-format-buffer-async)))

;; ------------------------------------------------------------
;; 4) Eglot server: Volar for vue-mode
;; ------------------------------------------------------------
(with-eval-after-load 'eglot
  (defun je/vue--volar-command ()
    "Command used to start Volar."
    (let ((root (je/vue--project-root)))
      (cond
       ;; Prefer project-local vue-language-server via npx
       ((and je/vue-prefer-project-volar
             (executable-find "npx")
             ;; only try project-local when there's a Node project
             (file-exists-p (expand-file-name "package.json" root)))
        '("npx" "--no-install" "vue-language-server" "--stdio"))

       ;; Fallback: global install
       ((executable-find "vue-language-server")
        '("vue-language-server" "--stdio"))

       ;; Last resort: try npx anyway (will error nicely in *EGLOT* / messages)
       (t
        '("npx" "--no-install" "vue-language-server" "--stdio")))))

  ;; Tell Eglot what to use for vue-mode
  (setf (alist-get 'vue-mode eglot-server-programs) #'je/vue--volar-command))

(defun je/vue-setup ()
  "Setup Vue buffer: start Eglot + enable async format-on-save."
  (eglot-ensure)
  (add-hook 'after-save-hook #'je/vue--maybe-format-on-save nil t))

(add-hook 'vue-mode-hook #'je/vue-setup)

(provide 'core-vue)
;;; core-vue.el ends here
