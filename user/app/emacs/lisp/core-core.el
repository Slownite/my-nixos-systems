;;; core-core.el --- Core defaults (perf + hygiene) -*- lexical-binding: t; -*-

;; Keep history for minibuffer + commands
(savehist-mode 1)

;; Sensible defaults
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-processes nil
      sentence-end-double-space nil)

;; Backups / autosaves (no-littering will redirect most of this)
(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 10
      kept-old-versions 2
      auto-save-default t
      create-lockfiles nil)

;; Clean filesystem
(use-package no-littering
  :init
  ;; If you prefer more aggressive tidying, uncomment:
  ;; (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory))
  ;; (setq no-littering-var-directory (expand-file-name "var/" user-emacs-directory))
  )

;; Better GC behavior (especially with LSP)
(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

;; Helpful help buffers (optional: keep if installed)
(when (require 'helpful nil t)
  (global-set-key (kbd "C-h f") #'helpful-function)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

(provide 'core-core)
;;; core-core.el ends here
