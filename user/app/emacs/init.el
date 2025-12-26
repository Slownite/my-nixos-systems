;;; init.el --- JoyEmacs entrypoint -*- lexical-binding: t; -*-

;; Nix owns packages (no ELPA/MELPA installs)
(setq package-enable-at-startup nil)
(setq use-package-always-ensure nil)
(setq use-package-ensure-function 'ignore)

;; Startup perf
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules (order matters a bit)
(require 'core-core)
(require 'core-treesitter)
(require 'core-ui)
(require 'core-completion)
(require 'core-evil)
(require 'core-project)
(require 'core-git)
(require 'core-terminal)
(require 'core-nix)
(require 'core-org)
(require 'core-magit)
;; Language modules (lightweight, only language specifics)
(require 'core-js)
(require 'core-vue)
(require 'core-python)

;; LSP (centralized)
(require 'core-lsp)

;; Leader keys last (it binds stuff from other modules)
(require 'core-leader)

(provide 'init)
;;; init.el ends here
