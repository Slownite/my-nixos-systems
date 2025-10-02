;;; init.el --- JoyEmacs entrypoint -*- lexical-binding: t; -*-

;; Basic hygiene
(setq use-package-always-ensure nil) ;; Nix provides packages
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(eval-when-compile (require 'use-package))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules
(require 'core-ui)
(require 'core-evil)
(require 'core-completion)
(require 'core-git)
(require 'core-project)
(require 'core-leader)
(require 'core-org)
(require 'core-treesitter)
(require 'eglot)  
(require 'core-nix)
(require 'core-python)
(require 'core-terminal)
;; config I don't know where to put
(recentf-mode 1)
(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; ---------------------------------
;; ------macbook config specific------
(when (and (eq system-type 'darwin) window-system)
  (when (boundp 'ns-option-modifier)       (setq ns-option-modifier 'meta
                                                 ns-right-option-modifier nil))
  (when (boundp 'mac-option-modifier)      (setq mac-option-modifier 'meta
                                                 mac-right-option-modifier nil))
  (when (boundp 'ns-command-modifier)      (setq ns-command-modifier 'super))
  (when (boundp 'mac-command-modifier)     (setq mac-command-modifier 'super)))
;; ------------------------------------
(provide 'init)
;;; init.el ends here

