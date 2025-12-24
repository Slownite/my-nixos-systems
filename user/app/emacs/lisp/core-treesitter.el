;;; core-treesitter.el --- Tree-sitter setup -*- lexical-binding: t; -*-

;; Make Emacs prefer *-ts-mode when available
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)))

;; If you use nix-ts-mode (external), keep it as-is
;; It doesn't follow the *-ts-mode naming scheme, so no remap needed.

(provide 'core-treesitter)
;;; core-treesitter.el ends here
