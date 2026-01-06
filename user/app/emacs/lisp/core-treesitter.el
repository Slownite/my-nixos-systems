;;; core-treesitter.el --- Tree-sitter setup -*- lexical-binding: t; -*-

;; Make Emacs prefer *-ts-mode when available
;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)
;;         (js-mode . js-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (css-mode . css-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (c++-mode . c++-ts-mode)
;;         (bash-mode . bash-ts-mode)
;;         (sh-mode . bash-ts-mode)
;;       (haskell-mode . haskell-ts-mode)))

;; nix-ts-mode and zig-ts-mode should work automatically via their packages

(provide 'core-treesitter)
;;; core-treesitter.el ends here
