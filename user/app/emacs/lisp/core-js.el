;;; core-js.el --- JavaScript / TypeScript -*- lexical-binding: t; -*-

;; JS (ts-mode via remap in core-treesitter)
(use-package js
  :config
  (setq js-indent-level 2))

;; TypeScript (ts-mode if Emacs 29+)
;; If you don't have typescript-ts-mode, Emacs will ignore it harmlessly.
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode)))

(provide 'core-js)
;;; core-js.el ends here
