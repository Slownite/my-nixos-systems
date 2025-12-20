;;; core-evil.el --- Evil mode config -*- lexical-binding: t; -*-

(use-package evil
  :init (setq evil-want-keybinding nil
              evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Comment operator (gcc, gc<motion>, gc in visual mode)
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(provide 'core-evil)
;;; core-evil.el ends here
