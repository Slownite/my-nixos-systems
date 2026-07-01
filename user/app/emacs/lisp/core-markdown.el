;;; core-markdown.el --- Markdown editing + grip preview -*- lexical-binding: t; -*-

(use-package markdown-mode
  :ensure nil
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-wiki-links t
        markdown-list-indent-width 2))

(use-package grip-mode
  :ensure nil
  :after markdown-mode
  :config
  (setq grip-update-changed-files t))

(provide 'core-markdown)
;;; core-markdown.el ends here
