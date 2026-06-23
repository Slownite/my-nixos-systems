;;; core-rust.el --- Rust -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-ts-mode-indent-offset 4))

(provide 'core-rust)
;;; core-rust.el ends here
