;;; core-lsp.el --- Eglot centralized -*- lexical-binding: t; -*-

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((python-mode         . eglot-ensure)
         (nix-mode            . eglot-ensure)
         (js-mode             . eglot-ensure)
         (typescript-mode     . eglot-ensure)
         (vue-mode            . eglot-ensure)
         (c-mode              . eglot-ensure)
         (c++-mode            . eglot-ensure)
         (zig-mode            . eglot-ensure)
         (haskell-mode        . eglot-ensure))
  :config
  ;; Prefer explicit server commands (Nix provides binaries)
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives
                     '("basedpyright-langserver" "--stdio")
                     '("pyright-langserver" "--stdio"))))

  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil")))

  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode)
                 . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))

  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls")))

  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

  ;; save hook
  (defun joy/nix-format-buffer ()
    (when (derived-mode-p 'nix-mode)
      (let ((cmd "alejandra --quiet"))
        (shell-command-on-region (point-min) (point-max) cmd t t))))

  (add-hook 'before-save-hook #'joy/nix-format-buffer)

  ;; Format on save only if server supports it
  (defun joy/eglot-format-on-save ()
    (add-hook 'before-save-hook
              (lambda ()
                (when (and (bound-and-true-p eglot-managed-mode)
                           (eglot-current-server)
                           (eglot--server-capable :documentFormattingProvider))
                  (ignore-errors (eglot-format-buffer))))
              nil t))

  (add-hook 'eglot-managed-mode-hook #'joy/eglot-format-on-save))

(use-package consult-eglot
  :after (eglot consult)
  :bind (("C-c e s" . consult-eglot-symbols)
         ("C-c e d" . consult-eglot-diagnostics)))

(provide 'core-lsp)
;;; core-lsp.el ends here
