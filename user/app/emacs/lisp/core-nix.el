;;; core-nix.el --- Nix tooling -*- lexical-binding: t; -*-

;; Nix major mode (tree-sitter)
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Direnv integration:
;; Prefer envrc (recommended). If not installed, fallback to direnv.
(cond
 ((require 'envrc nil t)
  (envrc-global-mode 1))
 ((require 'direnv nil t)
  (direnv-mode 1)))

;; Format .nix on save with alejandra (if available)
(defun joy/nix-format-buffer ()
  "Format current buffer with alejandra if available."
  (when (and (derived-mode-p 'nix-ts-mode 'nix-mode)
             (executable-find "alejandra"))
    (let ((p (point)))
      (call-process-region (point-min) (point-max) "alejandra" t t)
      (goto-char (min p (point-max))))))

(add-hook 'before-save-hook #'joy/nix-format-buffer)

(provide 'core-nix)
;;; core-nix.el ends here
