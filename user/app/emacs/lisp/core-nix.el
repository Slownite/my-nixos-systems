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

;; Format .nix on save with alejandra (if available).
;; alejandra prints status banners to stderr (unless --quiet) and emits parse
;; errors to stderr on invalid input. The previous version sent BOTH stdout and
;; stderr into the buffer, so those messages were written into the .nix file --
;; corrupting it at the end and, once broken, on every subsequent save. We now
;; (a) always pass --quiet, (b) capture stdout separately from stderr, and
;; (c) replace the buffer only on a clean (exit 0) run, so alejandra's output
;; can never leak into the file.
(defun joy/nix-format-buffer ()
  "Format the current Nix buffer with alejandra if available.
Replace the buffer contents only on a successful run; alejandra's
stderr/diagnostics are never inserted into the buffer."
  (when (and (derived-mode-p 'nix-ts-mode 'nix-mode)
             (executable-find "alejandra"))
    (let ((pos (point))
          (outbuf (generate-new-buffer " *alejandra-stdout*"))
          (errfile (make-temp-file "alejandra-stderr")))
      (unwind-protect
          (let ((status (call-process-region
                         (point-min) (point-max) "alejandra"
                         nil (list outbuf errfile) nil
                         "--quiet" "-")))
            (if (and (eq status 0) (> (buffer-size outbuf) 0))
                (let ((formatted (with-current-buffer outbuf (buffer-string))))
                  (unless (string= formatted (buffer-string))
                    (erase-buffer)
                    (insert formatted)
                    (goto-char (min pos (point-max)))))
              (message "alejandra: formatting skipped (exit %s)" status)))
        (kill-buffer outbuf)
        (delete-file errfile)))))

(add-hook 'before-save-hook #'joy/nix-format-buffer)

(provide 'core-nix)
;;; core-nix.el ends here
