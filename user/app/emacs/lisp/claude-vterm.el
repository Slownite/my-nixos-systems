;;; claude-vterm.el --- Claude Code in vterm using project.el -*- lexical-binding: t; -*-

(require 'project)

(defun joy/claude-project-root ()
  "Return the current project root using built-in project.el, or `default-directory`."
  (if-let ((proj (project-current nil)))
      (car (project-roots proj))
    default-directory))

(defun joy/claude--buffer-name (root)
  "Return the Claude vterm buffer name for ROOT."
  (format "*claude:%s*"
          (file-name-nondirectory (directory-file-name root))))

(defun joy/claude-code (&optional new)
  "Open (or reuse) a vterm running Claude in the project root.

With prefix arg NEW (C-u), always create a fresh buffer."
  (interactive "P")
  (unless (fboundp 'vterm)
    (user-error "vterm is not available"))

  (let* ((root (file-name-as-directory (joy/claude-project-root)))
         (name (joy/claude--buffer-name root))
         (buf  (and (not new) (get-buffer name))))
    ;; Create buffer if needed
    (unless (and buf (buffer-live-p buf))
      (setq buf
            (with-current-buffer (vterm (generate-new-buffer-name name))
              (setq default-directory root)
              (setq-local joy/claude-buffer t)
              (current-buffer))))

    (pop-to-buffer buf)

    ;; Start Claude only if this looks like a fresh session (simple heuristic).
    ;; If you prefer ALWAYS starting a new `claude`, delete the `when` and always send.
    (with-current-buffer buf
      (when (and (boundp 'vterm--process)
                 (process-live-p vterm--process)
                 (< (buffer-size) 200))
        (vterm-send-string "claude")
        (vterm-send-return)))))

(defun joy/claude-quit (&optional force)
  "Quit Claude session by killing the current vterm buffer.

With prefix arg FORCE (C-u), do not prompt.
Killing the vterm buffer also kills its underlying process."
  (interactive "P")
  (unless (derived-mode-p 'vterm-mode)
    (user-error "Not in a vterm buffer"))
  (when (or force (yes-or-no-p "Kill this Claude session? "))
    (kill-buffer (current-buffer))))

(defun joy/claude-switch ()
  "Switch to an existing Claude buffer.

Uses Consult if available; otherwise falls back to `completing-read`."
  (interactive)
  (let* ((names (mapcar #'buffer-name
                        (seq-filter (lambda (b)
                                      (string-prefix-p "*claude:" (buffer-name b)))
                                    (buffer-list)))))
    (unless names
      (user-error "No Claude buffers found"))
    (if (fboundp 'consult--read)
        (pop-to-buffer (consult--read names :prompt "Claude buffer: "))
      (pop-to-buffer (completing-read "Claude buffer: " names nil t)))))

;; ---- Keybindings ----
;; Launch / reuse Claude for this project
(global-set-key (kbd "C-c a") #'joy/claude-code)
;; Switch between existing Claude buffers
(global-set-key (kbd "C-c A") #'joy/claude-switch)

;; In vterm: 'q' to quit Claude buffer (asks), 'Q' to force quit (no prompt)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "q") #'joy/claude-quit)
  (define-key vterm-mode-map (kbd "Q") (lambda () (interactive) (joy/claude-quit t))))

(provide 'claude-vterm)
;;; joy-claude-vterm.el ends here
