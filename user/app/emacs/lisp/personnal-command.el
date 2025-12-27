;;; personnal-command.el --- personnal command setup -*- lexical-binding: t; -*-

(defvar personnal-command/home-manager-process nil
  "Process handle for the currently running home-manager switch, if any.")

(defun personnal-command/abort-home-manager ()
  "Abort a running home-manager switch process (if any)."
  (interactive)
  (if (and personnal-command/home-manager-process
           (process-live-p personnal-command/home-manager-process))
      (progn
        (kill-process personnal-command/home-manager-process)
        (message "Aborting home-manager switch..."))
    (message "No running home-manager switch process.")))

(defun personnal-command/reload-home-manager ()
  "Reload home-manager asynchronously, then load init.el on success."
  (interactive)
  (when (yes-or-no-p "Reload home-manager? ")
    (if (not (executable-find "home-manager"))
        (user-error "home-manager not found in PATH")
      ;; Prevent multiple overlapping runs (easy to do by accident).
      (when (and personnal-command/home-manager-process
                 (process-live-p personnal-command/home-manager-process))
        (user-error "home-manager is already running (use `personnal-command/abort-home-manager`)"))

      (let* ((buf (get-buffer-create "*Home Manager*"))
             ;; Keep your original flake path, but make it explicit/robust:
             (default-directory (expand-file-name "./"))
             (cmd (list "home-manager" "switch" "--flake" "./#sam")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert (format "$ %s\n\n" (mapconcat #'identity cmd " ")))
          (special-mode))
        (display-buffer buf)

        (setq personnal-command/home-manager-process
              (make-process
               :name "home-manager-switch"
               :buffer buf
               :command cmd
               :noquery t
               :sentinel
               (lambda (p event)
                 (when (memq (process-status p) '(exit signal))
                   (let ((exit (process-exit-status p)))
                     ;; Clear global handle once we're done.
                     (when (eq p personnal-command/home-manager-process)
                       (setq personnal-command/home-manager-process nil))

                     (with-current-buffer (process-buffer p)
                       (read-only-mode -1)
                       (goto-char (point-max))
                       (insert (format "\n\n[home-manager] %s (exit %d)\n"
                                       (string-trim event) exit))
                       (read-only-mode 1))

                     (if (= exit 0)
                         (condition-case err
                             (progn
                               (load-file "~/my-nixos-systems/user/app/emacs/init.el")
                               (message "home-manager reload complete; init.el loaded"))
                           (error
                            (message "home-manager succeeded, but loading init.el failed: %S" err)))
                       (message "home-manager reload failed (see *Home Manager*)")))))))

        (message "Started home-manager switch... (use `personnal-command/abort-home-manager` to cancel)")))))


(defun my/reload-config ()
  "Reload init.el (and the rest of your config)."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Reloaded Emacs config."))

(defun open-my-config ()
  "open init.el"
  (interactive)
  (find-file user-init-file))

(provide 'personnal-command)
