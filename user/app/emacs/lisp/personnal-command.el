;;; personnal-command.el --- personnal command setup -*- lexical-binding: t; -*-
(defun personnal-command/reload-home-manager ()
  "reload home manager and load the init.el"
  (interactive)
  (when (yes-or-no-p "Reload home-manager? ")
    (when (executable-find "home-manager")
      (let ((buf (get-buffer-create "*Home Manager*")))
        (with-current-buffer buf
          (erase-buffer))
        ;; EITHER: call-process directly (simpler)
        (if (= (call-process "home-manager" nil buf t
                             "switch" "--flake" "./#sam")
               0)
            (load-file "~/my-nixos-systems/user/app/emacs/init.el")
          (display-buffer buf))))))
(provide 'personnal-command)
