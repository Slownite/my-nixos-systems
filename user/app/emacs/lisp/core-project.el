;;; core-project.el --- Project/file tree -*- lexical-binding: t; -*-

;; 1) Teach Emacs what counts as a “project root”.
;; This helps project-current work well in uv/python repos.
(with-eval-after-load 'project
  ;; Emacs 29+: extra markers for VC projects (Git, etc.)
  (when (boundp 'project-vc-extra-root-markers)
    (dolist (marker '("pyproject.toml" "uv.lock"))
      (add-to-list 'project-vc-extra-root-markers marker))))

(use-package treemacs
  :commands (treemacs)
  :init (setq treemacs-width 32
              treemacs-show-hidden-files t))

(use-package treemacs-evil :after (treemacs evil))

(provide 'core-project)
;;; core-project.el ends here
