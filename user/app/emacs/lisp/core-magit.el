;;; core-magit.el --- magit default -*- lexical-binding: t; -*-
 (use-package magit
  :ensure nil                 ;; Nix manages installation
  :commands (magit-status)    ;; autoload entrypoint
  :init
  ;; Optional: if you want magit available quickly via a global key
  ;; (global-set-key (kbd "C-x g") #'magit-status)
  :custom
  (magit-commit-show-diff t)
  (magit-diff-refine-hunk 'all)
  :bind
  (:map magit-status-mode-map
        ;; Atomic prefix under "A"
        ("A" . nil)

        ;; Staging precision
        ("A s" . magit-stage)           ; stage at point (file/hunk/etc.)
        ("A u" . magit-unstage)         ; unstage at point
        ("A S" . magit-stage-modified)  ; stage all modified (sparingly)
        ("A U" . magit-unstage-all)     ; unstage everything

        ;; Commit flow
        ("A c" . magit-commit-create)   ; commit
        ("A a" . magit-commit-amend)    ; amend + edit msg
        ("A e" . magit-commit-extend)   ; amend w/o editing msg
        ("A r" . magit-commit-reword)   ; reword last commit

        ;; Review helpers
        ("A f" . magit-log-buffer-file)
        ("A l" . magit-log-current)

        ;; Careful: destructive discard
        ("A k" . magit-discard))

  :bind
  (:map magit-hunk-section-map
        ;; Hunk-level “atomic muscle memory”
        ("h" . magit-stage)
        ("H" . magit-unstage)
        ("v" . magit-diff-visit-file)
        ("x" . magit-discard))

  :bind
  (:map magit-file-section-map
        ;; File-level discard
        ("x" . magit-discard)))
 
(provide 'core-magit)
