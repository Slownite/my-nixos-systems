;;; core-org.el --- Org + Org-roam -*- lexical-binding: t; -*-
;; Base org
(setq org-directory (expand-file-name "~/Documents/org"))
;; Ensure org-directory exists
(unless (file-directory-p org-directory)
  (make-directory org-directory t))

(use-package org
  :config
  (setq org-ellipsis "…"
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-agenda-tags-column 0))

;; Pretty org
(use-package org-modern
  :demand t
  :config
  (global-org-modern-mode 1))

(provide 'core-org)
;;; core-org.el ends here
