;;; core-org.el --- Org + Org-roam -*- lexical-binding: t; -*-

;; Base org
(setq org-directory (expand-file-name "~/Documents/org"))

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

;; Org-roam
(use-package org-roam
  :demand t
  :custom
  (org-roam-directory (file-truename (expand-file-name "~/Documents/RoamNotes")))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies (classic binding)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Display nodes nicely in completion
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  ;; ✅ Dailies directory
  (setq org-roam-dailies-directory "daily/")

  ;; ✅ IMPORTANT: daily template with #+title to avoid "no title, skipping"
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)))

  ;; Optional: standard capture template for regular notes (also adds title)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)))

  (org-roam-db-autosync-mode 1)
  (require 'org-roam-protocol))

(provide 'core-org)
;;; core-org.el ends here
