;;; core-leader.el --- Doom-like leader key -*- lexical-binding: t; -*-

(use-package general
  :config
  :after evil
  ;; Doom-style leader
  (general-create-definer my/leader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Local leader (SPC m …)
  (general-create-definer my/localleader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "C-SPC m")

  ;; =====================
  ;; Top-level
  ;; =====================
  (my/leader
    "SPC" '(execute-extended-command :which-key "M-x"))

  ;; =====================
  ;; Files
  ;; =====================
  (my/leader
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(consult-recent-file :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")
    "fS" '(write-file :which-key "save as")
    "fd" '(delete-file :which-key "delete file"))

  ;; =====================
  ;; Buffers
  ;; =====================
  (my/leader
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(revert-buffer :which-key "revert buffer"))

  ;; =====================
  ;; Windows
  ;; =====================
  (my/leader
    "w"  '(:ignore t :which-key "windows")
    "wv" '(split-window-right :which-key "vertical split")
    "ws" '(split-window-below :which-key "horizontal split")
    "wd" '(delete-window :which-key "delete window")
    "wD" '(delete-other-windows :which-key "delete others")
    "wh" '(windmove-left :which-key "left")
    "wj" '(windmove-down :which-key "down")
    "wk" '(windmove-up :which-key "up")
    "wl" '(windmove-right :which-key "right"))

  ;; =====================
  ;; Project
  ;; =====================
  (my/leader
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "ps" '(consult-ripgrep :which-key "search")
    "pb" '(project-switch-to-buffer :which-key "buffers")
    "pt" '(treemacs :which-key "treemacs"))

  ;; =====================
  ;; Search / Telescope
  ;; =====================
  (my/leader
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search line")
    "sf" '(consult-find :which-key "find files")
    "sg" '(consult-ripgrep :which-key "live grep")
    "sb" '(consult-buffer :which-key "buffers")
    "sh" '(consult-history :which-key "history")
    "si" '(consult-imenu :which-key "symbols"))

  ;; Quick grep
  (my/leader
    "/" '(consult-ripgrep :which-key "ripgrep"))

  ;; =====================
  ;; Git
  ;; =====================
  (my/leader
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")
    "gb" '(magit-branch :which-key "branch")
    "gc" '(magit-commit :which-key "commit"))

  ;; =====================
  ;; LSP
  ;; =====================
  (my/leader
    "l"  '(:ignore t :which-key "lsp")
    "ld" '(consult-eglot-diagnostics :which-key "diagnostics")
    "ls" '(consult-eglot-symbols :which-key "symbols")
    "lr" '(eglot-rename :which-key "rename")
    "la" '(eglot-code-actions :which-key "code actions")
    "lf" '(eglot-format-buffer :which-key "format"))

  ;; =====================
  ;; Terminal
  ;; =====================
  (my/leader
    "t"  '(:ignore t :which-key "terminal")
    "te" '(eshell :which-key "eshell")
    "tE" '(project-eshell :which-key "project eshell")
    "tv" '(multi-vterm :which-key "vterm"))

  ;; =====================
  ;; Org
  ;; =====================
  (my/leader
    "o"  '(:ignore t :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture")
    "ot" '(org-todo-list :which-key "todo list")
    "os" '(org-store-link :which-key "store link")
    "oi" '(org-insert-link :which-key "insert link")
    "of" '(org-open-at-point :which-key "open link"))

;; Doom-like binding: SPC h r r (help -> reload -> reload)
(my/leader
  "h"  '(:ignore t :which-key "help")
  "hr" '(:ignore t :which-key "reload")
  "hrr" '(my/reload-config :which-key "reload config")))
(provide 'core-leader)
;;; core-leader.el ends here
