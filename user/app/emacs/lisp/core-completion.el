;;; core-completion.el --- Completion stack -*- lexical-binding: t; -*-

;; Minibuffer completion
(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-c p s" . consult-ripgrep)
         ("C-c p f" . consult-find)
         ("C-c b" . consult-buffer)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

;; In-buffer completion
(use-package corfu
  :init (global-corfu-mode 1)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t))

;; Add real completion sources
(use-package cape
  :init
  ;; Files / paths
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Buffer words
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Language keywords
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; Symbols
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(provide 'core-completion)
;;; core-completion.el ends here
