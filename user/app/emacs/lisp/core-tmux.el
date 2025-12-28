;; core-tmux.el --- tmux like experience for JoyEmacs -*- lexical-binding: t; -*-

(defun my/tmux-split-vertical ()
 "split a new buffer on the right with a vterm open"
 (interactive)
   (split-window-right nil nil)
   (other-window 1)
   (multi-vterm))

(defun my/tmux-split-horizontal ()
 "split a new buffer on the right with a vterm open"
 (interactive)
   (split-window-below nil nil)
   (other-window 1)
   (multi-vterm))

(defun my/tmux-pane-close ()
  "delete the window"
  (interactive)
  (if (one-window-p)
      (message "can't delete the last window")
  (delete-window)))

(defun my/tmux-pane-kill ()
  "kill the buffer containing the vterm"
  (interactive)
  (kill-buffer))

(defun my/tmux-pane-kill-close ()
  "kill the buffer containing the vterm and close the window"
  (interactive)
    (if (eq major-mode 'vterm-mode)
	(progn
	  (kill-buffer)
	  (when (window-live-p (selected-window))
	    (if (one-window-p)
		(message "can't delete the last window")
	  (delete-window))))
      (message "not a vterm")))

(defvar tmux-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'my/tmux-split-vertical)
    (define-key map (kbd "m") #'my/tmux-split-horizontal)
    (define-key map (kbd "k") #'my/tmux-pane-kill)
    (define-key map (kbd "c") #'my/tmux-pane-close)
    (define-key map (kbd "a") #'my/tmux-pane-kill-close)
    map))

(define-minor-mode tmux-mode
  "toggle tmux mode."
  :lighter "tmux"
  :init-value nil
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") tmux-prefix-map)
   map))
(define-globalized-minor-mode tmux-global-mode
  tmux-mode
  (lambda () (tmux-mode 1)))

(tmux-global-mode 1)

(with-eval-after-load 'vterm
  (add-to-list 'vterm-keymap-exceptions "C-a"))
(provide 'core-tmux)
