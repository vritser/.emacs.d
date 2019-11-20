(use-package xah-fly-keys
  :ensure t
  :init (setq xah-fly-use-control-key nil)
  :config
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate))

;; (require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)




(defun xah-fly-insert-mode-init ()
  "Set insertion mode keys"
  (interactive)
  ;; (setq xah-fly-key-map (make-sparse-keymap))
  ;; (setq xah-fly-key-map (make-keymap))

  (xah-fly--define-keys
   xah-fly-key-map
   '(

     ("SPC" . nil)
     ;; ("SPC" . xah-fly-space-key)
     ("DEL" . nil)

     ("'" . nil)
     ("," . nil)
     ("-" . nil)
     ("." . nil)
     ("/" . nil)
     (";" . nil)
     ("=" . nil)
     ("[" . nil)
     ("\\" . nil)
     ("]" . nil)
     ("`" . nil)
     ("~" . nil)

     ;; ("#" . nil)
     ;; ("$" . nil)

     ("1" . nil)
     ("2" . nil)
     ("3" . nil)
     ("4" . nil)
     ("5" . nil)
     ("6" . nil)
     ("7" . nil)
     ("8" . nil)
     ("9" . nil)
     ("0" . nil)

     ("a" . nil)
     ("b" . nil)
     ("c" . nil)
     ("d" . nil)
     ("e" . nil)
     ("f" . nil)
     ("g" . nil)
     ("h" . nil)
     ("i" . nil)
     ("j" . nil)
     ("k" . nil)
     ("l" . nil)
     ("m" . nil)
     ("n" . nil)
     ("o" . nil)
     ("p" . nil)
     ("q" . nil)
     ("r" . nil)
     ("s" . nil)
     ("t" . nil)
     ("u" . nil)
     ("v" . nil)
     ("w" . nil)
     ("x" . nil)
     ("y" . nil)
     ("z" . nil)

     ;;
     ))


  (define-key xah-fly-key-map (kbd "s-c") 'xah-copy-line-or-region)
  (define-key xah-fly-key-map (kbd "s-v") 'xah-paste-or-paste-previous)
  (define-key xah-fly-key-map (kbd "s-x") 'xah-cut-line-or-region)

  (progn
    (setq xah-fly-insert-state-q t )
    (modify-all-frames-parameters (list (cons 'cursor-type 'bar))))

  (setq mode-line-front-space "I")
  (force-mode-line-update)

  ;;
  )

;; xah fly key command mode init
(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version 2017-01-21"
  (interactive)
  (xah-fly--define-keys
   xah-fly-key-map
   '(
     ("~" . nil)
     (":" . nil)

     ("SPC" . xah-fly-leader-key-map)
     ("DEL" . xah-fly-leader-key-map)

     ("'" . xah-reformat-lines)
     ("," . xah-shrink-whitespaces)
     ("-" . xah-cycle-hyphen-underscore-space)
     ("." . xah-backward-kill-word)
     (";" . xah-comment-dwim)
     ("/" . hippie-expand)
     ("\\" . nil)
     ;; ("=" . xah-forward-equal-sign)
     ("[" . xah-backward-punct )
     ("]" . xah-forward-punct)
     ("`" . other-frame)

     ;; ("#" . xah-backward-quote)
     ;; ("$" . xah-forward-punct)

     ("1" . xah-extend-selection)
     ("2" . xah-select-line)
     ("3" . delete-other-windows)
     ("4" . split-window-below)
     ("5" . delete-char)
     ("6" . xah-select-block)
     ("7" . xah-select-line)
     ("8" . xah-extend-selection)
     ("9" . xah-select-text-in-quote)
     ("0" . xah-pop-local-mark-ring)

     ("a" . execute-extended-command)
     ("b" . (lambda () (interactive) (switch-to-buffer (previous-buffer))))
     ("c" . previous-line)
     ("d" . xah-beginning-of-line-or-block)
     ("e" . xah-delete-backward-char-or-bracket-text)
     ("f" . undo)
     ("g" . backward-word)
     ("h" . backward-char)
     ("i" . xah-delete-current-text-block)
     ("j" . xah-copy-line-or-region)
     ("k" . xah-paste-or-paste-previous)
     ;; ("l" . xah-fly-insert-mode-activate-space-before)
     ("l" . xah-insert-space-before)
     ("m" . xah-backward-left-bracket)
     ("n" . forward-char)
     ("o" . open-line)
     ("p" . xah-kill-word)
     ("q" . xah-cut-line-or-region)
     ("r" . forward-word)
     ("s" . xah-end-of-line-or-block)
     ("t" . next-line)
     ("u" . xah-fly-insert-mode-activate)
     ("v" . xah-forward-right-bracket)
     ("w" . xah-next-window-or-frame)
     ("x" . xah-toggle-letter-case)
     ("y" . set-mark-command)
     ("z" . xah-goto-matching-bracket)))

  (define-key xah-fly-key-map (kbd (xah-fly--key-char "a"))
    (cond ((fboundp 'smex) 'smex)
	  ((fboundp 'helm-M-x) 'helm-M-x)
	  ((fboundp 'counsel-M-x) 'counsel-M-x)
	  (t 'execute-extended-command)))

  ;; (when xah-fly-swapped-1-8-and-2-7-p
  ;;     (xah-fly--define-keys
  ;;      xah-fly-key-map
  ;;      '(
  ;;        ("8" . pop-global-mark)
  ;;        ("7" . xah-pop-local-mark-ring)
  ;;        ("2" . xah-select-line)
  ;;        ("1" . xah-extend-selection))))

  (progn
    (setq xah-fly-insert-state-q nil )
    (modify-all-frames-parameters (list (cons 'cursor-type 'box))))

  (setq mode-line-front-space "C")
  (force-mode-line-update)

  ;;
  )


(provide 'init-xah-fly-keys)
