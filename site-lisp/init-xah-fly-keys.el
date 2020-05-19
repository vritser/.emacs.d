;; must come before loading xah-fly-keys
;; (setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)

(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(require 'key-chord)

(if (not (assq 'key-chord-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(key-chord-mode " KeyC ")
		  minor-mode-alist)))

(key-chord-define xah-fly-insert-map "th" 'xah-fly-command-mode-activate)
(key-chord-mode 1)

;; (define-key xah-fly-key-map (kbd "<escape>") 'xah-fly-command-mode-activate)

(defun v-fly-insert-map-hook ()
  (interactive)
  (define-key xah-fly-key-map (kbd "C-r") 'v-open-line-indent)
  (define-key xah-fly-key-map (kbd "M-h") 'v-down-2-lines)
  (define-key xah-fly-key-map (kbd "s-h") 'backward-char)
  (define-key xah-fly-key-map (kbd "s-n") 'forward-char)
  (define-key xah-fly-key-map (kbd "s-c") 'previous-line)
  (define-key xah-fly-key-map (kbd "s-t") 'next-line)
  (define-key xah-fly-key-map (kbd "C-n") 'next-line)

  (define-key xah-fly-insert-map (kbd "<backspace>") 'hungry-delete-backward)
  )

(add-hook 'xah-fly-insert-mode-activate-hook 'v-fly-insert-map-hook)

(defun v-fly-command-map-hook ()
  (interactive)
  (define-key xah-fly-key-map (kbd "C-o") 'counsel-find-file)
  (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)

  (define-key dashboard-mode-map (kbd "p") 'dashboard-goto-projects)
  (define-key dashboard-mode-map (kbd "r") 'dashboard-goto-recent-files)
  (define-key dashboard-mode-map (kbd "b") 'dashboard-goto-bookmarks)
  )

(add-hook 'xah-fly-command-mode-activate-hook 'v-fly-command-map-hook)

(provide 'init-xah-fly-keys)
