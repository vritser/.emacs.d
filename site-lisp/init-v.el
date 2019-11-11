


(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default tab-width 4
	      indent-tabs-mode nil)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip "\t\f\v"))





(defun v-smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun v-down-2-lines ()
  (interactive)
  (next-line)
  (move-end-of-line nil)
  (newline-and-indent))

(defun v-open-line-indent ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
)

(defun my-dired-find-file ()
  "Open buffer on another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    ;; first element of attributes represents is it a folder
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun v-dired-up-directory ()
  "Goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

(global-set-key (kbd "C-j") 'v-smart-open-line)
(global-set-key (kbd "C-S-j") 'v-down-2-lines)

(define-key dired-mode-map (kbd "C-k") 'v-dired-up-directory)
(define-key dired-mode-map (kbd "C-f") 'my-dired-find-file)

;; (global-set-key (kbd "TAB") 'indent-for-tab-command)

;; projectile key map
(define-prefix-command 'v-p-map)
(define-key v-p-map (kbd "f") 'projectile-find-file)
(define-key v-p-map (kbd "p") 'projectile-switch-project)
(define-key v-p-map (kbd "b") 'projectile-switch-to-buffer)

(define-key xah-fly-leader-key-map (kbd "p") v-p-map)

;; remap xah-fly-h-keymap
(define-key xah-fly-h-keymap (kbd "j") 'avy-goto-char-timer)
(define-key xah-fly-h-keymap (kbd "i") 'counsel-imenu)

;; xah i map  remap
(define-key xah-fly-c-keymap (kbd "j") 'counsel-recentf)

(provide 'init-v)