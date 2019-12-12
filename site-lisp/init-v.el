


(setq make-backup-files nil)
(setq auto-save-default nil)
(setq help-window-select t)

;; Let help window display at bottom
(add-to-list 'display-buffer-alist
             `("*Help*"
               (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
             `("*Buffer List*"
               (display-buffer-same-window)))

(setq-default tab-width 4
	      indent-tabs-mode nil)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip "\t\f\v"))



(defun v-newline ()
  "newline and auto indent when match pairs."
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
  (indent-for-tab-command)))


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
  (if (looking-at ".+")
      (progn
        (move-beginning-of-line nil)
        (newline)
        (forward-line -1))
    (save-excursion
      (newline)))

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

(defun v-dired-open-dir ()
  "Goto current directory"
  (interactive)
  (dired "."))

(global-set-key (kbd "C-j") 'v-smart-open-line)
(global-set-key (kbd "M-j") 'v-down-2-lines)
(global-set-key (kbd "RET") 'v-newline)
(global-set-key (kbd "C-o") 'v-open-line-indent)
(global-set-key (kbd "C-a") 'back-to-indentation)

(global-set-key (kbd "C-x k") '(lambda ()
                                 "Kill current buffer"
                                 (interactive)
                                 (kill-buffer (buffer-name))))

;; dired mode key map
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-k") 'v-dired-up-directory))
(global-set-key (kbd "C-x d") 'v-dired-open-dir)
(global-set-key (kbd "C-x C-d") 'v-dired-open-dir)

;; imenu keymap
(global-set-key (kbd "M-m") 'counsel-imenu)

;; avy keymap
(global-set-key (kbd "C-c j") 'avy-goto-char-timer)

;; snails
(global-set-key (kbd "C-c C-s") 'snails)

;; Projectile configuration shortkey
(global-set-key (kbd "C-x p p") 'projectile-switch-project)
(global-set-key (kbd "C-x p f") 'projectile-find-file)
(global-set-key (kbd "C-x p b") 'projectile-switch-to-buffer)

;; (define-key dired-mode-map (kbd "C-k") 'v-dired-up-directory)
;; (define-key dired-mode-map (kbd "C-f") 'my-dired-find-file)

;; (global-set-key (kbd "TAB") 'indent-for-tab-command)

;; projectile key map
;;(define-prefix-command 'v-p-map)
;;(define-key v-p-map (kbd "f") 'projectile-find-file)
;;(define-key v-p-map (kbd "p") 'projectile-switch-project)
;;(define-key v-p-map (kbd "b") 'projectile-switch-to-buffer)

;;(define-key xah-fly-leader-key-map (kbd "p") v-p-map)

;; remap xah-fly-h-keymap
;;(define-key xah-fly-h-keymap (kbd "j") 'avy-goto-char-timer)
;;(define-key xah-fly-h-keymap (kbd "i") 'counsel-imenu)

;; xah i map  remap
;;(define-key xah-fly-c-keymap (kbd "j") 'counsel-recentf)







(defun v-backward-paragraph ()
  "Move backward to  beginning of paragraph"
  (interactive "p")
  (forward-paragraph prefix-numeric-value))

;; (global-set-key (kbd "C-[") 'v-backward-paragraph)

(global-set-key (kbd "M-i") 'beginning-of-defun)
(global-set-key (kbd "M-k") 'end-of-defun)



(provide 'init-v)
