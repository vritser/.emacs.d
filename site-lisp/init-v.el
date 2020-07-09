(setq make-backup-files nil)
(setq auto-save-default nil)
(setq help-window-select t)
(setq-default tab-width 2
							indent-tabs-mode nil)

;; Let help window display at bottom
(add-to-list 'display-buffer-alist
             `("*Help*"
               (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
             `("*Buffer List*"
               (display-buffer-same-window)))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\n\r\f\v"))

(defun v-newline ()
  "Newline and auto indent when match pairs."
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

;; dwim
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

(defun v-backward-paragraph ()
  "Move backward to  beginning of paragraph"
  (interactive "p")
  (forward-paragraph prefix-numeric-value))

(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; (set-frame-name "EDIT")

;; (progn
;;   (make-frame '((name . "TERM")))
;;   (select-frame-by-name "EDIT")
;;   (vterm))

;; (assq 'name (frame-parameters (selected-frame)))
;; (global-set-key (kbd "C-`") (lambda () (interactive) (select-frame-by-name "TERM")))

(defalias 'fmt 'lsp-format-buffer)
(defalias 'lml 'list-matching-lines)

;; Keybindings
(global-set-key (kbd "M-i") 'beginning-of-defun)
(global-set-key (kbd "M-k") 'end-of-defun)

(global-set-key (kbd "C-j") 'v-smart-open-line)
;; (global-set-key (kbd "C-m") 'v-down-2-lines)
;; (global-set-key (kbd "RET") 'v-newline)
;; (global-set-key (kbd "C-o") 'v-open-line-indent)
;; (global-set-key (kbd "C-a") 'back-to-indentation)

;; (global-set-key (kbd "C-x d") 'v-dired-open-dir)
;; (global-set-key (kbd "C-x C-d") 'v-dired-open-dir)

(global-set-key [C-tab] '(lambda ()
                                 (interactive)
                                 (switch-to-buffer (other-buffer))))
                                 ;;(switch-to-buffer (other-buffer (current-buffer) 1))))
;; imenu keymap
(global-set-key (kbd "M-m") 'counsel-imenu)

;; avy keymap
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-l") 'avy-goto-line)


(global-set-key (kbd "s-g") 'magit-status)
;; snails
;; (global-set-key (kbd "C-c C-s") 'snails)

;; Projectile configuration shortkey
;; (global-set-key (kbd "C-x p p") 'projectile-switch-project)
;; (global-set-key (kbd "C-x p f") 'projectile-find-file)
;; (global-set-key (kbd "C-x p b") 'projectile-switch-to-buffer)


(provide 'init-v)
;;; init-v.el ends here
