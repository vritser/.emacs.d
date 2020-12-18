;;; init-v.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


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

;; (add-to-list 'display-buffer-alist
;;              '("^CAPTURE.*org$" . ((display-buffer-reuse-window
;;                                     display-buffer-pop-up-window)
;;                                    . ((inhibit-duplicate-buffer . t)
;;                                       (inhibit-same-window      . t)))))

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
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (newline-and-indent)))

(defun v-smart-open-line ()
  "Smart open line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun v-down-2-lines ()
  "Jump out curly."
  (interactive)
  (forward-line)
  (move-end-of-line nil)
  (newline-and-indent))

(defun v-open-line-indent ()
  "Open line then indent."
  (interactive)
  (if (looking-at ".+")
      (progn
        (move-beginning-of-line nil)
        (newline)
        (forward-line -1))
    (save-excursion
      (newline)))

  (indent-according-to-mode))

(defun v-backward-paragraph ()
  "Move backward to  beginning of paragraph."
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
(defalias 'rg 'counsel-rg)


;; Keybindings
(global-set-key (kbd "M-i") 'beginning-of-defun)
(global-set-key (kbd "M-k") 'end-of-defun)

(global-set-key (kbd "C-j") 'v-smart-open-line)
;; (global-set-key (kbd "C-m") 'v-down-2-lines)
(global-set-key (kbd "RET") 'v-newline)
;; (global-set-key (kbd "C-o") 'v-open-line-indent)
;; (global-set-key (kbd "C-a") 'back-to-indentation)

;; (global-set-key (kbd "C-x d") 'v-dired-open-dir)
;; (global-set-key (kbd "C-x C-d") 'v-dired-open-dir)

(global-set-key [C-tab] '(lambda ()
                                 (interactive)
                                 (switch-to-buffer (other-buffer))))
                                 ;;(switch-to-buffer (other-buffer (current-buffer) 1))))

(global-set-key (kbd "M-j") 'capitalize-dwim)
;; imenu keymap
(global-set-key (kbd "M-m") 'counsel-imenu)

;; avy keymap
(global-set-key (kbd "M-c") 'avy-goto-char-timer)
(global-set-key (kbd "M-l") 'avy-goto-line)

(global-set-key (kbd "s-g") 'magit-status)
;; snails
;; (global-set-key (kbd "C-c C-s") 'snails)

;; Projectile configuration shortkey
;; (global-set-key (kbd "C-x p p") 'projectile-switch-project)
;; (global-set-key (kbd "C-x p f") 'projectile-find-file)
;; (global-set-key (kbd "C-x p b") 'projectile-switch-to-buffer)


;; erc proxy
;; (setq socks-noproxy '("localhost"))
;; (require 'socks)
;; (setq erc-server-connect-function 'socks-open-network-stream)
;; (setq url-gateway-method 'socks)
;; (setq socks-server (list "My socks server" "127.0.0.1" 1086 5))


(provide 'init-v)
;;; init-v.el ends here
;; (setq comp-async-jobs-number 8)
;; (native-compile-async "~/.emacs.d/elpa" t t)
