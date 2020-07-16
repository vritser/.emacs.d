;;; init-xah-fly-keys.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: extensions

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

;;; Requires:

;; must come before loading xah-fly-keys
;; (setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)

;;; Code:

(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(require 'key-chord)
;; (if (not (assq 'key-chord-mode minor-mode-alist))
;;       (setq minor-mode-alist
;; 	    (cons '(key-chord-mode " KeyC ")
;; 		  minor-mode-alist)))

(key-chord-define xah-fly-insert-map "th" 'xah-fly-command-mode-activate)
(key-chord-mode 1)

;; projectile key map
(define-prefix-command 'v-l-map)
(define-key v-l-map (kbd "f") 'projectile-find-file)
(define-key v-l-map (kbd "p") 'projectile-switch-project)
(define-key v-l-map (kbd "u") 'projectile-switch-to-buffer)

(define-key xah-fly-leader-key-map (kbd "l") v-l-map)

(defun v-fly-insert-map-hook ()
  "Custome xah fly key insert map."
  (interactive)
  (define-key xah-fly-key-map (kbd "C-r") 'v-open-line-indent)
  (define-key xah-fly-key-map (kbd "M-h") 'v-down-2-lines)
  (define-key xah-fly-key-map (kbd "C-n") 'next-line)
  (define-key xah-fly-key-map (kbd "C-a") 'beginning-of-line)
  ;; (define-key xah-fly-key-map (kbd "SPC-u") nil)

  (define-key xah-fly-insert-map (kbd "<backspace>") 'hungry-delete-backward))

(defun v-fly-command-map-hook ()
  "Custome xah fly key command map."
  (interactive)
  ;; (define-key xah-fly-key-map (kbd "g") 'magit-status)

  ;; (define-key dashboard-mode-map (kbd "p") 'dashboard-goto-projects)
  ;; (define-key dashboard-mode-map (kbd "r") 'dashboard-goto-recent-files)
  ;; (define-key dashboard-mode-map (kbd "b") 'dashboard-goto-bookmarks)

  ;; (define-key xah-fly-key-map (kbd "SPC u") 'projectile-switch-to-buffer)
  (define-key xah-fly-key-map (kbd "C-o") 'counsel-find-file)
  (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)
  (define-key xah-fly-key-map (kbd "b") 'swiper))

(add-hook 'xah-fly-insert-mode-activate-hook 'v-fly-insert-map-hook)
(add-hook 'xah-fly-command-mode-activate-hook 'v-fly-command-map-hook)


(provide 'init-xah-fly-keys)
;;; init-xah-fly-keys.el ends here
