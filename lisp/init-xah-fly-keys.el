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
;; (require 'xah-fly-keys)

;;; Code:

;; (setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys t)

(use-package key-chord
  :ensure t
  :init
  (key-chord-mode t)
  :config
  (key-chord-define xah-fly-insert-map "th" 'xah-fly-command-mode-activate)
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate))

(defun yuf/xah-fly-insert-map-hook ()
  "Custome xah fly key insert map."
  (define-key xah-fly-insert-map (kbd "C-r") 'v-open-line-indent)
  (define-key xah-fly-insert-map (kbd "M-h") 'v-down-2-lines)
  (define-key xah-fly-insert-map (kbd "C-n") 'next-line)
  (define-key xah-fly-insert-map (kbd "C-a") 'beginning-of-line)
  (define-key xah-fly-insert-map (kbd "<backspace>") 'hungry-delete-backward))

(defun yuf/xah-fly-command-map-hook ()
  "Custome xah fly key command map."
  (define-key xah-fly-command-map (kbd "C-o") 'counsel-find-file)
  (define-key xah-fly-command-map (kbd "a") 'counsel-M-x)
  ;; (define-key xah-fly-command-map (kbd "C-n") 'xah-new-empty-buffer)
  (define-key xah-fly-command-map (kbd "C-a") 'beginning-of-line)
  (define-key xah-fly-command-map (kbd "b") 'swiper))

(with-eval-after-load 'xah-fly-keys
  (add-hook 'xah-fly-insert-mode-activate-hook 'yuf/xah-fly-insert-map-hook)
  (add-hook 'xah-fly-command-mode-activate-hook 'yuf/xah-fly-command-map-hook)

  ;; Keybindings

  ;; global key map
  (global-set-key (kbd "s-N") 'make-frame)
  (global-set-key (kbd "s-n") 'xah-new-empty-buffer)

  ;; visual regexp query/replace
  ;;(define-key xah-fly-t-keymap (kbd "p") 'vr/replace)
  (define-key xah-fly-leader-key-map (kbd "p") 'vr/query-replace)

  ;; projectile key map
  (define-prefix-command 'yuf-l-map)
  (define-key yuf-l-map (kbd "f") 'projectile-find-file)
  (define-key yuf-l-map (kbd "p") 'projectile-switch-project)
  (define-key yuf-l-map (kbd "u") 'projectile-switch-to-buffer)
  (define-key xah-fly-leader-key-map (kbd "l") yuf-l-map))

(provide 'init-xah-fly-keys)
;;; init-xah-fly-keys.el ends here
