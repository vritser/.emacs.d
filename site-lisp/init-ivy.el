;;; init-ivy.el ---                                  -*- lexical-binding: t; -*-

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

;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
	       (ivy-mode . counsel-mode))

  :config
  (setq ivy-initial-inputs-alist nil)

  :init
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
	      ivy-use-virtual-buffers t
	      ivy-height 10
	      ivy-count-format "%d "
	      ivy-on-del-error-function nil)

  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-plus) ; Only counsel-M-x use flx fuzzy search
          (t . ivy--regex-plus)))

  :config
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; (use-package smex)
  )


(provide 'init-ivy)
;;; init-ivy.el ends here
