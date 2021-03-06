;;; init-magit.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: vc, extensions

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

(use-package magit
  :config
  (when (executable-find "cc")
    (use-package forge :demand)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(with-eval-after-load 'smerge
  (progn
    (define-key smerge-mode-map (kbd "C-p") 'smerge-prev)
    (define-key smerge-mode-map (kbd "C-n") 'smerge-next)
    (define-key smerge-mode-map (kbd "C-c u") 'smerge-keep-upper)
    (define-key smerge-mode-map (kbd "C-c o") 'smerge-keep-lower)
    (define-key smerge-mode-map (kbd "C-c a") 'smerge-keep-all)))

(provide 'init-magit)
;;; init-magit.el ends here
