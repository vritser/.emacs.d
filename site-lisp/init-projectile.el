;;; init-projectile.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: tools, extensions

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

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	projectile-sort-order 'recentf
	projectile-use-git-grep t
    projectile-globally-ignored-file-suffixes
        '(
          "blob"
          "class"
          "classpath"
          "gz"
          "iml"
          "ipr"
          "jar"
          "pyc"
          "tkj"
          "war"
          "xd"
          "zip"
          )
    projectile-globally-ignored-files '("TAGS" "*~")
    )

  :config
  (setq projectile-globally-ignored-directories
        (append (list
                 ".gradle"
                 "build"
                 "elpa"
                 "node_modules"
                 "output"
                 "reveal.js"
                 "semanticdb"
                 "target"
                 "venv"
                 )
                projectile-globally-ignored-directories)))

(provide 'init-projectile)
;;; init-projectile.el ends here
