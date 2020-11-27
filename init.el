;;; init.el ---                                      -*- lexical-binding: t; -*-

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

(defun add-subdirs-to-load-path (dir)
  "Recursive add dirs to `load-path`."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

(require 'init-const)
(require 'init-custom)
(require 'init-package)
(require 'init-base)
(require 'init-dashboard)

(require 'init-tools)
(require 'init-funcs)
(require 'init-xah-fly-keys)
(require 'init-thing-edit)
(require 'init-projectile)
(require 'init-ui)
(require 'init-v)

(require 'init-yasnippet)
(require 'init-awesome-pair)
(require 'init-elisp)
(require 'init-company)
(require 'init-go)
(require 'init-lsp)
;; (require 'init-nox)
(require 'init-highlight)
(require 'init-ivy)
(require 'init-prog)
(require 'init-magit)
(require 'init-web)
(require 'init-scala)
(require 'init-org)
(require 'init-flycheck)
(require 'grep-dired)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; (provide 'init)
;;; init.el ends here
