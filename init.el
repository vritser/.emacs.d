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
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/.emacs.d/lisp/")

(require 'init-const)
(require 'init-custom)
(require 'init-package)
(require 'init-base)
(require 'init-funcs)
(require 'init-dashboard)

(require 'init-thing-edit)
(require 'init-tools)
(require 'init-xah-fly-keys)
(require 'init-agent-shell)

(require 'init-completion)
(require 'init-ui)
(require 'init-v)

(require 'init-yasnippet)
(require 'init-awesome-pair)

(require 'init-go)
(require 'init-lsp)
(require 'init-highlight)
(require 'init-prog)
(require 'init-js)
(require 'init-scala)
(require 'init-org)
(require 'init-flycheck)
(require 'init-elfeed)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; init.el ends here
