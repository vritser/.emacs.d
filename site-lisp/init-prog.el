;;; init-prog.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: languages, prettify

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

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist
		'(("lambda" . ?λ)
      ("<-" . ?←)
      ("->" . ?→)
      ("=>" . ?⇒)
      ("/=" . ?≠)
      ("!=" . ?≠)
      ("==" . ?≡)
      ("<=" . ?≤)
      (">=" . ?≥)))
  (setq-default prettify-symbols-unprettify-at-point 'right-edge))


(use-package dockerfile-mode)
(use-package lua-mode)
(use-package csv-mode)

(use-package haskell-mode)

(use-package protobuf-mode
  :mode ".proto$")

(provide 'init-prog)
;;; init-prog.el ends here
