;;; init-go.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: languages

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

(use-package go-mode
  :bind (:map go-mode-map
	      ("‹f1›" . godoc-at-point))
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-rename)

  (use-package go-gen-test)
  (use-package gotest
    :bind (:map go-mode-map
		("C-c t p" . go-test-current-project)
		("C-c t f" . go-test-current-file)
		("C-c t t" . go-test-current-test)
		("C-c x x" . go-run))))

(provide 'init-go)
;;; init-go.el ends here
