;;; init-js.el ---                                  -*- lexical-binding: t; -*-

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


;; JSON mode
(use-package json-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        javascript-indent-level 2
        js-indent-level 2
        css-indent-offset 2
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode)
  :config
  (setq typescript-indent-level 2))

(provide 'init-js)
;;; init-js.el ends here
