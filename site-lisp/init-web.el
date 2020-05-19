;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 vritser

;; Author: vritser <vritser@gmail.com>
;; URL: https://github.com/vritser/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; General web configurations.
;;

;;; Code:

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-css-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode)

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

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
        js2-idle-timer-delay 0.1)
)

;; (use-package tide
;;   :ensure t
;;   :hook ((typescript-mode js2-mode) . tide-setup))


;; (use-package tide
;;     :diminish tide-mode
;;     :defines company-backends
;;     :preface
;;     (defun setup-tide-mode ()
;;       "Setup tide mode."
;;       (interactive)
;;       (tide-setup)
;;       (eldoc-mode 1)
;;       (tide-hl-identifier-mode 1))
;;     :hook (((typescript-mode js2-mode) . setup-tide-mode)
;;            (before-save . tide-format-before-save))
;;     :config
;;     (setq tide-format-options
;;           '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
;;             t
;;             :placeOpenBraceOnNewLineForFunctions
;;             nil))
;;     (add-to-list 'company-backends '(company-tide))
;;     ;; (with-eval-after-load 'company
;;     ;;   (cl-pushnew ( company-tide) company-backends))
;;     )

(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode)
  :config
  (setq typescript-indent-level 2))


(provide 'init-web)

;;; init-web.el ends here
