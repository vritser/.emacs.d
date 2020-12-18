;;; init-tools.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: tools

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

(require 'init-const)
(require 'restclient)
(require 'navicat)

;;; Code:

;; (use-package plantuml-mode
;;   :defer t
;;   :init
;;   (setq plantuml-default-exec-mode 'jar
;;         org-plantuml-jar-path "~/tools/plantuml.jar"
;;         plantuml-jar-path "~/tools/plantuml.jar"))

(with-eval-after-load 'restclient
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(with-eval-after-load 'navicat
  (add-to-list 'auto-mode-alist '("\\.nav\\'" . navicat-mode))
  (add-hook 'navicat-mode-hook 'sqlup-mode)
  (setq navicat-db-host "47.94.37.100"
        navicat-db-port "20617"
        navicat-db-user "root"
        navicat-db-passwd "qweQWE!@#"))

;; (use-package restclient
  ;; :mode ("\\.http\\'" . restclient-mode)
;;   :config
;;   (use-package restclient-test
;;     :diminish
;;     :hook (restclient-mode . restclient-test-mode))

;;   (with-eval-after-load 'company
;;     (use-package company-restclient
;;       :defines company-backends
  ;;       :init (add-to-list 'company-backends 'company-restclient)))
  ;; )

(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun my-youdao-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (if emacs/>=26p
            (youdao-dictionary-search-at-point-posframe)
        (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point))))

;; (use-package vterm)
;; (use-package vterm-toggle
;;   :bind ("C-`" . vterm-toggle)
;;   :ensure t)

;; (setq vterm-toggle-fullscreen-p nil)
;; (add-to-list 'display-buffer-alist
;;              '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
;;                 (display-buffer-reuse-window display-buffer-at-bottom)
;;                 ;;(display-buffer-reuse-window display-buffer-in-direction)
;;                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
;;                 ;;(direction . bottom)
;;                 ;;(dedicated . t) ;dedicated is supported in emacs27
;;                 (reusable-frames . visible)
;;                 (window-height . 0.5)))


(use-package visual-regexp)

(provide 'init-tools)
;;; init-tools.el ends here
