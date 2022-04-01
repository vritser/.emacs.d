;;; init-ui.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: faces

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

(use-package doom-themes
  :init (load-theme 'doom-one))

;; (use-package nano-theme
;;   :init (load-theme 'nano-dark))

(use-package all-the-icons
  ;; :ensure t
  :if (display-graphic-p)
  :init (unless (member "all-the-icons" (font-family-list))
          (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-toml-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

;; Hiden bars
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; Hiden titlebar
;; (set-frame-parameter (car (frame-list)) 'undecorated t)

;; (setq ns-transparent-titlebar t)
;; (setq ns-appearance t)

;; Turn off alarms completely
(setq ring-bell-function 'ignore)

;; Fullscreen emacs
;; (toggle-frame-fullscreen)
;; (require 'frame-maximize)
;; (toggle-frame-maximized)
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; (add-hook 'after-make-frame-functions 'toggle-frame-maximized)
;; Set
(set-face-attribute 'default nil
		    :height 160
		    :family "Monaco"
		    :weight 'normal
		    :width 'normal)


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert shell escapes to  color
;; (add-hook 'compilation-filter-hook
          ;; (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; (setq-default default-text-properties '(line-spacing 0.20 line-height 1.20))
(set-face-attribute 'font-lock-function-name-face nil
                    :height 180
                    :weight 'bold)

(set-frame-parameter (selected-frame) 'alpha (list 100 100))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-height 8
        doom-modeline-project-detection 'projectile
        doom-modeline-minor-modes nil))

(provide 'init-ui)
;;; init-ui.el ends here
