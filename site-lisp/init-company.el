;;; init-company.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: abbrev, extensions

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

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (:map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("<tab>" . company-complete-selection)

	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-tooltip-align-annotations t
	 company-tooltip-limit 10
	 company-idle-delay 0
   company-show-numbers t
	 company-echo-delay (if (display-graphic-p) nil 0)
	 company-minimum-prefix-length 3
	 company-require-match nil
	 company-dabbrev-ignore-case t
	 company-dabbrev-downcase nil)
   (setq company-backends (delete 'company-xcode company-backends))
   (setq company-backends (delete 'company-bbdb company-backends))
   (setq company-backends (delete 'company-eclim company-backends))
   (setq company-backends (delete 'company-gtags company-backends))
   (setq company-backends (delete 'company-etags company-backends))
   (setq company-backends (delete 'company-oddmuse company-backends))

  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; (use-package company-tabnine
  ;;   :ensure t
  ;;   :bind (("M-/" . company-tabnine))
  ;;   :config
  ;;   (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))

  (when emacs/>=26p
    (use-package company-box
      :diminish
      :functions (my-company-box--make-line
                  my-company-box-icons--elisp)
      :commands (company-box--get-color
                 company-box--resolve-colors
                 company-box--add-icon
                 company-box--apply-color
                 company-box--make-line
                 company-box-icons--elisp)
      :hook (company-mode . company-box-mode)
      :init (setq company-box-backends-colors nil
                  company-box-show-single-candidate t
                  company-box-max-candidates 50
                  company-box-doc-delay 0.5)
      :config
      ;; Support `company-common'
      (defun my-company-box--make-line (candidate)
        (-let* (((candidate annotation len-c len-a backend) candidate)
                (color (company-box--get-color backend))
                ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                          (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
                (align-string (when annotation
                                (concat " " (and company-tooltip-align-annotations
                                                 (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                (space company-box--space)
                (icon-p company-box-enable-icon)
                (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                              (company-box--apply-color icon-string i-color)
                              (company-box--apply-color candidate-string c-color)
                              align-string
                              (company-box--apply-color annotation-string a-color)))
                (len (length line)))
          (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                           'company-box--color s-color)
                               line)
          line))
      (advice-add #'company-box--make-line :override #'my-company-box--make-line)

      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      (when (and (display-graphic-p)
                 (require 'all-the-icons nil t))
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
                (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons))))

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.5))))


(provide 'init-company)
;;; init-company.el ends here
