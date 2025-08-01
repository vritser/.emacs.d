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

  ;; (use-package company-prescient
  ;;   :init (company-prescient-mode 1))

  ;; (use-package company-tabnine
  ;;   :ensure t
  ;;   :bind (("M-/" . company-tabnine))
  ;;   :config
  ;;   (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))

  ;; (use-package company-tabnine
  ;;   :defer 1
  ;;   ;; :custom (company-tabnine-max-num-results 9)
  ;;   :bind (("M-q" . company-other-backend))
  ;;   :hook
  ;;   (lsp-after-open . (lambda ()
  ;;                       (setq company-tabnine-max-num-results 3)
  ;;                       (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  ;;                       (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
  ;;   (kill-emacs . company-tabnine-kill-process)
  ;;   :config
  ;;   ;; Enable TabNine on default
  ;;   ;; (add-to-list 'company-backends #'company-tabnine)

  ;;   ;; Integrate company-tabnine with lsp-mode
  ;;   (defun company//sort-by-tabnine (candidates)
  ;;     (if (or (functionp company-backend)
  ;;             (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
  ;;         candidates
  ;;       (let ((candidates-table (make-hash-table :test #'equal))
  ;;             candidates-lsp
  ;;             candidates-tabnine)
  ;;         (dolist (candidate candidates)
  ;;           (if (eq (get-text-property 0 'company-backend candidate)
  ;;                   'company-tabnine)
  ;;               (unless (gethash candidate candidates-table)
  ;;                 (push candidate candidates-tabnine))
  ;;             (push candidate candidates-lsp)
  ;;             (puthash candidate t candidates-table)))
  ;;         (setq candidates-lsp (nreverse candidates-lsp))
  ;;         (setq candidates-tabnine (nreverse candidates-tabnine))
  ;;         (nconc (seq-take candidates-tabnine 3)
  ;;                (seq-take candidates-lsp 6))))))

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

            (when (icons-displayable-p)
        (defvar company-box-icons-nerd
          `((Unknown       . ,(nerd-icons-codicon "nf-cod-symbol_namespace"))
            (Text          . ,(nerd-icons-codicon "nf-cod-symbol_string"))
            (Method        . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
            (Function      . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
            (Constructor   . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-lpurple))
            (Field         . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
            (Variable      . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
            (Class         . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
            (Interface     . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
            (Module        . ,(nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
            (Property      . ,(nerd-icons-codicon "nf-cod-symbol_property"))
            (Unit          . ,(nerd-icons-codicon "nf-cod-symbol_key"))
            (Value         . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'nerd-icons-lblue))
            (Enum          . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'nerd-icons-orange))
            (Keyword       . ,(nerd-icons-codicon "nf-cod-symbol_keyword"))
            (Snippet       . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))
            (Color         . ,(nerd-icons-codicon "nf-cod-symbol_color"))
            (File          . ,(nerd-icons-codicon "nf-cod-symbol_file"))
            (Reference     . ,(nerd-icons-codicon "nf-cod-symbol_misc"))
            (Folder        . ,(nerd-icons-codicon "nf-cod-folder"))
            (EnumMember    . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
            (Constant      . ,(nerd-icons-codicon "nf-cod-symbol_constant"))
            (Struct        . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
            (Event         . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
            (Operator      . ,(nerd-icons-codicon "nf-cod-symbol_operator"))
            (TypeParameter . ,(nerd-icons-codicon "nf-cod-symbol_class"))
            (Template      . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))))
        (setq company-box-icons-alist 'company-box-icons-nerd))))

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
