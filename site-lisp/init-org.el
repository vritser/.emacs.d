;;; init-org.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: tex, extensions

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

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ;; (:map org-mode-map ("C-c C-p" . org-export-as-pdf-and-open))

  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (push '("[ ]" . ?☐) prettify-symbols-alist)
                       (push '("[X]" . ?☑) prettify-symbols-alist)
                       (push '("[-]" . ?⛝) prettify-symbols-alist)

                       (push '("#+ARCHIVE:" . ?📦) prettify-symbols-alist)
                       (push '("#+AUTHOR:" . ?👤) prettify-symbols-alist)
                       (push '("#+CREATOR:" . ?💁) prettify-symbols-alist)
                       (push '("#+DATE:" . ?📆) prettify-symbols-alist)
                       (push '("#+DESCRIPTION:" . ?⸙) prettify-symbols-alist)
                       (push '("#+EMAIL:" . ?🖂) prettify-symbols-alist)
                       (push '("#+OPTIONS:" . ?⛭) prettify-symbols-alist)
                       (push '("#+SETUPFILE:" . ?⛮) prettify-symbols-alist)
                       (push '("#+TAGS:" . ?🏷) prettify-symbols-alist)
                       (push '("#+TITLE:" . ?🕮) prettify-symbols-alist)

                       (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
                       (push '("#+END_SRC" . ?□) prettify-symbols-alist)
                       (push '("#+BEGIN_QUOTE" . ?») prettify-symbols-alist)
                       (push '("#+END_QUOTE" . ?«) prettify-symbols-alist)
                       (push '("#+HEADERS" . ?☰) prettify-symbols-alist)
                       (push '("#+RESULTS:" . ?💻) prettify-symbols-alist)

                       (prettify-symbols-mode 1)))

         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :init
  (setq org-log-done 'time
        org-use-speed-commands t
        org-confirm-babel-evaluate nil
        ;;       org-agenda-window-setup 'other-window
        org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE"))
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities nil
        org-hide-emphasis-markers nil
        org-src-tab-acts-natively t
        org-agenda-files '("~/Documents/org/"))

  (require 'org-tempo)
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-export-backends 'hugo)
  ;; (org-export-backends (quote (ascii html icalendar latex md odt)))

  :config
  ;; Prettify UI
  (use-package org-bullets
    ;; :if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    ;; :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫"))
    )

  ;; (use-package org-fancy-priorities
  ;;   :diminish
  ;;   :hook (org-mode . org-fancy-priorities-mode)
  ;;   :init (setq org-fancy-priorities-list
  ;;               (if (char-displayable-p ?⯀)
  ;;                   '("⯀" "⯀" "⯀" "⯀")
  ;;                 '("HIGH" "MIDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (python . t)
                               (js . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  ;; (if emacs/>=26p
  ;;     (cl-pushnew '(shell . t) load-language-list)
  ;;   (cl-pushnew '(sh . t) load-language-list))

  ;; (use-package ob-go
  ;;   :init (cl-pushnew '(go . t) load-language-list))

  ;; (use-package ob-rust
  ;;   :init (cl-pushnew '(rust . t) load-language-list))

  ;; (use-package ob-ipython
  ;;   :if (executable-find "jupyter")     ; DO NOT remove
  ;;   :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  ;; (use-package org-rich-yank
  ;;   :bind (:map org-mode-map
  ;;               ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  (use-package ox-hugo
    :after ox)

  ;; (use-package org-tree-slide
  ;;   :diminish
  ;;   :functions (org-display-inline-images
  ;;               org-remove-inline-images)
  ;;   :bind (:map org-mode-map
  ;;               ("C-<f7>" . org-tree-slide-mode)
  ;;               :map org-tree-slide-mode-map
  ;;               ("<left>" . org-tree-slide-move-previous-tree)
  ;;               ("<right>" . org-tree-slide-move-next-tree)
  ;;               ("S-SPC" . org-tree-slide-move-previous-tree)
  ;;               ("SPC" . org-tree-slide-move-next-tree))
  ;;   :hook ((org-tree-slide-play . (lambda ()
  ;;                                   (text-scale-increase 4)
  ;;                                   (org-display-inline-images)
  ;;                                   (read-only-mode 1)))
  ;;          (org-tree-slide-stop . (lambda ()
  ;;                                   (text-scale-increase 0)
  ;;                                   (org-remove-inline-images)
  ;;                                   (read-only-mode -1))))
  ;;   :config
  ;;   (org-tree-slide-simple-profile)
  ;;   (setq org-tree-slide-skip-outline-level 2))


  ;; (defun org-export-turn-on-syntax-highlight ()
  ;;   "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
  ;;   (interactive)
  ;;   (setq org-latex-listings 'minted
  ;;         org-latex-packages-alist '(("" "minted"))
  ;;         org-latex-pdf-process
  ;;         '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
  ;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  ;; (defun org-export-as-pdf-and-open ()
  ;;   "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
  ;;   (interactive)
  ;;   (save-buffer)
  ;;   (let* ((pdf-path (org-latex-export-to-pdf))
  ;;          (pdf-name (file-name-nondirectory pdf-path)))
  ;;     (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
  ;;         (progn
  ;;           (kill-matching-buffers (concat "^" pdf-name) t t)
  ;;           (find-file-other-window pdf-name))
  ;;       (find-file-other-window pdf-name))
  ;;     (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex"))))

  (defun v-org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture` template string for the new Hugo post."
    (let* ((title (read-from-minibuffer "Post Title: "))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity `(
                              ,(concat "** TODO " title)
                              ":PROPERTIES:"
                              ,(concat ":EXPORT_FILE_NAME: " fname)
                              ":END:"
                              "%?\n")
                 "\n")))

  (setq-default org-capture-templates
                '(("t" "TODO" entry (file+headline "~/Documents/org/gtd.org" "Tasks")
                   "* TODO %?\n %i\n")

                  ("s" "Code Snippet" entry (file+headline "~/Documents/org/snippets.org" "Code Snippets")
                   "** %^{title}\n #+BEGIN_SRC %^{language}\n %?\n #+END_SRC ")

                  ("b" "Blog" entry (file+headline "~/Documents/org/blog.org" "Technical Blogs")
                   (function v-org-hugo-new-subtree-post-capture-template) :empty-lines-after 1)
                  ("p" "Protocol" entry (file+headline "~/Documents/org/notes.org" "Inbox")
                   "* [[%:link][%:description]] \n\n %u \n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n %?")
	                ("L" "Protocol Link" entry (file+headline "~/Documents/org/notes.org" "Inbox")
                   "* %? [[%:link][%:description]] \nCaptured On: %U")))

  )

(provide 'init-org)
;;; init-org.el ends here
