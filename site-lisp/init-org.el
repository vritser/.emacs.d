(use-package org
  :ensure nil
  :defer t
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
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
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-agenda-files '("~/org/"))

  (setq-default org-capture-templates
                '(("t" "TODO" entry (file+headline "~/org/gtd.org" "Tasks")
                   "* TODO [#B] %?\n %i\n")
                  ("s" "Code Snippet" item (file+headline "~/org/snippets/snippets.org" "Code Snippets")
                   "- %^{title}\n #+BEGIN_SRC %^{language}\n %?\n #+END_SRC ")))
  ;; (org-export-backends (quote (ascii html icalendar latex md odt)))
  ;; (org-todo-keywords
  ;; '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  :config
  ;; Prettify UI
  (use-package org-bullets
    :if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫")))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?⯀)
                    '("⯀" "⯀" "⯀" "⯀")
                  '("HIGH" "MIDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  ;; (if emacs/>=26p
  ;;     (cl-pushnew '(shell . t) load-language-list)
  ;;   (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
                ("C-c M-o" . org-mime-htmlize)
                :map org-mode-map
                ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("C-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

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
  )


(provide 'init-org)
