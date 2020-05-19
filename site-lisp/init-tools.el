(require 'init-const)

(use-package plantuml-mode
  :defer t
  ;; :custom
  ;; (org-plantuml-jar-path (expand-file-name "~/tools/plantuml.jar"))
  :init
  (setq plantuml-default-exec-mode 'jar
        org-plantuml-jar-path "~/tools/plantuml.jar"
        plantuml-jar-path "~/tools/plantuml.jar")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t))))


(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))

  (with-eval-after-load 'company
    (use-package company-restclient
      :defines company-backends
      :init (add-to-list 'company-backends 'company-restclient))))


(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body))
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
      (youdao-dictionary-search-at-point)))
  ;; :config
  ;; (with-eval-after-load 'hydra
  ;;   (defhydra youdao-dictionary-hydra (:color blue)
  ;;     ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
  ;;     ("y" youdao-dictionary-play-voice-at-point "play voice at point")
  ;;     ("q" quit-window "quit")
  ;;     ("C-g" nil nil)
  ;;     ("h" nil nil)
  ;;     ("?" nil nil)))
  )


(use-package quickrun)


(provide 'init-tools)
