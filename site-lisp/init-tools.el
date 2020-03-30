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
  :defer t)

(provide 'init-tools)
