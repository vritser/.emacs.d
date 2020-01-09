(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	projectile-sort-order 'recentf
	projectile-use-git-grep t
    projectile-globally-ignored-file-suffixes
        '(
          "blob"
          "class"
          "classpath"
          "gz"
          "iml"
          "ipr"
          "jar"
          "pyc"
          "tkj"
          "war"
          "xd"
          "zip"
          )
    projectile-globally-ignored-files '("TAGS" "*~")
    )

  :config
  (setq projectile-globally-ignored-directories
        (append (list
                 ".gradle"
                 "build"
                 "elpa"
                 "node_modules"
                 "output"
                 "reveal.js"
                 "semanticdb"
                 "target"
                 "venv"
                 )
                projectile-globally-ignored-directories))

  )


(provide 'init-projectile)
