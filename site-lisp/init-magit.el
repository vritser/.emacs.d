(use-package magit
  :config
  (when (executable-find "cc")
    (use-package forge :demand)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)



(provide 'init-magit)
