(use-package elisp-mode
  :ensure nil
)

(use-package paren-face
  :hook (after-init . global-paren-face-mode))

(provide 'init-elisp)
