(use-package go-mode
  :bind (:map go-mode-map
	      ("‹f1›" . godoc-at-point))
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-rename)

  (use-package go-gen-test)
  (use-package gotest
    :bind (:map go-mode-map
		("C-c t p" . go-test-current-project)
		("C-c t f" . go-test-current-file)
		("C-c t t" . go-test-current-test)
		("C-c x x" . go-run))))


(provide 'init-go)
