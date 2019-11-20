(use-package lsp-mode
  :diminish lsp-mode
  :hook ((go-mode js-mode java-mode) . lsp-deferred)
  :bind (("M-RET" . lsp-organize-imports)
         :map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-auto-guess-root t
	      lsp-prefer-flymake nil
	      flymake-fringe-indicator-position 'right-fringe)
  :config
  (use-package lsp-clients
    :ensure nil)

  (use-package lsp-ui
    :commands lsp-ui-doc-hide
    :custom-face
    (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
    (lsp-ui-sideline-code-action ((t (:inherit warning))))

    :bind (:map lsp-ui-mode-map
		([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
		([remap xref-find-references] . lsp-ui-peek-find-references))
    :init (setq lsp-ui-doc-enable t
		lsp-ui-doc-delay 0.2
		lsp-ui-doc-use-webkit nil
		lsp-ui-include-signature t
		lsp-ui-doc-position 'top
		lsp-ui-doc-border (face-foreground 'default)
		lsp-eldoc-enable-hover nil

		lsp-ui-sideline-enable nil
		lsp-ui-sideline-show-hover nil
		lsp-ui-sideline-show-diagnostics nil
		lsp-ui-sideline-ignore-duplicate t)
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    (add-hook 'after-load-theme-hook
	      (lambda ()
		(setq lsp-ui-doc-border (face-foreground 'default))
		(set-face-background 'lsp-ui-doc-background
				     (face-background 'tooltip))))

    (use-package company-lsp
      :init (setq company-lsp-cache-candidates 'auto))

    (use-package lsp-java
      :hook (java-mode . (lambda ()
			   (require 'lsp-java)
			   (lsp-deferred))))
    ))




(provide 'init-lsp)
