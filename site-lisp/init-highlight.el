(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook (after-init . global-hl-line-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-shen-point-in-periphery t)

  )


(use-package symbol-overlay
  :diminish
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :init (setq symbol-overlay-idle-time 0.1
              symbol-overlay-faces
              '((:inherit (highlight bold))
                (:inherit (font-lock-builtin-face bold) :inverse-video t)
                (:inherit (warning bold) :inverse-video t)
                (:inherit (font-lock-constant-face bold) :inverse-video t)
                (:inherit (error bold) :inverse-video t)
                (:inherit (dired-mark bold) :inverse-video t)
                (:inherit (success bold) :inverse-video t)
                (:inherit (font-lock-keyword-face bold) :inverse-video t))))


(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
		highlight-indent-guides-auto-enabled t
		highlight-indent-guides-responsive 'top))

(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package pulse
  :ensure nil)

(provide 'init-highlight)
