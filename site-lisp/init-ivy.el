(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward))
  :hook ((after-init . ivy-mode)
	 (ivy-mode . counsel-mode))

  :config
  (setq ivy-initial-inputs-alist nil)

  :init
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
	ivy-use-virtual-buffers t
	ivy-height 10
	ivy-count-format "%d "
	ivy-on-del-error-function nil)

  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
        (t . ivy--regex-plus)))

  :config
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (use-package smex))


(provide 'init-ivy)
