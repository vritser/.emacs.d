(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward))
  :hook ((after-init . ivy-mode)
	 (ivy-mode . counsel-mode))

  :init
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
	ivy-use-virtual-buffers t
	ivy-height 10
	ivy-count-format "%d "
	ivy-on-del-error-function nil
	ivy-initial-inputs-alist nil)
  :config
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (use-package smex))


(provide 'init-ivy)
