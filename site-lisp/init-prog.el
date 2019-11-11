(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist
		'(("lambda" . ?λ)
                  ("<-" . ?←)
                  ("->" . ?→)
                  ("=>" . ?⇒)
                  ("map" . ?↦)
                  ("/=" . ?≠)
                  ("!=" . ?≠)
                  ("==" . ?≡)
                  ("<=" . ?≤)
                  (">=" . ?≥)))
  (setq-default prettify-symbols-unprettify-at-point 'right-edge))


(use-package dockerfile-mode)
(use-package lua-mode)
(use-package csv-mode)


(provide 'init-prog)
