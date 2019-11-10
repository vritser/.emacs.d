(defun add-subdirs-to-load-path (dir)
  "Recursive add dirs to `load-path`."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

(require 'init-const)
(require 'init-custom)
(require 'init-package)
(require 'init-base)

(require 'init-xah-fly-keys)
(require 'init-thing-edit)
(require 'init-projectile)
(require 'init-v)
(require 'init-ui)

(require 'init-yasnippet)
(require 'init-awesome-pair)
(require 'init-elisp)
(require 'init-company)
(require 'init-go)
(require 'init-lsp)
