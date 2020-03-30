(require 'xah-fly-keys)

(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(define-key xah-fly-key-map (kbd "<escape>") 'xah-fly-command-mode-activate)


(provide 'init-xah-fly-keys)
