(require 'thing-edit)

(defvar v-thing-edit-map (make-sparse-keymap)
  "Keybindings for `thing-edit`.")

(define-key v-thing-edit-map (kbd "w") 'thing-copy-word)

(global-set-key (kbd "C-w") 'thing-copy-symbol)


(provide 'init-thing-edit)
