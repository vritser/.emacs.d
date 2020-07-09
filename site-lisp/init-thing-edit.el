(require 'thing-edit)

;;; Code:
(defvar v-thing-edit-map (make-sparse-keymap)
  "Keybindings for `thing-edit`.")

;; thing edit copy commands
(define-key v-thing-edit-map (kbd "w") 'thing-copy-word)
(define-key v-thing-edit-map (kbd "v") 'thing-copy-symbol)
(define-key v-thing-edit-map (kbd "p") 'thing-copy-parentheses)
(define-key v-thing-edit-map (kbd "b") 'thing-copy-paragraph)
(define-key v-thing-edit-map (kbd "l") 'thing-copy-line)
(define-key v-thing-edit-map (kbd "h") 'thing-copy-defun)


;; thing edit cut commands
(define-key v-thing-edit-map (kbd "k w") 'thing-cut-word)
(define-key v-thing-edit-map (kbd "k v") 'thing-cut-symbol)
(define-key v-thing-edit-map (kbd "k p") 'thing-cut-parentheses)
(define-key v-thing-edit-map (kbd "k b") 'thing-cut-paragraph)
(define-key v-thing-edit-map (kbd "k l") 'thing-cut-line)
(define-key v-thing-edit-map (kbd "k h") 'thing-cut-defun)

;; thing edit replace commands
(define-key v-thing-edit-map (kbd "r w") 'thing-replace-word)
(define-key v-thing-edit-map (kbd "r v") 'thing-replace-symbol)
(define-key v-thing-edit-map (kbd "r p") 'thing-replace-parentheses)
(define-key v-thing-edit-map (kbd "r b") 'thing-replace-paragraph)
(define-key v-thing-edit-map (kbd "r l") 'thing-replace-line)
(define-key v-thing-edit-map (kbd "r h") 'thing-replace-defun)

(global-set-key (kbd "C-c") v-thing-edit-map)


(provide 'init-thing-edit)

;;; init-thing-edit ends here
