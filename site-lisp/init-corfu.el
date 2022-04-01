(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  :bind
  (:map corfu-map
        ([tab] . corfu-insert))
  :init
  (corfu-global-mode))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package cape
  :bind (("M-/" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c o i" . cape-ispell)
         ("C-c o l" . cape-line))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Use dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand)))

;; (add-to-list 'corfu-margin-formatters #'kind-example)

;; works in prog-mode buffers
(defun kind-example (_metadata)
  (when-let (kind (plist-get completion-extra-properties :company-kind))
    (lambda (cand)
      (format "<%s> " (funcall kind cand)))))

;; works in eshell
(defun file-example (metadata)
  (when (eq (completion-metadata-get metadata 'category) 'file)
    (lambda (cand)
      (format "<%s> " (file-name-extension cand)))))

;; (require 'kind-icon)
;; (use-package kind-icon
;; :after corfu
;; :custom
;; (kind-icon-use-icons nil)
;; (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;; :config
;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   (setq completion-cycle-threshold 3)

;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (setq tab-always-indent 'complete))

(provide 'init-corfu)
