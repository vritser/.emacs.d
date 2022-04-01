;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: extensions, tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package lsp-mode
  :diminish
  :defines (lsp-clients-python-library-directories lsp-rust-rls-server-command)
  :commands (lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       ;;   (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                         (add-hook 'before-save-hook #'lsp-format-buffer t t)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :bind (:map lsp-mode-map
         ("M-RET" . lsp-execute-code-action)
	       ("C-c C-d" . lsp-describe-thing-at-point)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))

  :init
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  (setq lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-signature-auto-activate nil
        lsp-eldoc-render-all nil
        ;; lsp-signature-doc-lines 2
        lsp-modeline-code-actions-enable nil
        lsp-eslint-enable t
        lsp-eslint-run "onSave"

        lsp-enable-file-watchers nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-auto-guess-root t

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  ;; (setq lsp-log-io t)
  ;; (setq lsp-gopls-server-args '("-mode=stdio" "-logfile=/usr/local/var/log/gopls/lsp.log" "-rpc.trace"))

  :config
  (with-no-warnings
       (defun my-lsp--init-if-visible (func &rest args)
         "Not enabling lsp in `git-timemachine-mode'."
         (unless (bound-and-true-p git-timemachine-mode)
           (apply func args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible))

  (use-package lsp-ivy
    :after lsp-mode
    :bind (:map lsp-mode-map
           ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
           ("C-s-." . lsp-ivy-global-workspace-symbol)))

  ;; (require 'lsp-java-boot)

  ;; to enable the lenses
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  (use-package lsp-ui
    :custom-face
    ;; (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :hook (lsp-mode . lsp-ui-mode)
    :bind (("C-c u" . lsp-ui-imenu)
           :map lsp-ui-mode-map
           ("M-<f6>" . lsp-ui-hydra/body)
           ("M-RET" . lsp-ui-sideline-apply-code-actions))

    :init (setq lsp-ui-doc-enable nil
                lsp-ui-doc-use-webkit nil
		            lsp-ui-doc-delay 1
                lsp-ui-doc-include-signature t
		            lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)
                lsp-eldoc-enable-hover nil

		            lsp-ui-sideline-enable t
		            lsp-ui-sideline-show-hover nil
		            lsp-ui-sideline-show-diagnostics nil
                lsp-ui-sideline-show-code-actions t
		            lsp-ui-sideline-ignore-duplicate t

                ;; lsp-ui-flycheck--save-mode t
                lsp-ui-imenu-enable t
                lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                      ,(face-foreground 'font-lock-string-face)
                                      ,(face-foreground 'font-lock-constant-face)
                                      ,(face-foreground 'font-lock-variable-name-face)))
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    (add-hook 'after-load-theme-hook
	          (lambda ()
		        (setq lsp-ui-doc-border (face-foreground 'default))
		        (set-face-background 'lsp-ui-doc-background
				                     (face-background 'tooltip))))

    ;; (use-package company-lsp
    ;;   :init (setq company-lsp-cache-candidates 'auto))

    ;; (use-package company-lsp
    ;;   :after company
    ;;   :defines company-backends
    ;;   :functions company-backend-with-yas
    ;;   :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

    ;; Microsoft python-language-server support
    (use-package lsp-python-ms
      :hook (python-mode . (lambda () (require 'lsp-python-ms)))
      :init
      (when (executable-find "python3")
        (setq lsp-python-ms-python-executable-cmd "python3")
        (setq-default python-shell-interpreter "python3")
        (setq python-indent-guess-indent-offset nil)))

    (use-package lsp-java
      :defer t
      :hook (java-mode . (lambda () (require 'lsp-java)))
      :init
      (setq lsp-java-vmargs
            (list
             "-noverify"
             "-Xmx1G"
             "-XX:+UseG1GC"
             "-XX:+UseStringDeduplication"
             ;; "-javaagent:/Users/pi/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
             )

            ;; Don't organise imports on save
            lsp-java-save-actions-organize-imports nil

            ;; Currently (2019-04-24), dap-mode works best with Oracle
            ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
            ))

    (use-package lsp-dart
      :defer t
      :hook (dart-mode . (lambda () (require 'lsp-dart)))
      :config
      (setq lsp-dart-sdk-dir "/usr/local/flutter/bin/cache/dart-sdk"))

    ;; https://marketplace.visualstudio.com/items?itemName=vscjava.vscode-java-debug
    ;; version
    ;; curl https://marketplace.visualstudio.com/_apis/public/gallery/publishers/vscjava/vsextensions/vscode-java-debug/0.35.0/vspackage  > ~/.emacs.d/.cache/lsp/eclipse.jdt.ls/bundles/java.debug.plugin.jar
    (use-package dap-java
      :ensure nil
      :after (lsp-java)
      :config
      (global-set-key (kbd "<f7>") 'dap-step-in)
      (global-set-key (kbd "<f8>") 'dap-next)
      (global-set-key (kbd "<f9>") 'dap-continue))
    ))

;; (add-hook 'java-mode-hook (lambda () (gradle-mode 1)))

(use-package dap-mode
  :defer t
  :diminish
  :bind (:map lsp-mode-map ("<f5>" . dap-debug))
  :hook ((after-init . dap-mode)
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((js-mode js2-mode) . (lambda () (require 'dap-node)))
         (python-mode . (lambda () (require 'dap-python))))
  :config
  (dap-ui-mode t)
  (tooltip-mode t)
  (dap-tooltip-mode t)
  (setq-default dap-python-executable "python3"))

;; (setq lsp-java-jdt-download-url "http://localhost:3000/")

(provide 'init-lsp)
;;; init-lsp.el ends here
