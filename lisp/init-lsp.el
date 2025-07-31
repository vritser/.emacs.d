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
  :commands (lsp-enable-which-key-integration
             lsp-format-buffer
             lsp-organize-imports
             lsp-install-server)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Format and organize imports
                       (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                         (add-hook 'before-save-hook #'lsp-format-buffer t t)))))
  :bind (:map lsp-mode-map
              ("M-RET" . lsp-execute-code-action)
	            ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  (setq lsp-auto-guess-root t
        lsp-eslint-run "onSave"

        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  ;; debug settings
  ;; (setq lsp-log-io t)
  ;; (setq lsp-gopls-server-args '("-mode=stdio" "-logfile=/usr/local/var/log/gopls/lsp.log" "-rpc.trace"))
  )

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-RET" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-doc-enable nil
		          lsp-ui-doc-delay 1
              lsp-ui-doc-include-signature t
		          lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'default)
              lsp-eldoc-enable-hover t

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

  ;; (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  (add-hook 'after-load-theme-hook
	          (lambda ()
		          (setq lsp-ui-doc-border (face-foreground 'default))
		          (set-face-background 'lsp-ui-doc-background
				                           (face-background 'tooltip)))))

;; (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.45.0/jdt-language-server-1.45.0-202502271238.tar.gz")
(use-package lsp-java
  :defer t
  ;; :ensure t
  :hook (java-mode . (lambda () (require 'lsp-java)))
  :init
  (setq lsp-java-vmargs
        '(
          "-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx1G"
          "-Xms100m"
          ;; When you need to use lombok, uncomment the below and change it to your path
          "-javaagent:/Users/ed/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"
          )
        ;; Don't organise imports on save
        lsp-java-save-actions-organize-imports nil
        ))

;; to enable the lenses
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")
    (setq-default python-shell-interpreter "python3")
    (setq-default python-indent-guess-indent-offset nil)))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))

;; (use-package lsp-dart
;;   :defer t
;;   :hook (dart-mode . (lambda () (require 'lsp-dart)))
;;   :config
;;   (setq lsp-dart-sdk-dir "/usr/local/flutter/bin/cache/dart-sdk"))

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))

;; Debug
(use-package dap-mode
  :defines dap-python-executable
  :diminish
  :bind (:map lsp-mode-map ("<f5>" . dap-debug))
  :hook ((after-init . dap-auto-configure-mode)

         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((js-mode js2-mode) . (lambda () (require 'dap-node)))
         (python-mode . (lambda () (require 'dap-python))))
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  :config
  (dap-ui-mode t)
  (tooltip-mode t)
  (dap-tooltip-mode t))

(provide 'init-lsp)
;;; init-lsp.el ends here
