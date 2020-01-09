;; init-lsp.el --- Initialize language server configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 vritser

;; Author: vritser <vritser@gmail.com>
;; URL: https://github.com/vritser/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; language server configurations.
;;

;;; Code:

(use-package lsp-mode
  :diminish lsp-mode
  :hook ((go-mode js-mode js2-mode java-mode scala-mode) . lsp-deferred)
  :bind (("M-RET" . lsp-execute-code-action)
         :map lsp-mode-map
	     ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-auto-guess-root t
              lsp-enable-on-type-formatting t
	          lsp-prefer-flymake nil
	          flymake-fringe-indicator-position 'right-fringe
              lsp-gopls-server-args '("-mode=stdio" "-logfile=/usr/local/var/log/gopls/lsp.log" "-rpc.trace"))
  :config
  (use-package lsp-clients
    :ensure nil)

  (use-package lsp-ui
    :commands lsp-ui-doc-hide
    :custom-face
    (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
    (lsp-ui-sideline-code-action ((t (:inherit warning))))

    :bind (:map lsp-ui-mode-map
		        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
		        ([remap xref-find-references] . lsp-ui-peek-find-references))
    :init (setq lsp-ui-doc-enable t
		        lsp-ui-doc-delay 0.2
		        lsp-ui-doc-use-webkit nil
		        ;; lsp-ui-include-signature t
		        lsp-ui-doc-position 'at-point
                lsp-ui-doc-enable nil
                lsp-ui-peek-enable t
		        ;; lsp-ui-doc-border (face-foreground 'default)
                ;; lsp-ui-doc-border "violet"
		        lsp-eldoc-enable-hover nil

		        lsp-ui-sideline-enable nil
		        lsp-ui-sideline-show-hover nil
		        lsp-ui-sideline-show-diagnostics nil
		        lsp-ui-sideline-ignore-duplicate t)
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    (add-hook 'after-load-theme-hook
	          (lambda ()
		        (setq lsp-ui-doc-border (face-foreground 'default))
		        (set-face-background 'lsp-ui-doc-background
				                     (face-background 'tooltip))))

    (use-package company-lsp
      :init (setq company-lsp-cache-candidates 'auto))

    (use-package lsp-java
      ;; :hook (java-mode . (lambda ()
	  ;;   	   (require 'lsp-java)
	  ;;   	   (lsp-deferred))))
      :ensure t
      :after lsp-mode
      :init
      (setq lsp-java-vmargs
            (list
             "-noverify"
             "-Xmx1G"
             "-XX:+UseG1GC"
             "-XX:+UseStringDeduplication"
             ;; "-javaagent:/home/torstein/.m2/repository/org/projectlombok/lombok/1.18.6/lombok-1.18.6.jar"
             )

            ;; Don't organise imports on save
            lsp-java-save-action-organize-imports nil
            lsp-java-jdt-download-url "http://localhost:3000/jdt-language-server-latest.tar.gz"

            ;; Currently (2019-04-24), dap-mode works best with Oracle
            ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
            ;;
            ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
            ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
            ))
    (use-package dap-mode
      :ensure t
      :after lsp-mode
      :config
      (dap-mode t)
      (dap-ui-mode t)
      (dap-tooltip-mode 1)
      (tooltip-mode 1)

      ;; (dap-register-debug-template "My Runner"
      ;;                              (list :type "java"
      ;;                                    :request "launch"
      ;;                                    :args ""
      ;;                                    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
      ;;                                    :projectName "myapp"
      ;;                                    :mainClass "com.domain.AppRunner"
      ;;                                    :env '(("DEV" . "1"))))

      ;; (dap-register-debug-template
      ;;  "localhost:5005"
      ;;  (list :type "java"
      ;;        :request "attach"
      ;;        :hostName "localhost"
      ;;        :port 5005))
    )

      (require 'dap-java)

      (use-package dap-java
        :ensure nil
        :after (lsp-java)
        :config
        (global-set-key (kbd "<f7>") 'dap-step-in)
        (global-set-key (kbd "<f8>") 'dap-next)
        (global-set-key (kbd "<f9>") 'dap-continue))
      ))




(provide 'init-lsp)

;;; init-lsp.el ends here
