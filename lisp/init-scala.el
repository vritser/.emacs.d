;;; init-scala.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: languages

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


;; Make sure to use coursier v1.1.0-M9 or newer.
;; https://get-coursier.io/

;; Linux
;; curl -L -o coursier https://git.io/coursier
;; chmod +x coursier

;; Install by brew
;; brew tap coursier/formulas
;; brew install coursier/formulas/coursier

;; Install metals
;; https://scalameta.org/metals/docs/editors/emacs.html
;; coursier bootstrap \
;;   --java-opt -Xss4m \
;;   --java-opt -Xms100m \
;;   --java-opt -Dmetals.client=emacs \
;;   org.scalameta:metals_2.12:0.7.6 \
;;   -r bintray:scalacenter/releases \
;;   -r sonatype:snapshots \
;;   -o /usr/local/bin/metals-emacs -f

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")
        sbt:display-command-buffer nil))

(use-package lsp-metals)

(defun compile-sbt-proj ()
  "Compile the sbt project."
  (sbt-command "test:compile"))

(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'after-save-hook (lambda ()
                                         (when (equal major-mode "scala-mode")
                                           (compile-sbt-proj))))))

(provide 'init-scala)
;;; init-scala.el ends here
