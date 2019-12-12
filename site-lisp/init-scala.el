;; Make sure to use coursier v1.1.0-M9 or newer.
;; https://get-coursier.io/

;; Linux
;; curl -L -o coursier https://git.io/coursier
;; chmod +x coursier

;; Install by brew
;; brew tap coursier/formulas
;; brew install coursier/formulas/coursier

;; Install metals
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
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)


(provide 'init-scala)
