;;; init-xah-fly-keys.el --- Xah Fly Keys modal editing.	-*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: extensions

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
;; Xah Fly Keys (upstream: https://github.com/xahlee/xah-fly-keys), used
;; with the Dvorak layout, plus personal keybindings.
;;
;; Configuration notes:
;;
;; 1. The options below must be set BEFORE `xah-fly-keys' is loaded,
;;    because the library binds and unbinds keys while loading.
;;
;; 2. Personal keys are installed by `yuf/xah-fly-keys-apply-keys', which
;;    also runs after every call to `xah-fly-define-keys'.  The latter
;;    rebuilds `xah-fly-leader-key-map' from scratch, so keys defined
;;    once at load time are silently dropped as soon as the layout is
;;    changed with `xah-fly-keys-set-layout'.
;;
;; 3. Keys are written in Dvorak key names and translated to the active
;;    layout by `xah-fly--define-keys', so they keep their physical
;;    position when the layout is switched.
;;
;; 4. Upstream owns "SPC e" (unicode insertion) and "SPC ." (highlight
;;    symbol), so embark lives on "SPC y" in this configuration.
;;
;; Leader key overview (command mode, "SPC" is the leader):
;;
;;   SPC l    project & consult: f file, p project, u buffer,
;;            g ripgrep, r recent file, d find, l recenter
;;   SPC y    embark: a act, y dwim, e export, i insert, k collect,
;;            b bindings
;;   SPC u    consult-buffer          SPC p    vr/query-replace
;;   SPC t p  vr/replace              SPC c .  find-file
;;
;; Single keys: "3" deletes other windows (command mode), "C-t" transposes
;; characters (global; upstream's C-t hippie-expand is still on "SPC /").
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;;; Declarations

(declare-function xah-fly--define-keys "xah-fly-keys"
                  (keymap key-cmd-alist &optional direct-p))
(declare-function xah-fly-keys-set-layout "xah-fly-keys" (layout))
(declare-function xah-fly-keys "xah-fly-keys" (&optional arg))
(declare-function xah-new-empty-buffer "xah-fly-keys")
(declare-function xah-fly-command-mode-activate "xah-fly-keys")
(declare-function v-open-line-indent "init-v")
(declare-function v-down-2-lines "init-v")

(defvar xah-fly-command-map)
(defvar xah-fly-insert-map)
(defvar xah-fly-leader-key-map)
(defvar xah-fly-use-control-key)
(defvar xah-fly-use-meta-key)
(defvar xah-fly-command-mode-hl-line)
(defvar xah-fly-command-mode-cursor-color)
(defvar xah-fly-insert-mode-cursor-color)

;;; Options (must precede loading xah-fly-keys)

(setq xah-fly-use-control-key t)        ; keep C-a/C-n/C-o/... editing keys
(setq xah-fly-use-meta-key nil)         ; leave M-* bindings alone
(setq xah-fly-command-mode-hl-line nil) ; hl-line stays on, see init-highlight.el
;; keep the cursor color of the current theme (upstream defaults are
;; red in command mode, gray in insert mode)
(setq xah-fly-command-mode-cursor-color nil)
(setq xah-fly-insert-mode-cursor-color nil)

;;; Personal keybindings

(defun yuf/xah-fly--define-keys (keymap key-cmd-alist)
  "Define KEY-CMD-ALIST in KEYMAP for the active keyboard layout.
Keys in KEY-CMD-ALIST are written in Dvorak names and are translated by
`xah-fly--define-keys' when that function is available."
  (if (fboundp 'xah-fly--define-keys)
      (xah-fly--define-keys keymap key-cmd-alist)
    (dolist (x key-cmd-alist)
      (define-key keymap (kbd (car x)) (cdr x)))))

(defun yuf/xah-fly-keys-apply-keys ()
  "Install personal keybindings on top of the `xah-fly-keys' defaults.
Safe to call repeatedly; it also runs after `xah-fly-define-keys', which
rebuilds the leader keymap."
  ;; command mode
  (yuf/xah-fly--define-keys
   xah-fly-command-map
   '(("b" . consult-line)               ; upstream: isearch-forward
     ("3" . delete-other-windows)       ; upstream: unbound ("-" also does this)
     ("C-o" . find-file)
     ("C-a" . beginning-of-line)))
  ;; insert mode
  (yuf/xah-fly--define-keys
   xah-fly-insert-map
   '(("C-r" . v-open-line-indent)
     ("C-n" . next-line)
     ("C-a" . beginning-of-line)
     ("M-h" . v-down-2-lines)
     ("<backspace>" . hungry-delete-backward)))
  ;; leader: project & consult
  (yuf/xah-fly--define-keys
   (define-prefix-command 'yuf-l-map)
   '(("f" . project-find-file)
     ("p" . project-switch-project)
     ("u" . project-switch-to-buffer)
     ("g" . consult-ripgrep)
     ("r" . consult-recent-file)
     ("d" . consult-find)
     ("l" . recenter-top-bottom)))      ; upstream's "SPC l", kept here
  ;; leader: embark
  (yuf/xah-fly--define-keys
   (define-prefix-command 'yuf-embark-map)
   '(("a" . embark-act)
     ("y" . embark-dwim)
     ("e" . embark-export)
     ("i" . embark-insert)
     ("k" . embark-collect)
     ("b" . embark-bindings)))
  ;; leader top level
  (yuf/xah-fly--define-keys
   xah-fly-leader-key-map
   '(("l" . yuf-l-map)                  ; upstream: recenter-top-bottom
     ("y" . yuf-embark-map)             ; upstream: unbound
     ("u" . consult-buffer)             ; upstream: switch-to-buffer
     ("p" . vr/query-replace)           ; upstream: query-replace
     ("t p" . vr/replace))))            ; upstream: query-replace-regexp

;;; Setup

(add-to-list 'load-path
             (expand-file-name "lisp/extensions/xah-fly-keys" user-emacs-directory))

(use-package xah-fly-keys
  :ensure nil
  :demand t                            ; must load eagerly, not on demand
  :config
  (xah-fly-keys-set-layout "dvorak")
  (xah-fly-keys 1)
  (yuf/xah-fly-keys-apply-keys)
  ;; re-apply after every layout switch
  (advice-remove 'xah-fly-define-keys #'yuf/xah-fly-keys-apply-keys)
  (advice-add 'xah-fly-define-keys :after #'yuf/xah-fly-keys-apply-keys)
  (global-set-key (kbd "<escape>") #'xah-fly-command-mode-activate)
  (global-set-key (kbd "C-t") #'transpose-chars) ; upstream: hippie-expand
  (global-set-key (kbd "s-N") #'make-frame)
  (global-set-key (kbd "s-n") #'xah-new-empty-buffer))

(use-package key-chord
  :ensure t
  :demand t
  :after xah-fly-keys
  :init (key-chord-mode 1)
  :config
  ;; "th" in insert mode switches back to command mode
  (key-chord-define xah-fly-insert-map "th" #'xah-fly-command-mode-activate))

(provide 'init-xah-fly-keys)
;;; init-xah-fly-keys.el ends here
