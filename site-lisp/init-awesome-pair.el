;;; init-awesome-pair.el ---                         -*- lexical-binding: t; -*-

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

;;; Requires:

(require 'awesome-pair)

;;; Code:

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'scala-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook
               'restclient-mode-hook
               'dart-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode t))))


(defun v-jump-pair-right ()
  "To right of next match parentheses."
  (interactive)
  (cond
   ;; Jump out of string if cursor in string area.
   ((looking-at "\")") (forward-char 2))

   ((awesome-pair-in-string-p)
    (goto-char (+ (cdr (awesome-pair-string-start+end-points)) 1)))
   ;; Jump to next pair.
   (t
    (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
    (forward-char 1))))


(defun awesome-pair-open-curly ()
  "Customize { in scala mode."
  (interactive)
  (cond
   ((region-active-p)
    (awesome-pair-wrap-curly))
   ((and (awesome-pair-in-string-p)
         (derived-mode-p 'js-mode))
    (insert "{}")
    (backward-char))
   ((or (awesome-pair-in-string-p)
        (awesome-pair-in-comment-p))
    (insert "{"))
   (t
    (cond ((derived-mode-p 'ruby-mode)
           (insert "{  }")
           (backward-char 2))

          ((derived-mode-p 'scala-mode)
           ;; (if (re-search-backward ".*\\(map\\|Map\\|foreach\\)\s?$" (line-beginning-position) t)
           (if (re-search-backward (rx (zero-or-more alpha)
                                       (or "map" "foreach")
                                       (zero-or-more space)
                                       eol)
                                    (line-beginning-position) t)
               (progn
                 (end-of-line)
                 (when (not (equal (string (preceding-char)) " "))
                   (insert " "))
                 (insert "{")
                 (yas-expand))
             (progn
               (insert "{}")
               (backward-char 1))
             ))

          (t
           (insert "{}")
           (backward-char)))
    )
   ))



(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

(define-key awesome-pair-mode-map (kbd "M-p") 'v-jump-pair-right)
;; (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
;; (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)


(provide 'init-awesome-pair)
;;; init-awesome-pair.el ends here
