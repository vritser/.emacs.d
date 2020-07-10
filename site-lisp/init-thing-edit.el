;;; crux.el --- A Collection of Ridiculously Useful eXtensions -*- lexical-binding: t -*-
;;
;; Copyright © 2015-2020 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/crux
;; Version: 0.4.0-snapshot
;; Keywords: convenience
;; Package-Requires: ((seq "1.11"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A cornucopia of useful interactive commands to make your Emacs
;; experience more enjoyable.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thing-edit)

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
