;;; init-funcs.el --- funcs                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: tools

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

(defun smart-bol (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun open-init-file ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun publish-hugo-posts-to-github-pages ()
  "Auto publish to github pages."
  (interactive)
  (org-hugo-export-wim-to-md t nil nil)
  (async-shell-command "sh ~/Documents/org/deploy.sh"))

(defalias 'hp 'publish-hugo-posts-to-github-pages)

(defun my-dired-find-file ()
  "Open buffer on another window."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    ;; first element of attributes represents is it a folder
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun v-dired-up-directory ()
  "Goto up directory and resue buffer."
  (interactive)
  (find-alternate-file ".."))

(defun v-dired-open-dir ()
  "Goto current directory."
  (interactive)
  (dired "."))


(provide 'init-funcs)
;;; init-funcs.el ends here
