;;; navicat.el --- tools                          -*- lexical-binding: t; -*-

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


(defgroup navicat nil
  ""
  :group 'tools)

(defcustom navicat-same-buffer-response t
  ""
  :group 'navicat
  :type 'boolean)

(defcustom navicat-db-host "127.0.0.1"
  ""
  :group 'navicat
  :type 'string)

(defcustom navicat-db-port "3306"
  ""
  :group 'navicat
  :type 'string)

(defcustom navicat-db-user "root"
  ""
  :group 'navicat
  :type 'string)

(defcustom navicat-db-passwd "root"
  ""
  :group 'navicat
  :type 'string)

(defcustom navicat-exec-program "mysql -u%s -p%s -h%s -P%s -e \"%s\""
  ""
  :group 'navicat
  :type 'string)

(defcustom navicat-same-buffer-response-name "*Navicat Result*"
  ""
  :group 'navicat
  :type 'string)

(defgroup navicat-faces nil
  ""
  :group 'navicat
  :group 'faces)

(defface navicat-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  ""
  :group 'navicat-face)

(defconst navicat-comment-separator "#")
(defconst navicat-comment-start-regexp (concat "^" navicat-comment-separator))
(defconst navicat-comment-not-regexp (concat "^[^" navicat-comment-separator "]"))
(defconst navicat-empty-line-regexp "^\\s-*$")

(defconst navicat-keyword-regexp "\\(SELECT\\|FROM\\|AS\\|WHERE\\|LEFT\\|OUTER\\|JOIN\\|RIGHT\\|ON\\|ORDER\\|BY\\|DESC\\)")

;; (defconst navicat-var-regexp (rx bol ))

(defconst navicat-mode-keywords
  (list (list navicat-keyword-regexp '(1 'navicat-keyword-face))))

(defconst navicat-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))


;; (defun navicat-find-vars-befor-point ()
;;   ""
;;   (let ((vars nil)
;;         (bound (point)))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp ))
;;     )))

(defun navicat--response-buffer ()
  "Generate sql execution result response buffer."
  (let ((buf (get-buffer-create navicat-same-buffer-response-name)))
    (with-current-buffer buf (org-mode))
    buf))

;; (defadvice async-shell-command (after render-table)
;;   ""
;;   (message "async shell advice")
;;   (with-current-buffer (get-buffer-create navicat-same-buffer-response-name) (org-mode)))


;; (defun render-shell-table (proc &rest rest)
  ;; (message "async shell advice")
  ;; (with-current-buffer (get-buffer-create navicat-same-buffer-response-name) (org-mode)))

;; (advice-add 'shell-command :after #'render-shell-table)

(defun navicat-do (sql)
  "Execute `SQL` statement."
  (async-shell-command (format navicat-exec-program
                               navicat-db-user
                               navicat-db-passwd
                               navicat-db-host
                               navicat-db-port
                               (string-trim sql))
                       (navicat--response-buffer)))

(defun navicat-current-min ()
  ""
  (save-excursion
    (beginning-of-line)
    (if (looking-at navicat-comment-start-regexp)
        (if (re-search-forward navicat-comment-not-regexp (point-max) t)
            (point-at-bol)
          (point-max))
      (if (re-search-backward navicat-comment-start-regexp (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun navicat-current-max ()
  ""
  (save-excursion
    (if (re-search-forward navicat-comment-start-regexp (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (progn (goto-char (point-max))
             (if (looking-at "^$")
                 (- (point) 1)
               (point))))))

(defun navicat-parse-current-and-do (func &rest args)
  ""
  (save-excursion
    (goto-char (navicat-current-min))
    (let* ((cmax (navicat-current-max))
          (sql (buffer-substring (min (point) cmax) cmax)))
      (apply func sql args))))

(defun navicat-send-current ()
  ""
  (interactive)
  (navicat-parse-current-and-do 'navicat-do))

(defvar navicat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'navicat-send-current)
    map))

(define-derived-mode navicat-mode fundamental-mode "Navicat"
  "Turn on navicat mode."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(navicat-mode-keywords)))

(provide 'navicat)

;;; navicat.el ends here
