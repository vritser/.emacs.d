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

(defalias 'zsh (lambda () (interactive) (find-file "~/.zshrc")))

(defun publish-hugo-posts-to-github-pages ()
  "Auto publish to github pages."
  (interactive)
  (org-hugo-export-wim-to-md t nil nil)
  (async-shell-command "sh ~/org/deploy.sh"))
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

;;
;; 限定场景使用
;;
(defun gen-interface-for-model ()
  "Generate interface for typescript sequelize model."
  (interactive)
  (save-excursion
    (let ((types #s(hash-table size 8 test equal data (
                                                       "INTEGER" "number"
                                                       "DECIMAL" "number"
                                                       "DATE" "Date"
                                                       "STRING" "string"
                                                       "BOOLEAN" "boolean")))
          (interface nil)
          (body nil))
      (goto-char (point-min))
      (re-search-forward "app.model.define(\n?\s*'\\(.*\\)'" nil t)
      (if (empty-string-p (match-string 1))
          (setq interface (read-from-minibuffer "Interface name: "))
        (setq interface (match-string 1)))

      (while (re-search-forward "\s?+\\(.+\\):\s?{\n?.*:\s?\\(\\w+\\),?" nil t)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (setq body (concat body key ": " (gethash val types) ";\n"))))
      (goto-char (point-max))
      (insert (concat "export interface I" interface " {\n" body "}")))))

(defun empty-string-p (str)
  "Return true if `STR` is empty or nil."
  (or (null str)
      (zerop (length (string-trim str)))))

(defun org-haskell-highlight ()
  "Replace empty src block to haskell."
(interactive)
(save-excursion
  (goto-char (point-min))
  (while (re-search-forward "\\(#\\+begin_src\\) *$" nil t)
    (message (match-string 1))
    (replace-match (concat (match-string 1) " haskell")))))

(defun v-jump-to-http ()
  "Jump to related http file."
  (interactive)
  (when (string-suffix-p ".ts" (buffer-name))
    (xref-push-marker-stack)
    (let* ((name (replace-regexp-in-string (rx (zero-or-one (or "Svc" "Ctrl")) ".ts" eol) "" (buffer-name)))
           (http (concat name ".http")))
      (if (buffer-live-p (get-buffer http))
          (switch-to-buffer http)
        (find-file (f-join default-directory http))))))
(global-set-key (kbd "C-c j") 'v-jump-to-http)

;; (defun v-transpose-line-up ()
;;   (interactive)
;;   (call-interactively 'transpose-lines)
;;   (forward-line -2)
;;   (end-of-line))

;; (defun v-transpose-line-down ()
;;   (interactive)
;;   (let ((current-prefix-arg '(-1)))
;;     (call-interactively 'transpose-lines)
;;     ;; (next-line -2)
;;     (end-of-line)))
;; (global-set-key (kbd "M-<up>") 'v-transpose-line-up)
;; (global-set-key (kbd "M-<down>") 'v-transpose-line-down)

;; (defun v-read ()
;;   (interactive)
;;   (read (current-buffer)))
;; (global-set-key (kbd "s-1") 'v-read)

;;
;; Convert parameters to destructured object
;;
(defun to_dest_obj ()
  "Convert parameters to destructured object."
  (interactive)
  (let* ((beg (progn
                (backward-up-list)
                (forward-char 1)
                (point)))
         (end (progn
                (up-list)
                (forward-char -1)
                (point)
                ))
         (params (buffer-substring beg end)))
    (goto-char beg)
    (delete-char (- end beg))
    (insert (format "{ %s }: { %s }"
                    (string-join (mapcar (lambda (s) (string-trim (car (split-string s ":")))) (split-string params ",")) ", ")
                    params
                    ))))

;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" centaur-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,centaur-proxy)
          ("https" . ,centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1086 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))



;; (defun vr (&rest args)
;;   (print args))

;; (vr
;;  :bind (("C" . 'next-line)
;;         :map ("A" . 'previous-line))
;;  :hook '(1 2 3))

(defun v-incr-at-point (&optional inc)
  (interactive "p")
    (if (not (number-at-point))
      (user-error "Not on a number")
      (unless inc (setq inc 1))
      )
    )



;; (buffer-live-p (get-buffer "test.nav"))

(defun foo () 123)

(defun foo-bar (orig &rest args)
  ""
  (apply orig args))
;; (advice-add 'org-capture-kill :around #'foo-bar)

;; (add-to-list 'company-backends '(company-capf :with company-files :separate))

(defun org-capture-advice-add-kill-no-save (&rest args)
  "为org-capture-kill配置一个补丁，调用前述函数时，读取:kill-no-save，并赋值给:no-save"
  (if (plist-get org-capture-current-plist :kill-no-save)
      (setq org-capture-current-plist
	    (plist-put org-capture-current-plist :no-save (plist-get org-capture-current-plist :kill-no-save)))))
;; (advice-add #'org-capture-kill :around #'org-capture-advice-add-kill-no-save)

(define-advice foo (:around (_oldfun) bar)
  456)
;; (foo)
;; (funcall (advice--cdr (symbol-function 'foo)))

(defun my-shell-execute(cmd)
   (interactive "sShell command: ")
   (shell (get-buffer-create "my-shell-buf"))
   (process-send-string (get-buffer-process "my-shell-buf") (concat cmd "\n")))

(defun replace-md-sub ()
  "Replace <sub> tag to _ in markdown."
  (interactive)
  (replace-regexp "<sub>\\(\\w+\\)</sub>" "_\\1"))

(defun open-navicat-file ()
  "Open project sql query file."
  (interactive)
  (find-file "/Users/vritser/company/lezhilong/rd-mytikas/rd-mytikas.nav"))

(defalias 'nav 'open-navicat-file)

;; (read-regexp "input")

;; (setq url-request-extra-headers '(("Content-Type" . "application/json") ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36") ("Authorization" . "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6ImFLOHpCVjhNIiwibW9iaWxlIjoiMTgzOTY4MTY4NzgiLCJpYXQiOjE2MDY0NzExMzksImV4cCI6MTYxMTY1NTEzOX0.vpGC_iRP_25v9vc1ZPNNJeHE_SRvn2jIiH8HoG7cZ5k")))

;; (url-retrieve
;;  "https://rdmytikasapitest.lezhilong.cn/api/v1/home/user"
;;  (lambda (events)
;;    (goto-char url-http-end-of-headers)
;;    (print (json-read))
   ;; (let ((json-object-type 'plist)
   ;;       (json-key-type 'symbol)
   ;;       (json-array-type 'vector))
   ;;   (let ((result (json-read)))
   ;;     ;; Do something with RESULT here
   ;;     (print result)
   ;;     ))
   ;; ))

;; (setq resp '((results (id . "aK8zBV8M") (mobile . "18396816878") (unionid . "oKto1w1PMA0--hxgIesuVpk_ts_0") (nick_name . "vritser") (avatar_file (file_type . "image/png") (file_url . "https://thirdwx.qlogo.cn/mmopen/vi_32/ZY1RUzSxf937TfZeuTeTa4icnC3WuEyBRZGUNkwtL2eo3ymWfaNoBiacMvbWibK4FZEILVQgFtAmiboI8AhRkCl6Zg/132")) (class (id . "neXd2mpO") (semester_id . "xzX47qVY") (version . "2") (trench . "noob") (teacher (nickname . "1839681687") (wxid . "12") (qrcode (file_type . "image/png") (file_url . "https://mytikas-testing.oss-cn-beijing.aliyuncs.com/f5a01230-e8e3-11ea-94a0-7f3a1171388eheader.png?x-oss-process=style/lezhilong"))) (semester (start_date . "2020-11-28") (end_date . "2020-12-02")) (finance (type . 1))) (purchased . t) (registered . :json-false) (exam . :json-false)) (err_code . 0)))

;; (loop for i upto 10 collect i)

;; (loop for i in '(10 20 30 40) collect (* i 2))

;; (loop for (a b) in '((1 2) (3 4) (5 6))
;;       do (print (+ a b)))

;; (loop for (results code) in resp
;;       do (print results))

;; (loop for (k . v) in (cdar resp)
;;       do (message "%s: %s" k v))

;; (assoc 'a '((a (b . e)) (c . d)))
;; (caadr (assoc 'a '((a (b . e)) (c . d))))


;; "https://api.github.com/orgs/emacs-eaf/repos"

;; (require 'request)

;; (switch-to-buffer (url-retrieve-synchronously
;;  "https://api.github.com/users/vritser"))

;; (request "https://api.github.com/orgs/emacs-eaf/repos"
;;   :parser 'json-read
;;   :success (cl-function
;;             (lambda (&key data &allow-other-keys)
;;               (message data))))

;; (url-retrieve
;;  "https://api.github.com/users/vritser"
;;  (lambda (status)
;;    (goto-char url-http-end-of-headers)
;;    (print ())
;;    (let ((json-object-type 'plist)
;;          (json-key-type 'string)
;;          (json-array-type 'vector))
;;      (let ((result (json-read)))
;;        ;; Do something with RESULT here
;;        (print result)
;;        ))
;;    ) nil 'silent)


;; (defun repeat (s n)
;;   (unless (= n 0)
;;     (concat s (repeat s (- n 1)))))

;; C-x 8 RET 20e3

(defun vritser/extract-var ()
  "Extract yasnippet variable name."
  (save-excursion
    (beginning-of-line)
    (kill-word 1)
    (delete-char 1))
  (yank))

(provide 'init-funcs)
;;; init-funcs.el ends here
