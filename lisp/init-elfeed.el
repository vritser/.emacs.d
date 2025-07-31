;;; init-elfeed.el --- elfeed                          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  vritser

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

(use-package elfeed
  :config
  (setq-default elfeed-feeds
        '(("https://sachachua.com/blog/feed/" SachaChua)
          ("https://nullprogram.com/feed/" nullprogram)
          "https://ag91.github.io/rss.xml"
          "https://emacs.stackexchange.com/feeds"
          ("https://cprss.s3.amazonaws.com/javascriptweekly.com.xml" javascript)
          ("https://cprss.s3.amazonaws.com/rubyweekly.com.xml" ruby)
          ("https://cprss.s3.amazonaws.com/frontendfoc.us.xml" frontend))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
