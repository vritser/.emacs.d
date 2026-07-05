;;; init-project.el --- Project management configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2024  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: project

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
;; Built-in project.el configuration.
;;

;;; Code:

(with-eval-after-load 'project
  (setq project-switch-commands 'project-find-file))

;;
;; Project root detection (fallback when project.el can't find one)
;;

(defun yf--guess-project-root ()
  "Guess a project root for the current buffer when `project-current' fails.

Walk up from the buffer's file looking for common project marker
files (package.json, Cargo.toml, Makefile, etc.).  Return the
nearest directory containing one, or the file's own directory if
nothing matches, or nil when not visiting a file."
  (let ((file (buffer-file-name)))
    (when file
      (let ((start-dir (file-name-directory file)))
        (or (locate-dominating-file
             start-dir
             (lambda (d)
               (cl-some (lambda (f) (file-exists-p (expand-file-name f d)))
                        '("package.json" "Cargo.toml" "go.mod"
                          "setup.py" "pyproject.toml" "CMakeLists.txt"
                          "Makefile" "BUILD" "WORKSPACE" "pom.xml"
                          "build.gradle"))))
            start-dir)))))

;;;###autoload
(defun yf/bash-dwim (arg)
  "Open a ghostel terminal at a sensible working directory.

If the current buffer belongs to a project (via `project-current'),
open a ghostel terminal at the project root, identically to
`ghostel-project'.

If not in a project, try to find the best root directory for the
current file \u2014 checking git root, then common project marker files
(Makefile, package.json, Cargo.toml, etc.) \u2014 and open a ghostel
there.  Falls back to the file's directory.

When called from a non-file buffer (e.g. Dired), opens a shell in
`default-directory'.

ARG is passed through to `ghostel' (prefix arg controls new buffer
vs. switching, as in `ghostel')."
  (interactive "P")
  (let ((proj (ignore-errors (project-current t))))
    (if proj
        ;; In a recognized project: delegate to ghostel-project
        (ghostel-project arg)
      ;; Not in a project: guess a sensible root, then open ghostel
      (let* ((dir (or (yf--guess-project-root) default-directory))
             (default-directory (expand-file-name dir))
             (proj2 (ignore-errors
                      (let ((default-directory dir))
                        (project-current nil))))
             (ghostel-buffer-name
              (if proj2
                  (project-prefixed-buffer-name
                   (string-trim ghostel-buffer-name "*" "*"))
                "*ghostel*")))
        (ghostel arg)))))

;;
;; Ghostel terminal (project-aware shell)
;;

(use-package ghostel
  :ensure t)

(provide 'init-project)
;;; init-project.el ends here
