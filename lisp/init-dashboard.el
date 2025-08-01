;;; init-dashboard.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: extensions, faces

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

(use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines persp-special-last-buffer
    :functions (nerd-icons-faicon
                nerd-icons-mdicon
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face
    (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    (dashboard-items-face ((t (:weight normal))))
    (dashboard-no-items-face ((t (:weight normal))))
    ;; :pretty-hydra
    ;; ((:title (pretty-hydra-title "Dashboard" 'material "nf-md-dashboard" :height 1.1 :v-adjust -0.225)
    ;;   :color pink :quit-key "q")
    ;;  ("Navigator"
    ;;   (("U" update-config-and-packages "update" :exit t)
    ;;    ("H" browse-homepage "homepage" :exit t)
    ;;    ("R" restore-session "recover session" :exit t)
    ;;    ("L" persp-load-state-from-file "list sessions" :exit t)
    ;;    ("S" open-custom-file "settings" :exit t))
    ;;   "Section"
    ;;   (("}" dashboard-next-section "next")
    ;;    ("{" dashboard-previous-section "previous")
    ;;    ("r" dashboard-goto-recent-files "recent files")
    ;;    ("m" dashboard-goto-bookmarks "projects")
    ;;    ("p" dashboard-goto-projects "bookmarks"))
    ;;   "Item"
    ;;   (("RET" widget-button-press "open" :exit t)
    ;;    ("<tab>" widget-forward "next")
    ;;    ("C-i" widget-forward "next")
    ;;    ("<backtab>" widget-backward "previous")
    ;;    ("C-n" next-line "next line")
    ;;    ("C-p" previous-line "previous  line"))
    ;;   "Misc"
    ;;   (("<f2>" open-dashboard "open" :exit t)
    ;;    ("g" dashboard-refresh-buffer "refresh" :exit t)
    ;;    ("Q" quit-dashboard "quit" :exit t))))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("H" . browse-homepage)
           ("R" . restore-session)
           ("L" . persp-load-state-from-file)
           ("S" . open-custom-file)
           ("U" . update-config-and-packages)
           ("q" . quit-dashboard)
           ("h" . dashboard-hydra/body)
           ("?" . dashboard-hydra/body)
           ("p" . dashboard-goto-projects))
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
    :init
    (setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-page-separator "\n\f\n"
          ;; dashboard-projects-backend 'project-el
          dashboard-path-style 'trucate-middle
          dashboard-path-max-length 60
          dashboard-center-content t
          dashboard-vertically-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer)

          dashboard-display-icons-p #'icons-displayable-p
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda    . "nf-oct-calendar")
                                    (projects  . "nf-oct-briefcase")
                                    (registers . "nf-oct-database"))

          dashboard-navigator-buttons
          `(((,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-github" :height 1.4))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url centaur-homepage)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-session)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-tools" :height 1.3))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
             (,(when (icons-displayable-p)
                 (nerd-icons-mdicon "nf-md-update" :height 1.3))
              "Update" "Update Centaur Emacs"
              (lambda (&rest _) (centaur-update)))
             (,(if (icons-displayable-p)
                   (nerd-icons-mdicon "nf-md-help" :height 1.2)
                 "?")
              "" "Help (?/h)"
              (lambda (&rest _) (dashboard-hydra/body)))))

          ;; dashboard-footer (format "Powered by vritser, %s" (format-time-string "%Y"))
          dashboard-footer-icon
          (if (icons-displayable-p)
              (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
            (propertize ">" 'face 'dashboard-footer)))
    (dashboard-setup-startup-hook)
    :config
    (defun my-banner-path (&rest _)
      "Return the full path to banner."
      (expand-file-name "banner.txt" user-emacs-directory))
    ;; (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
           (message "Error: Unable to restore last session -- %s" err)))
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))
        (message "Done")))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (funcall (local-key-binding "m"))))


(provide 'init-dashboard)
;;; init-dashboard.el ends here
