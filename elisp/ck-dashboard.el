;;; ck-dashboard.el --- My initial emacs buffer

;; Copyright (C) 2016  Christian Kellner

;; Author: Christian Kellner

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(defvar dashboard-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map "r" 'recentf-open-files)
     (define-key map "b" 'bookmark-bmenu-list)
     (define-key map "p" 'projectile-switch-project)
     (define-key map "s" (lambda () (interactive) (switch-to-buffer (get-buffer "*scratch*"))))
     (define-key map "m" (lambda () (interactive) (switch-to-buffer (get-buffer "*Messages*"))))
     map)
   "Keymap for `dashboard-mode'.")

;;;###autoload
(define-derived-mode dashboard-mode nil "Dashboard"
  "A major mode acting as a dashboard.

\\{dashboard-mode}"
   :syntax-table nil
   :abbrev-table nil
   (setq truncate-lines t)
   ;(use-local-map dashboard-mode-map)
   )

(defun dashboard-banner (head)
  "Insert a cool banner with a left pad of size HEAD."
  (insert
   (propertize
    (concat
     head "███████╗███╗***███╗*█████╗**██████╗███████╗\n"
     head "██╔════╝████╗*████║██╔══██╗██╔════╝██╔════╝\n"
     head "█████╗**██╔████╔██║███████║██║*****███████╗\n"
     head "██╔══╝**██║╚██╔╝██║██╔══██║██║*****╚════██║\n"
     head "███████╗██║*╚═╝*██║██║**██║╚██████╗███████║\n"
     head "╚══════╝╚═╝*****╚═╝╚═╝**╚═╝*╚═════╝╚══════╝\n")
    'face 'font-lock-comment-face))
   (insert
    (propertize
     (format "%s %s %d.%d"
	     head (make-string 35 ? )
	     emacs-major-version emacs-minor-version)
     'face 'font-lock-comment-face)))

(defun dashboard--insert-mnemonic (head key help)
  "Insert a line a la: HEAD [KEY] HELP."
  (insert
   (concat
    head "[" (propertize key 'face 'font-lock-keyword-face)
    "] " (propertize help 'face 'font-lock-comment-face)
    "\n")))

(defun dashboard-make ()
  "Create the content of the dashboard."
  (let ((head (make-string 3 ? )))
    (insert "\n\n")
    (dashboard-banner head)
    (insert "\n\n")
    (dashboard--insert-mnemonic head "r" "Recently opened files")
    (dashboard--insert-mnemonic head "b" "Open bookmarked file")
    (dashboard--insert-mnemonic head "p" "Switch to project")
    (dashboard--insert-mnemonic head "m" "*Messages* buffer")
    (dashboard--insert-mnemonic head "s" "*scratch* buffer")
    (insert "\n")))

(defun dashboard--startup-hook ()
  "Startup hook for the dashboard."
  (interactive)
  (with-current-buffer (get-buffer "*Dashboard*")
    (let ((buffer-read-only nil)
	  (head (make-string 3 ? )))
      (goto-char (point-max))
      (insert head (propertize (concat " >  Loaded in " (emacs-init-time) ".\n")
			       'face 'font-lock-comment-face))
      (set-buffer-modified-p nil))))

;;;###autoload
(defun dashboard-show ()
  "Show the dashboard."
  (interactive)
  (with-current-buffer (get-buffer-create "*Dashboard*")
    (erase-buffer)
    (dashboard-mode)
    (dashboard-make)
    (linum-mode -1)
    (switch-to-buffer (current-buffer))
    (if after-init-time
	(insert (propertize " > Loaded in " (emacs-init-time) ".\n"
			    'face 'font-lock-comment-face))
      (add-hook 'emacs-startup-hook 'dashboard--startup-hook t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(provide 'ck-dashboard)
;;; ck-dashboard.el ends here

