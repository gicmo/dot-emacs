;;; dashboard.el --- My initial emacs buffer

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

(require 'recentf)

(defvar dashboard-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map "r" 'recentf-open-files)
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

(defun dashboard-make ()
  "Create the content of the dashboard."
  (let ((head (make-string 3 ? )))
    (insert head "Loaded in " (emacs-init-time) ".\n"))
  )

(defun dashboard-show ()
  "Show the dashboard."
  (with-current-buffer (get-buffer-create "*Dashboard*")
    (erase-buffer)
    (dashboard-mode)
    (dashboard-make)
    (setq buffer-read-only t)
    (switch-to-buffer (current-buffer))
    ))

(provide 'dashboard)
;;; dashboard.el ends here

