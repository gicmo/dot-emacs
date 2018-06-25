;;; ck-core.el --- initial setup -*- lexical-binding: t -*-

;; Copyright (C) 2018  Christian Kellner

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

(defvar ck--file-name-handler-alist file-name-handler-alist)

(defun ck/startup-finish ()
    "Reset `gc-cons-threshold', `gc-cons-percentage' and `file-name-handler-alist'."

    ;; If you forget to reset this, you'll get stuttering and random freezes!
    (setq gc-cons-threshold (* 16 1024 1024)
          gc-cons-percentage 0.1
          file-name-handler-alist ck--file-name-handler-alist)
    (message "initialization done"))


;; -=[ small utility functions

(defun ck/show-trailing-ws()
  "Show trailing whitespaces."
  (setq-local show-trailing-whitespace t))


;; -=[ OS specific setup

(defun ck/init-osx ()
  "OS/X specific global initialization."
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (setq mac-option-modifier 'meta)
  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))
  (setq-default locate-command "mdfind")
  (when (display-graphic-p)
    (setq-default mac-emulate-three-button-mouse t)
    (global-set-key (kbd "M-`") 'other-frame)))

(defun ck/init-linux ()
  "GNU/Linux specific global initialization."
  (let* ((in-flatpak (file-exists-p "/.flatpak-info"))
	 (prefix (if in-flatpak "/app" "/usr"))
	 (default-directory (concat prefix "/share/emacs/site-lisp/")))
    (message "linux init")
    (normal-top-level-add-subdirs-to-load-path)))

;; -=[ warp 10

;;   [ custom - write custom's settings to separate file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/elisp/")



;; -=[ engage
(eval-and-compile
   (message "initializing")
  (setq gc-cons-threshold (* 256 1024 1024)
	gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook #'ck/startup-finish)

  ; os specific stuff
  (cond
   ((eq system-type 'gnu/linux) (ck/init-linux))
   ((eq system-type 'darwin) (ck/init-osx))))

(provide 'ck-core)
;;; ck-core.el ends here
