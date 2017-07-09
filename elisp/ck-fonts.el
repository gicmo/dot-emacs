;;; ck-fonts.el --- Font setup -*- lexical-binding: t; -*-
;; Copyright (C) 2017  Christian Kellner

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
(defconst ck-fonts
      '(("Hasklig")
	("Source Code Pro")
	("Inconsolata")
	("Menlo")
	("Monospace")))

(defun ck/system-font ()
  "Return the system's monospace font as a list with name and size."
  (cond ((eq system-type 'gnu/linux)
	 (let ((input (shell-command-to-string
		       (mapconcat 'identity
				  '("gsettings get org.gnome.desktop.interface"
				    "monospace-font-name")
				  " "))))
	   (when (string-match "[ \t\"]*'\\(.*\\) \\([0-9]+\\)'[ \n\"]*$" input)
	     (list (match-string 1 input)
		   (string-to-number (match-string 2 input))))))))

(defun ck/font-size-for-system ()
  "Determine the optimal font size to use."
  (cond ((eq system-type 'gnu/linux)
	 (let ((sys-font (ck/system-font)))
	   (if sys-font
	       (nth 1 sys-font)
	     11
	     )))
	  ((eq system-type 'darwin)
	   12)
	  (t 12)))

(defvar ck/default-font-size
  (ck/font-size-for-system)
  "The default font size to use.")

(defun font-existsp (name)
  "Check if a font with a given NAME (or its Powerline version)."
  (cond ((find-font (font-spec :name (concat name " for Powerline")))
	 (format "%s for Powerline-%d" name ck/default-font-size))
	((find-font (font-spec :name name))
	 (format "%s-%d" name ck/default-font-size))))

(defun ck-first-font (lst)
  "Return the first valid font from LST."
  (or (apply 'font-existsp (car lst))
      (ck-first-font (cdr lst))))

;;;###autoload
(defun ck-set-font ()
  "Set the default font from the ck-fonts list."
  (set-face-attribute 'default nil :font (ck-first-font ck-fonts)))

(provide 'ck-fonts)
;;; ck-fonts.el ends here
