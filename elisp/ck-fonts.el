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
  '("Fira Code"
    "Hasklig"
    "Source Code Pro"
    "Inconsolata"
    "Consolas"
    "Menlo"
    "Monospace"))

(defun ck/gnome-mono-font ()
  "Return the 's monospace font as a list with name and size."
  (let ((input (shell-command-to-string
		 (mapconcat 'identity
			    '("gsettings get org.gnome.desktop.interface"
			      "monospace-font-name")
			    " "))))
    (when (string-match "[ \t\"]*'\\(.*\\) \\([0-9]+\\)'[ \n\"]*$" input)
      (list (match-string 1 input)
	    (string-to-number (match-string 2 input))))))

(defun ck/font-size-for-system ()
  "Determine the optimal font size to use."
  (cond ((eq system-type 'gnu/linux)
	 (let ((sys-font (ck/gnome-mono-font)))
	   (if sys-font
	       (nth 1 sys-font)
	     11)))
	  ((eq system-type 'darwin)
	   12)
	  ((eq system-type 'windows-nt)
	   10)
	  (t 12)))

(defun font-existsp (name)
  "Check if a font with a given NAME (or its Powerline version)."
  (if (find-font (font-spec :name name))
      name))

(defun ck/mk-font-checked (name size)
  "Format NAME and SIZE if NAME is available."
  (if (font-existsp name)
      (format "%s-%d" name size)))

(defun ck-first-font (lst)
  "Return the first valid font from LST."
  (or (font-existsp (car lst))
      (ck-first-font (cdr lst))))


(defcustom ck-font-size
  (ck/font-size-for-system)
  "The default font size to use."
  :type 'number
  :group 'ck)

(defcustom ck-font
  (ck-first-font ck-fonts)
  "The default font to use."
  :options ck-fonts
  :type 'string
  :group 'ck)

;;;###autoload
(defun ck-set-font ()
  "Set the default font from the ck-fonts list."
  (let ((font (ck/mk-font-checked ck-font ck-font-size)))
    (if font
	(progn
	  (set-face-attribute 'default nil :font font)
	  (when (string-prefix-p "PragmataPro" font)
	    (require 'ck-pragmata-pro)
	    (ck/pragmata-pro-prettify-for '(prog-mode))))
      (message "Could not apply font [%s]" font))))

(provide 'ck-fonts)
;;; ck-fonts.el ends here
