;;; ck-mail.el --- my mail related setup -*- lexical-binding: t -*-

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

(defun ck/mk-name-email (email)
  "Return an combination of EMAIL and name."
  (concat user-full-name " <" email ">"))

(defun ck/mail-account-from-js (js)
  "Make one mail account from JS."
  (let ((id     (gethash "id" js nil))
	(refers (gethash "refers" js nil))
	(from   (ck/mk-name-email (gethash "from" js nil)))
	(orga   (gethash "orga" js nil))
	)
    ;; id, Refers, From, Organization, Headers, Body, Signature
  (list id refers from orga nil nil nil)))

(defun ck/mail-mk-identiy-alist ()
  "Create the gnus indenity list from a json file."
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json (json-read-file "~/.config/gnus-alias.js"))
	 (accounts (gethash "accounts" json)))
    (mapcar 'ck/mail-account-from-js accounts)))


(defun ck/gnus-alias-setup ()
  "GNUS alias setup."
  ;; Refers, From, Organization, Headers, Body, Signature
  (setq gnus-alias-identity-alist (ck/mail-mk-identiy-alist)
	gnus-alias-default-identity (caar gnus-alias-identity-alist)
	gnus-alias-override-user-mail-address t
	gnus-alias-identity-rules '(("to-redhat" ("To" ".+@redhat\\.com" current) "redhat"))))

(provide 'ck-mail)
;;; mail.el ends here
