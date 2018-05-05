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



(defun ck/mu4-gen-match-func (maildir)
  "Generate a matching function for MAILDIR."
    `(lambda (msg)
	(when msg
	  (string-prefix-p ,maildir
			   (mu4e-message-field msg :maildir)))))

(defun ck/mu4e-context-from-js (js)
  "Make one mail account from JS."
  (let* ((id       (gethash "id"     js nil))
	 (refers   (gethash "refers" js nil))
	 (from     (gethash "from"   js nil))
	 (orga     (gethash "orga"   js nil))
	 (is-gmail (gethash "gmail"  js nil))
	 (maildir  (concat "/" (file-name-as-directory id)))
	)
    (unless refers
      (make-mu4e-context
       :name id
       :match-func (ck/mu4-gen-match-func maildir)
       :vars `((user-mail-address . ,from)
	       (mu4e-sent-folder . ,(concat maildir "Sent"))
	       (mu4e-drafts-folder . ,(concat maildir "Drafts"))
	       (mu4e-trash-folder .  ,(concat maildir "Trash"))
	       (mu4e-refile-folder . ,(concat maildir "Archive"))
	       (mu4e-sent-messages-behavior . ,(if is-gmail 'delete 'sent)))))))

(defun ck/mail-mk-mu4e-contexts ()
  "Create the gnus indenity list from a json file."
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json (json-read-file "~/.config/gnus-alias.js"))
	 (accounts (gethash "accounts" json)))
    (delq nil
	  (mapcar 'ck/mu4e-context-from-js accounts))))

;; Taken from https://github.com/fjl/dotemacs/lisp/init-mu4e.el
;; Patch the trash mark action to not mark the message as trashed.
;; mbsync interprets 'trashed' as 'deleted', so the messages are
;; expunged. Move them to the trash folder instead.
;;
;; Also mark them as read when moving.

(defvar ck/mu4e-trash-mark
  `(trash :char "d"
          :prompt "dtrash"
          :dyn-target ,(lambda (target msg) (mu4e-get-trash-folder msg))
          :action ,(lambda (docid msg target) (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-N"))))

(defun ck/mu4e-patch-trash (markslist)
  "Patch MARKSLIST so the trash action does not set the T flag."
  (mapcar (lambda (m)
	    (if (eq (car m) 'trash) ck/mu4e-trash-mark m))
	  markslist))

(provide 'ck-mail)
;;; ck-mail.el ends here
