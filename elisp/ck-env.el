;;; ck-env.el --- cached import of the login shell environment -*- lexical-binding: t -*-
;;; Commentary:
;;;  Asking the login shell for its environment costs a shell spawn on every
;;;  startup, so keep the answer in a cache file.  The environment rarely
;;;  changes on a given machine; when it does, refresh it with `ck-env-refresh'.
;;; Code:

(defgroup ck-env nil
  "Import environment variables from the login shell."
  :group 'ck)

(defcustom ck-env-variables
  '("EMAIL" "GOPATH" "RUST_SRC_PATH" "WORKON_HOME" "MANPATH" "PATH")
  "Environment variables to import from the login shell."
  :type '(repeat string)
  :group 'ck-env)

(defcustom ck-env-file
  (expand-file-name "emacs/env.eld"
		    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "File the imported environment is cached in."
  :type 'file
  :group 'ck-env)

(declare-function exec-path-from-shell-getenvs "ext:exec-path-from-shell.el")

(defun ck-env--read ()
  "Return the cached environment, or nil if there is none to read."
  (and (file-readable-p ck-env-file)
       (ignore-errors
	 (with-temp-buffer
	   (insert-file-contents ck-env-file)
	   (read (current-buffer))))))

(defun ck-env--fetch ()
  "Ask the login shell for `ck-env-variables' and cache the answer."
  (require 'exec-path-from-shell)
  (let ((env (exec-path-from-shell-getenvs ck-env-variables)))
    (make-directory (file-name-directory ck-env-file) t)
    (with-file-modes #o600
      (with-temp-file ck-env-file
	(prin1 env (current-buffer))
	(insert "\n")))
    env))

(defun ck-env--apply (env)
  "Set the variables in ENV, an alist of (NAME . VALUE)."
  (pcase-dolist (`(,name . ,value) env)
    (when value
      (setenv name value)
      (when (string-equal name "PATH")
	(setq exec-path (append (parse-colon-path value)
				(list exec-directory)))))))

;;;###autoload
(defun ck-env-load ()
  "Apply the shell environment, asking the shell if it is not cached yet."
  (ck-env--apply (or (ck-env--read) (ck-env--fetch))))

;;;###autoload
(defun ck-env-refresh ()
  "Ask the login shell for the environment again and apply it."
  (interactive)
  (let ((env (ck-env--fetch)))
    (ck-env--apply env)
    (message "ck-env: imported %d variables" (length env))))

(provide 'ck-env)
;;; ck-env.el ends here
