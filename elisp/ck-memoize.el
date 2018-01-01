;;; ck-memoize.el --- memoizing functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Heavily based on the code of Emacs doom by @hlissner:
;;;;  https://github.com/hlissner/doom-emacs/blob/master/core/autoload/memoize.el
;;; Code:

;;;###autoload
(defvar ck-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions.
The keys are argument lists, and the value is the function's return value.")

;;;###autoload
(defun ck-memoize (name)
  "Memoizes an existing function.  NAME is a symbol."
  (let ((func (symbol-function name)))
    (put name 'function-documentation
         (concat (documentation func) " (memoized)"))
    (fset name
          `(lambda (&rest args)
             (let ((key (cons ',name args)))
               (or (gethash key ck-memoized-table)
                   (puthash key (apply ',func args)
                            ck-memoized-table)))))))

;;;###autoload
(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function.  NAME, ARGLIST, DOCSTRING and BODY have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(,(if (bound-and-true-p byte-compile-current-file)
         'with-no-warnings
       'progn)
     (defun ,name ,arglist ,@body)
(ck-memoize ',name)))

(provide 'ck-memoize)
;;; ck-memoize.el ends her
