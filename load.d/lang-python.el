;; Python
;; pymacs - load manually for now
(defun load-ropemacs()
     (interactive)
     (require 'pymacs)
     (autoload 'pymacs-apply "pymacs")
     (autoload 'pymacs-call "pymacs")
     (autoload 'pymacs-eval "pymacs" nil t)
     (autoload 'pymacs-exec "pymacs" nil t)
     (autoload 'pymacs-load "pymacs" nil t)
     (pymacs-load "ropemacs" "rope-")
     (setq ropemacs-enable-autoimport t)
     (setq ropemacs-confirm-saving 'nil))
