;;; ck-modeline.el --- custom modeline configuration
;;; Commentary:
;;;  Heavily based on the code of Emacs doom by @hlissner:
;;;;  https://github.com/hlissner/.emacs.d/blob/master/core/core-ui.el
;;; Code:

(use-package powerline)
(defalias '! 'eval-when-compile)

(! (progn
     (require 'f)
     (require 'projectile)))

(defvar mode-line-height 20)
(defvar mode-line-bar          (! (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
(defvar mode-line-eldoc-bar    (! (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
(defvar mode-line-inactive-bar (! (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))

(defface mode-line-is-modified nil "Face for mode-line modified symbol")
(defface mode-line-buffer-path nil "Face for mode-line buffer file path")
(defface mode-line-highlight nil "")
(defface mode-line-2 nil "")

(defface mode-line-vcs-info nil '((t (:inherit warning))))
(defface mode-line-vcs-warning nil '((t (:inherit warning))))

; use for anzu-mode
(defface mode-line-count-face nil "")

(defun *shorten-directory (dir &optional max-length)
  "Show directory name of `DIR', reduced to `MAX-LENGTH' characters."
  (unless max-length (setq max-length (truncate (/ (window-body-width) 1.75))))
  (if (> (length dir) max-length)
      (let ((path (reverse (split-string dir "/")))
	    (output ""))
	(when (and path (equal "" (car path)))
	  (setq path (cdr path)))
	(while (and path (< (length output) (- max-length 4)))
	  (setq output (concat (car path) "/" output))
	  (setq path (cdr path)))
	(when path
	  (setq output (concat "…/" output)))
	output)
    dir))

(defun *f-dirname (path)
  "Return the dirname of PATH, but return empty string for /, ./, ../."
  (let ((dirname (f-dirname path)))
    (cond ((string= dirname "./") "")
	  ((string= dirname "../") "")
	  ((string= dirname "/") "")
	  ((eq dirname nil) "")
	  (t dirname))))

(defun *project-root-safe ()
  "Return the root of the project or nil."
    (condition-case nil
	(projectile-project-root)
      (error nil)))

(defun *project-name ()
  "Return the formatted name of an active project."
  (when (*project-root-safe)
    (propertize
     (format "%s" (projectile-project-name))
     'mouse-face 'mode-line-highlight
     'face (if active 'mode-line-buffer-path)
     'help-echo (format "@ %s" (*project-root-safe)))))


(defun non-empty? (str)
  "Return STR if non empty, nil otherwise."
  (not (or (null str) (string= "" str))))

(defun split-filename (filename)
  "Split FILENAME into (list dirname basename); dirname will be nil if no slash in path."
  (if (not (string-match "/" filename))
      (list filename nil)
    (let ((basename (file-name-nondirectory filename))
	  (dirname  (file-name-directory filename)))
      (list dirname basename))))

(defun remove-trailing-slash (name)
  "Remove a single trailing slash from NAME."
  (when (and (non-empty? name)
	     (string= "/" (substring name -1 nil)))
    (substring name 0 -1)))

(defun file-relpath (target dir)
  "Like `file-relative-name' with `file-truename' called on both TARGET and DIR."
  (file-relative-name (file-truename target)
		      (and (non-empty? dir) (file-truename dir))))

(defun *shorten-path (dir &optional max-length)
  "Show directory name of `DIR', reduced to `MAX-LENGTH' characters."
  (unless max-length (setq max-length (truncate (/ (window-body-width) 1.75))))
  (if (> (length dir) max-length)
      (let ((path (reverse (split-string dir "/")))
	    (output ""))
	(when (and path (equal "" (car path)))
	  (setq path (cdr path)))
	(while (and path (< (length output) (- max-length 4)))
	  (setq output (concat (car path) "/" output))
	  (setq path (cdr path)))
	(when path
	  (setq output (concat "…/" output)))
	output)
    dir))


(defun *ml-path (path &optional colorize)
  "Prepare PATH for mode-line: shorten, COLORIZE and add help text."
  (let ((short (*shorten-path path)))
    (propertize short
		'mouse-face 'mode-line-highlight
		'face (if colorize 'mode-line-buffer-path)
		'help-echo path)))

(defun *project-id (project-root project-name filename)
  "Generate a project id based on PROJECT-ROOT, PROJECT-NAME and FILENAME."
  (let* ((sep (if filename ":" " • "))
	 (filepath (or filename default-directory))
	 (relname (file-relpath filepath project-root))
	 (path-filename (split-filename relname))
	 (path (first path-filename))
	 (name (second path-filename))
	 )
    (concat project-name sep (*ml-path path active) name)))

(defun *buffer-state ()
  "The state of the buffer (read-only, modified, new-file)."
  (when buffer-file-name
    (propertize
     (concat (if (not (file-exists-p buffer-file-name))
                 "∄"
               (if (buffer-modified-p) "●"))
             (if buffer-read-only ""))
     'face 'mode-line-is-modified)))

(defun *buffer-name ()
  "The buffer's name."
  (string-trim-left (format-mode-line "%b")))

(defun *buffer-cwd ()
  "Displays `default-directory'."
  (propertize
   (concat "[" (*shorten-directory (abbreviate-file-name default-directory)) "]")
   'face (if active 'mode-line-2)))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (concat (symbol-name buffer-file-coding-system) " ")))

(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'mode-line-vcs-info)
                        ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                         'mode-line-vcs-warning)))))
      (if active
          (propertize backend 'face face)
        backend))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (if (and (= start 1)
             (= end pend))
        ":All"
      (cond ((= start 1) ":Top")
            ((= end pend) ":Bot")
            (t (format ":%d%%%%" (/ end 0.01 pend)))))))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the current match and the total number."
  (when (and (featurep 'anzu) anzu--state)
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'mode-line-count-face))))

(defun ck/mode-line ()
  "Our custom mode line."
  '(:eval
    (let* ((active (powerline-selected-window-active))
	   (project-root (*project-root-safe))
	   (project-name (and project-root (*project-name)))
	   (filename buffer-file-name)
	   (process (powerline-process))
	   ;; now build the mode line
           (lhs (list (propertize " " 'display (if active
						   mode-line-bar
						 mode-line-inactive-bar))
                      " "
		      (cond (project-name
			     (*project-id project-root project-name filename))
			    (filename filename)
			    (t (*buffer-name)))
                      " "
                      (*buffer-state)
		      (unless (or project-name filename)
			(concat (*buffer-cwd) " "))
		      (if process (concat process " "))
		      " "
		      (powerline-major-mode)
		      "  "
		      (powerline-minor-modes)
		      "  "
		      (*anzu)
		      ))
	   (rhs (list (*buffer-encoding-abbrev)
                      (*vc)
                      (propertize (concat " (%l,%c) " (*buffer-position)) 'face (if active 'mode-line-2))
		      ))
	   (center (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs))))))))
	   )
      (list lhs center rhs))))

(provide 'ck-modeline)
;;; ck-modeline.el ends here
