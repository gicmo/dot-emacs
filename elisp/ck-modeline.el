;;; ck-modeline.el --- custom modeline configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;  Heavily based on the code of Emacs doom by @hlissner:
;;;;  https://github.com/hlissner/.emacs.d/blob/master/core/core-ui.el
;;; Code:

(defalias '! 'eval-when-compile)

(use-package ck-memoize
  :commands ck-memoize
  :ensure f)

(use-package all-the-icons
  :commands (all-the-icons-icon-for-buffer
	     all-the-icons-icon-family-for-buffer))

(use-package f
  :commands f-dirname)

(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(defgroup +ck-modeline nil
  ""
  :group 'ck)

(defface mode-line-is-modified nil "Face for mode-line modified symbol")
(defface mode-line-buffer-path nil "Face for mode-line buffer file path")

(defface ck-modeline-highlight
  '((t (:inherit mode-line-highlight)))
  "Face for bright segments of the mode-line."
  :group '+ck-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments."
  :group '+doom-modeline)

(defface ck-modeline-panel
  '((t (:inherit doom-modeline-panel)))
  "Face for 'X out of Y' segments"
  :group '+ck-modeline)

(defface ck-modeline-info
  '((t (:inherit success)))
  "Face for info-level messages in the modeline."
  :group '+ck-modeline)

(defface ck-modeline-warning
  '((t (:inherit warning)))
  "Face for warnings in the modeline."
  :group '+ck-modeline)

(defface ck-modeline-urgent
  '((t (:inherit error)))
  "Face for errors in the modeline."
  :group '+ck-modeline)

(defface ck-modeline-dimmed
  '((t (:inherit shadow)))
  "Face for less important information in the modeline"
  :group '+ck-modeline)

;; Bar
(defface ck-modeline-bar
  '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+ck-modeline)

(defface ck-modline-bar-eldoc
  '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is active."
  :group '+ck-modeline)

(defface ck-modeline-bar-inactive
  '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+ck-modeline)

(defvar ck-modeline-height 20)

;;; Forward declarations of Optional Dependencies
(declare-function anzu--format-here-position "ext:anzu.el")
(declare-function flycheck-count-errors  "ext:flycheck.el")
(declare-function mc/num-cursors "ext:multiple-cursors.el")
(declare-function projectile-project-root "ext:projectile.el")
(declare-function projectile-project-name "ext:projectile.el")

(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
;(defvar org-clock-current-task)

;; Helper to build modelines
(defsubst ck/ml-segment-intern (name)
  "Return the internal NAME for a segment."
  (intern (format "ck/ml--segment-%s" name)))

(defsubst ck/ml-format-intern (name)
  "Return the internal NAME for a mode line format."
  (intern (format "ck/ml--format-%s" name)))

(defsubst ck/ml-prepare-segments (segments)
  "Prepare modeline SEGMENTS."
  (mapcar (lambda (s) (if (stringp s) s (ck/ml-segment-intern s))) segments))

(defsubst ck/ml-form-to-body (forms active)
  "Convert segments in FORMS to mode line body passing ACTIVE along."
  (mapconcat (seq-filter 'identity
			 (lambda (x) (if (stringp x) x (funcall x active))))
	     forms " "))

(defmacro def-ml-segment! (name args &body body)
  "Define a modeline segment with NAME, ARGS and &BODY and byte compile it."
  (declare (indent defun) (doc-string 3))
  (let ((sym (ck/ml-segment-intern name)))
    `(progn
       (defun ,sym (,@args) (,@body))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format with NAME, LHS and RHS and byte compiles it."
  (let ((sym (ck/ml-format-intern name))
	(lhs-forms (ck/ml-prepare-segments lhs))
	(rhs-forms (ck/ml-prepare-segments rhs)))
    `(progn
       (defun ,sym ()
	 (let* ((active (eq ck-modeline-current-window (selected-window)))
		(lhs-body (ck/ml-form-to-body '(,@lhs-forms) active))
		(rhs-body (ck/ml-form-to-body '(,@rhs-forms) active))
		(rhs-str  (format-mode-line rhs-body)))
	   (list lhs-body
		 (propertize
		  " " 'display
		  `((space :align-to (- (+ right right-fringe right-margin)
					,(+ 1 (string-width rhs-str))))))
		 rhs-str)))
       ,(when (bound-and-true-p byte-compile-current-file)
	  `(let (byte-compile-warnings)
	     (byte-compile #',sym))))))

(defun ck/modeline (name)
  "Return a mode line configuration associated with NAME."
  (let ((fn (ck/ml-format-intern name)))
    (when (functionp fn)
      `(:eval (,fn)))))

;;;###autoload
(defun ck/modeline-set (name &optional default)
  "Set the modeline format with NAME.
DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (ck/modeline name)))
    (when modeline
      (setf (if default
		(default-value 'mode-line-format)
	      (buffer-local-value 'mode-line-format (current-buffer)))
	    modeline))))

;; helper to create bar pixmap
;; Adapted from @hlissner's version of `powerline's `pl/make-xpm'.
(defun ck/ml-make-xpm-bar (color height width)
  "Create an XPM bitmap for COLOR with HEIGHT x WIDTH dimensions."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       "/* XPM */\nstatic char *bar[] = "
       (format "{\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(ck-memoize 'ck/ml-make-xpm-bar)

;; keep track of the current active window
(defvar ck-modeline-current-window (frame-selected-window))

(defun ck/ml-set-current-window (&rest _)
  "Set `ck-modeline-current-window' appropriately."
  (let ((win (frame-selected-window)))
    (unless (or (not win) (minibuffer-window-active-p win))
      (setq ck-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'ck/ml-set-current-window)
(add-hook 'focus-in-hook #'ck/ml-set-current-window)
(advice-add #'handle-switch-frame :after #'ck/ml-set-current-window)
(advice-add #'select-window :after #'ck/ml-set-current-window)

;; mode line segments
(def-ml-segment! bar (active)
  "Show a bar, honoring ACTIVE."
  (if (display-graphic-p)
      (let* ((face  (if active 'ck-modeline-bar 'ck-modeline-bar-inactive))
	     (color (face-background face nil 't)))
	(ck/ml-make-xpm-bar color ck-modeline-height 3))
    ""))

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

(defun ck/ml-project-name (active)
  "Return the formatted name of an active project (honor ACTIVE)."
  (when (*project-root-safe)
    (propertize
     (format "%s" (projectile-project-name))
     'mouse-face 'ck-modeline-highlight
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
		'mouse-face 'ck-modeline-highlight
		'face (if colorize 'mode-line-buffer-path)
		'help-echo path)))

(defun ck/ml-project-id (project-root project-name filename active)
  "Generate a project id based on PROJECT-ROOT, PROJECT-NAME and FILENAME (honor ACTIVE)."
  (let* ((sep (if filename ":" " • "))
	 (filepath (or filename default-directory))
	 (relname (file-relpath filepath project-root))
	 (path-filename (split-filename relname))
	 (path (first path-filename))
	 (name (second path-filename))
	 )
    (concat project-name sep (*ml-path path active) name)))

(def-ml-segment! process (_)
    "Show the currently running process, if any."
    (cond
     ((symbolp mode-line-process) (symbol-value mode-line-process))
     ((listp mode-line-process) (format-mode-line mode-line-process))
     (t mode-line-process)))

(def-ml-segment! buffer-state (_)
  "The state of the buffer (read-only, modified, new-file)."
  (when buffer-file-name
    (propertize
     (if (buffer-modified-p)
	 (if (file-exists-p buffer-file-name)
	     "●"
	   "◉")
       (if buffer-read-only
	   ""
	 "○"))
     'face 'mode-line-is-modified)))

(defun *buffer-name ()
  "The buffer's name."
  (string-trim-left (format-mode-line "%b")))

(defun ck/ml-buffer-cwd (active)
  "Displays `default-directory' (honoring ACTIVE)."
  (propertize
   (concat "[" (*shorten-directory (abbreviate-file-name default-directory)) "]")
   'face (if active 'ck-modeline-dimmed)))

(def-ml-segment! buffer-encoding-abbrev (active)
  "The line ending convention used in the buffer (honour ACTIVE)."
  (when buffer-file-name
    (let* ((eol-type (coding-system-eol-type buffer-file-coding-system))
	   (eol-str (cond ((eq eol-type 0) "LF")
			  ((eq eol-type 1) "CRLF")
			  ((eq eol-type 2) "CR")))
	   (sys (coding-system-plist buffer-file-coding-system))
	   (sys-name (plist-get sys :name))
	   (sys-cat (plist-get sys :category)))
      (propertize
       (cond ((memq sys-cat '(coding-category-undecided coding-category-utf-8))
	      "utf-8")
	     (t (downcase (symbol-name sys-name))))
       'mouse-face 'ck-modeline-highlight
       'face (if active 'ck-modeline-dimmed)
       'help-echo eol-str
       ))))

(defun ck/ml-vc-face (active)
  "Face for current vc, honoring ACTIVE."
  (when active
    (let ((state (vc-state buffer-file-name)))
      (cond ((memq state '(edited added))
	     'ck-modeline-info)
	    ((memq state '(removed needs-merge needs-update conflict removed unregistered))
	     'ck-modeline-warning)))))

(def-ml-segment! vc (active)
  "Displays the current branch, colored based on its state (honoring ACTIVE)."
  (when vc-mode
    (let* ((branch (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))
	   (face   (ck/ml-vc-face active))
	   (icon   (ck/ml-icon "octicon" "git-branch" :fallback "" :v-adjust 0.1 :height 0.8 :face face)))
      (concat icon " " (propertize branch 'face face)))))

(def-ml-segment! buffer-position (active)
  "A more vim-like buffer position (honoring ACTIVE)."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (propertize
     (if (and (= start 1)
	      (= end pend))
	 ":All"
       (cond ((= start 1) ":Top")
	     ((= end pend) ":Bot")
	     (t (format ":%d%%%%" (/ end 0.01 pend)))))
     'face (if active 'ck-modeline-dimmed))))

(make-variable-buffer-local 'anzu--state)
(def-ml-segment! anzu (active)
  "Show the current match and the total number (honoring ACTIVE)."
  (when (and (featurep 'anzu) anzu--state)
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'ck-modeline-panel))))

(def-ml-segment! num-cursors (ml-active)
  "If more then one cursor is active, show how many; highlighted if ML-ACTIVE."
  (when (fboundp 'mc/num-cursors)
    (let ((n (mc/num-cursors)))
      (when (> n 1)
	(propertize (format " %d " n) 'face (if ml-active 'ck-modeline-panel))))))

(defun ck/have-all-the-iconsp ()
  "Check if we have all-the-icons support."
  (and (window-system)
       (find-font (font-spec :family "all-the-icons"))))

(defvar ck/use-icon-font (ck/have-all-the-iconsp)
  "Use and icon (from all the icons) for the major mode.")

(defun ck/ml-icon (family name &rest args)
  "Get an icon for the FAMILY with the NAME and optionally a :fallback in ARGS."
  (if ck/use-icon-font
      (apply (cond
	      ((equal family "octicon") 'all-the-icons-octicon)
	      ((equal family "material") 'all-the-icons-material))
	     (append (list name) args))
    (propertize (or (plist-get args :fallback) name) 'face (plist-get args :face))))

(ck-memoize 'ck/ml-icon)

(def-ml-segment! indicator-for-major-mode (_)
  "Get an indicator (icon or name) for the major mode."
  (let* ((maybe-icon (and ck/use-icon-font (all-the-icons-icon-for-buffer)))
	 (icon (if (not (symbolp maybe-icon)) maybe-icon))
	 (face (if icon (list :family (all-the-icons-icon-family-for-buffer) :height 0.8)))
	 (elevation (if icon 0.1 0.0))
	 (the-mode (format-mode-line mode-name))
	 (minor-modes (format-mode-line minor-mode-alist))
	 (indicator (or icon the-mode)))
    (propertize indicator
		'help-echo (format "%s | %s" the-mode minor-modes)
		'display `(raise ,elevation)
		'face face
		'mouse-face 'ck-modeline-highlight
		'local-map (let ((map (make-sparse-keymap)))
			     (define-key map [mode-line down-mouse-1]
			       `(menu-item ,(purecopy "Menu Bar") ignore
					   :filter (lambda (_) (mouse-menu-major-mode-map))))
			     (define-key map [mode-line mouse-2] 'describe-mode)
			     (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
			     map))))

;;
(defun ck/ml-mm-mouse-menu (mm)
  "Get the mouse menu for the minor mode MM."
  (let* ((map (cdr-safe (assoc mm minor-mode-map-alist)))
	 (menu (and (keymapp map) (lookup-key map [menu-bar])))
	 (mouse-menu (if menu (mouse-menu-non-singleton menu))))
    (popup-menu mouse-menu)))

(defun ck/ml-make-mm-mouse-map (mm)
  "Make a mouse map for minor mode MM."
  (let ((my-map (make-sparse-keymap)))
    (define-key my-map
      [mode-line down-mouse-1]
      `(lambda (event)
	 (interactive "@e")
	 (ck/ml-mm-mouse-menu (quote ,mm))))
    my-map))

(defun ck/ml-make-minor-mode (mm str)
  "Make a insertable string for the modeline MM replace with STR."
  (propertize str
	      'mouse-face 'ck-modeline-highlight
	      'local-map (ck/ml-make-mm-mouse-map mm)))

(defun ck/list-minor-modes (modes)
  "Return the intersection of MODES and active minor modes."
  (seq-filter (lambda (mm)
		(let* ((name (car mm))
		       (active (and (boundp name) (symbol-value name))))
		(and active (seq-filter (lambda (x) (eq name x)) modes))))
	      minor-mode-alist))

(defun ck/mm-fill-mode ()
  "Indicator for fill mode."
  (when (and (boundp 'auto-fill-function) (symbol-value 'auto-fill-function))
    (propertize (ck/ml-icon "material" "line_style"
			    :fallback "ⓕ"
			    :v-adjust -0.1
			    :height 0.8)
		'help-echo (format "Auto-Fill: %d" fill-column)
		'mouse-face 'ck-modeline-highlight)))

(defun ck/mm-flyspell-mode ()
  "Indicator for fill mode."
  (when (and (boundp 'flyspell-mode) (symbol-value 'flyspell-mode))
    (propertize (ck/ml-icon "material" "playlist_add_check"
			    :fallback "ⓢ"
			    :v-adjust -0.1
			    :height 0.8)
		'help-echo (format "Spell checker active")
		'mouse-face 'ck-modeline-highlight)))

(def-ml-segment! minor-modes (_)
  "Return the well-defined list of minor mode indicators (honor ACTIVE)."
  (let ((modes (list (ck/mm-fill-mode)
		     (ck/mm-flyspell-mode))))
    (mapconcat #'identity (seq-filter 'identity modes) " ")))

(defsubst ck/ml-find-bookmark (filename bm-list)
  "Find the bookmark with FILENAME in BM-LIST."
  (let ((needle-name (file-name-nondirectory filename)))
    (seq-find
     (lambda (x) (and (string= (car x) needle-name) ; names match
		 (string= (expand-file-name filename)
			  (expand-file-name (cdr (assq 'filename x))))))
     bm-list)))

(def-ml-segment! ibookmark (_)
  "File indicator if the current file is bookmarked."
  (when (and (buffer-file-name) (boundp 'bookmark-alist))
    (let* ((bmark (ck/ml-find-bookmark (buffer-file-name) bookmark-alist)))
      (if bmark
	  (propertize (ck/ml-icon "material" "bookmark"
				  :fallback "ⓑ"
				  :v-adjust -0.1
				  :height 0.8)
		      'help-echo (format "File is bookmarked")
		      'mouse-face 'ck-modeline-highlight)))))

;; flycheck
(defun ck/ml-flycheck-face (state warnings errors)
  "Face for flycheck based on STATE, WARNINGS and ERRORS."
  (pcase state
    ('finished (cond ((> errors 0) 'ck-modeline-urgent)
		     ((> warnings 0) 'ck-modeline-warning)
		     ('t 'ck-modeline-info)))
    ('no-checker 'ck-modeline-warning)
    ('errored 'ck-modeline-urgent)
    (_ 'ck-modeline-dimmed)))

(defvar ck--ml-flycheck-issues nil)
(defun ck/ml-flycheck-mk-text (state issues)
  "STATE with ISSUES."
  (pcase state
    ('finished (setq ck--ml-flycheck-issues (and (> issues 0) (number-to-string issues))))
    ('running ck--ml-flycheck-issues)
    ('no-checker "-")
    ('errored "Error")
    ('interrupted "Interrupted")
    ('not-checked nil)
    (_ "?")))

(def-ml-segment! flycheck (active)
  "Displays flycheck status if ACTIVE."
  (when (boundp 'flycheck-last-status-change)
    (let* ((state flycheck-last-status-change)
	   (total-errors (flycheck-count-errors flycheck-current-errors))
	   (errors (or (alist-get 'error total-errors) 0))
	   (warnings (or (alist-get 'warning total-errors) 0))
	   (issues (+ errors warnings))
	   (saved (not (buffer-modified-p)))
	   (face (if (and active saved) (ck/ml-flycheck-face state warnings errors) 'ck-modeline-dimmed))
	   (text (ck/ml-flycheck-mk-text state issues))
	   (icon (pcase state
		   ('finished (if (> issues 0)
				  (list "material" "warning" :fallback "⚠")
				(list "material" "check" :fallback "✓")))
		   ('running     (list "material" "access_time" :fallback "…"))
		   ('no-checker  (list "material" "error" :fallback "!"))
		   ('errored     (list "material" "error" :fallback "!"))
		   ('interrupted (list "material" "pause"  :fallback "⏸"))
		   ('not-checked (list "material" "do_not_disturb_alt" :fallback "□"))
		   (_            (list "material" "do_not_disturb_alt" :fallback "?")))))
      (ck/ml-make-minor-mode 'flycheck-mode
       (concat (apply 'ck/ml-icon (append icon (list :face face :v-adjust -0.1 :height 0.8)))
	       (if text " ")
	       (if text (propertize text 'face face)))))))

(def-ml-segment! buffer-id (active)
  "The identification of the buffer (honoring ACTIVE)."
  (let* ((project-root (*project-root-safe))
	 (project-name (and project-root (ck/ml-project-name active)))
	 (filename buffer-file-name))
    (cond (project-name (ck/ml-project-id project-root project-name filename active))
	  (filename      filename)
	  (t            (concat (*buffer-name) " " (ck/ml-buffer-cwd active))))))

(def-ml-segment! cursor-position (active)
  "The current cursor position, honoring ACTIVE."
  (propertize "(%l,%c)" 'face (if active 'ck-modeline-dimmed)))

;; the mode-lines

(def-modeline! default
  (bar indicator-for-major-mode buffer-id minor-modes ibookmark process anzu num-cursors)
  (flycheck buffer-encoding-abbrev cursor-position buffer-position))


(provide 'ck-modeline)
;;; ck-modeline.el ends here

