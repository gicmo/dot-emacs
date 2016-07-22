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

(defun *shorten-directory (dir max-length)
  "Show directory name of `DIR', reduced to `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun *buffer-path ()
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (projectile-project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-buffer-path))))

(defun *buffer-state ()
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
   (concat "[" (*shorten-directory default-directory 20) "]")
   'face 'mode-line-2))

(defun *major-mode ()
  "The major mode, including process, environment."
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defun *minor-modes ()
  "The minor modes."
  (format-mode-line minor-mode-alist)
  )

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

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

(defun ck/mode-line ()
  "Our custom mode line."
  '(:eval
    (let* ((active (powerline-selected-window-active))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
		      (if (eq nil (buffer-file-name)) (*buffer-cwd))
		      "  "
		      (powerline-major-mode)
		      " "
		      (powerline-process)
		      " "
		      (powerline-minor-modes)
		      "  "
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
