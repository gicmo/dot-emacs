;; .emacs by Christian Kellner <christian@kellner.me>

; === osx: load path for homebrew  ===
(when (memq window-system '(mac ns))
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

; === casl+pallet package manager
(require 'cask)
(cask-initialize)
(require 'pallet)

; === set the path from shell on osx ===
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(set-fringe-style -1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode (quote right))
(tooltip-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell t)
(delete-selection-mode t)

(global-linum-mode t)
(setq linum-format "%d ")

(setq use-dialog-box nil)

;; == elisp path ==
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "opt/")))

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

; === tabbar ===
(require 'tabbar)
(tabbar-mode)

; === ido ====
(ido-mode)

; prevent ido to globally search for files automatically
(setq ido-auto-merge-work-directories-length -1)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
      (lambda()
        (interactive)
        (ido-initiate-auto-merge (current-buffer))))

; === minimap ==
(require 'minimap)
(setq minimap-window-location (quote right))

; === git gutter ===
(require 'git-gutter-fringe)
(setq git-gutter-fr:side 'right-fringe)
;(setq-default right-fringe-width 22)

; === cmake mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

; === yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; === autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

; == autocomplete config
; (setq ac-auto-start nil)
(setq ac-quick-help-delay 0.3)

;; == backup ==
(defun make-backup-file-name (filename)
  (defvar backups-dir "~/.backups/")
  (make-directory backups-dir t)
  (expand-file-name
   (concat backups-dir "."  (file-name-nondirectory filename) "~")
   (file-name-directory filename)))


;; == group tabs ==
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t

     "User Buffer"
     )
    )))

; == recent files ==
(recentf-mode 1)
(global-set-key (kbd "C-c r") 'recentf-open-files)

; == uniquify ==
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming mode customizations
(setq-default show-trailing-whitespace t)

; Fortran
(add-to-list 'auto-mode-alist
             '("\\.[fF]\\(03\\|08\\)\\'" . f90-mode))

; C
(setq c-hungry-delete-key t)

(add-hook 'c-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))))

; spaces, not tabs
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq c-indent-level 4)))

(add-hook 'objc-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq c-indent-level 4)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq c-indent-level 4)))

; objc mode for header files
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

; switch between header and implementation
(add-hook 'c-mode-common-hook
         (lambda ()
           (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)))

(defconst cc-style-nix
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "cc-style-nix" cc-style-nix)
(setq safe-local-variable-values
    '((c-set-style . "cc-style-nix")
     (c-offsets-alist
      (quote innamespace)
      [0])))

;; == CUDA mode ==
(require 'cuda-mode)

; Tex Mode
(setq tex-command "xelatex")

; color theme

;(if window-system
;    (load-theme 'solarized-light t)
;  (load-theme 'solarized-dark t))

;(load-theme 'solarized-dark t)
;(color-theme-molokai)
(load-theme 'flatland 1)

;; resize the initial emacs window
(setq initial-frame-alist '((width . 160)
                            (height . 47)))
(setq default-frame-alist '((width . 160)
                            (height . 47)))

(require 'powerline)
(require 'cl)

(powerline-default-theme)

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


;; multiple cursors
(require 'multiple-cursors)


(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; OS X stuff
(defun set-osx-stuff()
    (setq x-select-enable-clipboard t)
    (global-set-key (kbd "C-c d") 'dash-at-point))

(if (eq system-type 'darwin) (set-osx-stuff))


;; (require 'load-dir)
(setq load-dirs "~/.emacs.d/load.d")
(load-dirs)

;; all done, pheww

