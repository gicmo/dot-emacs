;; .emacs by Christian Kellner <ck@xatom.net>

; === pallet package manager
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

; === set the path from shell on osx ===
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

(global-linum-mode t)
(setq linum-format "%d ")

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

; === tabbar ===
(require 'tabbar)
(tabbar-mode)

; === ido ====
(ido-mode)

; === minimap ==
(require 'minimap)
(setq minimap-window-location (quote right))

; === git gutter ===
(require 'git-gutter-fringe)
(setq git-gutter-fr:side 'right-fringe)
;(setq-default right-fringe-width 22)

; === yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; === autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)

; == autocomplete-clang
(require 'auto-complete-clang)
(global-set-key (kbd "C-`") 'ac-complete-clang)

; == autocomplete config
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(my-ac-config)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; == backup ==
(defun make-backup-file-name (filename)
  (defvar backups-dir "~/.backups/")
  (make-directory backups-dir t)
  (expand-file-name
   (concat backups-dir "."  (file-name-nondirectory filename) "~")
   (file-name-directory filename)))

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

(recentf-mode 1)
(global-set-key (kbd "C-c r") 'recentf-open-files)

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
(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq objc-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))

; Tex Mode
(setq tex-command "xelatex")

; color theme

;(if window-system
;    (load-theme 'solarized-light t)
;  (load-theme 'solarized-dark t))

;(load-theme 'solarized-dark t)
(color-theme-molokai)

;; resize the initial emacs window
(setq initial-frame-alist '((width . 160)
                            (height . 47)))
(setq default-frame-alist '((width . 160)
                            (height . 47)))

(require 'powerline)
(require 'cl)

;; Python
;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(require 'pymacs)
 (pymacs-load "ropemacs" "rope-")


;; multiple cursors
(require 'multiple-cursors)


(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; OS X stuff
(defun set-osx-stuff()
    (setq x-select-enable-clipboard t)
    (global-set-key (kbd "C-c C-d") 'dash-at-point))

(if (eq system-type 'darwin) (set-osx-stuff))


(load "time" t t)
(display-time)
;;(put 'downcase-region 'disabled nil)
;;(put 'upcase-region 'disabled nil)

