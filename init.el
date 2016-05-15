;;; init.el --- main Emacs initialization
;;; Commentary:
;;; Setup Emacs, make it homely and cosy
;; Author: Christian Kellner <christian@kellner.me>

;;; Code:

(add-to-list 'load-path "~/.emacs.d/init.d/")

;; === package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; no auto package loading,
;; loading is handled via use-package
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

;; -=[ OSX
(when (eq system-type 'darwin)
  (require 'init-osx))

;; -=[ custom - write custom's settings to separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode (quote right))
(tooltip-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode t)
(global-auto-revert-mode t)
(global-linum-mode t)
(setq linum-format "%4d ")

(setq use-dialog-box nil)
(prefer-coding-system 'utf-8)

;; visual bell causes rendering errors
;; use custom function from 'stack-exchange'

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit
				      exit-minibuffer keyboard-quit))
	  (invert-face 'mode-line)
	  (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; == elisp path ==
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "opt/")))

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

; -=[ EditorConfig

(use-package editorconfig
    :ensure t
    :init
    (editorconfig-mode 1))

; === ido ====
(ido-mode)
;; prevent ido to globally search for files automatically
(setq ido-auto-merge-work-directories-length -1)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

; -=[ flycheck
(use-package flycheck
  :diminish " ⓕ"
  :ensure t
  :init (global-flycheck-mode))

;; -=[ git
(use-package git-gutter-fringe
  :diminish (git-gutter-mode . "")
  :config
  (setq git-gutter-fr:side 'right-fringe)
  ;;(setq-default right-fringe-width 22)
  (add-hook 'prog-mode-hook #'git-gutter-mode))

(use-package gitconfig-mode
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
	 ("\\.git/config\\'" . gitconfig-mode)
	 ("\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(use-package git-timemachine
  :commands git-timemachine
  :config
  (setq git-timemachine-abbreviation-length 6))

;; === cmake mode
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

;; -=[ docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :defer t)

;; -=[ yasnippet
(use-package yasnippet
  :diminish (yas-minor-mode . " ⓨ")
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

; === autocompletion
(use-package company
  :diminish " ⓒ"
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; == backup ==
(defun make-backup-file-name (filename)
  (defvar backups-dir "~/.backups/")
  (make-directory backups-dir t)
  (expand-file-name
   (concat backups-dir "."  (file-name-nondirectory filename) "~")
   (file-name-directory filename)))

; == recent files ==
(recentf-mode 1)
(global-set-key (kbd "C-c r") 'recentf-open-files)

; == uniquify ==
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; -=[ text formats
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming mode customizations
(setq-default show-trailing-whitespace t)

; -=[ common packages
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package paredit
  :commands enable-paredit-mode)


; -=[ C/C++/ObjC and friends
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
		  (and (string= (file-name-extension (or buffer-file-name "DEF-NAME")) "h")
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

(use-package irony
  :commands irony-mode
  :diminish " ⓘ"
  :init
  (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony
    :config
    (company-irony-setup-begin-commands))
  (use-package flycheck-irony
    :after flycheck
    :config
    (flycheck-irony-setup))
  (use-package irony-eldoc
     :init
     (add-hook 'irony-mode-hook 'irony-eldoc)
    )
  )

(use-package cuda-mode
  :mode "\\.cu\\'")

; -=[ clojure
(use-package clojure-mode
  :mode "\\.clj"
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (use-package clojure-mode-extra-font-locking))

; -=[ Fortran
(use-package f90
  :mode ("\\.[fF]\\(03\\|08\\)\\'" . f90-mode))

; -=[ Go
(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
	      ("M-." . godef-jump)
	      ("C-c C-r" . go-rename))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goreturns")
  (use-package go-guru)
  (use-package go-stacktracer)
  (use-package go-playground)
  (use-package go-dlv)
  (use-package company-go
    :config
    (add-to-list 'company-backends 'company-go))
  )

(use-package go-rename
  :commands (go-rename))

(use-package go-eldoc
  :commands (go-eldoc-setup)
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; -=[ Python
(use-package elpy
  :commands elpy-enable
  :init
  (with-eval-after-load 'python
    (elpy-enable))
  :config
  (setq-default flycheck-flake8-maximum-line-length 100)
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; === I will never learn how to spell ===

(use-package langtool
  :bind (("C-x c w" . langtool-check)
         ("C-x c W" . langtool-check-done)
         ("C-x c l" . langtool-switch-default-language)
         ("C-x c 4" . langtool-show-message-at-point)
         ("C-x c c" . langtool-correct-buffer))
  :init
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.0/libexec/languagetool-commandline.jar"
	langtool-default-language "en-US"
	langtool-disabled-rules '("WHITESPACE_RULE"
				  "EN_UNPAIRED_BRACKETS"
				  "COMMA_PARENTHESIS_WHITESPACE"
				  "EN_QUOTES")))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(use-package synosaurus
  :bind ("C-c s l" . synosaurus-lookup)
  :config (setq synosaurus-backend 'synosaurus-backend-wordnet))

;; -=[ UI

;; -=[ fonts
(setq ck-fonts
      '(("Hasklig" 12)
	("Source Code Pro" 12)
	("Inconsolata" 12)
	("Menlo" 12)))

(defun font-existsp (name size)
  (cond ((find-font (font-spec :name (concat name " for Powerline")))
	 (format "%s for Powerline-%d" name size))
	((find-font (font-spec :name name))
	 (format "%s-%d" name size))))

(defun ck-first-font (lst)
  (or (apply 'font-existsp (car lst))
      (ck-first-font (cdr lst))))

(when (display-graphic-p)
 (let ((foo-font (ck-first-font ck-fonts)))
  (set-face-attribute 'default nil :font foo-font)
  (message (concat "Setting font to: " foo-font))
  ))

;; -=[ color themes

(use-package flatland-theme
  :defer t
  :config
  (load-theme 'flatland 1))

(use-package leuven-theme)

;; resize the initial emacs window
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 150))

(use-package powerline
  :config
  (powerline-default-theme))
(use-package cl)

;; multiple cursors

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package dash-at-point
  :bind (("C-c d" . dash-at-point)))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

;; all done, pheww
;;; init.el ends here
