;;; init.el --- main Emacs initialization
;;; Commentary:
;;; Setup Emacs, make it homely and cosy
;; Author: Christian Kellner <christian@kellner.me>

;;; Code:

; -=[ sane defaults
(setq gc-cons-threshold (* 128 1024 1024))
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
(dolist (m '(tooltip-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp m) (funcall m -1)))
(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq use-dialog-box nil)
(prefer-coding-system 'utf-8)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)
(put 'scroll-left 'disabled nil)
(fset 'display-startup-echo-area-message #'ignore)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; visual bell causes rendering errors
;; use custom function from 'stack-exchange'
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit
				      exit-minibuffer keyboard-quit))
	  (invert-face 'mode-line)
	  (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

;; -=[ custom - write custom's settings to separate file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/elisp/")

;; -=[ Dashboard
(eval-when-compile
  (progn
    (require 'dashboard)
    (dashboard-show)))


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
  (require 'use-package)
  (use-package cl))

(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

;; -=[ OSX
(when (eq system-type 'darwin)
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (setq mac-option-modifier 'meta)
  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))
  (setq-default locate-command "mdfind")
  (when (display-graphic-p)
    (setq-default mac-emulate-three-button-mouse t)
    (global-set-key (kbd "M-`") 'other-frame))
  )

;; pick up the correct path from a login shell
(use-package exec-path-from-shell
  :init
  (customize-set-variable 'exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))


(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil
	global-dired-hide-details-mode -1)
  :config
  (diredp-toggle-find-file-reuse-dir 1)
  (define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file)
  (define-key dired-mode-map (kbd "C-<up>")
    (lambda ()
      (interactive) (find-alternate-file ".."))))

;; -=[ Editing

;; multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

; -=[ EditorConfig

(use-package editorconfig
  :diminish ""
  :init
  (editorconfig-mode 1))

; -=[ interactively do things

(use-package ido
  :init
  (ido-mode t)
  :config
  ;; prevent ido to globally search for files automatically
  (setq ido-auto-merge-work-directories-length -1)
  (define-key ido-file-dir-completion-map (kbd "C-c C-s")
    (lambda()
      (interactive)
      (ido-initiate-auto-merge (current-buffer))))
  (use-package flx-ido
    :init
    (flx-ido-mode t)))

(use-package anzu
  :diminish ""
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init
  (global-anzu-mode 1)
  :config
  (setq anzu-cons-mode-line-p nil))

(use-package ag
  :commands (ag ag-project))

(use-package nlinum
  :commands nlinum-mode
  :init (add-hook 'prog-mode-hook 'nlinum-mode)
  :config
  (setq nlinum-format "%4d "))

(use-package hl-line
  :commands hl-line-mode)

;; -=[ spell checking, because I will never learn how to spell
(use-package flyspell
  :commands (flyspell-prog-mode flyspell-mode flyspell-buffer)
  :diminish (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-issue-message-flag nil
	flyspell-issue-welcome-flag nil)
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)
    (setq ispell-extra-args '("-d en_US")))))

; -=[ Org
(use-package org
  :commands org-mode
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-window-setup 'current-window)
  (use-package org-journal
    :init
    (setq org-journal-dir "~/Documents/Notes/journal"
	  org-journal-file-format "%Y-%B.org"))
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook
	      (lambda ()
		(org-bullets-mode t)))))


; -=[ Projects via projectile

(defun ck/projectile-commander-setup ()
  "Setup projectile commander shortcuts."
  (require 'projectile)
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired)))

(use-package projectile
  :diminish ""
  :config
  (projectile-global-mode t)
  (setq projectile-switch-project-action 'projectile-dired)
  (ck/projectile-commander-setup))

; -=[ flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :diminish " ⓕ"
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip))

;; -=[ git
(use-package git-gutter-fringe
  :if window-system
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

(use-package git-commit
  :commands global-git-commit-mode
  :init
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package magit
  :bind (("C-x g" . magit-status))
  :init
  (add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
  (add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)
  :config
  (setq magit-diff-refine-hunk t))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

;; -=[ yasnippet
(use-package yasnippet
  :commands yas-minor-mode
  :diminish (yas-minor-mode . " ⓨ")
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

; === autocompletion
(use-package company
  :diminish " ⓒ"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t))

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
(use-package recentf
  :init
  (setq recentf-exclude '("/\\.git/.*\\'"
                          "/elpa/.*\\'"
                          "/cache/.*\\'"
                          ".*\\.gz\\'")
        recentf-max-saved-items 50
        recentf-max-menu-items 35)
  (recentf-mode 1))

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

;; -=[ pdf viewing
(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t))

;; -=[ goto-address, makes links click-able

(use-package goto-addr
  :commands (goto-address-mode goto-address-prog-mode)
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming mode customizations
(setq-default show-trailing-whitespace t)

; -=[ common packages
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :commands enable-paredit-mode)

; -=[ Assembler modes

(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$")

; -=[ C/C++/ObjC and friends
(setq c-hungry-delete-key t)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq c-indent-level 4)
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
	    (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
	    ))

; objc mode for header files
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension (or buffer-file-name "DEF-NAME")) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

(defconst cc-style-nix
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(add-hook 'cc-mode-hook
	  (lambda ()
	    (c-add-style "cc-style-nix" cc-style-nix)))

(use-package irony
  :commands irony-mode
  :diminish " ⓘ"
  :init
  (add-hook 'c-mode-common-hook 'irony-mode)
  :config
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
     (add-hook 'irony-mode-hook 'irony-eldoc)))

(use-package rtags
  :bind (:map c-mode-base-map
	      ("M-." . rtags-find-symbol-at-point))
  :config
  (rtags-enable-standard-keybindings))

(use-package cmake-ide
  :commands cmake-ide-setup
  :init
  (add-hook 'c-mode-common-hook 'cmake-ide-setup)
  :config
  (message "cmake ide starting")
  (require 'rtags))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package cuda-mode
  :mode "\\.cu\\'")

;; -=[ docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :defer t)

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
	      ("M-," . godef-jump-back)
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
  (use-package go-projectile)
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

(use-package ein
  :defer t)

;; -=[ Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hock 'rustfmt-enable-on-save)
  (use-package flycheck-rust
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :commands racer-mode
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  :bind (:map rust-mode-map
	 ("M-." . racer-find-definition))
  :config
  (racer-turn-on-eldoc)
  (use-package company-racer
    :config
    (add-to-list 'company-backends 'company-racer)
    (setq company-tooltip-align-annotations t)
    :bind (:map rust-mode-map
		("M-." . racer-find-definition)))
  )

(use-package cargo
  :commands cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

;; -=[ packaging
(use-package rpm-spec-mode
  :mode "\\.spec\\'")

;; -=[ web stuff
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.xhtml$"   . web-mode)
	 ("\\.vue\\'"   . web-mode))
  :config
  (setq web-mode-enable-engine-detection t))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

;; -=[ documentation
(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind (("C-c d" . dash-at-point)))

(use-package devhelp
  :ensure f
  :if (eq system-type 'gnu/linux)
  :bind (("C-c d" . devhelp-word-at-point)))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

;; -=[ config files

(use-package apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
	 ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
	 ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(use-package ini-mode
  :ensure f ; locally installed in elisp
  :mode (("\\.ini\\'" . ini-mode)
	 ("\\.desktop\\'" . ini-mode)))

(use-package nginx-mode
  :mode ("nginx.conf$" "/etc/nginx/.*"))

;; -=[ better writing
(defun ck-find-langtool ()
  "Find the locations of all available langtool jar (sorted) or nil."
  (let ((basedir '"/usr/local/Cellar/languagetool")
	(suffix '"/libexec/languagetool-commandline.jar"))
    (if (file-exists-p basedir)
	(mapcar (lambda (d) (concat d suffix))
		(reverse (sort
			  (directory-files basedir t "[0-9].*" t)
			  'string<))))))

(use-package langtool
  :bind (("C-x c w" . langtool-check)
         ("C-x c W" . langtool-check-done)
         ("C-x c l" . langtool-switch-default-language)
         ("C-x c 4" . langtool-show-message-at-point)
         ("C-x c c" . langtool-correct-buffer))
  :config
  (setq langtool-language-tool-jar (car (ck-find-langtool))
	langtool-default-language "en-US"
	langtool-disabled-rules '("WHITESPACE_RULE"
				  "EN_UNPAIRED_BRACKETS"
				  "COMMA_PARENTHESIS_WHITESPACE"
				  "EN_QUOTES")))
(use-package synosaurus
  :bind ("C-c s l" . synosaurus-lookup)
  :config (setq synosaurus-backend 'synosaurus-backend-wordnet))


;; -=[ UI
;; resize the initial emacs window
;;(add-to-list 'default-frame-alist '(height . 40))
;;(add-to-list 'default-frame-alist '(width . 150))

(use-package neotree
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq neo-vc-integration nil
	neo-banner-message nil
	neo-show-updir-line nil
	projectile-switch-project-action 'neotree-projectile-action))

;; -=[ fonts
(defconst ck-fonts
      '(("Hasklig")
	("Source Code Pro")
	("Inconsolata")
	("Menlo")
	("Monospace")))

(defun ck/system-font ()
  "Return the system's monospace font as a list with name and size."
  (cond ((eq system-type 'gnu/linux)
	 (let ((input (shell-command-to-string
		       (mapconcat 'identity
				  '("gsettings get org.gnome.desktop.interface"
				    "monospace-font-name")
				  " "))))
	   (when (string-match "[ \t\"]*'\\(.*\\) \\([0-9]+\\)'[ \n\"]*$" input)
	     (list (match-string 1 input)
		   (string-to-number (match-string 2 input))))))))

(defun ck/font-size-for-system ()
  "Determine the optimal font size to use."
  (cond ((eq system-type 'gnu/linux)
	 (let ((sys-font (ck/system-font)))
	   (if sys-font
	       (nth 1 sys-font)
	     11
	     )))
	  ((eq system-type 'darwin)
	   12)
	  (t 12)))

(defvar ck/default-font-size
  (ck/font-size-for-system)
  "The default font size to use.")

(defun font-existsp (name)
  "Check if a font with a given NAME (or its Powerline version)."
  (cond ((find-font (font-spec :name (concat name " for Powerline")))
	 (format "%s for Powerline-%d" name ck/default-font-size))
	((find-font (font-spec :name name))
	 (format "%s-%d" name ck/default-font-size))))

(defun ck-first-font (lst)
  "Return the first valid font from LST."
  (or (apply 'font-existsp (car lst))
      (ck-first-font (cdr lst))))

(defun ck-set-font ()
  "Set the default font from the ck-fonts list."
  (set-face-attribute 'default nil :font (ck-first-font ck-fonts)))

(defun new-frames-setup (frame)
  "Called for each new FRAME in daemon mode."
  (let ((have-gui (memq (framep frame) '(x w32 ns mac)))
	(is-mac (eq system-type 'darwin)))
    (set-frame-parameter frame 'menu-bar-lines (if (and have-gui is-mac) 1 0))
    (when have-gui
      (with-selected-frame frame
	(ck-set-font))
      )))


(add-hook 'after-make-frame-functions 'new-frames-setup)
(unless (daemonp)
  (new-frames-setup (selected-frame)))

;; -=[ mode-line
(use-package all-the-icons)
(eval-when-compile
  (progn
    (require 'ck-modeline)
    (setq-default mode-line-format (ck/mode-line))))

;; -=[ color themes

;(use-package leuven-theme)

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)
  (require 'doom-neotree)
;;  (require 'doom-nlinum)
  :config
  (diminish 'doom-buffer-mode)
  )

;; all done, pheww
;;; init.el ends here
