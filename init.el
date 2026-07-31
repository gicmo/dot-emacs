;;; init.el --- main Emacs initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Setup Emacs, make it homely and cosy
;; Author: Christian Kellner <christian@kellner.me>

;;; Code:

; -=[ sane defaults
(blink-cursor-mode 0)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq use-short-answers t)
(tooltip-mode -1)
(column-number-mode 1)
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq use-dialog-box nil)
(prefer-coding-system 'utf-8)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)
(put 'scroll-left 'disabled nil)
(fset 'display-startup-echo-area-message #'ignore)
(setq mode-line-default-help-echo nil)

(setq split-width-threshold 160
      split-height-threshold nil)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

;; -=[ initialize the core
(require 'ck-core (concat user-emacs-directory "elisp/ck-core"))

;; === package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; package-enable-at-startup is set in early-init.el
(package-initialize)

(require 'use-package)
(require 'bind-key)

(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;; -=[ Dashboard
(use-package ck-dashboard
  :load-path "elisp"
  :ensure nil
  :commands dashboard-show
  :init
  (dashboard-show))

;; pick up the correct path from a login shell
(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :custom
  (exec-path-from-shell-variables '("EMAIL"
				    "GOPATH"
				    "RUST_SRC_PATH"
				    "WORKON_HOME"
				    "MANPATH"
				    "PATH"))
  :init
  (customize-set-variable 'exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))

(use-package which-key
  :ensure nil
  :defer t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0)
  (dolist (pair '(("SPC" . "␣")
		  ("TAB" . "↹")
		  ("RET" . "⏎")
		  ("DEL" . "⌫")
		  ("ESC" . "⎋")
		  ("deletechar" . "⌦")))
    (let ((for-char  (car pair))
	  (symbol (cdr pair)))
      (push `((,for-char . nil) . (,symbol . nil))
	    which-key-replacement-alist))))

;; -=[ evil mode

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package ck-evil
  :ensure nil
  :load-path "elisp"
  :after (evil consult consult-projectile consult-lsp projectile
	       lsp-mode lsp-ui flycheck diff-hl magit expand-region))

(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
	      ([mouse-2] . dired-mouse-find-file)
	      ("C-<up>" . dired-up-directory)))

;; -=[ Editing

;; Insert matching brackets.
(electric-pair-mode t)

;; multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package iedit
  :bind ("C-;" . iedit-mode))

; -=[ EditorConfig

(use-package editorconfig
  :init
  (editorconfig-mode 1))


; -=[ vertico completion

(use-package vertico
  :custom
  (vertico-cycle t)
  :hook (after-init . vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :hook (after-init . marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package consult
  :bind (("C-x C-f" . find-file)
	 ("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ("M-s r" . consult-ripgrep)
	 ("M-s g" . consult-grep)
	 ("M-s l" . consult-line)))

(use-package consult-projectile
  :after (consult projectile)
  :bind ("C-c p f" . consult-projectile))

(use-package embark
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package ck-quick-open
  :ensure nil
  :load-path "elisp"
  :commands ck/quick-open
  :bind ("M-O" . ck/quick-open))

(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)))
  :config
  (vertico-posframe-mode 1))

; -=[ navigation and searching

(use-package anzu
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

(use-package fzf
  :commands fzf)

(use-package back-button
  :commands (back-button-mode)
  :defer 2
  :init
  (setq back-button-show-toolbar-buttons nil)
  :config
  (back-button-mode 1))

;; -=[ spell checking, because I will never learn how to spell
(defvar ck/have-spell-checker
  (and (or (executable-find "aspell") (executable-find "hunspell")) t)
  "Non-nil if a spell checker backend is installed.")

(defun ck/flyspell-maybe ()
  "Enable `flyspell-mode' when a spell checker is available."
  (when ck/have-spell-checker (flyspell-mode 1)))

(defun ck/flyspell-prog-maybe ()
  "Enable `flyspell-prog-mode' when a spell checker is available."
  (when ck/have-spell-checker (flyspell-prog-mode 1)))

(use-package flyspell
  :ensure nil
  :if ck/have-spell-checker
  :commands (flyspell-prog-mode flyspell-mode flyspell-buffer)
  :hook ((prog-mode . flyspell-prog-mode)
	 (nxml-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :config
  (setq flyspell-issue-message-flag nil
	flyspell-issue-welcome-flag nil)
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-really-hunspell t
	  ispell-extra-args '("-i" "utf-8")
	  ispell-local-dictionary-alist
	  '(("deutsch"
	     "[A-Za-zöäüß]" "[^A-Za-zöäüß]" "[']" nil
	     ("-d" "de_DE")
	     nil utf-8)
	    ("english"
	     "[A-Za-z]" "[^A-Za-z]" "[']" nil
	     ("-d" "en_US")
	     nil utf-8))
	  ispell-dictionary "english"))))

;; -=[ Org
(use-package org
  :commands org-mode
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-directory "~/Documents/Notes/"
	org-agenda-files '("~/Documents/Notes/")
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-window-setup 'current-window
	org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell  . t))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :after org
  :custom
  (org-journal-dir "~/Documents/Notes/journal")
  (org-journal-file-format "%G-w%V.org")
  (org-journal-file-type 'weekly))

; -=[ Projects via projectile

(use-package projectile
  :defer 1
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired))

; -=[ flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 256 384 448 480 496 480 448 384 256 0 0 0 0 0]
    ))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 7
	flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

;; -=[ git
(use-package diff-hl
  :if window-system
  :hook ((prog-mode . diff-hl-mode)
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'right)
  :config
  (define-fringe-bitmap 'ck/diff-hl-bar [224] nil nil '(center repeated))
  (setq diff-hl-fringe-bmp-function (lambda (_type _pos) 'ck/diff-hl-bar))
  (diff-hl-flydiff-mode 1))

(use-package git-modes
  :defer t)

(use-package git-timemachine
  :commands git-timemachine
  :config
  (setq git-timemachine-abbreviation-length 6))

(use-package git-commit
  :ensure nil ; ships with magit
  :commands global-git-commit-mode
  :init
  (setq git-commit-summary-max-length 50)
  (when ck/have-spell-checker
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))
  (add-hook 'git-commit-setup-hook 'ck/show-trailing-ws)
  (add-hook 'git-commit-setup-hook
	    (lambda () (setq-local fill-column 72))))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t))

(use-package forge
  :after magit
  :config
  (dolist (url '("gitlab.freedesktop.org"))
    (add-to-list 'forge-alist (list url (concat url "/api/v4") url forge-gitlab-repository))))

;; -=[ yasnippet
(use-package yasnippet
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

; === autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t))

(use-package company-box
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

; == recent files ==
(use-package recentf
  :defer 1
  :init
  (setq recentf-exclude '("/\\.git/.*\\'"
                          "/elpa/.*\\'"
                          "/cache/.*\\'"
                          ".*\\.gz\\'")
        recentf-max-saved-items 50
        recentf-max-menu-items 35
	recentf-auto-cleanup 'never)
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
  :hook ((markdown-mode . ck/flyspell-maybe)
	 (markdown-mode . ck/show-trailing-ws)))

;; -=[ pdf viewing
(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t))

;; -=[ goto-address, makes links click-able

(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
	 (prog-mode . goto-address-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming mode customizations

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . ck/show-trailing-ws))

; -=[ common packages
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

(use-package paredit
  :commands enable-paredit-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (dolist (dir '("\\.vscode$" "[/\\\\]\\.cache[/\\\\]clangd\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir))
  (setq lsp-headerline-arrow
	(if ck/use-icon-font
	    (nerd-icons-mdicon "nf-md-chevron_right"
			       :face 'lsp-headerline-breadcrumb-separator-face)
	  (propertize "›" 'face 'lsp-headerline-breadcrumb-separator-face)))
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  (require 'lsp-ui-flycheck)
  (setq lsp-ui-sideline-show-hover nil)
  :bind (:map lsp-ui-mode-map
	      ("C-c r ." . lsp-ui-peek-find-definitions)
	      ("C-c r ?" . lsp-ui-peek-find-references)
	      ("C-c r d" . lsp-ui-peek-find-definitions)
	      ("C-c r r" . lsp-ui-peek-find-references)
	      ("C-c r i" . lsp-ui-imenu)
	      ("C-c r F" . lsp-ui-sideline-apply-code-actions)
	      ("C-c r R" . lsp-rename)))

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

; -=[ Assembler modes

(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$")

(use-package bpftrace-mode
  :mode "\\.bt$")

; -=[ C/C++/ObjC and friends
(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . lsp-deferred)
  :bind (:map c-mode-base-map
	      ("C-c o" . ff-find-other-file)
	      ("C-c r h" . lsp-treemacs-type-hierarchy)
	      ("C-c r H" . lsp-treemacs-call-hierarchy)
	      ("C-c r L" . lsp-lens-mode)
	      ("C-c r m" . lsp-treemacs-symbols))
  :config
  (setq c-hungry-delete-key t
	indent-tabs-mode nil
	gdb-many-windows t
	gdb-show-main t))

(use-package lsp-treemacs
  :commands (lsp-treemacs-call-hierarchy
	     lsp-treemacs-type-hierarchy
	     lsp-treemacs-symbols)
  :config
  (treemacs-resize-icons 16))

(use-package dtrt-indent
  :hook (c-mode-common . dtrt-indent-mode))

; detect major mode (objc, c++-mode) for header
(use-package dummy-h-mode
  :load-path "ewiki"
  :ensure nil
  :mode "\\.h$")

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package cuda-mode
  :mode "\\.cu\\'")

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.vert\\'" . glsl-mode)
	 ("\\.geom\\'" . glsl-mode)
	 ("\\.frag\\'" . glsl-mode)))

(use-package cocci-mode
  :load-path "ewiki"
  :ensure nil
  :mode "\\.cocci$")

(use-package meson-mode
  :mode "\\meson\\.build\\'")

(use-package vala-mode
  :mode (("\\.vala\\'" . vala-mode)
         ("\\.vapi\\'" . vala-mode))
  :hook ((vala-mode . (lambda () (lsp))))
  :config
  (run-hooks 'prog-mode-hook)
  (dolist (suffix '("\\.vala\\'" "\\.vapi\\'"))
    (add-to-list 'file-coding-system-alist (cons suffix 'utf-8)))
  (require 'lsp-mode)
  (add-to-list 'lsp-language-id-configuration '(vala-mode . "vala"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "vala-language-server")
    :major-modes '(vala-mode)
    :server-id 'vala)))

(use-package vala-snippets
  :after vala-mode)

;; -=[ docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :defer t)

; -=[ jvm: java, clojure, scala
(use-package clojure-mode
  :mode "\\.clj"
  :hook ((clojure-mode . enable-paredit-mode)
	 (clojure-mode . subword-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package lsp-java
  :after lsp
  :hook ((java-mode . (lambda () (require 'lsp-java) (lsp)))))

; -=[ Fortran
(use-package f90
  :mode ("\\.[fF]\\(03\\|08\\)\\'" . f90-mode))

; -=[ Go
(defun ck/go-mode-setup ()
  "Organize imports and format on save, in this buffer only."
  (add-hook 'before-save-hook #'lsp-organize-imports nil t)
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
	 (go-mode . ck/go-mode-setup))
  :bind (:map go-mode-map
	      ("M-." . godef-jump)
	      ("M-," . godef-jump-back)))

(use-package go-stacktracer
  :after go-mode)

(use-package go-playground
  :after go-mode)

(use-package go-dlv
  :after go-mode)

(use-package go-projectile
  :after go-mode)

(use-package go-eldoc
  :commands (go-eldoc-setup)
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; -=[ Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)
	 ("\\.cabal\\'" . haskell-cabal-mode)))

;; -=[ Python

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package ein
  :defer t)

;; -=[ Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp)
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :after flycheck
  :commands flycheck-rust-setup
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
	 ("/Pipfile\\'" . toml-mode)))

;; -=[ Swift
(use-package swift-mode
  :mode "\\.swift\\'"
  :hook (swift-mode . lsp-deferred))

(use-package lsp-sourcekit
  :after (lsp-mode swift-mode))

;; -=[ packaging
(use-package rpm-spec-mode
  :mode "\\.spec\\'"
  :hook ((rpm-spec-mode . ck/flyspell-prog-maybe)
	 (rpm-spec-mode . ck/show-trailing-ws)))

;; -=[ web stuff
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.xhtml$"   . web-mode)
	 ("\\.vue\\'"   . web-mode))
  :config
  (setq web-mode-enable-engine-detection t))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package fish-mode
  :mode (("\\.fish\\'" . fish-mode)))

;; -=[ json
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
	 ("/Pipfile.lock\\'" . json-mode)))

;; -=[ documentation
(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind (("C-c d" . dash-at-point)))

(use-package devhelp
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :bind (("C-c d" . devhelp-word-at-point)))

(use-package eldoc
  :commands eldoc-mode)

;; -=[ config files
(use-package conf-mode
  :ensure nil
  :mode (("\\.ini\\'" . conf-unix-mode)
	 ("\\.desktop\\'" . conf-desktop-mode))
  :hook (conf-mode . ck/show-trailing-ws))

(use-package apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
	 ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
	 ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(use-package nginx-mode
  :mode ("nginx.conf$" "/etc/nginx/.*"))

;; -=[ better writing
(defun ck-find-langtool ()
  "Find the locations of all available langtool jar (sorted) or nil."
  (let ((basedir (seq-find #'file-directory-p
			   '("/opt/homebrew/Cellar/languagetool"
			     "/usr/local/Cellar/languagetool")))
	(suffix '"/libexec/languagetool-commandline.jar"))
    (if basedir
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

(use-package ck-dir-locals
  :load-path "elisp")

;; -=[ Emacs as App

;; mailer
(defun ck/message-mode-setup()
  "Adjustments for message mode"
  (interactive)
  (when (and buffer-file-name
	     (string-match "gitsend" buffer-file-name))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)))

(use-package message
  :ensure nil
  :commands (compose-mail message-mode)
  :mode (("0000-cover-letter.patch" . message-mode)
	 (".*/\.git/\.gitsendemail.msg.*" . message-mode))
  :config
  (setq  message-send-mail-function 'message-send-mail-with-sendmail
	 sendmail-program "msmtp"
	 message-sendmail-f-is-evil 't
	 message-sendmail-extra-arguments '("--read-envelope-from")
	 mail-host-address "kellner.me")
  (add-hook 'message-mode-hook 'ck/message-mode-setup)
  (add-hook 'message-mode-hook #'ck/flyspell-maybe)
  (add-hook 'message-mode-hook 'ck/show-trailing-ws))

(use-package ck-mail
  :commands (ck/gnus-alias-setup)
  :ensure nil
  :load-path "elisp"
  :init
  (setq user-mail-address (getenv "EMAIL")))

(use-package gnus-alias
  :hook ('message-setup . gnus-alias-determine-identity)
  :config
  (ck/gnus-alias-setup))

;; -=[ server
(use-package server
  :defer 2
  :ensure nil
  :commands (server-start server-running-p)
  :init (unless (server-running-p)
	  (server-start)))

;; -=[ UI

(use-package neotree
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq neo-vc-integration nil
	neo-banner-message nil
	neo-show-updir-line nil
	projectile-switch-project-action 'neotree-projectile-action))

;; -=[ fonts
(use-package ck-fonts
  :ensure nil
  :commands ck-set-font)

(defun set-dark-frame (frame)
  "Make the FRAME's window decoration dark."
  (let ((frame-name (cdr (assq 'name (frame-parameters frame)))))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \""
             frame-name
             "\""))))

(defun new-frames-setup (frame)
  "Called for each new FRAME in daemon mode."
  (let ((have-gui (memq (framep frame) '(x w32 ns mac)))
	(is-mac (eq system-type 'darwin))
	(is-linux (eq system-type 'gnu/linux)))
    (set-frame-parameter frame 'menu-bar-lines (if (and have-gui is-mac) 1 0))
    (when have-gui
      (with-selected-frame frame
	(ck-set-font))
      (when is-linux
	(set-dark-frame frame)))))

(add-hook 'after-make-frame-functions 'new-frames-setup)
(unless (daemonp)
  (new-frames-setup (selected-frame)))

;; -=[ mode-line
(use-package ck-modeline
  :commands ck/modeline-set
  :ensure nil
  :load-path "elisp"
  :init
  (ck/modeline-set 'default 't))

;; -=[ color themes

;(use-package leuven-theme)

(defcustom ck-theme 'doom-nord
  "Which doom theme to load."
  :type '(choice
	  (const :tag "Vibrant" 'doom-vibrant)
	  (const :tag "One" 'doom-one)
	  (const :tag "One-Light" 'doom-one-light)
	  (const :tag "Nord" 'doom-nord)
	  (const :tag "CityLights" 'doom-city-lights)
	  (const :tag "Moonlight" 'doom-moonlight)
	  (const :tag "Spacegrey" 'doom-spacegrey))
  :group 'ck)

(use-package doom-themes
  :init
  (if (daemonp)
      (load-theme 'doom-vibrant t)
    (load-theme ck-theme t))
  (doom-themes-neotree-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; all done, pheww
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
