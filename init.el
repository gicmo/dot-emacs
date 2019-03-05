;;; init.el --- main Emacs initialization
;;; Commentary:
;;; Setup Emacs, make it homely and cosy
;; Author: Christian Kellner <christian@kellner.me>

;;; Code:

; -=[ sane defaults
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
(setq mode-line-default-help-echo nil)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; Title bar shows name of current buffer.
(setq frame-title-format '("emacs: %*%+ %b"))

;; -=[ initialize the core
(require 'ck-core (concat user-emacs-directory "elisp/ck-core"))

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

(require 'bind-key)

(setq use-package-always-ensure t)

;; -=[ Dashboard
(use-package ck-dashboard
  :load-path "elisp"
  :ensure f
  :commands dashboard-show
  :init
  (dashboard-show))

;; pick up the correct path from a login shell
(use-package exec-path-from-shell
  :if (memq system-type '(gnu gnu/linux darwin))
  :init
  (customize-set-variable 'exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  (exec-path-from-shell-copy-env "WORKON_HOME"))

(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))


(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired+
  :load-path "ewiki"
  :defer t
  :init
  (defun setup-dired ()
    "Setup dired."
     (setq diredp-hide-details-initially-flag nil
	   global-dired-hide-details-mode -1)
     (require 'dired+))
  (add-hook 'dired-load-hook #'setup-dired)
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

(use-package smart-region
  :bind (("C-SPC" . smart-region)))

; -=[ EditorConfig

(use-package editorconfig
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
    (flx-ido-mode t))
  (use-package ido-vertical-mode
    :init
    (ido-vertical-mode 1)))

(use-package idomenu
  :commands idomenu
  :load-path "ewiki"
  :ensure f)

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

(use-package nlinum
  :hook (prog-mode . nlinum-mode)
  :config
  (setq nlinum-format "%4d "
	nlinum-highlight-current-line 't))

(use-package hl-line
  :disabled
  :after nlinum
  :hook (nlinum-mode . hl-line-mode))

(use-package back-button
  :commands (back-button-mode)
  :defer 2
  :init
  (setq back-button-show-toolbar-buttons nil)
  :config
  (back-button-mode 1))

;; -=[ spell checking, because I will never learn how to spell
(use-package flyspell
  :commands (flyspell-prog-mode flyspell-mode flyspell-buffer)
  :hook (prog-mode . flyspell-prog-mode)
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
	  ispell-dictionary "english"))
   (t (setq ispell-program-name nil))))

					; -=[ Org
(use-package org
  :commands org-mode
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-directory "~/Documents/Notes/"
	org-agenda-files '("~/Documents/Notes/")
	org-mobile-directory "~/Documents/Notes/.mobile"
	org-mobile-inbox-for-pull  "~/Documents/Notes/todo.org"
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
     (sh     . t)))
  (use-package org-journal
    :init
    (setq org-journal-dir "~/Documents/Notes/journal"
	  org-journal-file-format "%Y-%B.org"))
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)))

; -=[ Projects via projectile

(defun ck/projectile-commander-setup ()
  "Setup projectile commander shortcuts."
  (require 'projectile)
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired)))

(use-package projectile
  :defer 1
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired)
  (ck/projectile-commander-setup))

; -=[ flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-pos-tip-timeout 7
	  flycheck-display-errors-delay 0.5)
    (flycheck-pos-tip-mode +1))
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 256 384 448 480 496 480 448 384 256 0 0 0 0 0]
    ))

;; -=[ git
(use-package git-gutter-fringe
  :if window-system
  :config
  (use-package fringe-helper)
  (setq git-gutter-fr:side 'right-fringe)
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)
  (setq-default fringes-outside-margins t)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted  '(center repeated)
    "XXX....."))

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
        fill-column 72)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'ck/show-trailing-ws))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-post-refresh-hook
	    'git-gutter:update-all-windows))

(use-package forge
  :after magit
  :config
  (dolist (url '("gitlab.freedesktop.org" "gitlab.gnome.org"))
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
  :hook ((markdown-mode . flyspell-mode)
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
  :ensure f
  :hook (prog-mode . ck/show-trailing-ws))

; -=[ common packages
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

(use-package paredit
  :commands enable-paredit-mode)

(use-package lsp-mode
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t
	lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode)
	 (lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1))))
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

(use-package company-lsp
  :commands company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp)
  :custom
  (company-lsp-async t)
  (company-lsp-enable-snippet t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

; -=[ Assembler modes

(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$")

; -=[ C/C++/ObjC and friends
(use-package cc-mode
  :bind (:map c-mode-base-map
	      ("C-c o" . ff-find-other-file))
  :config
  (setq c-hungry-delete-key t
	indent-tabs-mode nil
	gdb-many-windows t
	gdb-show-main t)
  (use-package flycheck-clang-analyzer
    :disabled
    :after flycheck
    :config
    (flycheck-clang-analyzer-setup)))

(use-package ccls
  :hook ((c-mode-common . (lambda () (require 'ccls) (lsp))))
  :bind (:map c-mode-base-map
	      ("C-c r h" . ccls-inheritance-hierarchy)
	      ("C-c r H" . ccls-call-hierarchy)
	      ("C-c r L" . ccls-code-lens-mode)
	      ("C-c r m" . ccls-member-hierarchy)))

; detect major mode (objc, c++-mode) for header
(use-package dummy-h-mode
  :load-path "ewiki"
  :ensure f
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

(use-package meson-mode
  :mode "\\meson\\.build\\'")

(use-package vala-mode
  :mode (("\\.vala\\'" . vala-mode)
         ("\\.vapi\\'" . vala-mode))
  :config
  (use-package vala-snippets)
  (run-hooks 'prog-mode-hook)
  (dolist (suffix '("\\.vala\\'" "\\.vapi\\'"))
    (add-to-list 'file-coding-system-alist `(quote ,suffix . utf-8))))

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
	 (clojure-mode . rainbow-delimiters-mode))
  :config
  (use-package clojure-mode-extra-font-locking))

(use-package lsp-java
  :after lsp
  :hook ((java-mode . (lambda () (require 'lsp-java) (lsp)))))

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

;; -=[ Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)
	 ("\\.cabal\\'" . haskell-cabal-mode))
  :config
  (use-package intero
    :hook (haskell-mode . intero-mode)))

;; -=[ Python
(use-package python-mode
  :ensure nil ;; builtin
  :hook (rust-mode . lsp)
  :config
  (require 'lsp-clients))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package ein
  :defer t)

;; -=[ Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp)
  :config
  (require 'lsp-clients)
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cargo
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
	 ("/Pipfile\\'" . toml-mode)))

;; -=[ packaging
(use-package rpm-spec-mode
  :mode "\\.spec\\'"
  :hook ((rpm-spec-mode . flyspell-prog-mode)
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
  :ensure f
  :if (eq system-type 'gnu/linux)
  :bind (("C-c d" . devhelp-word-at-point)))

(use-package eldoc
  :commands eldoc-mode)

;; -=[ config files
(use-package conf-mode
  :ensure f
  :hook (conf-mode . ck/show-trailing-ws))

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

;; -=[ Emacs as App

;; mailer
(defun ck/message-mode-setup()
  "Adjustments for message mode"
  (interactive)
  (when (and buffer-file-name
	     (string-match "gitsend" buffer-file-name))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)))

(use-package message
  :ensure f
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
  (add-hook 'message-mode-hook #'flyspell-mode)
  (add-hook 'message-mode-hook 'ck/show-trailing-ws))

(use-package ck-mail
  :commands (ck/gnus-alias-setup
	     ck/mail-mk-mu4e-contexts
	     ck/mu4e-patch-trash)
  :ensure f
  :load-path "elisp")

(use-package gnus-alias
  :hook ('message-setup . gnus-alias-determine-identity)
  :config
  (ck/gnus-alias-setup))

(use-package mu4e
  :commands mu4e
  :defer t
  :ensure f
  :config
  (setq mu4e-maildir (expand-file-name "~/.mail")
	mu4e-get-mail-command "mbsync -a mu4e"
	mu4e-headers-include-related t
	mu4e-headers-skip-duplicates t
	mu4e-compose-dont-reply-to-self t
	mu4e-change-filenames-when-moving t
	mu4e-view-show-images t
	mu4e-view-show-addresses t
	mu4e-context-policy 'pick-first
	mu4e-contexts (ck/mail-mk-mu4e-contexts)
	mu4e-marks (ck/mu4e-patch-trash mu4e-marks))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (use-package mu4e-maildirs-extension
    :init
    (mu4e-maildirs-extension)
    :config
    (setq mu4e-maildirs-extension-maildir-expanded-prefix "")
    (setq mu4e-maildirs-extension-maildir-default-prefix "")))

;; -=[ server
(use-package server
  :defer 2
  :ensure f
  :commands (server-start server-running-p)
  :init (unless (server-running-p)
	  (server-start)))

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
(use-package ck-fonts
  :ensure f
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

(let ((fr-size 4))
  (push (cons 'left-fringe  fr-size) default-frame-alist)
  (push (cons 'right-fringe fr-size) default-frame-alist))

;; -=[ mode-line
(use-package ck-modeline
  :commands ck/modeline-set
  :ensure f
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
	  (const :tag "Nord" 'doom-nord))
  :group 'ck)

(use-package doom-themes
  :init
  (if (daemonp)
      (load-theme 'doom-vibrant t)
    (load-theme ck-theme t))
  (doom-themes-neotree-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (use-package solaire-mode
    :init
    (setq solaire-mode-remap-modeline nil)
    (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
    (add-hook 'after-revert-hook #'turn-on-solaire-mode)
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)))

;; all done, pheww
;;; init.el ends here
