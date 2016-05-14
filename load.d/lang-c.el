;;; lang-c.el -- c langauge famiy setup

;;; Commentary:
;;; lang-c - setup c and friends


;;; Code:
(require 'use-package)

; -=[ C
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
    :init
    (require 'flycheck)
    :config
    (flycheck-irony-setup))
  )


;; -=[ CUDA mode
(use-package cuda-mode
  :mode "\\.cu\\'")


;;; lang-c.el ends here
