;;; init-osx.el -- OSX specific initialization
;; Author: Christian Kellner <christian@kellner.me>

;;; Commentary:
;; Initialize OSX specific things.

;;; Code:

; === load path for homebrew  ===
(when (memq window-system '(mac ns))
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

(setq mac-option-modifier 'meta)
(if (boundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

;; pick up the correct path from a login shell
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (customize-set-variable 'exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))

(provide 'init-osx)
;;; init-osx.el ends here

