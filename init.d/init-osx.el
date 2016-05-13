;;; init-osx.el -- OSX specific initialization
;; Author: Christian Kellner <christian@kellner.me>

;;; Commentary:
;; Initialize OSX specific things.

;;; Code:

(setq mac-option-modifier 'meta)
(if (boundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(provide 'init-osx)
;;; init-osx.el ends here

