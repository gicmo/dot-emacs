;;; osx.el -- OSX specific setup

;;; Commentary:
;;; osx setup

;;; Code:

(when (memq window-system '(mac ns))
  (setq mac-option-modifier 'meta)
  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))
  )


;;; osx.el ends here

