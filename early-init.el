;;; early-init.el --- runs before package.el and the first frame -*- lexical-binding: t; -*-
;;; Commentary:
;;; Loaded by Emacs 27+ before package.el activates packages.
;;; Code:

;; Must be here; by the time init.el is read, package.el has already
;; activated every installed package.
(setq package-enable-at-startup nil)

;; Shape the initial frame before it is mapped, so chrome is never drawn.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(width . 160) default-frame-alist)
(push '(height . 45) default-frame-alist)

(let ((fr-size 4))
  (push (cons 'left-fringe  fr-size) default-frame-alist)
  (push (cons 'right-fringe fr-size) default-frame-alist))

;; Keep in sync with `ck-theme': the frame is mapped long before the theme
;; is loaded, and would otherwise be painted white until then.
(push '(background-color . "#282C34") default-frame-alist)
(push '(foreground-color . "#BBC2CF") default-frame-alist)

(setq-default fringes-outside-margins t)

;;; early-init.el ends here
