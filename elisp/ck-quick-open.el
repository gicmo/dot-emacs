;;; ck-quick-open.el --- Xcode-style "Open Quickly" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'consult)
(require 'consult-projectile)
(require 'consult-lsp)

(defun ck/quick-open--symbol-candidates ()
  "Return workspace-symbol candidates for the current lsp-mode project.
Backend-specific: swap this body for an eglot/consult-eglot equivalent
if the LSP client is ever switched."
  (when-let* ((ws (lsp-workspaces)))
    (mapcan #'consult-lsp--symbols--make-transformer
            (with-lsp-workspaces ws
              (lsp-request "workspace/symbol" (list :query "") :no-merge t)))))

(defvar ck/quick-open--symbol-source
  (list :name     "Symbol"
        :narrow   ?s
        :category 'consult-lsp-symbols
        :enabled  (lambda () (bound-and-true-p lsp-mode))
        :state    #'consult-lsp--symbols--state
        :items    #'ck/quick-open--symbol-candidates)
  "Consult source for project-wide LSP symbols.")

(defun ck/quick-open ()
  "Xcode-style \"Open Quickly\": fuzzy search files and symbols together."
  (interactive)
  (consult--multi (list consult-projectile--source-projectile-file
                        ck/quick-open--symbol-source)
                  :prompt "Open Quickly: "
                  :sort nil))

(provide 'ck-quick-open)
;;; ck-quick-open.el ends here
