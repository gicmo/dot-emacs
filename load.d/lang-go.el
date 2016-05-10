;; -=[ Go mode
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
  (use-package go-autocomplete)
  )

(use-package go-rename
  :commands (go-rename))

