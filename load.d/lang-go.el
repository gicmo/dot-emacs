;; -=[ Go mode
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
			    (local-set-key (kbd "M-.") 'godef-jump)))
  (setq gofmt-command "goreturns")
  (use-package go-guru)
  (use-package go-stacktracer)
  (use-package go-playground)
  (use-package go-dlv)
  (use-package go-autocomplete)
  )
