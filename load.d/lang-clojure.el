;; clojure setup

(use-package clojure-mode
  :mode "\\.clj"
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (use-package clojure-mode-extra-font-locking))
