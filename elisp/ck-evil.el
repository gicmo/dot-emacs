;;; ck-evil.el --- evil-mode keybindings mirroring the nvim config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'evil)

;; -=[ leader / localleader

(defvar ck/leader-map (make-sparse-keymap)
  "Keymap for leader-key (SPC) bindings, mirroring nvim's <leader>.")

(defvar ck/localleader-map (make-sparse-keymap)
  "Keymap for localleader-key (;) bindings, mirroring nvim's <localleader>.")

(evil-define-key '(normal visual) 'global (kbd "SPC") ck/leader-map)
(evil-define-key '(normal visual) 'global (kbd ";") ck/localleader-map)

(defmacro ck/leader-def (key def &optional desc)
  "Bind KEY to DEF in `ck/leader-map', optionally naming it DESC."
  `(progn
     (define-key ck/leader-map (kbd ,key) ,def)
     ,(when desc `(which-key-add-key-based-replacements (concat "SPC " ,key) ,desc))))

;; -=[ pickers (Snacks -> consult/projectile)

(ck/leader-def "ff" #'consult-projectile "Find Files")
(ck/leader-def "fb" #'consult-buffer "Buffers")
(ck/leader-def "fs" #'ck/quick-open "Smart Find Files")
(ck/leader-def "fp" #'projectile-switch-project "Projects")
(ck/leader-def "fr" #'consult-recent-file "Recent")

(ck/leader-def "sg" #'consult-ripgrep "Grep")
(ck/leader-def "ss" #'consult-lsp-file-symbols "LSP Symbols")
(ck/leader-def "sS" #'consult-lsp-symbols "LSP Workspace Symbols")

;; -=[ LSP navigation (Snacks pickers + lsp.lua on_attach -> lsp-mode)

(evil-define-key 'normal 'global
  "gd" #'lsp-find-definition
  "gD" #'lsp-find-declaration
  "gy" #'lsp-find-type-definition
  "gA" #'lsp-find-references
  "gI" #'lsp-find-implementation
  "gi" #'lsp-find-implementation
  "gs" #'consult-lsp-file-symbols
  "gS" #'consult-lsp-symbols
  "gh" #'flycheck-display-error-at-point
  "K"  #'lsp-ui-doc-glance)

(evil-define-key 'normal 'global (kbd "C-k") #'lsp-signature-activate)

(ck/leader-def "rn" #'lsp-rename "Rename")
(ck/leader-def "ca" #'lsp-execute-code-action "Code Action")
(ck/leader-def "F"  #'lsp-format-buffer "Format")
(ck/leader-def "D"  #'lsp-find-type-definition "Type Definition")
(ck/leader-def "wa" #'lsp-workspace-folders-add "Add Workspace Folder")
(ck/leader-def "wr" #'lsp-workspace-folders-remove "Remove Workspace Folder")
(ck/leader-def "wl" #'lsp-workspace-show-log "List Workspace Folders")

;; -=[ diagnostics (nvim vim.diagnostic -> flycheck)

(ck/leader-def "e" #'flycheck-display-error-at-point "Show diagnostics")
(ck/leader-def "q" #'flycheck-list-errors "Diagnostics to list")

;; -=[ git hunks (gitsigns -> diff-hl / magit)

(evil-define-key 'normal 'global
  "]c" #'diff-hl-next-hunk
  "[c" #'diff-hl-previous-hunk)

(ck/leader-def "hs" #'diff-hl-stage-current-hunk "Stage hunk")
(ck/leader-def "hr" #'diff-hl-revert-hunk "Reset hunk")
(ck/leader-def "hp" #'diff-hl-show-hunk "Preview hunk")
(ck/leader-def "hb" #'magit-blame-addition "Blame line")
(ck/leader-def "hd" #'magit-diff-buffer-file "Diff this")
(ck/leader-def "tb" #'magit-blame-echo "Toggle line blame")

;; -=[ magit (neogit -> magit)

(ck/leader-def "gg" #'magit-status "Magit Status")

;; -=[ incremental selection (treesitter -> expand-region)

(evil-define-key '(normal visual) 'global (kbd "C-SPC") #'er/expand-region)

(provide 'ck-evil)
;;; ck-evil.el ends here
