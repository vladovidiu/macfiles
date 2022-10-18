;;;; vt-programming.el --- Programming Module        -*- lexical-binding: t; -*-

;; Author: Vlad Tutunea

;;; Commentary:

;; 

;;; Code:
;; Dependencies
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'ws-butler)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'flycheck)

;; Parentheses
(electric-pair-mode 1)

;;; ws-butler
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;; Evil Nerd Commenter
(evilnc-default-hotkeys)

;;; Tree Sitter
(require 'tree-sitter)
(require 'tree-sitter-langs)

(add-hook 'prog-mode-hook #'tree-sitter-hl-mode)

;;; LSP
(require 'lsp-mode)
(require 'lsp-ui)

;; Settings
(customize-set-variable 'read-process-output-max (* 1024 1024))
(customize-set-variable 'lsp-keymap-prefix "C-c l")
(customize-set-variable 'lsp-completion-provider :none)
(customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
(customize-set-variable 'lsp-ui-peek-enable t)
(customize-set-variable 'lsp-ui-doc-max-height 14)
(customize-set-variable 'lsp-ui-doc-max-width 72)
(customize-set-variable 'lsp-ui-doc-delay 0.75)
(customize-set-variable 'lsp-ui-doc-show-with-mouse nil)
(customize-set-variable 'lsp-ui-doc-position 'at-point)
(customize-set-variable 'lsp-ui-sideline-ignore-duplicate t)

;; Start lsp and lsp ui
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'enh-ruby-mode-hook 'lsp-deferred)
(add-hook 'ruby-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode 'lsp-ui-mode)

;;; Flycheck
(require 'flycheck)
(add-hook 'lsp-mode 'flycheck-mode)

(provide 'vt-programming)
;;; vt-programming.el ends here
