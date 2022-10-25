;;; vt-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad

;; Author: Vlad <vt@andromeda.local>
;; Keywords: lisp, 

;;; Commentary:

;; Rust configuration file. It uses `rustic-mode'

;;; Code:

;; Dependencies
(straight-use-package 'rustic)

;; Settings
(customize-set-variable 'lsp-rust-analyzer-cargo-watch-command "clippy")
(customize-set-variable 'lsp-rust-analyzer-proc-macro-enable t)
(customize-set-variable 'lsp-rust-clippy-preference "on")
(customize-set-variable 'lsp-rust-analyzer-server-display-inlay-hints t)
(customize-set-variable 'lsp-rust-analyzer-display-chaining-hints t)
(customize-set-variable 'lsp-rust-analyzer-display-closure-return-type-hints t)



;; Hooks
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'flycheck-mode)

(provide 'vt-rust)
;;; vt-rust.el ends here
