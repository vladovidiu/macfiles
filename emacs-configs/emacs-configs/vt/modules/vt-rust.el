;;; vt-rust.el --- Rust configuration                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad

;; Author: Vlad <vt@andromeda.local>
;; Keywords: lisp, 

;;; Commentary:

;; Rust configuration file. It uses `rustic-mode'

;;; Code:

;; Dependencies
(straight-use-package 'rustic)

;; Hooks
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'flycheck-mode)

(provide 'vt-rust)
;;; vt-rust.el ends here
