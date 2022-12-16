;;; vt-lisp.el --- Lisp Tooling                      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad

;; Author: Vlad <vt@andromeda.local>
;; Keywords: lisp

;;; Commentary: Lisp packages to improve editing
;;; Code:

;; Dependencies
(straight-use-package 'lispy)
(straight-use-package 'lispyville)

(require 'lispy)
(require 'lispyville)

;; Hooks
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lispy-mode-hook #'lispyville-mode)

(provide 'vt-lisp)
;;; vt-lisp.el ends here
