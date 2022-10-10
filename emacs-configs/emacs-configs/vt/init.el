;;; init.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp,

;;; Commentary:
;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set the default coding system
(set-default-coding-systems 'utf-8)

(customize-set-variable 'visible-bell 1) ; turn off beeps, make them flash
(customize-set-variable 'large-file-warning-threshold 100000000) ; change to ~100 MB

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; When writing vt-modules, insert header from skeleton
(auto-insert-mode)
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name user-emacs-directory) "modules/vt-.*\\.el")
          "vt Emacs Lisp Skeleton")
    '("vt Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "

;; Author: Vlad Tutunea

;;; Commentary:

;; " _ "

;;; Code:

(provide '"
       (file-name-base (buffer-file-name))
       ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

(defvar vt-config-file (expand-file-name "config.el" vt-config-path)
  "The Emacs config file.")
(defvar vt-config-etc-directory (expand-file-name "etc/" vt-config-path)
  "The Emacs etc folder.")
(defvar vt-config-var-directory (expand-file-name "var/" vt-config-path)
  "The Emacs var folder.")

(mkdir vt-config-etc-directory t)
(mkdir vt-config-var-directory t)

;; Load the config file if it exists
(when (file-exists-p vt-config-file)
  (load vt-config-file nil 'nomessage))

(require 'vt-startup)
(setq initial-buffer-choice #'vt-startup-screen)

;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
