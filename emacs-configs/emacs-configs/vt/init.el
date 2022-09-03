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
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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

;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
