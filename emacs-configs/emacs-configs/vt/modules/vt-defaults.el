;;; vt-defaults.el Saner defaults -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Vlad Tutunea
;;
;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Maintainer: Vlad Tutunea <me@vladtutunea.xyz>
;; Created: September 03, 2022
;; Modified: September 03, 2022
;; Version: 0.0.1
;; Keywords: lisp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Dependencies
(straight-use-package 'no-littering)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'recentf-save-file
                        (expand-file-name "recentf" vt-config-var-directory))

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicate t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make sheband (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable save-hist mode for command history
(savehist-mode 1)

;; Set a custom file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Saner defaults
(setq-default
 create-lockfiles nil
 auto-save-default nil
 auto-windows-vscroll nil
 delete-by-moving-to-trash t
 fill-column 80
 enable-recursive-minibuffers t
 help-window-select t
 x-stretch-cursor t
 echo-keystrokes 0.1
 tab-always-indent 'complete
 ring-bell-function 'ignore
 vc-follow-symlinks t)

(setq backup-directory-alist '(("." . "~/.save")))

;; Use ibuffer instead of *Buffer List*
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'vt-defaults)
;;; vt-defaults.el ends here
