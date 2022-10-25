;;; vt-evil.el Evil configuration -*- lexical-binding: t; -*-
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

(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'undo-fu)
(straight-use-package 'undo-fu-session)

;; Turn on undo-fu-session-globally
(global-undo-fu-session-mode)
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-kill-on-visual-paste nil)
(customize-set-variable 'evil-want-C-i-jump t)
(customize-set-variable 'evil-want-C-u-scroll t)
(customize-set-variable 'evil-want-C-d-scroll t)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-want-C-h-delete t)
(customize-set-variable 'evil-undo-system 'undo-fu)
(customize-set-variable 'evil-want-fine-undo t)
(customize-set-variable 'evil-want-Y-yank-to-eol t)

;; Load evil end enable it globally
(require 'evil)
(evil-mode 1)

;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

;; Turn on evil nerd commenter
(evilnc-default-hotkeys)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

(evil-collection-init)

;;; Keybinds
;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-global-set-key 'normal "]e" 'next-error)
(evil-global-set-key 'motion "]e" 'next-error)
(evil-global-set-key 'normal "[e" 'previous-error)
(evil-global-set-key 'motion "[e" 'previous-error)

(provide 'vt-evil)
;;; vt-evil.el ends here
