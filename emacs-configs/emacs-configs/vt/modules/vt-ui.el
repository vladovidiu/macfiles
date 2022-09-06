;;; vt-ui.el --- User Interface customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp

;;; Commentary:
;;; Code:

;; Install dependencies
(straight-use-package '(ef-themes :type git :host github :repo "protesilaos/ef-themes"))
(straight-use-package 'all-the-icons)
(straight-use-package 'helpful)
(straight-use-package 'elisp-demos)
(straight-use-package 'ns-auto-titlebar)
(straight-use-package 'popper)
(straight-use-package 'diminish)

;;; Misc
(display-time-mode 1)
(set-fringe-mode 10)
(global-hl-line-mode 1)
(setq display-line-numbers-width t)

;;; ns-auto-titlebar
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;;; Help Buffers
;; Make `describe-*' screens more helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;;; Elisp Demos
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;;; Popper
(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "\\*WoMan\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*ruby\\*"
        help-mode
        helpful-mode
        compilation-mode))

(global-set-key (kbd "M-`") 'popper-toggle-latest)
(global-set-key (kbd "C-M-`") 'popper-cycle)
(popper-mode 1)

;;; Diminish
(require 'diminish)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'eldoc-mode)
(diminish 'yard-mode)
(diminish 'rubocop-mode)
(diminish 'ws-butler-mode)
(diminish 'flycheck-mode)
(diminish 'tree-sitter-mode)

;;; Comint
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'vt-ui)
;;; vt-ui.el ends here
