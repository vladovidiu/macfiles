;;; vt-completion.el --- Setup completion packages   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad

;; Author: Vlad <vt@andromeda.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(straight-use-package 'cape)
(straight-use-package 'consult)
(straight-use-package 'corfu-doc)
(straight-use-package 'corfu)
(straight-use-package 'kind-icon)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'vertico)

;; Quality of Life for easier navigation in minibuffer
(defun vt-completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

;;; Vertico
(require 'vertico)

(add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" straight-base-dir))
(require 'vertico-directory)
(require 'vertico-flat)

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

(customize-set-variable 'vertico-cycle t)

(vertico-mode 1)
(vertico-flat-mode 1)

(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)

;;; Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(customize-set-variable 'marginalia-align 'right)
(customize-set-variable 'marginalia-max-relative-age 0)

(marginalia-mode 1)

;;; Consult
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)

;;; Orderless
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))

;;; Embark
(require 'embark)
(require 'embark-consult)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Corfu
(add-to-list 'load-path (expand-file-name "straight/build/corfu/extensions" straight-base-dir))
(require 'corfu-history)

(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto t)
(customize-set-variable 'corfu-auto-prefix 2)
(customize-set-variable 'corfu-auto-delay 0)
(customize-set-variable 'corfu-min-width 60)
(customize-set-variable 'corfu-max-width 60)
(customize-set-variable 'corfu-count 14)
(customize-set-variable 'corfu-scroll-margin 4)
(customize-set-variable 'corfu-echo-documentation 0.25)
(customize-set-variable 'corfu-separator ?\s)
(customize-set-variable 'corfu-quit-no-match 'separator)

(customize-set-variable 'corfu-doc-delay 0.5)
(customize-set-variable 'corfu-doc-max-width 60)
(customize-set-variable 'corfu-doc-max-height 20)

(global-corfu-mode 1)
(corfu-history-mode 1)

(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

;;; Kind Icons
(require 'kind-icon)
(customize-set-variable 'kind-icon-default-face 'corfu-default)
(customize-set-variable 'kind-icon-blend-background nil)
(customize-set-variable 'kind-icon-blend-frac 0.08)
(customize-set-variable 'kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;;; Cape
(require 'cape)
;; Add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                            corfu-quit-no-match t
                            corfu-auto nil)
            (corfu-mode)))

(provide 'vt-completion)
;;; vt-completion.el ends here
