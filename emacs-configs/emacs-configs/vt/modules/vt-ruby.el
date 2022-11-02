;;;; vt-ruby.el --- Ruby Module                      -*- lexical-binding: t; -*-

;; Author: Vlad Tutunea

;;; Commentary:

;;

;;; Code:

;; Dependencies
(straight-use-package 'ruby-mode)
(straight-use-package 'yard-mode)
(straight-use-package 'inf-ruby)
(straight-use-package 'rubocop)
(straight-use-package 'bundler)
(straight-use-package 'rake)
(straight-use-package 'chruby)
(straight-use-package 'rspec-mode)
(straight-use-package 'minitest)
(straight-use-package 'projectile-rails)
(straight-use-package 'inflections)

(defun set-electric! (modes &rest plist)
  "Declare that WORDS (list of strings) or CHARS (lists of chars) should trigger
electric indentation.
Enables `electric-indent-local-mode' in MODES.
\(fn MODES &key WORDS CHARS)"
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+electric--init-%s-h" mode))))
      (cond ((null (car-safe plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset
              fn (lambda ()
                   (when (eq major-mode mode)
                     (setq-local electric-indent-inhibit nil)
                     (cl-destructuring-bind (&key chars words) plist
                       (electric-indent-local-mode +1)
                       (if chars (setq-local electric-indent-chars chars))
                       (if words (setq +electric-indent-words words))))))
             (add-hook hook fn))))))

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:repl' setting.")

(defun set-repl-handler! (modes command &rest plist)
  "Defines a REPL for MODES.
MODES is either a single major mode symbol or a list of them. COMMAND is a
function that creates and returns the REPL buffer.
COMMAND can either be a function that takes no arguments, or an interactive
command that will be called interactively. COMMANDS must return either the repl
buffer or a function that takes no arguments and returns the repl buffer.
PLIST is a property list that map special attributes to this repl. These are
recognized:
  :persist BOOL
    If non-nil, this REPL won't be killed when its window is closed.
  :send-region FUNC
    A function that accepts a BEG and END, and sends the contents of the region
    to the REPL. Defaults to `+eval/send-region-to-repl'.
  :send-buffer FUNC
    A function of no arguments that sends the contents of the buffer to the REPL.
    Defaults to `+eval/region', which will run the :send-region specified function
    or `+eval/send-region-to-repl'."
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (setf (alist-get mode +eval-repls)
          (cons command plist))))

;;; Ruby Mode
(setq ruby-insert-encoding-magic-comment nil)
(set-electric! 'ruby-mode :words '("else" "end" "elsif"))
(set-repl-handler! 'ruby-mode #'inf-ruby)

(add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)

;;; Yard Mode
(add-hook 'ruby-mode-hook #'yard-mode)

;;; RuboCop
(add-hook 'ruby-mode-hook #'rubocop-mode)

;;; Chruby
(add-hook 'ruby-mode-hook #'chruby-use-corresponding)

;;; Minitest
(add-hook 'minitest-mode-hook #'evil-normalize-keymaps)

;;; Sorbet
(require 'lsp-mode)

(defgroup lsp-sorbet nil
  "LSP support for Ruby, using the Sorbet language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/sorbet/sorbet")
  :package-version '(lsp-mode . "7.1.0"))

(defcustom lsp-sorbet-use-bundler nil
  "Run sorbet under bundler"
  :type 'boolean
  :group 'lsp-sorbet
  :package-version '(lsp-mode . "7.1.0"))

(defun lsp-sorbet--build-command ()
  "Build sorbet command"
  (let ((lsp-command '("srb" "typecheck" "--lsp" "--disable-watchman")))
    (if lsp-sorbet-use-bundler
              (append '("bundle" "exec") lsp-command)
            lsp-command)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   #'lsp-sorbet--build-command)
  :priority 100
  :major-modes '(ruby-mode)
  :server-id 'sorbet-ls))

(provide 'vt-ruby)
;;; vt-ruby.el ends here
