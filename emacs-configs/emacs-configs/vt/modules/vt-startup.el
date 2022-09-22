;;; vt-startup.el --- Emacs startup customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp

;;; Commentary:
;;; Code:
(defgroup vt-startup '()
  "Startup configuration for vt Emacs."
  :tag "vt startup"
  :group 'vt)

(defcustom vt-startup-recentf-count 10
  "The number of recent files to display on the splash screen."
  :type 'number
  :group 'vt-startup)

(defconst vt-startup-text
  `((:face (variable-pitch font-lock-comment-face (:height 1.5) bold)
           ,(let* ((welcome-text "Welcome to Emacs, Vlad!\n\n")
                   (welcome-len (length welcome-text))
                   (welcome-mid (/ welcome-len 2)))
              (concat
               (make-string (abs (- (/ (window-width) 2)
                                    welcome-mid))
                            ? )
               welcome-text))
           :face variable-pitch
           :link ("View Emacs Manual" ,(lambda (_button) (info "emacs")))
           "\tView the Emacs Manual using Info\n"
           "\n"))
  "A list of texts to show in the middle part of splash screens.
Each element in the list should be a list of strings or pairs
`:face FACE`, like `fancy-splash-insert` accepts them.")

(defun vt-startup-tail (&optional concise)
  "Insert the tail part of the splash screen into the current buffer."
  (fancy-splash-insert
   :face 'variable-pitch
   "\nTo start...  "
   :link `("Open a file"
           ,(lambda (_button) (call-interactively 'find-file))
           "Specify a new file's name, to edit the file")
   "    "
   :link `("Open home directory"
           ,(lambda (_button) (dired "~"))
           "Open home directory in dired.")
   "    "
   :link `("Open Emacs config"
           ,(lambda (_button) (dired vt-config-path))
           "Open the Emacs configuration directory.")
   "\n"))

(defun vt-startup-recentf ()
  (message "Showing recents on splash screen")
  (fancy-splash-insert
   :face '(variable-pitch font-lock-string-face italic)
   (condition-case recentf-list
       (if (not (seq-empty-p recentf-list))
           "Recent files:\n"
         "\n")
     (error "\n")))
  (condition-case recentf-list
      (if (not (seq-empty-p recentf-list))
          (dolist (file (seq-take recentf-list vt-startup-recentf-count))
            (fancy-splash-insert
             :face 'default
             :link `(,file ,(lambda (_button) (find-file file)))
             "\n"))
        "\n")
    (error "\n")))

(defun vt-startup-screen (&optional concise)
  "Display fancy startup screen.
if CONCISE is non-nil, display a concise version of the
splash screen in another window."
  (message "Loading vt Startup Screen")
  (let ((splash-buffer (get-buffer-create "*vt Emacs*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (make-local-variable 'vt-startup-screen-inhibit-startup-screen)
        (if pure-space-overflow
            (insert pure-space-overflow-message))
        (dolist (text vt-startup-text)
          (apply #'fancy-splash-insert text)
          (insert "\n"))
        (insert "\n\n")
        (vt-startup-recentf)
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (insert "\n")
        (vt-startup-tail concise))
      (use-local-map splash-screen-keymap)
      (setq-local browse-url-browser-function 'eww-browse-url)
      (setq tab-width 22
            buffer-read-only t)
      (set-buffer-modified-p nil)
      (if (and view-read-only (not view-mode))
          (view-mode-enter nil 'kill-buffer))
      (goto-char (point-min))
      (forward-line (if concise 2 4)))
    (if concise
        (progn
          (display-buffer splash-buffer)
          ;; If the splash screen is in a split window, fit it.
          (let ((window (get-buffer-window splash-buffer t)))
            (or (null window)
                (eq window (selected-window))
                (eq window (next-window window))
                (fit-window-to-buffer window))))
      (switch-to-buffer splash-buffer))))

(provide 'vt-startup)
;;; vt-startup.el ends here
