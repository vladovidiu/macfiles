;;; early-init.el --- early-init sets some sane defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp

;;; Commentary:

;;; Code:

;; Increase the GC threshold for faster startup
;; The default is 800kb. Measure in bytes
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading newest compiled .el files
(setq load-prefer-newer 'noninteractive)
(customize-set-variable 'load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happen async
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Don't use package.el, but rather straight.el
(setq package-enable-at-startup nil)

;; Remove unneeded UI elements
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Loads a nice blue theme, avoids the white screen flash on startup
(load-theme 'deeper-blue t)

;; Make the initial buffer load faster
(customize-set-variable 'initial-major-mode 'fundamental-mode)

;; Declare vt config path
(defvar vt-config-path
  (let ((home-dir (getenv "HOME")))
    (expand-file-name "emacs-configs/vt" home-dir))
  "The vt emacs location")

;;; early-init.el ends here
