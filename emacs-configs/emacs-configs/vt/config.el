;;; config.el --- Personal config file               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp,

;;; Commentary:
;;; Code:

;; Mandatory
(require 'vt-defaults)
(require 'vt-completion)
(require 'vt-evil)
(require 'vt-ui)

;; Optional
(require 'vt-programming)
(require 'vt-ruby)
(require 'vt-rust)

;; Set the default face.
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "PragmataPro Mono Liga 24" :height 240))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Iosevka Aile 24" :height 240)))))))

(progn
  (disable-theme 'deeper-blue)
  (load-theme 'ef-autumn t))

;;; config.el ends here
