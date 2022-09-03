;;; config.el --- Personal config file               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Vlad Tutunea

;; Author: Vlad Tutunea <me@vladtutunea.xyz>
;; Keywords: lisp,

;;; Commentary:
;;; Code:

(require 'vt-defaults)
(require 'vt-evil)
(require 'vt-ui)

;; Set the default face.
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "PragmataPro Mono Liga 20"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Iosevka Aile 20")))))))

(progn
  (disable-theme 'deeper-blue)
  (load-theme 'ef-day t))

;;; config.el ends here
