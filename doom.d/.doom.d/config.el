(setq user-full-name "Vlad Tutunea"
      user-mail-address "me@vladtutunea.xyz")

(setq default-directory "~")

(setq-default
 delete-by-moving-to-trash t
 tab-width 2
 uniquify-buffer-name-style 'forward
 window-combination-resize t
 x-stretch-cursor t
 undo-limit 8000000
 evil-want-fine-undo t
 auto-save-default t
 inhibit-compacting-font-caches t
 )

(global-set-key (kbd "C-x C-b") 'ibuffer)

(display-time-mode 1)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq doom-font (font-spec :family "PragmataPro Mono Liga" :size 20)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 18))

(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 200))

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(setq doom-theme 'doom-tokyo-night)

(menu-bar-mode t)
(setq display-line-numbers-type nil)
(blink-cursor-mode 1)
(set-ligatures! 'typescript-tsx-mode
  :symbol "keyword")

(setq org-directory "~/org"
      org-ellipsis " ▼ ")

(defun vt/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . vt/org-mode-visual-fill))

(setq +format-with-lsp nil)

(setq zoom-mode t)
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

(when (file-directory-p "~/code")
  (setq projectile-project-search-path '("~/code")))

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
       (:map dired-mode-map
        :leader
        :desc "Peep-dired image previews"
        "d p" #'peep-dired
        :leader
        :desc "Dired view file"
        "d v" #'dired-view-file)))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package! tree-sitter
  :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                            (error-message-string e))
            (signal (car e) (cadr e)))))))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-all))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(setq! marginalia-align 'right)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
