;;; init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 👋 Hello, world. This is my personal Emacs configuration.

;;; Initial Setup
(dolist (path '("lisp" "modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;;; Modules
;;;; Package management
(require 'my-config-straight-pkg)

;;;; General
;; load no-littering first
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))
  (no-littering-theme-backups))

(use-package my-config-defaults
  :straight nil)

(use-package my-config-keybindings
  :straight nil
  :config
  (my/define-leader-key
    "wo" 'ace-window))

(use-package my-config-fonts
  :straight nil)

(use-package my-config-mode-line
  :straight nil)

(use-package my-config-themes
  :straight nil
  :config
  (keymap-global-set "<f5>" #'my/themes-toggle-themes))

(use-package my-config-completion-vertico
  :straight nil
  :config
  (my/define-leader-key
    "b"  'consult-buffer
    "s"  'consult-line
    "tt" 'consult-theme
    "rr" 'consult-recent-file
    "rb" 'consult-bookmark
    "rm" 'consult-bookmark
    ))

(use-package my-config-programming
  :straight nil
  :config
  (my/define-leader-key
    "ei" 'treemacs
    "ee" 'treemacs-add-and-display-current-project-exclusively))

(use-package my-config-writing
  :straight nil
  :config
  (my/define-leader-key
    "do"   #'dictionary-lookup-definition
    "twh"  #'my/large-headings-mode
    "twbw" 'writeroom-mode
    "twbb" 'olivetti-mode))

(use-package my-config-notes
  :straight nil
  :config
  (my/define-leader-key
    "nn" 'my/notes-new
    "ni" 'my/notes-new-and-link
    "ng" 'my/notes-consult-ripgrep))

(use-package my-config-app
  :straight nil
  :config
  (my/define-leader-key
    "tee" 'eat
    "tei" 'eat-project-other-window
    "tej" 'eat-project))

(use-package my-config-utils
  :straight nil
  :config
  (my/define-leader-key
    "lr" 'my/buffer-insert-relative-link-to-file))

;;; Other
;;;; Theme
(my/themes-set-theme "modus-operandi")

;;;; Keybindings
(keymap-global-set "C-," 'set-mark-command)

(my/define-leader-key
  "SPC" 'cycle-spacing
  "C-," 'set-mark-command
  "." 'find-file
  "-" 'kill-this-buffer
  "lk" 'downcase-dwim
  "lj" 'capitalize-dwim
  "cc" 'compile
  "do" 'dictionary-lookup-definition
  "hd" 'devdocs-lookup
  "ho" 'describe-symbol
  "hf" 'describe-face
  "hk" 'describe-key
  "j" 'execute-extended-command
  "o" nil
  "og" 'magit-status
  "kjj" 'kill-buffer-and-window
  "k SPC" 'kill-buffer
  "kl" 'delete-other-windows
  "kk" 'delete-window
  "mf" 'make-frame
  "qe" 'eval-last-sexp
  "tn" 'display-line-numbers-mode
  "twv" 'variable-pitch-mode
  "us" 'scratch-buffer
  "wj" 'winner-undo
  "wk" 'winner-redo
  )

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)

