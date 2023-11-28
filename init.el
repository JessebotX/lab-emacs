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
;; load `no-littering' first
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory)))
  (no-littering-theme-backups))

(use-package my-config-defaults
  :straight nil)

(use-package my-config-keybindings
  :straight nil
  :config
  (my/define-leader-key
    "o" 'ace-window))

(use-package my-config-mode-line
  :straight nil)

(use-package my-config-themes
  :straight nil)

(use-package my-config-completion-vertico
  :straight nil
  :config
  (my/define-leader-key
    "b"  'consult-buffer
    "s"  'consult-line
    "tt" 'consult-theme
    "rr" 'consult-recent-file
    "rb" 'consult-bookmark))

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
    "twbw" 'writeroom-mode
    "twbb" 'olivetti-mode))

(use-package my-config-notes
  :straight nil
  :config
  (my/define-leader-key
    "nn" 'my/notes-new
    "ng" 'my/notes-consult-ripgrep))

;;; Other
;;;; Theme
(my/themes-set-theme "modus-operandi")

;;;; Keybindings
(keymap-global-set "C-," 'set-mark-command)

(my/define-leader-key
  "C-," 'set-mark-command
  "."   'find-file
  "-"   'kill-this-buffer
  "cc"  'compile
  "j"   'execute-extended-command
  "kjj" 'kill-buffer-and-window
  "kk"  'kill-buffer
  "kl"  'delete-other-windows
  "qe"  'eval-last-sexp
  "tn"  'display-line-numbers-mode
  "twv" 'variable-pitch-mode
  "wj"  'winner-undo
  "wk"  'winner-redo
  )

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)
