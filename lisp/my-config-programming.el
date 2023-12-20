;;; -*- lexical-binding: t; -*-

(use-package hungry-delete
  :hook (emacs-startup . global-hungry-delete-mode)
  :custom
  ;; delete the following on backspace: SPC, TAB, ^M, ^L, ^K
  (hungry-delete-chars-to-skip " 	"))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package devdocs
  :bind (("C-h d" . devdocs-lookup)))

(use-package dumb-jump
  :config
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package treemacs
  :commands
  (treemacs
   treemacs-add-and-display-current-project-exclusively
   treemacs-add-and-display-current-project)
  :custom
  (treemacs-width 30)
  :config
  (evil-define-key '(normal emacs) treemacs-mode-map
    "d" 'treemacs-delete-file
    "r" 'treemacs-rename-file
    "o" 'treemacs-create-file
    "gr" 'treemacs-refresh))

;;; Languages
;;;; C/C++
(defun my/cc--setup ()
  (c-set-style "bsd")
  (setq-local tab-width 4
              indent-tabs-mode nil
              evil-shift-width 4)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'my/cc--setup)
(add-hook 'c++-mode-hook #'my/cc--setup)

;;;; Common Lisp (SLIME)
(use-package slime
  :hook (lisp-mode . slime-mode)
  :custom
  (inferior-lisp-program "sbcl"))

;;;; CSS
(defun my/css--setup ()
  (setq-local tab-width 2
              indent-tabs-mode nil
              evil-shift-width 2))

(setopt css-indent-offset 2)

(add-hook 'css-mode-hook #'my/css--setup)

;;;; Git modes
(use-package git-modes
  :defer t)

;;;; Go
(use-package go-mode
  :preface
  (defun my/go--setup ()
    (setq-local tab-width 4
                indent-tabs-mode t
                evil-shift-width 4))
  :mode "\\.go\\'"
  :hook (go-mode . my/go--setup))

;;; Hare
(use-package hare-mode
  :preface
  (defun my/hare--setup ()
    (setq-local tab-width 4
                indent-tabs-mode t
                evil-shift-width 4))
  :straight (:type git :repo "https://git.sr.ht/~bbuccianti/hare-mode")
  :mode "\\.ha\\'"
  :config
  (add-hook 'hare-mode-hook #'my/hare--setup))

;;;; HTML
(setopt sgml-basic-offset 2)

(defun my/html--setup ()
  (display-line-numbers-mode 1)
  (setq-local tab-width 2
              indent-tabs-mode nil
              evil-shift-width 2))
(add-hook 'html-mode-hook #'my/html--setup)

;;; Hy
(use-package hy-mode
  :mode "\\.hy\\'"
  :preface
  (defun my/hy--setup ()
    (setq-local tab-width 4
                indent-tabs-mode nil
                evil-shift-width 4)))

;;;; JavaScript
(setopt js-indent-level 4)

(defun my/javascript--setup ()
  (setq-local tab-width js-indent-level
              indent-tabs-mode nil
              evil-shift-width js-indent-level))

(add-hook 'js-mode-hook #'my/javascript--setup)

;;;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :preface
  (defun my/lua--setup ()
    (setq-local tab-width 4
                indent-tabs-mode nil
                evil-shift-width 4))
  :custom
  (lua-indent-level 4)
  :config
  (add-hook 'lua-mode-hook #'my/lua--setup))

;;; Python
(defun my/python--setup ()
  (setq-local tab-width 4
              indent-tabs-mode nil
              evil-shift-width 4))

(add-hook 'python-mode-hook #'my/python--setup)

;;;; Typescript
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :preface
  (defun my/typescript--setup ()
    (setq-local tab-width js-indent-level
                indent-tabs-mode nil
                evil-shift-width js-indent-level))
  :config
  (add-hook 'typescript-mode-hook #'my/typescript--setup))

;;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :preface
  (defun my/yaml--setup ()
    (setq-local tab-width 4
                indent-tabs-mode nil
                evil-shift-width 4))
  :config
  (add-hook 'yaml-mode-hook #'my/yaml--setup))

(provide 'my-config-programming)
