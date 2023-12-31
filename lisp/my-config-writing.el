;;; -*- lexical-binding: t; -*-

;;; Packages
;; Spellcheck only available for unix-like OSes
(when (not (eq system-type 'windows-nt))
  (use-package jinx
    :defer t))

(use-package dictionary
  :straight nil
  :commands (dictionary-lookup-definition)
  :custom
  (dictionary-server "dict.org")
  :config
  (evil-define-key '(normal emacs) dictionary-mode-map
    (kbd "SPC") nil))

(use-package writegood-mode
  :defer t)

(use-package edit-indirect
  :defer t)

(use-package writeroom-mode
  :commands (writeroom-mode global-writeroom-mode)
  :custom
  (writeroom-width 80))

(use-package olivetti
  :commands (olivetti-mode)
  :hook (markdown-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80))

(use-package imenu-list
  :commands (imenu-list-smart-toggle)
  :bind ("C-'" . imenu-list-smart-toggle)
  :init
  (setq imenu-list-focus-after-activation t))

(use-package adaptive-wrap-vp
  :straight (:type git :host github :repo "brentonk/adaptive-wrap-vp")
  :hook (markdown-mode . adaptive-wrap-prefix-vp-mode))

(use-package my-large-headings
  :straight nil
  :commands my/large-headings-mode)

;;; Languages
(use-package markdown-mode
  :preface
  (defun my/markdown--setup ()
    "Settings for `markdown-mode'"
    (setq-local tab-width 2
                indent-tabs-mode nil
                evil-shift-width 2)
    (display-line-numbers-mode -1)
    (visual-line-mode 1))
  :mode "\\.\\(?:md\\|txt\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :custom
  (markdown-italic-underscore t)
  (markdown-list-indent-width 2)
  (markdown-fontify-code-blocks-natively t)
  (markdown-max-image-size '(512 . 512))
  :config
  (add-hook 'markdown-mode-hook #'my/markdown--setup)
  (add-to-list 'markdown-code-lang-modes '("c" . c-mode))
  (add-to-list 'markdown-code-lang-modes '("yml" . yaml-mode))
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
  (add-to-list 'markdown-code-lang-modes '("toml" . conf-toml-mode))
  (add-to-list 'markdown-code-lang-modes '("go" . go-mode)))

(use-package org
  :straight nil
  :preface
  (defun my/org--setup ()
    "Settings for `org-mode-hook'"
    (setq-local tab-width 2
                indent-tabs-mode nil
                evil-shift-width 2)
    (visual-line-mode 1)
    (display-line-numbers-mode -1)
    (org-indent-mode 1)
    )
  :hook (org-mode . my/org--setup)
  :custom
  (org-id-link-to-org-use-id 'use-existing)
  (org-agenda-files '("~/Sync/agenda.org"))
  (org-agenda-file-regexp "\\`[^.].*_project.*\\.org\\'")
  (org-ellipsis "…")
  (org-auto-align-tags nil)
  (org-src-preserve-indentation t)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-link-file-path-type 'relative)
  (org-hide-emphasis-markers t)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WIP(w)"
                        "HOLD(h)"
                        "EDIT(e)"
                        "REDO(r)"
                        "|" ; separate active and inactive states
                        "ARCHIVE(a)"
                        "DONE(d)"
                        "CANCELLED(w)")))
  :config
  (add-to-list 'org-export-backends 'md)

  ;;; Commands/functions/macros
  (defun my/org-toggle-markup ()
    "Toggle hiding/showing `org-mode' emphasis markers."
    (interactive)
    (if org-hide-emphasis-markers
        (setopt org-hide-emphasis-markers nil)
      (setopt org-hide-emphasis-markers t))
    (org-mode-restart))

  ;;; Binds
  (let ((map org-mode-map))
    (define-key map (kbd "C-,") nil) ; remove org agenda cycle list
    (define-key map (kbd "C-c m i") 'org-id-store-link)
    (define-key map (kbd "C-c m l") 'org-store-link)
    (define-key map (kbd "C-c C-x RET") #'my/org-toggle-markup)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode))
  :custom
  (org-modern-star '("◉" "●" "○" "◈" "◇"))
  (org-modern-block-fringe nil)
  (org-modern-hide-stars nil))

(use-package org-wc
  :commands org-wc-display)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package ox-epub
  :commands (org-export-dispatch))

(provide 'my-config-writing)
