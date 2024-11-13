;;; -*- lexical-binding: t; -*-

;;; Preface

(defvar my/font-size-default 140)
(defvar my/package-etc-directory (locate-user-emacs-file "etc"))
(defvar my/package-var-directory (locate-user-emacs-file "var"))

;;;; Commands

(defvar my/define-leader-key-prefix "C-c")
(defun my/define-leader-key (key action)
  (keymap-global-set (concat my/define-leader-key-prefix " " key) action))

(defun my/indent-with-spaces (width)
  "Set the local buffer's line indent size to WIDTH and insert as
spaces.
The value of WIDTH should be a positive integer."
  (interactive "nIndent space width: ")
  (setq-local evil-shift-width width
              tab-width width
              indent-tabs-mode nil))

(defun my/indent-with-tabs (width)
  "Set the local buffer's line indent size to be WIDTH and insert as
a tab character.
The value of WIDTH should be a positive integer."
  (interactive "nIndent tab width: ")
  (setq-local evil-shift-width width
              tab-width width
              indent-tabs-mode t))

(defun my/set-theme (theme)
  "Set emacs current color theme to THEME.

THEME is a string matching its symbol name.
e.g. \"tango-dark\" => 'tango-dark"
  (interactive (list (completing-read "Set theme: " (custom-available-themes))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme (intern theme) t)
  (enable-theme (intern theme)))

;;;; Load path

(dolist (dir '("lisp/" "modules/"))
  (add-to-list 'load-path (locate-user-emacs-file dir)))

;;; Package Manager

(require 'my-setup-elpaca-package-manager)
;(add-to-list 'warning-suppress-log-types '(elpaca installer))
;(add-to-list 'warning-suppress-types '(elpaca installer))

;;; No Littering
;; Keep folders clean from random emacs files

(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-etc-directory my/package-etc-directory)
  (setq no-littering-var-directory my/package-var-directory)
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

  (no-littering-theme-backups))

;;; Base Emacs

(use-package emacs
  :ensure nil
  :init
  ;; Load saved customizations file (after init using elpaca-after-init-hook)
  (setq custom-file (expand-file-name "custom.el" my/package-etc-directory))
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
  :custom
  (ad-redefinition-action 'accept)
  (backward-delete-char-untabify-method 'hungry)
  (create-lockfiles nil)
  (display-time-default-load-average nil)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (fast-but-imprecise-scrolling t)
  (global-auto-revert-non-file-buffers t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "   (%s/%s)")
  (make-backup-files nil)
  (ring-bell-function #'ignore)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")
  (use-short-answers t)
  (visible-bell nil)
  (whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9])))
  (whitespace-style '(face tabs tab-mark trailing))
  (word-wrap nil)
  :config
;;;; Coding system
  (set-default-coding-systems 'utf-8)

;;;; Other variables

  (setq auto-window-vscroll nil)

  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default compile-command "make")

;;;; Default modes

  (blink-cursor-mode -1)
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (global-so-long-mode 1)
  ;(save-place-mode 1)
  (winner-mode 1)
  (recentf-mode 1)

;;;; Base keybindings

  (global-set-key [remap list-buffers] 'ibuffer)
  (keymap-global-set "<escape>" 'keyboard-escape-quit)
  (keymap-global-set "C--" 'text-scale-decrease)
  (keymap-global-set "C-=" 'text-scale-increase)
  (keymap-global-set "C-+" 'text-scale-increase)
  (keymap-global-set "M-]" 'forward-paragraph)
  (keymap-global-set "M-[" 'backward-paragraph)

  ;; Jump to new window on split
  (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
  (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

;;;; Hooks
  (add-hook 'text-mode-hook #'whitespace-mode)

  (defun my/hook--after-init ()
    "Settings after emacs init."
    (my/set-theme "modus-operandi-tinted"))
  ;; use `elpaca-after-init-hook' because we are using `elpaca' package manager
  (add-hook 'elpaca-after-init-hook #'my/hook--after-init)

;;;; Modeline Basics
  (display-time-mode 1)
  (column-number-mode)

  ;; Battery: only display when on a laptop or something
  (require 'battery)
  (when
      (and battery-status-function
           (not (string-match-p "N/A"  (battery-format "%B" (funcall battery-status-function)))))
    (display-battery-mode 1)))

;;; User Interface
;;;; Fonts

(use-package nerd-icons
  :ensure t)

(when (display-graphic-p)
  (use-package fontaine
    :ensure t
    :init
    (defun hook--fontaine-set-preset ()
      (set-fontset-font
       t
       (if (version< emacs-version "28.1")
           '(#x1f300 . #x1fad0)
         'emoji)
       (cond
        ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
        ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
        ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
        ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
        ((member "Symbola" (font-family-list)) "Symbola")))
      ;; Cascadia Code symbols: ●
      (set-fontset-font t '(#x25e6 . #x25e6) "Cascadia Code")
      (set-fontset-font t '(#x25ce . #x25ce) "Cascadia Code")
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")
      (set-fontset-font t '(#x25c9 . #x25c9) "Cascadia Code")
      (set-fontset-font t '(#x25cb . #x25cb) "Cascadia Code")
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code"))
    (add-hook 'fontaine-set-preset-hook #'hook--fontaine-set-preset)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
    :custom
    (fontaine-presets
     `((regular)
       (cascadia-code
        :default-family "Cascadia Code"
        :fixed-pitch-family "Cascadia Code")
       (poppins-variable-pitch
        :variable-pitch-family "Poppins")
       (urbanist-variable-pitch
        :variable-pitch-family "Urbanist")
       (outfit-variable-pitch
        :variable-pitch-family "Outfit")
       (tengwar-cursive-variable-pitch
        :variable-pitch-family "Tengwar Cursive")
       (julia-mono
        :default-family "JuliaMono")
       (jetbrains-mono
        :default-family "JetBrains Mono")
       (ubuntu-mono
        :default-family "Ubuntu Mono"
        :default-height ,(+ my/font-size-default 20)
        :variable-pitch-family "Ubuntu")
       (t
        :default-family "Maple Mono"
        :default-weight regular
        :default-height ,my/font-size-default
        :fixed-pitch-family "JetBrains Mono"
        :fixed-pitch-weight regular
        :variable-pitch-height ,(+ my/font-size-default 30)
        :variable-pitch-family "Spectral")))
    :config
    (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)))

;;;; Themes

(use-package modus-themes
  :ensure t
  :defer t
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides
   '((fg-line-number-inactive "gray50")
     (fg-line-number-active red-cooler)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)
     (border-mode-line-active bg-dim)
     (bg-mode-line-active bg)
     (bg-mode-line-inactive bg)))
  :config
  (keymap-global-set "<f5>" 'modus-themes-toggle))

(use-package adwaita-dark-theme
  :ensure t
  :defer t
  :custom
  (adwaita-dark-theme-pad-mode-line t))

(use-package magit
  :ensure t
  :commands (magit-status))

;;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-modal nil)
  (doom-modeline-enable-word-count t))

;;; Minibuffer / Completion

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-<backspace>") #'my/minibuffer--backward-kill)
  (define-key map (kbd "M-<backspace>") #'my/minibuffer--backward-kill))

(use-package vertico
  :ensure t
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-M-j" . vertico-next-group)
              ("C-M-k" . vertico-previous-group))
  :custom
  (vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :after vertico
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap bookmark-jump] 'consult-bookmark))

(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode))

;; (use-package corfu
;;   :ensure t
;;   :hook (prog-mode . corfu-mode)
;;   :bind
;;   (:map corfu-map ("M-SPC" . corfu-insert-separator))
;;   :custom
;;   (corfu-popupinfo-delay nil)
;;   (corfu-auto t)
;;   (corfu-quit-no-match 'separator))

;;; Other apps

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package denote
  :ensure t
  :init
  (my/define-leader-key "n n" 'denote)
  (my/define-leader-key "n l" 'denote-link)
  (my/define-leader-key "n i" 'denote-link-or-create)
  :custom
  (denote-directory "~/Sync/denote")
  (denote-known-keywords nil)
  (denote-prompts '(title keywords subdirectory))
  (denote-file-type 'org)
  :config
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package dictionary
  :ensure nil
  :init
  (my/define-leader-key "d o" 'dictionary-lookup-definition)
  :commands (dictionary-lookup-definition)
  :custom
  (dictionary-server "dict.org"))

(use-package enlight
  :ensure t
  :init
  (setq-default enlight-buffer-name "*Emacs*")
  :custom
  (enlight-content
   (enlight-menu
    '(("General"
       ("Open File" (call-interactively #'find-file)           ".")
       ("Bookmarks" (call-interactively #'consult-bookmark)    "b")
       ("Recent"    (call-interactively #'consult-recent-file) "r"))
      ("Notebook"
       ("Homepage" (find-file
                    (convert-standard-filename "~/Sync/notebook2/_README/README.md"))
        "n"))
      )))
  :config
  (keymap-set enlight-mode-map "j" 'enlight-menu-forward-item)
  (keymap-set enlight-mode-map "k" 'enlight-menu-backward-item)
  (keymap-set enlight-mode-map "SPC" nil)
  (keymap-set enlight-mode-map "SPC ." 'find-file)
  (keymap-set enlight-mode-map "SPC r r" 'consult-recent-file)
  (keymap-set enlight-mode-map "SPC r m" 'consult-bookmark)
  (keymap-set enlight-mode-map "SPC j" 'execute-extended-command)
  (keymap-set enlight-mode-map "SPC b" 'switch-to-buffer)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'enlight-mode 'emacs))
  (setopt initial-buffer-choice #'enlight))

(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle)
  :bind ("C-'" . imenu-list-smart-toggle)
  :init
  (setq imenu-list-focus-after-activation t))

(use-package pulse
  :ensure nil
  :preface
  (defun my/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  :config
  ;; Default
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     windmove-left
                     windmove-right
                     windmove-down
                     windmove-up
                     forward-paragraph
                     backward-paragraph
                     move-to-window-line-top-bottom
                     recenter-top-bottom
                     other-window))
    (advice-add command :after #'my/pulse-line))

  (with-eval-after-load 'org
    (dolist (command '(org-forward-element
                       org-backward-element
                       org-forward-paragraph
                       org-backward-paragraph
                       org-forward-sentence
                       org-backward-sentence
                       org-forward-heading-same-level
                       org-backward-heading-same-level))
      (advice-add command :after #'my/pulse-line)))

  (with-eval-after-load 'markdown-mode
    (dolist (command '(markdown-forward-block
                       markdown-backward-block
                       markdown-forward-paragraph
                       markdown-backward-paragraph
                       markdown-backward-page
                       markdown-forward-page))
      (advice-add command :after #'my/pulse-line)))

  (with-eval-after-load 'evil
    (dolist (command '(evil-forward-paragraph
                       evil-backward-paragraph
                       evil-scroll-up
                       evil-scroll-down
                       evil-ex-search-next
                       evil-ex-search-previous
                       evil-goto-line))
      (advice-add command :after #'my/pulse-line))))

(use-package which-key
  :ensure t
  :hook (emacs-startup . which-key-mode))

;;; Text Editing / Programming

;;;; Utilities

(use-package dumb-jump
  :ensure t
  :config
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  :custom
  ;; delete the following on backspace: SPC, TAB, ^M, ^L, ^K
  (hungry-delete-chars-to-skip " 	"))

(use-package writeroom-mode
  :ensure t
  :init
  (keymap-global-set "C-c t t" 'writeroom-mode)
  :commands (writeroom-mode global-writeroom-mode)
  :custom
  (writeroom-width 80))

(use-package olivetti
  :ensure t
  :init
  :commands (olivetti-mode)
  :hook ((markdown-mode   . olivetti-mode)
         (org-mode        . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80))

(use-package my-large-headings-mode
  :ensure nil
  :commands (my/large-headings-mode))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (keymap-global-set "M-;" 'evilnc-comment-or-uncomment-lines))

;;;; Markdown mode

(use-package markdown-mode
  :ensure t
  :mode "\\.\\(?:md\\|txt\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :preface
  (defun my/hook--markdown-mode ()
    "Settings for `markdown-mode-hook'"
    (my/indent-with-spaces 2))
  :custom
  (markdown-max-image-size '(512 . 512))
  :config
  (add-hook 'markdown-mode-hook #'my/hook--markdown-mode))

;;;; Org mode

(use-package org
  :ensure nil
  :preface
  (defun my/hook--org-mode ()
    "Settings for `org-mode-hook'"
    (my/indent-with-spaces 8)
    (setq paragraph-start "\\|[    ]*$")
    (setq paragraph-separate "[     ]*$")
    (visual-line-mode 1)
    (display-line-numbers-mode -1)
    (my/large-headings-mode 1))
  :hook (org-mode . my/hook--org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-id-link-to-org-use-id 'use-existing)
  (org-ellipsis " +")
  (org-auto-align-tags nil)
  (org-src-preserve-indentation t)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-link-file-path-type 'relative)
  (org-hide-emphasis-markers t)
  (org-startup-folded 'showall)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WIP(w)"
                        "HOLD(h)"
                        "EDIT(e)"
                        "REDO(r)"
                        "APPOINTMENT(a)"
                        "|" ; separate active and inactive states
                        "ARCHIVED(s)"
                        "DONE(d)"
                        "CANCELLED(w)")))
  (org-agenda-span 5)
  (org-agenda-start-day "+0d")
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  :config
  (add-to-list 'org-export-backends 'md)

  ;; let j,k move around `org-agenda-mode'
  (with-eval-after-load 'evil
    (evil-define-key '(normal emacs) org-agenda-mode-map
      (kbd "j") 'org-agenda-next-item
      (kbd "k") 'org-agenda-previous-item
      (kbd "N") 'org-agenda-goto-date
      (kbd "P") 'org-agenda-capture))

  ;; Commands/functions/macros
  (defun my/org-toggle-markup ()
    "Toggle hiding/showing `org-mode' emphasis markers."
    (interactive)
    (if org-hide-emphasis-markers
        (setopt org-hide-emphasis-markers nil)
      (setopt org-hide-emphasis-markers t))
    (org-mode-restart))

  ;; Binds
  (let ((map org-mode-map))
    (keymap-set map "C-'" nil) ; remove org agenda cycle list
    (keymap-set map "C-," nil) ; remove org agenda cycle list
    (keymap-set map "C-c m i" 'org-id-store-link)
    (keymap-set map "C-c m l" 'org-store-link)
    (keymap-set map "C-c C-x RET" #'my/org-toggle-markup)))

(use-package org-modern
  :ensure t
  :hook ((org-mode . global-org-modern-mode)
         (org-agenda-mode . global-org-modern-mode))
  :custom
  (org-modern-keyword "» ")
  (org-modern-star '("◉" "●" "○" "◈" "◇"))
  (org-modern-block-fringe nil)
  (org-modern-hide-stars t)
  :config
  (defun my/org-modern-hide-star-headings ()
    "Remove all stars from a heading, increase heading sizes and disable `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars t)
    (setopt org-hide-leading-stars t)
    (org-mode-restart)
    (my/large-headings-mode 1)
    (org-indent-mode -1))

  (defun my/org-modern-star-headings ()
    "Enable stars and `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars nil)
    (setopt org-hide-leading-stars nil)
    (org-mode-restart)
    (my/large-headings-mode -1)
    (org-indent-mode 1)))

(use-package org-wc
  :ensure t
  :commands org-wc-display)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package ox-epub
  :ensure t
  :commands (org-export-dispatch))

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)

