;;; init.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defconst my/auto-save-files-directory (locate-user-emacs-file "var/auto-saves")
  "Directory to store temporary auto-save files.")

;;; ├─ GENERAL CONFIGURATION

(setq-default display-line-numbers-width 4
              display-line-numbers-widen t
              indent-tabs-mode nil
              tab-width 3)

(setq adaptive-fill-regexp "[-–!|#%;>*+·•‣⁃◦ 	]* +"
      ansi-color-for-compilation-mode t
      auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t))
      auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory)
      auto-window-vscroll nil
      backward-delete-char-untabify-method 'hungry
      bookmark-default-file (locate-user-emacs-file "var/bookmarks.el")
      create-lockfiles nil
      compilation-always-kill t
      compilation-scroll-output 'first-error
      completion-ignore-case t
      completions-detailed t
      custom-file (locate-user-emacs-file "etc/emacs-custom.el")
      delete-by-moving-to-trash t
      delete-pair-blink-delay 0
      delete-pair-push-mark t
      dired-listing-switches "-g -h -o -a --group-directories-first"
      dictionary-server "dict.org"
      dictionary-default-strategy "prefix"
      dired-kill-when-opening-new-dired-buffer t
      display-time-default-load-average nil
      doc-view-resolution 200
      global-auto-revert-non-file-buffers t
      history-length 300
      ;; fast-but-imprecise-scrolling t
      ibuffer-human-readable-size t
      isearch-lazy-count t
      jit-lock-defer-time 0
      kill-buffer-delete-auto-save-files t
      kill-do-not-save-duplicates t
      kill-region-dwim (if (version< emacs-version "31") t 'emacs-word)
      lazy-count-prefix-format nil
      lazy-count-suffix-format "   (%s/%s)"
      make-backup-files nil
      multisession-directory (locate-user-emacs-file "var/multisession")
      project-list-file (locate-user-emacs-file "var/projects.el")
      read-answer-short t
      recentf-save-file (locate-user-emacs-file "var/recentf.el")
      redisplay-skip-fontification-on-input t
      ring-bell-function 'ignore
      save-place-file (locate-user-emacs-file "var/places.el")
      save-place-limit 600
      savehist-file (locate-user-emacs-file "var/savehist.el")
      scroll-conservatively 8
      scroll-preserve-screen-position t
      sentence-end-double-space nil
      show-paren-delay 0.1
      show-paren-highlighting-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      transient-history-file (locate-user-emacs-file "var/transient/history.el")
      treesit-extra-load-path (list (locate-user-emacs-file "var/tree-sitter"))
      undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000)
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/"
      use-short-answers t
      visible-bell nil
      which-key-idle-delay 0.1
      whitespace-display-mappings '((tab-mark 9 [#x7C 9] [92 9]))
      whitespace-style '(face tabs tab-mark trailing)
      whitespace-line-column nil)

(put 'narrow-to-region 'disabled nil)

;;; ├─ MODULES

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'my-config-utils)
(keymap-global-set "C-c f f" #'my/switch-frame)
(keymap-global-set "<escape>" #'my/keyboard-quit-dwim)
(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

(require 'my-config-fonts)
(keymap-global-set "C-c C-0" 'my/font-size-set)
(keymap-global-set "C-c C-1" 'my/font-family-set)
(add-hook 'after-init-hook #'my/font-load-my-font)
(add-hook 'after-init-hook #'my/font-load-emoji-fonts)

(require 'my-config-themes)
(keymap-global-set "C-<f5>" #'my/theme-set)
(add-hook 'after-init-hook #'my/theme-load-my-theme)

(require 'my-config-mode-line)
(add-hook 'enable-theme-functions #'my/subtle-mode-line-set-faces)
(add-hook 'after-init-hook #'my/mode-line-display-position-mode)

(require 'my-config-minibuffer)
(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)
(add-hook 'after-init-hook #'icomplete-vertical-mode)

(require 'my-config-editor-languages)
(global-set-key [remap delete-backward-char] #'my/editor-delete-to-tab-stop)
(global-set-key [remap delete-backward-char-untabify] #'my/editor-delete-to-tab-stop)
(keymap-global-set "C-c m t w" #'my/editor-writeroom-mode)
(keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
(keymap-global-set "C-M-n" 'mc/mark-next-like-this)
(keymap-global-set "C-M-p" 'mc/mark-previous-like-this)
(keymap-global-set "C->" 'mc/mark-next-like-this)
(keymap-global-set "C-<" 'mc/mark-previous-like-this)
(keymap-global-set "C-c C-<" 'mc/mark-all-like-this)

;;; ├─ KEYBINDINGS

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-c C-b" nil)

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-s M-s" 'grep)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-c -" 'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" 'just-one-space)
(keymap-global-set "C-c d" 'dictionary-lookup-definition)
(keymap-global-set "C-c m l" 'display-line-numbers-mode)
(keymap-global-set "C-c m w" 'whitespace-mode)

(keymap-global-set "C-c f b" 'bookmark-jump)
(keymap-global-set "C-c f p" 'project-find-file)
(keymap-global-set "<f8>" 'project-compile)
(with-eval-after-load 'recentf
  (keymap-global-set "C-c f r" 'recentf))

;;; ├─ HOOKS

(add-hook 'after-init-hook
          (defun my/emacs-after-init-hook ()
            (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
            (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

            (blink-cursor-mode -1)
            (delete-selection-mode 1)
            (electric-indent-mode -1)
            (global-auto-revert-mode 1)
            (pixel-scroll-precision-mode 1)
            (save-place-mode 1)
            (savehist-mode 1)
            (which-key-mode 1)
            (winner-mode 1)))

(add-hook 'prog-mode-hook #'whitespace-mode)

;;; ├─ END

(load (locate-user-emacs-file "etc/local-init.el") :no-error-if-file-is-missing :nomessage) ; should be ignored in version control
