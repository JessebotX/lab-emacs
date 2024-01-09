;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; A set of good defaults for Emacs

;;; Ensure UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; Variables
(setq auto-window-vscroll nil)

(setq-default tab-width 4
              indent-tabs-mode nil)

(setopt backward-delete-char-untabify-method 'hungry
        ad-redefinition-action 'accept
        create-lockfiles nil
        custom-file (no-littering-expand-etc-file-name "custom.el")
        display-line-numbers-type 'relative
        display-line-numbers-width 3
        fast-but-imprecise-scrolling t
        global-auto-revert-non-file-buffers t
        isearch-lazy-count t
        lazy-count-prefix-format nil
        lazy-count-suffix-format "   (%s/%s)"
        next-error-message-highlight t
        require-final-newline t
        ring-bell-function #'ignore
        scroll-conservatively 101
        scroll-preserve-screen-position t
        uniquify-buffer-name-style 'forward
        uniquify-ignore-buffers-re "^\\*"
        uniquify-separator "/"
        use-short-answers t
        visible-bell nil
        whitespace-style '(face tabs tab-mark trailing)
        whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9]))
        )

;;; Load custom file
(load custom-file 'noerror 'nomessage)

;;; Modes
(blink-cursor-mode -1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(global-so-long-mode 1)
(save-place-mode 1)
(winner-mode 1)

(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Keybindings
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

;; When we split open a new window, we usually want to jump to the new window.
(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

;;; End
(provide 'my-config-defaults)
