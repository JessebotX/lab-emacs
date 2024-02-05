;;; -*- lexical-binding: t; -*-

(require 'my-config-keybindings)

(defcustom my/default-font-size 140
  "Default font size (face height)")

(use-package nerd-icons)

(when (display-graphic-p)
  (use-package fontaine
    :init
    (defun my/set-proper-fontset ()
      (set-fontset-font
       t
       (if (version< emacs-version "28.1")
           '(#x1f300 . #x1fad0)
         'emoji
         )
       (cond
        ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
        ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
        ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
        ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
        ((member "Symbola" (font-family-list)) "Symbola")))
      ;; Cascadia Code symbols: ●
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")
      (set-fontset-font t '(#x25c9 . #x25c9) "Cascadia Code")
      (set-fontset-font t '(#x25cb . #x25cb) "Cascadia Code"))
    (add-hook 'fontaine-set-preset-hook #'my/set-proper-fontset)
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")
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
       (t
        :default-family "Maple Mono"
        :default-weight regular
        :default-height ,my/default-font-size
        :fixed-pitch-family "JetBrains Mono"
        :fixed-pitch-weight regular
        :variable-pitch-height ,(+ my/default-font-size 20)
        :variable-pitch-family "Inter")))
    :config
    (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)))

(defun my/font-reset-size ()
  "Reset the default font size (face height) to `my/default-font-size'"
  (interactive)
  (set-face-attribute 'default nil :height my/default-font-size))

(defun my/font-increase-size (&optional increment)
  "Increase default font size (face height) by INCREMENT. If no
INCREMENT is provided, increase by 10."
  (interactive)
  (if increment
      (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) increment))
    (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10))))

(defun my/font-decrease-size (&optional decrement)
  "Decrease default font size (face height) by DECREMENT. If no
DECREMENT is provided, increase by 10."
  (interactive)
  (if decrement
      (set-face-attribute 'default nil :height (- (face-attribute 'default :height) decrement))
    (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10))))

(defhydra my/font-size-hydra (:timeout 4)
  "Increase/decrease/reset current font size."
  ("=" my/font-increase-size "increase")
  ("+" my/font-increase-size "increase")
  ("-" my/font-decrease-size "decrease")
  ("0" my/font-reset-size "reset"))

(provide 'my-config-fonts)
