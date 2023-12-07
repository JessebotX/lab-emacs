;;; -*- lexical-binding: t; -*-

(defcustom my/default-font-size 140
  "Default font size (face height)")

(use-package nerd-icons)

(when (display-graphic-p)
  (use-package fontaine
    :init
    (defun my/set-proper-fontset ()
      (set-fontset-font t '(#x2500 . #x25ff) "Cascadia Code")
      (set-fontset-font t '(#x2733 . #x2733) "JetBrains Mono"))
    (add-hook 'fontaine-set-preset-hook #'my/set-proper-fontset)
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

(provide 'my-config-fonts)
