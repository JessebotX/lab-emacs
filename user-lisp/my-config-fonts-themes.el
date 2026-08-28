;;; my-config-fonts-themes.el -*- lexical-binding: t; -*-

(require 'my-core)

(defcustom my/font-family "Maple Mono"
  "Default font family.")

(defcustom my/font-size 140
  "Default font size.")

(defcustom my/theme 'modus-operandi-tinted
  "Default Emacs theme.")

(defcustom my/theme-toggle-options '(modus-operandi-tinted modus-vivendi-tinted)
  "Two Emacs themes to toggle between that are available for
loading (`custom-available-themes').")

(defun my/font-size-set (value)
  "Set the base font size to VALUE (integer)."
  (interactive "nNew font size: ")
  (set-face-attribute 'default (selected-frame) :height value))

(defun my/font-size-decrement ()
  "Decrement base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (- font-size 10)))
    (set-face-attribute 'default (selected-frame) :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-size-increment ()
  "Increment base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (+ font-size 10)))
    (set-face-attribute 'default (selected-frame) :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-family-set (font)
  "Set emacs `default' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'default nil :family font))

(defun my/font-family-variable-pitch-set (font)
  "Set emacs `variable-pitch' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'variable-pitch nil :family font))

(defun my/font-load-my-font ()
  "Set `default' font face using `my/font-family' and `my/font-size'."
  (interactive)
  (set-face-attribute 'default nil :family my/font-family :height my/font-size))

;; 👋 Display emojis 🖥️⌨️🖱️
(defun my/font-load-emoji-fonts ()
  (interactive)
  (set-fontset-font
   t 'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

(defun my/theme-set (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

;; set the theme after init
(defun my/theme-load-my-theme ()
  "Set/reset theme based on the value of `my/theme'."
  (interactive)
  (my/theme-set my/theme))

(defun my/theme-toggle ()
  "Toggle between the two themes stored in `my/theme-toggle-options'"
  (interactive)
  (if (= (length my/theme-toggle-options) 2)
      (let ((theme-1 (car my/theme-toggle-options))
            (theme-2 (car (cdr my/theme-toggle-options))))
        (if (member theme-1 custom-enabled-themes)
            (my/theme-set theme-2)
          (my/theme-set theme-1)))
    (message "Variable `my/theme-toggle-options' must have exactly 2 options.")))

(with-eval-after-load 'modus-themes
  (my/set modus-themes-italic-constructs t)
  (my/set modus-themes-bold-constructs t)
  (my/set modus-themes-common-palette-overrides
          '((fg-line-number-inactive "gray50")
            (fg-line-number-active fg-main)

            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified)

            (bg-mode-line-active bg-main)
            ;; (bg-mode-line-active bg-dim)
            (fg-mode-line-active fg-main)

            (border-mode-line-active bg-dim)
            (border-mode-line-inactive bg-mode-line-inactive)))
  (my/set modus-vivendi-tinted-palette-overrides
          '((bg-main "#0e0e0e")
            (bg-dim "#333333")
            (border "#333333")
            (bg-paren-match "#454545")
            (cursor blue))))

(add-hook 'enable-theme-functions
          (defun my/themes--bold-dired-directory (_theme)
            (with-eval-after-load 'dired
              (set-face-attribute 'dired-directory nil :weight 'bold))))

;;; END

(provide 'my-config-fonts-themes)
