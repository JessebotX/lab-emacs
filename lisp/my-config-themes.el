;;; my-config-themes.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defcustom my/theme 'modus-operandi-tinted
  "Default Emacs theme.")

(defcustom my/theme-toggle-options '(modus-operandi-tinted modus-vivendi)
  "Two Emacs themes to toggle between that are available for
loading (`custom-available-themes').")

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

;;; ├─ MODUS THEMES

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-common-palette-overrides
      '((fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)

        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)

        ;; (bg-mode-line-active bg-main)
        (bg-mode-line-active bg-dim)
        (fg-mode-line-active fg-main)

        (border-mode-line-active bg-dim)
        (border-mode-line-inactive bg-mode-line-inactive)))

;;; ├─ END

(provide 'my-config-themes)
