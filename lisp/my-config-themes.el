;;; -*- lexical-binding: t; -*-

(use-package modus-themes
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
     (bg-mode-line-inactive bg))))

(use-package adwaita-dark-theme
  :defer t)

(use-package ef-themes
  :defer t
  :custom
  (ef-themes-to-toggle '(ef-bio ef-frost)))

(use-package catppuccin-theme
  :defer t
  :custom
  (catppuccin-flavor 'mocha))

(use-package doom-themes
  :defer t)

(defcustom my/themes-to-toggle '(modus-vivendi modus-operandi)
  "Themes to toggle between.")

(defun my/themes-set-theme (theme)
  "Set emacs current color theme to THEME.

THEME is a string matching its symbol name.
e.g. \"tango-dark\" => 'tango-dark"
  (interactive (list (completing-read "Set theme: " (custom-available-themes))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme (intern theme) t)
  (enable-theme (intern theme)))

(defun my/themes-toggle-themes ()
  "Toggle between the two `my/themes-to-toggle'.

Credit: `modus-themes-toggle'"
  (interactive)
  (let* ((one (car my/themes-to-toggle))
         (two (cadr my/themes-to-toggle)))
    (my/themes-set-theme
     (if (eq (car custom-enabled-themes) one)
         (symbol-name two)
       (symbol-name one)))))

(provide 'my-config-themes)
