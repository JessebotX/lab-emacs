;;; -*- lexical-binding: t; -*-

(deftheme custom
  "My custom color theme")

(let* ((bg "#ffffff")
       (fg "#111111"))
  (custom-theme-set-faces
   'custom

   `(default ((t (:foreground ,fg :background ,bg))))

   ))

(provide-theme 'custom)
