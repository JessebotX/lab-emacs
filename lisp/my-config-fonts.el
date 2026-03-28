;;; my-config-fonts.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defcustom my/font-family "Maple Mono"
  "Default font family.")

(defcustom my/font-size 120
  "Default font size.")

(defun my/font-size-set (value)
  "Set the base font size to VALUE (integer)."
  (interactive "nNew font size: ")
  (set-face-attribute 'default nil :height value))

(defun my/font-size-decrement ()
  "Decrement base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (- font-size 10)))
    (set-face-attribute 'default nil :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-size-increment ()
  "Increment base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (+ font-size 10)))
    (set-face-attribute 'default nil :height new-size)
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

;;; ├─ END

(provide 'my-config-fonts)
