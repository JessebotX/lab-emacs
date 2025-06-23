;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Make mode-line colors more subtle.

;;; Code:

(defcustom my/subtle-mode-line-colors-mode-color (face-foreground 'shadow)
  "Face for less important mode-line elements."
  :group 'mode-line
  :type 'string)

(defun my/subtle-mode-line-colors-mode--set-faces (_theme)
  (let ((subtle my/subtle-mode-line-colors-mode-color))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box unspecified)))
     `(mode-line-inactive ((t :background unspecified :box unspecified :overline ,subtle :foreground ,subtle))))))

(defun my/subtle-mode-line-colors-mode--unset-faces ()
  (custom-set-faces
   '(mode-line (()))
   '(mode-line-active (()))
   '(mode-line-inactive (()))))

;;;###autoload
(define-minor-mode my/subtle-mode-line-colors-mode
  "Make mode-line colors more subtle."
  :global t
  :group 'mode-line
  (if my/subtle-mode-line-colors-mode
      (progn
        (my/subtle-mode-line-colors-mode--set-faces nil)

        (add-hook 'enable-theme-functions #'my/subtle-mode-line-colors-mode--set-faces))
    (progn
      (my/subtle-mode-line-colors-mode--unset-faces)

      (remove-hook 'enable-theme-functions #'my/subtle-mode-line-colors-mode--set-faces))))

(provide 'my-subtle-mode-line-colors-mode)
