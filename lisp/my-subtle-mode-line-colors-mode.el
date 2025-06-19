;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Make mode-line colors more subtle.

;;; Code:

(defun my/subtle-mode-line-colors-mode--set-faces (_theme)
  (let ((subtle (face-foreground 'shadow)))
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
  (if my/subtle-mode-line-colors-mode
      (progn
        (my/subtle-mode-line-colors-mode--set-faces nil)

        (add-hook 'enable-theme-functions #'my/subtle-mode-line-colors-mode--set-faces))
    (progn
      (my/subtle-mode-line-colors-mode--unset-faces)

      (remove-hook 'enable-theme-functions #'my/subtle-mode-line-colors-mode--set-faces))))

(provide 'my-subtle-mode-line-colors-mode)
