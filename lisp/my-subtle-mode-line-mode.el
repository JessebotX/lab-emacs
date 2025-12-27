;;; my-subtle-mode-line-mode.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Make mode line colors more subtle.
;;
;;; Code:

(defun my/subtle-mode-line-mode--set-faces (_theme)
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box unspecified)))
     `(mode-line-inactive ((t :background unspecified :box unspecified :overline ,subtle :foreground ,subtle))))))

(defun my/subtle-mode-line-mode--activate ()
  (my/subtle-mode-line-mode--set-faces nil)

  (add-hook 'enable-theme-functions #'my/subtle-mode-line-mode--set-faces))

(defun my/subtle-mode-line-mode--deactivate ()
  (custom-set-faces
   '(mode-line (()))
   '(mode-line-active (()))
   '(mode-line-inactive (())))

  (remove-hook 'enable-theme-functions #'my/subtle-mode-line-mode--set-faces))

(defvar my/subtle-mode-line-mode nil)
;;;###autoload
(define-minor-mode my/subtle-mode-line-mode
  "Make mode-line colors more subtle."
  (if my/subtle-mode-line-mode
      (my/subtle-mode-line-mode--activate)
    (my/subtle-mode-line-mode--deactivate)))

;; END
(provide 'my-subtle-mode-line-mode)
