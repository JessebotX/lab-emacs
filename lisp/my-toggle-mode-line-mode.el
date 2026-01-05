
;;; Code:

(defvar my/toggle-mode-line-mode--default-format mode-line-format
  "Default value of `mode-line-format'")

;;;###autoload
(define-minor-mode my/toggle-mode-line-mode
  "Toggle visibility of the mode-line."
  :group 'mode-line
  (if my/toggle-mode-line-mode
      (setq-local mode-line-format nil)
    (setq-local mode-line-format my/toggle-mode-line-mode--default-format)))
(make-variable-buffer-local 'my/toggle-mode-line-mode)

;;;###autoload
(defun my/hide-mode-line ()
  (interactive)
  (my/toggle-mode-line-mode 1))

;;;###autoload
(defun my/show-mode-line ()
  (interactive)
  (my/toggle-mode-line-mode -1))

;;; End
(provide 'my-toggle-mode-line-mode)
