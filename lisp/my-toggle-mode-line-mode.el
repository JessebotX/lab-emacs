
;;; Code:

(defvar my/toggle-mode-line-mode--default-format mode-line-format
  "Default value of `mode-line-format'")

;;;###autoload
(define-minor-mode my/toggle-mode-line-mode
  "Toggle visibility of the mode-line."
  :global t
  :group 'mode-line
  (if my/toggle-mode-line-mode
      (setq-default mode-line-format nil)
    (setq-default mode-line-format my/toggle-mode-line-mode--default-format)))

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
