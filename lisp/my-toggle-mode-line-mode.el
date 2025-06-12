;;; my-toggle-mode-line-mode.el -*- lexical-binding: t; -*-

(defvar my/toggle-mode-line-mode--restore mode-line-format
  "Default mode line format saved when `my/toggle-mode-line-mode' is
disabled.")

(defun my/toggle-mode-line-mode--hide ()
  "Hide the mode line."
  (setq-default mode-line-format nil))

(defun my/toggle-mode-line-mode--show ()
  "Show the mode line that is saved in `my/toggle-mode-line-mode--restore'."
  (setq-default mode-line-format my/toggle-mode-line-mode--restore))

;;;###autoload
(define-minor-mode my/toggle-mode-line-mode
  "Toggle visibility of the mode line."
  :global t
  (if my/toggle-mode-line-mode
      (my/toggle-mode-line-mode--hide)
    (my/toggle-mode-line-mode--show)))

;; End
(provide 'my-toggle-mode-line-mode)
