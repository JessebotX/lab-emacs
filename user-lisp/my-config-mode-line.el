;;; my-config-mode-line.el -*- lexical-binding: t; -*-

(require 'my-core)

(with-eval-after-load 'time
  (my/set display-time-default-load-average nil))

(with-eval-after-load 'project
  (my/set project-mode-line t))

;;; MODE LINE POSITION

(my/set mode-line-percent-position nil)
(my/set mode-line-position-line-format '("(%l:)"))
(my/set mode-line-position-column-format '("(:%c)"))
(my/set mode-line-position-column-line-format '("(%l:%c)"))

;;;###autoload
(define-minor-mode my/mode-line-display-position-mode
  "Toggle displaying local buffer position in the mode line."
  :group 'mode-line
  :global t
  (if my/mode-line-display-position-mode
      (progn
        (line-number-mode 1)
        (column-number-mode 1))
    (progn
      (line-number-mode -1)
      (column-number-mode -1))))

;;; FORMAT

(autoload 'my/mode-line-mode "my-mode-line")

;;; END

(provide 'my-config-mode-line)
