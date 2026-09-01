;;; -*- lexical-binding: t; -*-

;;; PREFACE

(defvar my/mode-line--format-default mode-line-format)

;;; SEGMENTS

(defvar-local my/mode-line--segment-buffer-name
    '(:eval
      (propertize "%b" 'face 'mode-line-buffer-id))
  "Display name of buffer.")
(put 'my/mode-line--segment-buffer-name 'risky-local-variable t)

;;; MODE

(defvar my/mode-line-mode)

;;;###autoload
(define-minor-mode my/mode-line-mode
  "Toggle custom mode line."
  :group 'mode-line
  :global t
  (if my/mode-line-mode
      (setq-default mode-line-format
                    '( ;; Left aligned
                      "%e"
                      mode-line-front-space
                      mode-line-mule-info
                      mode-line-client
                      mode-line-modified
                      mode-line-remote
                      mode-line-window-dedicated
                      " "
                      my/mode-line--segment-buffer-name
                      " "
                      mode-line-position

                      ;; Right aligned
                      mode-line-format-right-align
                      (:eval mode-line-misc-info)))
    (setq-default mode-line-format my/mode-line--format-default)))

;;; END

(provide 'my-mode-line)
