;;; my-config-mode-line.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(setq project-mode-line t
      mode-line-percent-position nil
      mode-line-position-line-format '("(%l:)")
      mode-line-position-column-format '("(:%c)")
      mode-line-position-column-line-format '("(%l:%c)"))

;;; ├─ CUSTOM MODE-LINE FORMAT

(defvar-local my/mode-line--segment-buffer-name
    '(:eval
      (propertize "%b" 'face 'mode-line-buffer-id))
  "Display name of buffer.")
(put 'my/mode-line--segment-buffer-name 'risky-local-variable t)

(defvar-local my/mode-line--segment-buffer-status
    '(:eval
      (let ((indicator (cond
                        ((and (buffer-modified-p) (buffer-narrowed-p)) ">*< ")
                        ((and (not (buffer-modified-p)) (buffer-narrowed-p)) "><  ")
                        ((and (buffer-modified-p) (not (buffer-narrowed-p))) "* ")
                        (t "§ "))))
        (propertize indicator 'face 'shadow))))
(put 'my/mode-line--segment-buffer-status 'risky-local-variable t)

(defvar-local my/mode-line--segment-major-mode
    '(:eval mode-name)
  "Display current buffer's major mode.")
(put 'my/mode-line--segment-major-mode 'risky-local-variable t)

(defvar-local my/mode-line--segment-major-mode-indicator
    '(:eval (propertize "λ" 'face 'shadow)))
(put 'my/mode-line--segment-major-mode-indicator 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" " "
                my/mode-line--segment-buffer-status
                my/mode-line--segment-buffer-name
                " "
                my/mode-line--segment-major-mode-indicator
                " "
                my/mode-line--segment-major-mode
                " "
                (:eval mode-line-misc-info)))

;;; ├─ TOGGLE MODE-LINE

(autoload #'my/toggle-mode-line-mode "my-toggle-mode-line-mode" nil t)
(autoload #'my/hide-mode-line "my-toggle-mode-line-mode" nil t)
(autoload #'my/show-mode-line "my-toggle-mode-line-mode" nil t)

;;; ├─ END

(provide 'my-config-mode-line)
