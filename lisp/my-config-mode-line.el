;;; -*- lexical-binding: t; -*-

;;; Mode line
(use-package my-mode-line
  :straight nil
  :config
  (my-mode-line-mode 1))

;;; Time
(setopt display-time-default-load-average nil)
(display-time-mode 1)

;;; Battery
(require 'battery)
(when
    (and battery-status-function
         (not (string-match-p "N/A"  (battery-format "%B" (funcall battery-status-function)))))
  (display-battery-mode 1))

(provide 'my-config-mode-line)
