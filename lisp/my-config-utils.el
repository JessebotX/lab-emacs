;;; -*- lexical-binding: t; -*-

(defun my/increment-number-at-point ()
  "Increment integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun my/decrement-number-at-point ()
  "Decrement integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(keymap-global-set "C-c =" #'my/increment-number-at-point)
(keymap-global-set "C-c -" #'my/decrement-number-at-point)

(provide 'my-config-utils)
