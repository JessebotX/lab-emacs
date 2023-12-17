;;; -*- lexical-binding: t; -*-

;;; Increment/decrement numbers
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

;;; Copying/linking buffers
(defun my/buffer-copy-base-file-name ()
  "Copy the current buffer's file name"
  (interactive)
  (if (buffer-file-name)
      (kill-new
       (concat
        (file-name-base buffer-file-name)
        "."
        (file-name-extension buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

(defun my/buffer-insert-relative-link-to-file (filepath &optional useless)
  "Get a relative link from the file in the current buffer to FILEPATH.

USELESS is not used."
  (interactive (find-file-read-args "Link to file: " (confirm-nonexistent-file-or-buffer)))
  (if (buffer-file-name)
      (insert
       (file-relative-name filepath (file-name-directory buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

(provide 'my-config-utils)
