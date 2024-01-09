;;; -*- lexical-binding: t; -*-

(defcustom my/terminal "alacritty"
  "Terminal emulator")

(defcustom my/terminal-windows "wt -d"
  "Terminal emulator on Windows systems")

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

(defun my/open-in-terminal ()
  "Open the current directory in a new terminal window

Credit: xahlee.info"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (format "%s \"%s\"" my/terminal-windows (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   (t (start-process
       "" nil my/terminal
       (format "--working-directory \"%s\""
               (expand-file-name default-directory))))))

(defun my/open ()
  "Open file.

Credit: xahlee.info"
  (interactive)
  (let ((path (if (eq major-mode 'dired-mode)
                  (if (eq nil (dired-get-marked-files))
                      default-directory
                    (car (dired-get-marked-files)))
                (if buffer-file-name
                    buffer-file-name
                  default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command
       (format "PowerShell -Command invoke-item '%s'" (expand-file-name path))))
     ((eq system-type 'darwin)
      (shell-command (concat "open -R " (shell-quote-argument path))))
     (t
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "xdg-open %s" (file-name-directory path)))))))

;;; End
(provide 'my-config-utils)

