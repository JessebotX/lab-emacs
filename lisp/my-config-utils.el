;;; my-config-utils.el -*- lexical-binding: t; -*-

(let* ((package-path (locate-user-emacs-file "lisp/packages/disable-mouse"))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (autoload #'disable-mouse-mode "disable-mouse" nil t)
    (autoload #'disable-mouse-global-mode "disable-mouse" nil t)))

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when the
minibuffer is open.  Whereas we want it to close the minibuffer, even
without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'.

Credit: ripped from
https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
"
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; Found in: https://github.com/LionyxML/emacs-solo/
;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.

(defun my/diff-current-to-saved-file ()
  "Show diff between the current unsaved buffer/file contents and the saved
buffer/file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(defun my/open-terminal ()
  "Open the current dir in a new terminal window.

URL `http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html'
Version: 2020-11-21 2022-08-04 2023-03-01 2023-06-26"
  (interactive)
  (let ((shell-dir (shell-quote-argument (expand-file-name default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command (format "wt -d \"%s\"" default-directory)))
     ((eq system-type 'darwin)
      (shell-command (concat "open -a terminal " shell-dir)))
     ((eq system-type 'gnu/linux)
      (call-process "setsid" nil 0 nil "x-terminal-emulator" (concat "--working-directory=" shell-dir)))
     ((eq system-type 'berkeley-unix)
      (call-process "setsid" nil 0 nil "x-terminal-emulator" (concat "--working-directory=" shell-dir))))))

(defun my/open-file ()
  "Open current buffer/file in external app.

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
                    (format "xdg-open '%s'" (expand-file-name path)))))))

(defun my/open-current-directory ()
  "Open the current directory"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command
     (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -R " (shell-quote-argument (expand-file-name default-directory)))))
   (t
    (call-process shell-file-name nil 0 nil
                  shell-command-switch
                  (format "xdg-open '%s'" (expand-file-name default-directory))))))

;;; ├─ END

(provide 'my-config-utils)
