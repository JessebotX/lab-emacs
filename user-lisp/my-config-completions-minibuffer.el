;;; my-config-completions-minibuffer.el -*- lexical-binding: t; -*-

(require 'my-core)

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(my/set completion-ignore-case t)
(my/set completions-detailed t)

;;; END

(provide 'my-config-completions-minibuffer)
