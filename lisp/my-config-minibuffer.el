;;; my-config-minibuffer -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

;;; ├─ ICOMPLETE

(setq icomplete-show-matches-on-no-input t
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t)

(add-hook 'icomplete-mode-hook
          (lambda ()
            (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
            (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
            (keymap-set icomplete-minibuffer-map "C-M-i" #'minibuffer-complete)))

;;; ├─ END

(provide 'my-config-minibuffer)
