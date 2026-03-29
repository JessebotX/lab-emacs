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

;;; ├─ ORDERLESS

(let* ((package-path (locate-user-emacs-file "lisp/packages/orderless"))
       (package-exists-p (file-directory-p package-path)))
  (if package-exists-p
      (progn
        (add-to-list 'load-path package-path)
        (add-hook 'minibuffer-setup-hook
                  (lambda ()
                    (require 'orderless)
                    (setq completion-styles '(orderless basic))
                    (setq completion-category-overrides '((file (styles orderless partial-completion))))
                    ;; Emacs 31: partial-completion behaves like substring
                    (setq completion-pcm-leading-wildcard t))))
    (progn
      (setq completion-styles '(flex initials basic))
      (setq completion-category-overrides
            '((file (styles basic partial-completion)))))))

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
