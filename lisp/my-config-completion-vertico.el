;;; -*- lexical-binding: t; -*-

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-<backspace>") #'my/minibuffer--backward-kill)
  (define-key map (kbd "M-<backspace>") #'my/minibuffer--backward-kill))

(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-M-j" . vertico-next-group)
              ("C-M-k" . vertico-previous-group))
  :custom
  (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after vertico
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer))

(use-package marginalia
  :hook (vertico-mode . marginalia-mode))

(use-package corfu
  :hook (prog-mode . corfu-mode)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :custom
  (corfu-popupinfo-delay nil)
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

;;; End
(provide 'my-config-completion-vertico)
