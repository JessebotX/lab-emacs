;;; my-mode-line.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom mode-line inspired by
;; https://github.com/gexplorer/simple-modeline and
;; https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial.

;;; Code:

;;; PREFACE

(defgroup my/mode-line nil
  "My custom mode-line."
  :prefix "my/mode-line-"
  :group 'mode-line)

(defcustom my/mode-line-format
  `("%e"
    my/mode-line-buffer-modified
    my/mode-line-buffer-name
    my/mode-line-buffer-narrowed
    (:propertize " λ " face shadow)
    (:eval (when (derived-mode-p 'prog-mode)
             mode-line-position))
    mode-line-format-right-align
    mode-line-misc-info
    my/mode-line-encoding
    my/mode-line-major-mode
    (:propertize "λ" face shadow)
    )
  "My custom mode-line format.")

(defvar my/mode-line--default-mode-line mode-line-format
  "Default Emacs mode-line.")

;;; SEGMENTS

(defvar-local my/mode-line-buffer-narrowed
    '(:eval
      (when (buffer-narrowed-p)
        (propertize "⮷" 'face 'bold))))
(put 'my/mode-line-buffer-narrowed 'risky-local-variable t)

(defvar-local my/mode-line-buffer-modified
    '(:eval
      (propertize
       (cond
        ((and buffer-read-only (buffer-file-name))
         "🔒 ")
        ((string-match-p "\\*.*\\*" (buffer-name))
         "◈ ")
        ((and (buffer-modified-p)
              (not (string-match-p "\\*.*\\*" (buffer-name))))
         "○ ")
        (t
         "● "))
       'face 'font-lock-variable-name-face)))
(put 'my/mode-line-buffer-modified 'risky-local-variable t)

(defvar-local my/mode-line-buffer-name
    '(:eval
      (propertize "%b" 'face 'mode-line-buffer-id)))
(put 'my/mode-line-buffer-name 'risky-local-variable t)

(defvar-local my/mode-line-major-mode
    '(:eval
      (if (or (and (not (derived-mode-p 'prog-mode))
                   (not (derived-mode-p 'text-mode))
                   (not (derived-mode-p 'conf-mode))
                   (not (eq major-mode 'fundamental-mode)))
              (derived-mode-p 'lisp-interaction-mode))
        (list `(:propertize ("" mode-name) face bold) " "))))
(put 'my/mode-line-major-mode 'risky-local-variable t)

(defvar-local my/mode-line-encoding
    '(:eval
      (when (and buffer-file-coding-system
               (or (derived-mode-p 'prog-mode)
                   (derived-mode-p 'text-mode)))
        (propertize
         (format "%s " (upcase (symbol-name buffer-file-coding-system)))
         'face
         'shadow))))
(put 'my/mode-line-encoding 'risky-local-variable t)

;;; DEFINE MODE

(defvar my/mode-line-mode)

;;;###autoload
(define-minor-mode my/mode-line-mode
  "Minor mode to enable a custom mode-line."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'my/mode-line
  :global t
  (if my/mode-line-mode
      (progn
        (setq-default mode-line-format my/mode-line-format))
    (progn
      (setq-default mode-line-format my/mode-line--default-mode-line))))

;;; End
(provide 'my-mode-line)
