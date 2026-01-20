;;; my-mode-line-mode.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; A mode line inspired by simple-modeline and Protesilaos's mode line.
;;
;;; Code:

;;; Segments
(defvar-local my/mode-line--segment-buffer-name
  '(:eval
    (propertize "%b" 'face 'mode-line-buffer-id))
  "Display name of buffer.")

(defvar-local my/mode-line--segment-buffer-narrowed-status
  '(:eval
    (when (buffer-narrowed-p)
      (propertize " ><" 'face 'shadow)))
  "Display symbol when the buffer is narrowed.")

(defvar-local my/mode-line--segment-buffer-status
  '(:eval
    (if (buffer-modified-p)
        (propertize "○" 'face 'shadow)
      (propertize "●" 'face 'shadow)))
  "Display if buffer has been modified or not.")

(defvar-local my/mode-line--segment-major-mode
  '(:eval mode-name)
  "Display current buffer's major mode.")

(defvar-local my/mode-line--segment-major-mode-indicator
  '(:eval
    (when (mode-line-window-selected-p)
      (let ((indicator (cond
                        ((derived-mode-p 'text-mode) "§")
                        ((derived-mode-p 'prog-mode) "λ")
                        ((or
                          (derived-mode-p 'vterm-mode)
                          (derived-mode-p 'term-mode)
                          (derived-mode-p 'shell-mode)
                          (derived-mode-p 'eshell-mode)
                          (derived-mode-p 'comint-mode)) ">_")
                        (t "◦"))))
        (propertize indicator 'face 'shadow))))
  "Display a symbol to represent the type of the current major mode")

(defvar-local my/mode-line--segment-eol
  '(:eval
    (when (mode-line-window-selected-p)
      (let ((eol (coding-system-eol-type buffer-file-coding-system)))
        (propertize
         (pcase eol
           (0 "LF ")
           (1 "CRLF ")
           (2 "CR ")
           (_ ""))
         'face 'shadow
         'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                            (pcase eol
                              (0 "Unix-style LF")
                              (1 "DOS-style CRLF")
                              (2 "Mac-style CR")
                              (_ "Undecided")))
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                      map)))))
  "Display line ending type.

Credit: doom-modeline
<https://github.com/seagle0128/doom-modeline>")

(defvar-local my/mode-line--segment-encoding
  '(:eval
    (when (mode-line-window-selected-p)
      (let* ((sys (coding-system-plist buffer-file-coding-system))
             (cat (plist-get sys :category))
             (encoding-symbol (if (memq cat
                                        '(coding-category-undecided coding-category-utf-8))
                                  'utf-8
                                (plist-get sys :name))))
        (propertize (upcase (symbol-name encoding-symbol)) 'face 'shadow))))
  "Display the coding system of the current buffer.

Credit: doom-modeline
<https://github.com/seagle0128/doom-modeline>")

(dolist (segment '(my/mode-line--segment-major-mode
                   my/mode-line--segment-buffer-name
                   my/mode-line--segment-encoding
                   my/mode-line--segment-eol
                   my/mode-line--segment-buffer-status
                   my/mode-line--segment-buffer-narrowed-status
                   my/mode-line--segment-major-mode-indicator
                   )
                 )
  (put segment 'risky-local-variable t))

;;; Activate/Deactivate
(defvar my/mode-line--default-mode-line mode-line-format)

(defun my/mode-line-mode--activate ()
  "Enable `my/mode-line-mode'"
  (setq-default mode-line-format
                '(" "
                  my/mode-line--segment-buffer-status
                  my/mode-line--segment-buffer-narrowed-status
                  " "
                  my/mode-line--segment-buffer-name
                  " "
                  my/mode-line--segment-major-mode-indicator
                  " "
                  my/mode-line--segment-major-mode
                  " "
                  (:eval (propertize "(" 'face 'shadow))
                  my/mode-line--segment-eol
                  my/mode-line--segment-encoding
                  (:eval (propertize ")" 'face 'shadow))
                  " "
                  (:eval mode-line-misc-info))))

(defun my/mode-line-mode--deactivate ()
  "Disable `my/mode-line-mode'"
  (setq-default mode-line-format my/mode-line--default-mode-line))

;;;###autoload
(define-minor-mode my/mode-line-mode
  "A custom mode line."
  :global t
  (if my/mode-line-mode
      (my/mode-line-mode--activate)
    (my/mode-line-mode--deactivate)))

;;; End
(provide 'my-mode-line)
