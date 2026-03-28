;;; my-config-mode-line.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(setq project-mode-line t
      mode-line-percent-position nil
      mode-line-position-line-format '("(%l:)")
      mode-line-position-column-format '("(:%c)")
      mode-line-position-column-line-format '("(%l:%c)"))

;;; ├─ CUSTOM MODE-LINE FORMAT

(defvar-local my/mode-line--segment-buffer-name
    '(:eval
      (propertize "%b" 'face 'mode-line-buffer-id))
  "Display name of buffer.")
(put 'my/mode-line--segment-buffer-name 'risky-local-variable t)

(defvar-local my/mode-line--segment-buffer-status
    '(:eval
      (let ((indicator (cond
                        ((and (buffer-modified-p) (buffer-narrowed-p)) ">*< ")
                        ((and (not (buffer-modified-p)) (buffer-narrowed-p)) "><  ")
                        ((and (buffer-modified-p) (not (buffer-narrowed-p))) "* ")
                        (t "§ "))))
        (propertize indicator 'face 'shadow))))
(put 'my/mode-line--segment-buffer-status 'risky-local-variable t)

(defvar-local my/mode-line--segment-major-mode
    '(:eval mode-name)
  "Display current buffer's major mode.")
(put 'my/mode-line--segment-major-mode 'risky-local-variable t)

(defvar-local my/mode-line--segment-major-mode-indicator
    '(:eval (propertize "λ" 'face 'shadow)))
(put 'my/mode-line--segment-major-mode-indicator 'risky-local-variable t)

(defvar-local my/mode-line--segment-eol
  '(:eval
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
                    map))))
  "Display line ending type.

Credit: doom-modeline
<https://github.com/seagle0128/doom-modeline>")
(put 'my/mode-line--segment-eol 'risky-local-variable t)

(defvar-local my/mode-line--segment-encoding
  '(:eval
    (let* ((sys (coding-system-plist buffer-file-coding-system))
           (cat (plist-get sys :category))
           (encoding-symbol (if (memq cat
                                      '(coding-category-undecided coding-category-utf-8))
                                'utf-8
                              (plist-get sys :name))))
      (propertize (upcase (symbol-name encoding-symbol)) 'face 'shadow)))
  "Display the coding system of the current buffer.

Credit: doom-modeline
<https://github.com/seagle0128/doom-modeline>")
(put 'my/mode-line--segment-encoding 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" " "
                my/mode-line--segment-buffer-status
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
                (:eval mode-line-misc-info)))

;;; ├─ TOGGLE MODE-LINE

(autoload #'my/toggle-mode-line-mode "my-toggle-mode-line-mode" nil t)
(autoload #'my/hide-mode-line "my-toggle-mode-line-mode" nil t)
(autoload #'my/show-mode-line "my-toggle-mode-line-mode" nil t)

;;; ├─ FACES

(defun my/subtle-mode-line-set-faces (_theme)
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box unspecified)))
     `(mode-line-inactive ((t :background unspecified :box unspecified :overline ,subtle :foreground ,subtle))))))

;;; ├─ END

(provide 'my-config-mode-line)
