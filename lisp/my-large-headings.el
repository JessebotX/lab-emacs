;;; -*- lexical-binding: t; -*-

(defun my/large-headings--set-faces (_theme)
  (custom-set-faces
   `(org-level-1 ((t :height 1.8)))
   `(org-level-2 ((t :height 1.5)))
   `(org-level-3 ((t :height 1.2)))
   `(org-level-4 ((t :height 1.0)))
   `(org-level-5 ((t)))
   `(org-level-6 ((t)))

   `(markdown-header-face-1 ((t :height 1.8)))
   `(markdown-header-face-2 ((t :height 1.5)))
   `(markdown-header-face-3 ((t :height 1.2)))
   `(markdown-header-face-4 ((t :height 1.0)))
   `(markdown-header-face-5 ((t)))
   `(markdown-header-face-6 ((t)))))

;;;###autoload
(define-minor-mode my/large-headings-mode
  "Increase font size (face height) of headers in `org-mode' and
`markdown-mode'."
  :global t
  (if my/large-headings-mode
      (my/large-headings-mode--activate)
    (my/large-headings-mode--deactivate)))

(defun my/large-headings-mode--activate ()
  (my/large-headings--set-faces nil)
  (add-hook 'enable-theme-functions #'my/large-headings--set-faces))

(defun my/large-headings-mode--deactivate ()
  (custom-set-faces
   `(org-level-1 (()))
   `(org-level-2 (()))
   `(org-level-3 (()))
   `(org-level-4 (()))
   `(org-level-5 (()))
   `(org-level-6 (()))

   `(markdown-header-face-1 (()))
   `(markdown-header-face-2 (()))
   `(markdown-header-face-3 (()))
   `(markdown-header-face-4 (()))
   `(markdown-header-face-5 (()))
   `(markdown-header-face-6 (())))

  (remove-hook #'enable-theme-functions #'my/large-headings--set-faces))

;;; End
(provide 'end-large-headings)
