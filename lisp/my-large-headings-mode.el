;;; -*- lexical-binding: t; -*-

(defun my/large-headings-mode--set-faces (_theme)
  (custom-set-faces
   `(outline-1 ((t :height 2.2)))
   `(outline-2 ((t :height 1.6)))
   `(outline-3 ((t :height 1.2)))
   `(outline-4 ((t :height 1.0)))

   `(modus-themes-heading-1 ((t :height 2.2)))
   `(modus-themes-heading-2 ((t :height 1.6)))
   `(modus-themes-heading-3 ((t :height 1.2)))
   `(modus-themes-heading-4 ((t :height 1.0)))
   )
  )

;;;###autoload
(define-minor-mode my/large-headings-mode
  "Increase font size (face height) of headings in `org-mode' and
`markdown-mode'."
  :global t
  (if my/large-headings-mode
      (my/large-headings-mode--activate)
    (my/large-headings-mode--deactivate)))

(defun my/large-headings-mode--activate ()
  (my/large-headings-mode--set-faces nil)
  (add-hook 'enable-theme-functions #'my/large-headings-mode--set-faces))

(defun my/large-headings-mode--deactivate ()
  (custom-set-faces
   `(outline-1 (()))
   `(outline-2 (()))
   `(outline-3 (()))
   `(outline-4 (()))

   `(modus-themes-heading-1 (()))
   `(modus-themes-heading-2 (()))
   `(modus-themes-heading-3 (()))
   `(modus-themes-heading-4 (()))
   )

  (remove-hook #'enable-theme-functions #'my/large-headings-mode--set-faces))

;;; End
(provide 'my-large-headings-mode)
