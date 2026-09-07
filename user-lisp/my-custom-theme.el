;;; -*- lexical-binding: t; -*-

(deftheme my-custom
  "My custom color theme")

(let* ((bg "#f3f5f1")
       (shadow-0.25 "#bababa")
       (shadow-0.5 "#bbbbbb")
       (shadow "#cdcdcd")
       (shadow-2 "#dedede")
       (fg "#111111")
       (fg-2 "#555555")
       (fg-3 "#565656")
       (fg-link "#2962ff")
       (comment "#757575"))
  (custom-theme-set-faces
   'my-custom

   `(default ((t (:foreground ,fg :background ,bg))))
   `(bold ((t (:weight bold))))
   `(italic ((t (:slant italic))))
   `(shadow ((t (:foreground ,shadow))))

   `(mode-line ((t (:foreground ,fg :background ,bg :overline ,shadow))))
   `(mode-line-inactive ((t (:foreground ,shadow :overline ,shadow-2))))

   `(font-lock-function-name-face ((t (:foreground ,fg))))
   ;; `(font-lock-punctuation-face ((t (:foreground ,shadow))))
   `(font-lock-delimiter-face ((t (:foreground ,fg-2))))
   `(font-lock-bracket-face ((t (:foreground ,fg-3))))
   `(font-lock-property-use-face ((t (:foreground ,fg))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:weight bold :foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg-2))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic :underline t))))
   `(font-lock-doc-face ((t (:background ,shadow-2 :foreground ,fg))))

   `(link ((t (:underline nil :foreground ,fg-link :weight bold))))

   `(markdown-ts-list-marker ((t (:weight bold :foreground ,fg))))
   `(markdown-ts-heading-1 ((t (:weight bold))))
   `(markdown-ts-heading-2 ((t (:weight bold))))
   `(markdown-ts-heading-3 ((t (:weight bold))))
   `(markdown-ts-heading-4 ((t (:weight bold))))
   `(markdown-ts-heading-5 ((t (:weight bold))))
   `(markdown-ts-heading-6 ((t (:weight bold))))
   `(markdown-ts-block-quote ((t (:foreground ,fg :slant italic))))
   `(markdown-ts-delimiter ((t (:weight normal :foreground ,shadow))))

   ))

(provide-theme 'my-custom)
