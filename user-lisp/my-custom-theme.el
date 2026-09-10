;;; -*- lexical-binding: t; -*-

(deftheme my-custom "My custom color theme.")

(let* (

       (shadow "#aaaaaa")
       (shadow-2 "#cccccc")
       (shadow-3 "#dcdcdc")
       (shadow-4 "#ddeedd")
       (primary "#304ffe")

       (bg "#f4f2f6")
       (fg "#111111")
       (fg-2 "#555555")
       (fg-3 "#565656")
       (comment "#757575")

       )
  (custom-theme-set-faces
   'my-custom

   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,fg))))
   `(bold ((t (:weight bold))))
   `(italic ((t (:slant italic))))
   `(shadow ((t (:foreground ,shadow))))

   `(mode-line ((t (:foreground ,fg :background ,bg :overline ,shadow))))
   `(mode-line-inactive ((t (:foreground ,shadow :overline ,shadow-4))))

   `(minibuffer-prompt ((t (:weight bold :foreground ,fg))))
   `(minibuffer-depth-indicator ((t (:weight bold :foreground ,shadow))))

   `(font-lock-function-name-face ((t (:foreground ,fg))))
   ;; `(font-lock-punctuation-face ((t (:foreground ,shadow))))
   `(font-lock-delimiter-face ((t (:foreground ,fg-2))))
   `(font-lock-bracket-face ((t (:foreground ,fg-3))))
   `(font-lock-property-name-face ((t (:foreground ,fg))))
   `(font-lock-property-use-face ((t (:foreground ,fg))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:weight bold :foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg-2))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic :underline ,shadow-2))))
   `(font-lock-doc-face ((t (:background ,shadow-3 :foreground ,fg))))

   `(link ((t (:underline nil :foreground ,primary :weight bold))))

   `(icomplete-selected-match ((t (:background ,shadow-3 :foreground ,fg))))

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
