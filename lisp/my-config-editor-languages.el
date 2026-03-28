;;; -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defcustom my/editor-lang-indents
  '((css        :size 2 :use-tabs nil)
    (go         :size 4 :use-tabs t)
    (html       :size 2 :use-tabs nil)
    (lisp       :size 8 :use-tabs nil)
    (markdown   :size 2 :use-tabs nil)
    (make       :size 4 :use-tabs t)
    (rst        :size 2 :use-tabs nil)
    (org        :size 8 :use-tabs nil)
    (typst      :size 2 :use-tabs nil)
    (yaml       :size 2 :use-tabs nil))
  "List of language-specific indentation settings. Access values using the
functions`my/editor-lang-indent-size' and `my/editor-lang-indent-use-tabs'.

Elements of this alist are of the form:

  (LANG-SYMBOL [:size SIZE] [:use-tabs USE-TABS])

where LANG-SYMBOL is a unique key name that represents a language, SIZE
is the width of each indent in columns, and USE-TABS is a boolean where
if non-nil, indentation will use tabs instead of spaces."
  :group 'indent)

(defun my/editor-lang-indent-size (lang)
  "Get the size of indentation, in columns, for LANG, where LANG is a
symbol and a key to the `my/editor-lang-indents' list.

If the key or the size property of the language does not exist, then
return the default indentation size defined in `my/indent-size-default'."
  (let ((val (cdr (assoc lang my/editor-lang-indents))))
    (if (plist-get val :size)
        (plist-get val :size)
      tab-width)))

(defun my/editor-lang-indent-use-tabs (lang)
  "Get whether indentation will use tabs instead of spaces on
indent for LANG, where LANG is a symbol and a key to the
`my/editor-lang-indents' list.

If the key or the use-tabs property of the language does not exist
then return the default use-tabs value defined in
`my/indent-use-tabs-default'."
  (let ((val (cdr (assoc lang my/editor-lang-indents))))
    (plist-get val :use-tabs)))

(defun my/editor-lang-set-indent-local (lang)
  "Set default emacs indent rules based on LANG in local buffer. Note: you
may still need to modify the major-mode specific indent settings."
  (setq-local tab-width (my/editor-lang-indent-size lang))
  (setq-local indent-tabs-mode (my/editor-lang-indent-use-tabs lang)))

(defun my/editor-set-local-indent (size use-tabs)
  "Configure buffer-local indentation settings, where SIZE is the
indentation size in columns, and USE-TABS is a boolean where if non-nil,
tabs will be used instead of spaces."
  (interactive
   (list
    (read-number "Indent size (# of columns): ")
    (y-or-n-p "Use tabs instead of spaces")))
  (setq-local tab-width size
              indent-tabs-mode use-tabs))

(defun my/editor--paragraph-default-local ()
  (interactive)
  (setq-local paragraph-start (default-value 'paragraph-start)
	      paragraph-separate (default-value 'paragraph-separate)))

(defun my/editor-delete-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

;;; ├── LANGUAGE: C

(defun my/editor--lang-c ()
  (my/editor-lang-set-indent-local 'c)
  (setq-local compile-command "make "
              c-ts-mode-indent-style 'bsd
              c-ts-mode-indent-offset (my/editor-lang-indent-size 'c)))

(when (treesit-language-available-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-hook 'c-ts-mode-hook #'my/editor--lang-c))

;;; ├── LANGUAGE: CPP

(defun my/editor--lang-cpp ()
  (my/editor-lang-set-indent-local 'cpp)
  (setq-local compile-command "make "
              c-ts-mode-indent-style 'bsd
              c-ts-mode-indent-offset (my/editor-lang-indent-size 'cpp)))

(add-to-list 'treesit-load-name-override-list '(c++ "libtree-sitter-cpp" "tree_sitter_cpp"))
(when (treesit-language-available-p 'c++)
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-hook 'c++-ts-mode-hook #'my/editor--lang-cpp))


;;; ├── LANGUAGE: LISP

(defun my/editor--lang-lisp ()
  (my/editor-lang-set-indent-local 'lisp)
  (electric-indent-local-mode 1)
  (electric-pair-local-mode 1))

(add-hook 'lisp-mode-hook #'my/editor--lang-lisp)
(add-hook 'emacs-lisp-mode-hook #'my/editor--lang-lisp)

;;; ├── LANGUAGE: MARKDOWN

(defun my/editor--lang-markdown ()
  (my/editor-lang-set-indent-local 'markdown)
  (visual-line-mode 1))

(when (and (treesit-language-available-p 'markdown)
           (treesit-language-available-p 'markdown-inline))
    (progn
      ;; TODO: Support emacs-version< 31
      ;; (when (version< emacs-version "31.0")
      ;;   (autoload #'markdown-ts-mode "markdown-ts-mode" nil t))
      (add-hook 'markdown-ts-mode-hook #'my/editor--lang-markdown)
      (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\)\\'" . markdown-ts-mode))))

;;; ├─ END

(provide 'my-config-editor-languages)
