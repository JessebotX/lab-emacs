;;; my-config-editor-langs.el -*- lexical-binding: t; -*-

;;; PREFACE

(require 'my-core)

(defcustom my/language-indent-settings
  '((go         :size 3 :use-tabs t)
    (lisp       :size 8 :use-tabs nil)
    (make       :size 3 :use-tabs t)
    (markdown   :size 2 :use-tabs nil)
    (org        :size 8 :use-tabs nil))
  "List of language-specific indentation settings. Access values using the
functions`my/language-indent-size' and `my/language-indent-use-tabs'.

Elements of this alist are of the form:

  (LANG-SYMBOL [:size SIZE] [:use-tabs USE-TABS])

where LANG-SYMBOL is a unique key name that represents a language, SIZE
is the width of each indent in columns, and USE-TABS is a boolean where
if non-nil, indentation will use tabs instead of spaces."
  :group 'indent)

(defun my/language-indent-size (lang)
  "Get the size of indentation, in columns, for LANG, where LANG is a
symbol and a key to the `my/language-indent-settings' list.

If the key or the size property of the language does not exist, then
return the default indentation size defined in `my/indent-size-default'."
  (let ((val (cdr (assoc lang my/language-indent-settings))))
    (if (plist-get val :size)
        (plist-get val :size)
      tab-width)))

(defun my/language-indent-use-tabs (lang)
  "Get whether indentation will use tabs instead of spaces on
indent for LANG, where LANG is a symbol and a key to the
`my/language-indent-settings' list.

If the key or the use-tabs property of the language does not exist
then return the default use-tabs value defined in
`my/indent-use-tabs-default'."
  (let ((val (cdr (assoc lang my/language-indent-settings))))
    (plist-get val :use-tabs)))

(defun my/editor-lang-set-indent-local (lang)
  "Set default emacs indent rules based on LANG in local buffer. Note: you
may still need to modify the major-mode specific indent settings."
  (setq-local tab-width (my/language-indent-size lang))
  (setq-local indent-tabs-mode (my/language-indent-use-tabs lang)))

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

;;; LANGUAGE: C

(defun my/editor--lang-c ()
  (my/editor-lang-set-indent-local 'c)
  (setq-local compile-command "ninja ")
  (setq-local c-ts-mode-indent-style 'bsd)
  (setq-local c-ts-mode-indent-offset (my/language-indent-size 'c)))

(defun my/editor--c-ts-mode ()
  (c-ts-mode-set-style 'bsd))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-hook 'c-ts-mode-hook #'my/editor--lang-c)
(add-hook 'c-ts-mode-hook #'my/editor--c-ts-mode)

;;; LANGUAGE: C++

(add-to-list 'treesit-load-name-override-list '(c++ "libtree-sitter-cpp" "tree_sitter_cpp"))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-hook 'c++-ts-mode-hook
          (defun my/--c++-ts-mode ()
            (setq-local compile-command "ninja ")
            (my/editor-lang-set-indent-local 'cpp)
            (c-ts-mode-set-style 'bsd)
            (setq-local c-ts-mode-indent-style 'bsd)
            (setq-local c-ts-mode-indent-offset (my/language-indent-size 'cpp))))

;;; LANGUAGE: JSON

(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-hook 'json-ts-mode-hook
          (defun my/--json-ts-mode ()
            (my/editor-lang-set-indent-local 'json)))
(with-eval-after-load 'json-ts-mode
  (my/set json-ts-indent-offset (my/language-indent-size 'json)))

;;; LANGUAGE: MARKDOWN

(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(dolist (re '("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'"))
  (add-to-list 'auto-mode-alist (cons re 'markdown-ts-mode)))
(add-hook 'markdown-ts-mode
          (defun my/--markdown-ts-mode ()
            (my/editor-lang-set-indent-local 'markdown)
            (visual-line-mode 1)))

(with-eval-after-load 'markdown-ts-mode
  (require 'markdown-ts-mode-x))

;;; LANGUAGE: LISP

(add-hook 'lisp-mode-hook
          (defun my/--lisp-mode ()
            (my/editor-lang-set-indent-local 'lisp)
            (outline-minor-mode 1)
            (electric-indent-local-mode 1)
            (electric-pair-local-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          (defun my/--emacs-lisp-mode ()
            (my/editor-lang-set-indent-local 'lisp)
            (outline-minor-mode 1)
            (electric-indent-local-mode 1)
            (electric-pair-local-mode 1)))

;;; LANGUAGE: TOML

(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
(add-hook 'json-ts-mode-hook
          (defun my/--json-ts-mode ()
            (my/editor-lang-set-indent-local 'toml)))
(with-eval-after-load 'toml-ts-mode
  (my/set toml-ts-indent-offset (my/language-indent-size 'toml)))

;;; END

(provide 'my-config-editor-langs)
