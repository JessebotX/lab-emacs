;;; -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defcustom my/editor-lang-indents
  '((c          :size 3 :use-tabs nil)
    (cmake      :size 3 :use-tabs nil)
    (cpp        :size 3 :use-tabs nil)
    (css        :size 1 :use-tabs nil)
    (go         :size 3 :use-tabs t)
    (html       :size 1 :use-tabs nil)
    (javascript :size 3 :use-tabs nil)
    (lisp       :size 8 :use-tabs nil)
    (markdown   :size 2 :use-tabs nil)
    (make       :size 3 :use-tabs t)
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

;;; ├── EXTRAS

(let* ((package-path (expand-file-name "lisp/packages/adaptive-wrap" user-emacs-directory))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (autoload #'adaptive-wrap-prefix-mode "adaptive-wrap" nil t)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)))

(let* ((package-path (expand-file-name "lisp/packages/olivetti" user-emacs-directory))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (autoload #'olivetti-mode "olivetti" nil t)

    (defun my/editor--olivetti-no-newline-in-fringe ()
      "Hack to prevent cursor from going into the fringe."
      (setq-local overflow-newline-into-fringe nil))
    (add-hook 'olivetti-mode-hook #'my/editor--olivetti-no-newline-in-fringe)

    (with-eval-after-load 'olivetti
      (let ((map olivetti-mode-map))
        (define-key map [left-margin mouse-1] nil)
        (define-key map [right-margin mouse-1] nil)
        (define-key map [left-fringe mouse-1] nil)
        (define-key map [right-fringe mouse-1] nil)
        ;; This code is taken from https://github.com/joostkremers/visual-fill-column
        (when (and (bound-and-true-p mouse-wheel-mode)
                   (boundp 'mouse-wheel-down-event)
                   (boundp 'mouse-wheel-up-event))
          (define-key map (vector 'left-margin 'mouse-wheel-down-event) nil)
          (define-key map (vector 'left-margin 'mouse-wheel-up-event) nil)
          (define-key map (vector 'right-margin 'mouse-wheel-down-event) nil)
          (define-key map (vector 'right-margin 'mouse-wheel-up-event) nil))))

    ;;;###autoload
    (define-minor-mode my/editor-writeroom-mode
      "Minor mode that toggles a nice writing environment."
      :init-value nil
      (if my/editor-writeroom-mode
          (progn
            (whitespace-mode -1)
            (my/hide-mode-line)
            (olivetti-mode 1))
        (progn
          (whitespace-mode 1)
          (my/show-mode-line)
          (olivetti-mode -1))))
    (make-variable-buffer-local 'my/editor-writeroom-mode)))

(let* ((package-path (locate-user-emacs-file "lisp/packages/multiple-cursors"))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (setq mc/list-file (locate-user-emacs-file "var/mc-lists.el"))
    (add-hook 'after-init-hook
              (defun my/editor--multiple-cursors-setup ()
                (require 'multiple-cursors)))))

;;; ├── LANGUAGE: C

(defun my/editor--lang-c ()
  (my/editor-lang-set-indent-local 'c)
  (setq-local compile-command "make "
              c-ts-mode-indent-style 'bsd
              c-ts-mode-indent-offset (my/editor-lang-indent-size 'c)))

(defun my/editor--c-ts-mode ()
  (c-ts-mode-set-style 'bsd))

(when (treesit-language-available-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-hook 'c-ts-mode-hook #'my/editor--lang-c)
  (add-hook 'c-ts-mode-hook #'my/editor--c-ts-mode))

;;; ├── LANGUAGE: CMAKE

(let* ((package-path (expand-file-name "lisp/packages/cmake-mode" user-emacs-directory))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (autoload #'cmake-mode "cmake-mode" nil t)
    (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))))

(defun my/editor--lang-cmake ()
  (setq-local cmake-tab-width (my/editor-lang-indent-size 'cmake))
  (my/editor-lang-set-indent-local 'cmake))
(add-hook 'cmake-mode-hook #'my/editor--lang-cmake)

;;; ├── LANGUAGE: CPP

(defun my/editor--lang-cpp ()
  (my/editor-lang-set-indent-local 'cpp)
  (setq-local compile-command "cmake --build out"
              c-ts-mode-indent-style 'bsd
              c-ts-mode-indent-offset (my/editor-lang-indent-size 'cpp)))

(defun my/editor--cpp-ts-mode ()
  (c-ts-mode-set-style 'bsd))

(add-to-list 'treesit-load-name-override-list '(c++ "libtree-sitter-cpp" "tree_sitter_cpp"))
(when (treesit-language-available-p 'c++)
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-hook 'c++-ts-mode-hook #'my/editor--lang-cpp)
  (add-hook 'c++-ts-mode-hook #'my/editor--cpp-ts-mode))

;;; ├── LANGUAGE: CSS

(defun my/editor--lang-css ()
  (my/editor-lang-set-indent-local 'css)
  (setq-local css-indent-offset (my/editor-lang-indent-size 'css)))

(if (treesit-language-available-p 'css)
    (progn
      (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
      (add-hook 'css-ts-mode-hook #'my/editor--lang-css))
  (progn
    (add-hook 'css-mode-hook #'my/editor--lang-css)))

;;; ├── LANGUAGE: GO

(defun my/editor--lang-go ()
  (my/editor-lang-set-indent-local 'go)
  (setq-local compile-command "go build "
              go-ts-mode-indent-offset (my/editor-lang-indent-size 'go)))

(when (treesit-language-available-p 'gomod)
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
  (add-hook 'go-ts-mode-hook #'my/editor--lang-go))

(when (treesit-language-available-p 'go)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-hook 'go-ts-mode-hook #'my/editor--lang-go))

;;; ├── LANGUAGE: HTML

(defun my/editor--lang-html ()
  (my/editor-lang-set-indent-local 'html)
  (setq-local html-ts-indent-offset (my/editor-lang-indent-size 'html))
  (setq-local html-ts-js-css-indent-offset (my/editor-lang-indent-size 'html))
  (setq-local mhtml-ts-js-css-indent-offset (my/editor-lang-indent-size 'html))
  (setq-local sgml-basic-offset (my/editor-lang-indent-size 'html))
  (setq-local js-indent-level (my/editor-lang-indent-size 'js))
  (setq-local css-indent-offset (my/editor-lang-indent-size 'css)))

(if (treesit-language-available-p 'html)
    (progn
      (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
      (unless (version< emacs-version "31.0")
        (add-to-list 'major-mode-remap-alist '(mhtml-mode . mhtml-ts-mode)))
      (add-hook 'html-ts-mode-hook #'my/editor--lang-html))
  (progn
      (add-hook 'html-mode-hook #'my/editor--lang-html)))

;;; ├── LANGUAGE: LISP

(defun my/editor--lang-lisp ()
  (my/editor-lang-set-indent-local 'lisp)
  (outline-minor-mode 1)
  (electric-indent-local-mode 1)
  (electric-pair-local-mode 1))

(add-hook 'lisp-mode-hook #'my/editor--lang-lisp)
(add-hook 'emacs-lisp-mode-hook #'my/editor--lang-lisp)

;;; ├── LANGUAGE: MARKDOWN

(let* ((package-path (locate-user-emacs-file "lisp/packages/markdown-indent-mode"))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (add-to-list 'load-path package-path)
    (autoload #'markdown-indent-mode "markdown-indent-mode" nil t)))

(defun my/editor--lang-markdown ()
  (my/editor-lang-set-indent-local 'markdown)
  (visual-line-mode 1)
  (setq-local adaptive-fill-regexp "[-–!|#%;>·•‣⁃◦ 	]*"))

(when (and (treesit-language-available-p 'markdown)
           (treesit-language-available-p 'markdown-inline))
  (let* ((package-path (locate-user-emacs-file "lisp/packages/markdown-ts-mode"))
         (package-exists-p (file-directory-p package-path)))
    (when package-exists-p
      (add-to-list 'load-path package-path)
      (autoload #'markdown-ts-mode "markdown-ts-mode" nil t)))

  (add-hook 'markdown-ts-mode-hook #'my/editor--lang-markdown)
  (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\)\\'" . markdown-ts-mode)))

;;; ├─ END

(provide 'my-config-editor-languages)
