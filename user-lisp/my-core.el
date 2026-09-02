;;; -*- lexical-binding: t; -*-

(defconst my/enable-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(defvar my/packages-enabled '()
  "Stores a list of manually-installed packages.")

(defmacro my/set (var val)
  `(funcall (or (get ',var 'custom-set) #'set-default) ',var ,val))

(defun my/locate-user-var-file (path)
  (expand-file-name path (locate-user-emacs-file "var")))

(defun my/locate-user-etc-file (path)
  (expand-file-name path (locate-user-emacs-file "etc")))

(defun my/locate-user-lisp-file (path)
  (expand-file-name path user-lisp-directory))

;;; END

(provide 'my-core)
