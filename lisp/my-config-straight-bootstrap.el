;;; -*- lexical-binding: t; -*-

(setopt straight-check-for-modifications nil
        straight-base-dir (locate-user-emacs-file "var"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setopt straight-use-package-by-default t)

;;; End of my-config-straight-bootstrap.el
(provide 'my-config-straight-bootstrap)
