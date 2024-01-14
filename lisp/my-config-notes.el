;;; -*- lexical-binding: t; -*-

(defcustom my/notes-directory "~/Sync/man"
  "My main directory of notes")

(defcustom my/notes-org-directory (expand-file-name "warp" my/notes-directory)
  "Directory for org notes (using denote)")

(defun my/notes-view-directory ()
  "View the path `my/notes-directory' in `dired'."
  (interactive)
  (dired my/notes-directory))

(defun my/notes-new (title)
  "Create a new note in `my/notes-directory'"
  (interactive "MTitle: ")
  (my/notes--create title nil))

(defun my/notes-new-and-link (title)
  "Create a new note in `my/notes-directory'"
  (interactive "MTitle: ")
  (my/notes--create title t))

(defun my/notes--create (title add-existing-link)
  (setq-local i 1)
  (while
      (file-directory-p
       (convert-standard-filename
        (expand-file-name (int-to-string i) my/notes-directory)))
    (setq-local i (1+ i)))
  (setq-local file-path
              (convert-standard-filename
               (expand-file-name
                "index.org"
                (expand-file-name (int-to-string i) my/notes-directory))))
  (setq-local link (string-replace
                    (directory-file-name (expand-file-name my/notes-directory))
                    ".."
                    file-path))
  (if add-existing-link
      (insert (format "+ [[%s][%s (%d)]]" link title i)))
  (find-file file-path)
  (insert (format "#+title: %s\n\n" title)))

(defun my/notes-link (path)
  (interactive (list
                (completing-read
                 "File: "
                 (directory-files-recursively
                  my/notes-directory
                  ".*/?index[^\\.]*.*"))))
  (setq-local link (string-replace
                    (directory-file-name my/notes-directory)
                    ".."
                    path))
  (setq-local node-num (string-replace "/index.org" ""
                                       (string-replace
                                        (concat (directory-file-name my/notes-directory) "/")
                                        ""
                                        path)))
  (insert
   (format
    "+ [[%s][%s (%s)]]" link (my/notes--get-title path) node-num)))

;(my/notes--get-title "~/Sync/man/5/index.org")
(defun my/notes--get-title (path)
  "Retrieve the org document's `title' keyword at PATH."
  (let (title)
    (when path
      (with-temp-buffer
        (insert-file-contents path)
        (org-mode)
        (pcase (org-collect-keywords '("TITLE"))
          (`(("TITLE" . ,val))
           (setq title (car val)))))
      title)))

;;; Denote
;; Used to resolve denote:id org links more rather than actually using it
(use-package denote
  :custom
  (denote-directory my/notes-org-directory
  (denote-keywords nil)
  (denote-prompts '(title keywords))
  (denote-file-type 'org))
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(defun my/notes-consult-grep ()
  (interactive)
  (consult-grep my/notes-directory))

(defun my/notes-consult-ripgrep ()
  (interactive)
  (consult-ripgrep my/notes-directory))

(provide 'my-config-notes)
