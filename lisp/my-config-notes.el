;;; -*- lexical-binding: t; -*-

(defcustom my/notes-directory "~/Sync/cerebrum"
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
                "README.md"
                (expand-file-name (int-to-string i) my/notes-directory))))
  (setq-local link (string-replace
                    (directory-file-name (expand-file-name my/notes-directory))
                    ".."
                    file-path))
  (if add-existing-link
      (insert (format "- [%s](%s)" title link)))
  (find-file file-path)
  (insert (format "---\ntitle: \"%s\"\n---\n\n# %s\n\n" title title)))

(defun my/notes-link (path)
  (interactive (list
                (completing-read
                 "File: "
                 (directory-files-recursively
                  my/notes-directory
                  ".*/?[^\\.]*.*"))))
  (setq-local link (string-replace
                    (directory-file-name my/notes-directory)
                    ".."
                    path))
  (setq-local node-num (string-replace "/README.md" ""
                                       (string-replace
                                        (concat (directory-file-name my/notes-directory) "/")
                                        ""
                                        path)))
  (insert
   (format
    "- [Node %s](%s)" node-num link)))

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
