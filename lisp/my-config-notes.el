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
  (find-file file-path)
  (insert (format "---\ntitle: \"%s\"\n---\n\n# %s\n\n" title title)))

;;; Denote
;; Used to resolve denote:id org links more rather than actually using it
(use-package denote
  :custom
  (denote-directory my/notes-org-directory
  (denote-keywords nil)
  (denote-prompts '(title keywords))
  (denote-file-type 'org)))

(defun my/notes-consult-grep ()
  (interactive)
  (consult-grep my/notes-directory))

(defun my/notes-consult-ripgrep ()
  (interactive)
  (consult-ripgrep my/notes-directory))

(provide 'my-config-notes)
