;;; -*- lexical-binding: t; -*-

(unless (eq system-type 'windows-nt)
  (use-package nerd-icons-dired
    :hook (dired-mode . nerd-icons-dired-mode)))

(use-package image
  :straight nil
  :custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  (image-use-external-converter t))

(use-package dired
  :straight nil
  :custom
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-agho")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  )

(use-package dashboard
  :custom
  (dashboard-items '((agenda . 6)
                     (recents . 2)))
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

(unless (eq system-type 'windows-nt)
  (use-package elfeed
    :commands (elfeed elfeed-update)
    :custom
    (elfeed-feeds '(("https://memorylane.cerelia.xyz/warp/rss.xml" my writing)
                    ("https://www.mollymovieclub.com/feed" podcast)
                    )
                  )))

(unless (eq system-type 'windows-nt)
  (use-package eat
    :straight
    (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))))

(use-package magit
  :commands (magit magit-status))

(provide 'my-config-app)
