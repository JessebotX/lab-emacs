;;; early-init.el -*- lexical-binding: t; -*-

;;; HACK: Speed up startup
(defvar my/early-init--file-name-handler-alist file-name-handler-alist)

(defun my/early-init--startup-setup ()
  "Startup settings."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist my/early-init--file-name-handler-alist))
(add-hook 'emacs-startup-hook #'my/early-init--startup-setup)

;;; HACK: Don't show startup message in echo area by overriding the function
(defun display-startup-echo-area-message ()
  (message nil))

;;; Initial Values
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      vc-follow-symlinks nil
      package-enable-at-startup t
      inhibit-startup-message t
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; User interface
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines       . 0)   default-frame-alist)
(push '(tool-bar-lines       . 0)   default-frame-alist)

;;;; Fringe padding faces
(push '(internal-border-width . 24) default-frame-alist)
(push '(right-divider-width   . 30) default-frame-alist)
(push '(scroll-bar-width      . 8)  default-frame-alist)

(defun my/early-init--set-invisible-window-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my/early-init--set-invisible-window-dividers)

;;; Load a theme to avoid initial flash of light
(load-theme 'tango-dark t)
(enable-theme 'tango-dark)

;;; End: load post-early-init.el
(load (locate-user-emacs-file "post-early-init.el") :noerror :nomessage)
;;; End: machine specific configuration
(load (locate-user-emacs-file "machine-early-init.el") :noerror :nomessage)
