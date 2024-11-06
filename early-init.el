;;; early-init.el -*- lexical-binding: t; -*-

;;; Emacs dirs
(defvar my/lisp-directory (locate-user-emacs-file (convert-standard-filename "lisp/"))
  "The directory where emacs lisp files are loaded.")

(defvar my/var-directory (locate-user-emacs-file (convert-standard-filename "var/"))
  "The directory for persistent package data files.")

(defvar my/etc-directory (locate-user-emacs-file (convert-standard-filename "etc/"))
  "The directory for package configuration files.")

(defun my/expand-lisp-directory (file)
  "Expand filename FILE relative to `my/lisp-directory'."
  (expand-file-name (convert-standard-filename file)
                    my/lisp-directory))

(defun my/expand-var-directory (file)
  "Expand filename FILE relative to `my/var-directory'."
  (expand-file-name (convert-standard-filename file)
                    my/var-directory))

(defun my/expand-etc-directory (file)
  "Expand filename FILE relative to `my/etc-directory'."
  (expand-file-name (convert-standard-filename file)
                    my/etc-directory))

;;; System
(defvar my/tiling-window-manager-regexp "\\|sway\\|hyprland\\|bspwm\\|herbstluftwm\\|i3\\|dwm"
  "Regular expression to tiling window managers. See definition of
  `my/with-desktop-session'.

Credit: `https://protesilaos.com/emacs/dotemacs'.")

(defmacro my/with-desktop-session (&rest body)
  "Expand BODY if desktop session is not a tiling window manager. See
`my/tiling-window-manager-regexp' for what constitutes a matching
tiling window manager.

Credit: `https://protesilaos.com/emacs/dotemacs'."
  (declare (indent 0))
  `(when-let ((eq system-type 'gnu/linux)
              (eq system-type 'gnu)
              (eq system-type 'gnu/kfreebsd)
              (session (getenv "DESKTOP_SESSION"))
              ((not (string-match-p session my/tiling-window-manager-regexp))))
     ,@body))

;;; HACK: Speed up emacs startup
(defvar my/early-init--file-name-handler-alist file-name-handler-alist)
(defvar my/early-init--vc-handled-backends vc-handled-backends)

;; temporary startup settings for improving emacs init speed
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      vc-handled-backends nil)

(defun my/early-init--hook-emacs-startup ()
  "After startup settings for improving emacs init speed."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1
        file-name-handler-alist my/early-init--file-name-handler-alist
        vc-handled-backends my/early-init--vc-handled-backends))
(add-hook 'emacs-startup-hook #'my/early-init--hook-emacs-startup)

;;; Early-init Emacs Settings
(setq package-enable-at-startup nil) ; use straight.el package management
(setq vc-follow-symlinks nil)
(setq inhibit-startup-screen t)
(setq inhibit-x-resources t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-buffer-menu t)
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq use-short-answers t)

;; HACK: Don't show startup message in echo area by overriding the function
(defun display-startup-echo-area-message ()
  (message nil))

;;; User Interface
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;;;; Fringe padding
(push '(internal-border-width . 24) default-frame-alist)
(push '(right-divider-width   . 30) default-frame-alist)
(push '(scroll-bar-width      . 8)  default-frame-alist)

(defun my/early-init--set-invisible-window-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg)))
     `(olivetti-fringe ((t :background ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my/early-init--set-invisible-window-dividers)

;;; End of early-init.el
(load (locate-user-emacs-file "post-early-init.el") :noerror :nomessage)
(load (locate-user-emacs-file "machine-early-init.el") :noerror :nomessage) ; ignore in version control
