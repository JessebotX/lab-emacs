;;; early-init.el -*- lexical-binding: t; -*-

;; Credits: <https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el>

;;; [VARIABLES]

(defvar my/emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")
(defvar my/gc-cons-threshold (* 32 1024 1024)
  "Value to set `gc-cons-threshold' after startup.")
(defvar my/gc-cons-percentage gc-cons-percentage
  "Value to set `gc-cons-percentage' after startup.")
(defvar my/emacs--file-name-handler-alist file-name-handler-alist
  "Default value to set `file-name-handler-alist' after startup.")
(defvar my/emacs--vc-handled-backends vc-handled-backends
  "Default value to set `vc-handled-backends' after startup.")

;;; [SPEED-UP STARTUP HACKS]
;; Disable some functionality and reset them on startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(setq file-name-handler-alist nil)
(setq load-prefer-newer noninteractive)
(setq vc-handled-backends nil)

(defun my/emacs--restore-early-init-settings ()
  "Restore settings after performing Emacs speedup hacks."
  (setq load-prefer-newer t)
  (setq gc-cons-threshold my/gc-cons-threshold)
  (setq gc-cons-percentage my/gc-cons-percentage)
  (setq file-name-handler-alist my/emacs--file-name-handler-alist)
  (setq vc-handled-backends my/emacs--vc-handled-backends))
(add-hook 'emacs-startup-hook #'my/emacs--restore-early-init-settings 100)

;;; [NATIVE COMP]
;;; Native compilation and byte compilation

;; Activate `native-compile'
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (setq native-comp-deferred-compilation t
            native-comp-jit-compilation t
            package-native-compile t)
      (startup-redirect-eln-cache ; change directory of eln cache
 (convert-standard-filename
 (expand-file-name "var/eln-cache" user-emacs-directory))))
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq byte-compile-warnings my/emacs-debug)
(setq byte-compile-verbose my/emacs-debug)
(setq jka-compr-verbose my/emacs-debug)
(setq native-comp-warning-on-missing-source my/emacs-debug)
(setq native-comp-async-report-warnings-errors (or my/emacs-debug 'silent))
(setq native-comp-verbose (if my/emacs-debug 1 0))

;;; [OTHER SETTINGS]
(setq site-run-file nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-input-method nil)

(setq garbage-collection-messages my/emacs-debug)
(setq package-enable-at-startup nil)
(setq vc-follow-symlinks nil)

(setq gnutls-verify-error t) ; Check certificate issues
(setq tls-checktrust t) ; verify trust of SSL/TLS connections
(setq gnutls-min-prime-bits 3072) ; Stronger GnuTLS encryption

(setq warning-minimum-level (if my/emacs-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

;; Frame settings
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq frame-title-format '("%b"))
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq inhibit-x-resources t)
(setq inhibit-startup-echo-area-message user-login-name)
;; HACK: hide startup message if the inhibit above does not work
(defun display-startup-echo-area-message ()
  (message ""))

;; Disable bidirectional text scanning for a modest performance boost.
;; Give up some bidirectional for slightly faster re-display.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Faster/better buffer and chunk reading
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Dont ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Increase mem usage for better speed
(setq inhibit-compacting-font-caches t)

;;; [USE PACKAGE]

;(setq use-package-compute-statistics t)
;(setq use-package-expand-minimally (not my/emacs-debug))
;(setq use-package-verbose my/emacs-debug)
;(setq use-package-minimum-reported-time (if my/emacs-debug 0 0.1))
;(setq use-package-hook-name-suffix nil)

;;; [USER INTERFACE]

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(font . "Maple Mono 12") default-frame-alist) ; fallback font for frames

;;; [FRAME MARGIN/PADDING]

;; Credit: `spacious-padding' package by Protesilaos
(push '(internal-border-width . 24) default-frame-alist)
(push '(right-divider-width   . 30) default-frame-alist)
(push '(scroll-bar-width      . 8)  default-frame-alist)

(defun my/emacs--set-invisible-window-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg)))
     `(olivetti-fringe ((t :background ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my/emacs--set-invisible-window-dividers)

;; End: load post-early-init files
(load (locate-user-emacs-file "etc/machine-early-init.el") :no-error-if-file-is-missing :nomessage) ; should be ignored in version control

