;;; early-init.el -*- lexical-binding: t; -*-

;; Credits: https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el

;;; PREFACE

(defvar my/emacs--gc-cons-threshold (* 32 1024 1024)
  "Value of `gc-cons-threshold' on `emacs-startup-hook'.")

(defvar my/emacs--gc-cons-percentage gc-cons-percentage
  "Value of `gc-cons-percentage' on `emacs-startup-hook'.")

(defvar my/emacs--file-name-handler-alist file-name-handler-alist
  "Value of `file-name-handler-alist' on `emacs-startup-hook'.")

(defvar my/emacs--vc-handled-backends vc-handled-backends
  "Value of `vc-handled-backends' on `emacs-startup-hook'.")

;;; DEBUGGING

(defvar my/emacs-enable-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(setq debug-on-error my/emacs-enable-debug)

(setq byte-compile-warnings my/emacs-enable-debug)
(setq byte-compile-verbose my/emacs-enable-debug)
(setq garbage-collection-messages my/emacs-enable-debug)
(setq jka-compr-verbose my/emacs-enable-debug)
(setq native-comp-warning-on-missing-source my/emacs-enable-debug)
(setq native-comp-async-report-warnings-errors (or my/emacs-enable-debug 'silent))
(setq warning-minimum-level (if my/emacs-enable-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when my/emacs-enable-debug
  (setq message-log-max 16384))

(unless my/emacs-enable-debug
  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

;;; SPEED-UP STARTUP HACKS

(setq load-prefer-newer noninteractive)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(setq file-name-handler-alist nil)
(setq vc-handled-backends nil)

(defun my/emacs--restore-early-init-settings ()
  "Restore settings that were changed to speedup Emacs startup times"
  (setq load-prefer-newer non-interactive)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 1.0)
  (setq file-name-handler-alist nil)
  (setq vc-handled-backends nil))

;;; MISCELLANEOUS

;;;; Multilingual language environment
(set-language-environment "UTF-8")
(setq default-input-method nil) ; unwanted from `set-language-environment'

;;;; Frame settings
(setq frame-resize-pixelwise t)
(setq frame-title-format '("%b"))

;;;; Initial startup inhibits
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-x-resources t)
(setq initial-buffer-choice nil)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; HACK
(defun display-startup-echo-area-message ()
  "HACK: hide startup message if the inhibit above does not work"
  (message ""))

;; Better buffer/chunk rendering
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 2 1024 1024)) ; 1024kb

;;;; Security

;; Dont ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Stronger GnuTLS encryption
(setq gnutls-min-prime-bits 3072)

;; Prompts user if there are certificate issues
(setq gnutls-verify-error t)

;; Ensure SSL/TLS connections undergo trust verification
(setq tls-checktrust t)

;;;; Other
;; Legacy advice API warnings
(setq ad-redefinition-action 'accept)

;; No second pass of case-insensitive search over `auto-mode-alist'.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
;; Give up some bidirectional for slightly faster re-display.
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

(setq inhibit-compacting-font-caches t)
(setq package-enable-at-startup nil)
(setq site-run-file nil)
(setq vc-follow-symlinks nil)

;; Remove latency in Emacs PGTK
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;;; UI ELEMENTS

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(font . "Maple Mono 12") default-frame-alist) ; fallback font for frames

;;;; Frame margin & padding

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
