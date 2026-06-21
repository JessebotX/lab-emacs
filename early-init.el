;;; early-init.el -*- lexical-binding: t; -*-

;;; PREFACE

(startup-redirect-eln-cache (locate-user-emacs-file "var/eln-cache"))

;;; DEBUGGING / WARNINGS

(defconst my/emacs-enable-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(setq byte-compile-warnings my/emacs-enable-debug)
(setq byte-compile-verbose my/emacs-enable-debug)
(setq garbage-collection-messages my/emacs-enable-debug)
(setq jka-compr-verbose my/emacs-enable-debug)
(setq native-comp-async-report-warnings-errors (or my/emacs-enable-debug 'silent))
(setq native-comp-warning-on-missing-source my/emacs-enable-debug)
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

(setq ad-redefinition-accept 'accept) ; Legacy advice API warnings

;;; SPEED UP EMACS

(defconst my/emacs--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold (* 256 1024 1024))
(setq gc-cons-percentage 1.0)
(setq load-prefer-newer noninteractive)
(setq file-name-handler-alist nil)
(setq vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (defun my/emacs-startup-hook ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (setq gc-cons-percentage 0.1)
            (setq load-prefer-newer nil)
            (setq vc-handled-backends '(Git))
            (setq file-name-handler-alist my/emacs--file-name-handler-alist)))

;; Disable bidirectional text scanning for a modest performance boost.
;; Give up some bidirectional for slightly faster re-display.
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

(setq auto-mode-case-fold nil)
(setq inhibit-compacting-font-caches t)

;; ;; Better buffer/chunk rendering
;; (setq process-adaptive-read-buffering nil)
;; (setq read-process-output-max (* 2 1024 1024)) ; 1024mb

;; Remove latency in Emacs PGTK
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;;; SETTINGS

;;;; CHARSET
(set-language-environment "UTF-8")
(setq default-input-method nil) ; unwanted from `set-language-environment'

;;;; FRAME
(setq frame-resize-pixelwise t)
(setq frame-title-format '("%b"))

;;;; SECURITY
(setq ffap-machine-p-known 'reject) ; Dont ping things that look like domain names
(setq gnutls-min-prime-bits 3072)   ; Stronger GnuTLS encryption
(setq gnutls-verify-error t)        ; Inform users of certificate issues
(setq tls-checktrust t)            ; Check trust of SSL/TLS connections

;;;; Misc
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-x-resources t)

(setq initial-buffer-choice nil)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq native-comp-async-on-battery-power nil)
(setq package-enable-at-startup nil)
(setq site-run-file nil)
(setq vc-follow-symlinks nil)

;; HACK
(defun display-startup-echo-area-message () (message ""))

;;; UI ELEMENTS

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;;;; Frame margin & padding

;; ;; Credit: `spacious-padding' package by Protesilaos
;; (push '(internal-border-width . 20) default-frame-alist)
;; (push '(right-divider-width   . 10) default-frame-alist)
;; (push '(scroll-bar-width      . 4)  default-frame-alist)

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

;;; END

(load (locate-user-emacs-file "etc/local-early-init.el") :no-error-if-file-is-missing :nomessage) ; should be ignored in version control
