;;; early-init.el -*- lexical-binding: t; -*-

;;; PREFACE

(defconst my/enable-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(defmacro my/set (var val)
  `(funcall (or (get ',var 'custom-set) #'set-default) ',var ,val))

(defun my/locate-user-var-file (path)
  (expand-file-name path (locate-user-emacs-file "var")))

(defun my/locate-user-etc-file (path)
  (expand-file-name path (locate-user-emacs-file "etc")))

(defun my/locate-user-lisp-file (path)
  (expand-file-name path user-lisp-directory))

(startup-redirect-eln-cache (my/locate-user-var-file "eln-cache"))
(my/set load-prefer-newer t)

;;; DEBUGGING & WARNINGS

(when my/enable-debug
  (my/set message-log-max 16384))

(unless my/enable-debug
  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (my/set command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (my/set command-line-x-option-alist nil)))

(unless (and (featurep 'native-compile)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  ;; Deactivate the `native-compile' feature if it is not available
  (my/set native-comp-jit-compilation nil)
  (my/set features (delq 'native-compile features)))

(my/set native-comp-warning-on-missing-source my/enable-debug)
(my/set native-comp-async-report-warnings-errors (or my/enable-debug 'silent))
(my/set jka-compr-verbose my/enable-debug)
(my/set byte-compile-warnings my/enable-debug)
(my/set byte-compile-verbose my/enable-debug)

(my/set ad-redefinition-action 'accept)

;;; GARBAGE COLLECTION
;; Temporarily raise gc thresholds

(unless noninteractive
  (my/set gc-cons-threshold most-positive-fixnum)
  (my/set gc-cons-percentage 1.0)
  (add-hook 'emacs-startup-hook
            (defun my/--emacs-startup-hook--gc-optimizations ()
              (my/set gc-cons-threshold 33554432) ; 32 MiB
              (my/set gc-cons-percentage 0.1))))

;;; OTHER OPTIMIZATIONS

(defvar my/--file-name-handler-alist file-name-handler-alist)
(my/set file-name-handler-alist nil)
(my/set vc-handled-backends nil)
(add-hook 'emacs-startup-hook
          (defun my/--emacs-startup-hook--other-optimizations ()
            (my/set file-name-handler-alist my/--file-name-handler-alist)
            (my/set vc-handled-backends '(Git SVN))))

(my/set auto-mode-case-fold nil)
(my/set bidi-inhibit-bpa t)
(my/set bidi-display-reordering 'left-to-right)
(my/set bidi-paragraph-direction 'left-to-right)
(my/set inhibit-compacting-font-caches t)

(my/set process-adaptive-read-buffering nil)
(my/set read-process-output-max 1048576)

(when (boundp 'pgtk-wait-for-event-timeout)
  (my/set pgtk-wait-for-event-timeout 0.001))

;;; INTERFACE

(my/set frame-inhibit-implied-resize t)
(my/set frame-resize-pixelwise t)
(my/set frame-title-format '("%b - Emacs"))
(my/set menu-bar-mode nil)
(my/set tool-bar-mode nil)
(my/set scroll-bar-mode nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

(add-hook 'enable-theme-functions
          (defun my/--enable-theme-functions--invisible-window-dividers (_theme)
            "Make window dividers for THEME invisible."
            (let ((bg (face-background 'default)))
              (custom-set-faces
               `(fringe ((t :background ,bg)))
               `(olivetti-fringe ((t :background ,bg)))
               `(window-divider ((t :background ,bg :foreground ,bg)))
               `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
               `(window-divider-last-pixel ((t :background ,bg :foreground ,bg)))))))

;;; MISCELLANEOUS

(set-language-environment "UTF-8")
(my/set default-input-method nil) ; unwanted from `set-language-environment'

(my/set ffap-machine-p-known 'reject) ; Dont ping things that look like domain names
(my/set gnutls-min-prime-bits 3072) ; Stronger GnuTLS encryption
(my/set gnutls-verify-error t) ; Inform users of certificate issues
(my/set tls-checktrust t) ; Check trust of SSL/TLS connections

(my/set package-enable-at-startup nil)
(my/set vc-follow-symlinks nil)

(my/set initial-scratch-message nil)
(my/set inhibit-startup-screen t)
(my/set inhibit-startup-buffer-menu t)
(my/set inhibit-startup-echo-area-message user-login-name)
(my/set inhibit-x-resources t)
(my/set initial-buffer-choice nil)
(my/set initial-major-mode 'fundamental-mode)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add 'display-startup-screen :override #'ignore)

;;; END

(load (my/locate-user-etc-file "local-early-init.el") :no-error-if-file-is-missing :nomessage)
