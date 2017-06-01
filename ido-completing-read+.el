;;; ido-completing-read+.el --- A completing-read-function using ido  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ryan C. Thompson

;; Filename: ido-completing-read+.el
;; Author: Ryan Thompson
;; Created: Sat Apr  4 13:41:20 2015 (-0700)
;; Version: 3.17
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Keywords: ido, completion, convenience

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This package implements the `ido-completing-read+' function, which
;; is a wrapper for `ido-completing-read'. Importantly, it detects
;; edge cases that ordinary ido cannot handle and either adjusts them
;; so ido *can* handle them, or else simply falls back to Emacs'
;; standard completion instead.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst ido-completing-read+-version "3.17"
  "Currently running version of ido-completing-read+.

Note that when you update ido-completing-read+, this variable may
not be updated until you restart Emacs.")

(require 'ido)
(require 'cl-lib)

;;; Debug messages

(define-minor-mode ido-cr+-debug-mode
  "If non-nil, ido-cr+ will print debug info.

Debug info is printed to the *Messages* buffer."
  nil
  :global t
  :group 'ido-completing-read-plus)

(defsubst ido-cr+--debug-message (format-string &rest args)
  (when ido-cr+-debug-mode
    (apply #'message (concat "ido-completing-read+: " format-string) args)))

;;; Core code

(defvar ido-cr+-minibuffer-depth -1
  "Minibuffer depth of the most recent ido-cr+ activation.

If this equals the current minibuffer depth, then the minibuffer
is currently being used by ido-cr+, and ido-cr+ feature will be
active. Otherwise, something else is using the minibuffer and
ido-cr+ features will be deactivated to avoid interfering with
the other command.

This is set to -1 by default, since `(minibuffer-depth)' should
never return this value.")

(defvar ido-cr+-force-on-functional-collection nil
  "If non-nil, the next call to `ido-completing-read+' will operate on functional collections.

This is not meant to be set permanently, but rather let-bound
before calling `ido-completing-read+' under controlled
circumstances.")

(defvar ido-cr+-orig-completing-read-args nil
  "Original arguments passed to `ido-completing-read+'.

These are used for falling back to `completing-read-default'.")

(defvar ido-cr+-before-fallback-hook nil
  "Hook run when ido-cr+ triggers a fallback.

The hook is run right before calling `ido-cr+-fallback-function'.")

(defgroup ido-completing-read-plus nil
  "Extra features and compatibility for `ido-completing-read'."
  :group 'ido)

(defcustom ido-cr+-fallback-function
  ;; Initialize to the current value of `completing-read-function',
  ;; unless that is already set to the ido completer, in which case
  ;; use `completing-read-default'.
  (if (memq completing-read-function
            '(ido-completing-read+
              ido-completing-read
              ;; Current ido-ubiquitous function
              completing-read-ido-ubiquitous
              ;; Old ido-ubiquitous functions that shouldn't be used
              completing-read-ido
              ido-ubiquitous-completing-read))
      'completing-read-default
    completing-read-function)
  "Alternate completing-read function to use when ido is not wanted.

This will be used for functions that are incompatible with ido
or if ido cannot handle the completion arguments. It will also be
used when the user requests non-ido completion manually via C-f
or C-b."
  :type '(choice (const :tag "Standard emacs completion"
                        completing-read-default)
                 (function :tag "Other function"))
  :group 'ido-completing-read-plus)

(defcustom ido-cr+-max-items 30000
  "Max collection size to use ido-cr+ on.

If `ido-completing-read+' is called on a collection larger than
this, the fallback completion method will be used instead. To
disable fallback based on collection size, set this to nil."
  :type '(choice (const :tag "No limit" nil)
                 (integer
                  :tag "Limit" :value 30000
                  :validate
                  (lambda (widget)
                    (let ((v (widget-value widget)))
                      (if (and (integerp v)
                               (> v 0))
                          nil
                        (widget-put widget :error "This field should contain a positive integer")
                        widget)))))
  :group 'ido-completing-read-plus)

;;;###autoload
(defcustom ido-cr+-replace-completely nil
  "If non-nil, replace `ido-completeing-read' completely with ido-cr+.

Enabling this may interfere with or cause errors in other
packages that use `ido-completing-read'. If you discover any such
incompatibilities, please file a bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues"
  :type 'boolean)

;; Signal used to trigger fallback
(put 'ido-cr+-fallback 'error-conditions '(ido-cr+-fallback error))
(put 'ido-cr+-fallback 'error-message "ido-cr+-fallback")

(defsubst ido-cr+--explain-fallback (arg)
  ;; This function accepts a string, or an ido-cr+-fallback
  ;; signal.
  (when ido-cr+-debug-mode
    (when (and (listp arg)
               (eq (car arg) 'ido-cr+-fallback))
      (setq arg (cadr arg)))
    (ido-cr+--debug-message "Falling back to `%s' because %s."
                            ido-cr+-fallback-function arg)))

(defsubst ido-cr+-active ()
  "Returns non-nil if ido-cr+ is currently using the minibuffer."
  (>= ido-cr+-minibuffer-depth (minibuffer-depth)))

;;;###autoload
(defun ido-completing-read+ (prompt collection &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them."
  (let (;; Save the original arguments in case we need to do the
        ;; fallback
        (ido-cr+-orig-completing-read-args
         (list prompt collection predicate require-match
               initial-input hist def inherit-input-method)))
    (condition-case sig
        (progn
          ;; Check a bunch of fallback conditions
          (cond
           (inherit-input-method
            (signal 'ido-cr+-fallback
                    '("ido cannot handle non-nil INHERIT-INPUT-METHOD")))
           ((bound-and-true-p completion-extra-properties)
            (signal 'ido-cr+-fallback
                    '("ido cannot handle non-nil `completion-extra-properties'")))
           ((and (functionp collection)
                 (not ido-cr+-force-on-functional-collection))
            (signal 'ido-cr+-fallback
                    '("ido cannot handle COLLECTION being a function (but see `ido-cr+-force-on-functional-collection')"))))

          ;; Expand all possible completions. (Apologies to everyone
          ;; who worked so hard to make lazy collections work; ido
          ;; doesn't know how to handle those.)
          (setq collection (all-completions "" collection predicate))
          ;; Check for a specific bug
          (when (and (version< emacs-version "26.1")
                     ido-enable-dot-prefix
                     (member "" collection))
            (signal 'ido-cr+-fallback
                    '("ido cannot handle the empty string as an option when `ido-enable-dot-prefix' is non-nil; see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997")))
          ;; No point in using ido unless there's a collection
          (when (= (length collection) 0)
            (signal 'ido-cr+-fallback '("ido is not needed for an empty collection")))
          ;; Check for excessively large collection
          (when (and ido-cr+-max-items
                     (> (length collection) ido-cr+-max-items))
            (signal 'ido-cr+-fallback
                    (list
                     (format
                      "there are more than %i items in COLLECTION (see `ido-cr+-max-items')"
                      ido-cr+-max-items))))
          ;; ido doesn't natively handle DEF being a list. If DEF is a
          ;; list, prepend it to COLLECTION and set DEF to just the
          ;; car of the default list.
          (when (and def (listp def))
            (setq collection
                  (append def
                          (nreverse (cl-set-difference collection def)))
                  def (car def)))
          ;; Work around a bug in ido when both INITIAL-INPUT and
          ;; DEF are provided.
          (let ((initial
                 (or (if (consp initial-input)
                         (car initial-input)
                       initial-input)
                     "")))
            (when (and def initial
                       (stringp initial)
                       (not (string= initial "")))
              ;; Both default and initial input were provided. So keep
              ;; the initial input and preprocess the collection list
              ;; to put the default at the head, then proceed with
              ;; default = nil.
              (setq collection (cons def (remove def collection))
                    def nil)))
          ;; Ready to do actual ido completion
          (prog1
              (let ((ido-cr+-minibuffer-depth (1+ (minibuffer-depth))))
                (ido-completing-read
                 prompt collection
                 predicate require-match initial-input hist def
                 inherit-input-method))
            ;; This detects when the user triggered fallback mode
            ;; manually.
            (when (eq ido-exit 'fallback)
              (signal 'ido-cr+-fallback '("user manually triggered fallback")))))

      ;; Handler for ido-cr+-fallback signal
      (ido-cr+-fallback
       (ido-cr+--explain-fallback sig)
       (run-hooks 'ido-cr+-before-fallback-hook)
       (apply ido-cr+-fallback-function ido-cr+-orig-completing-read-args)))))

;;;###autoload
(defadvice ido-completing-read (around ido-cr+ activate)
  "This advice is the implementation of `ido-cr+-replace-completely'."
  ;; If this advice is autoloaded, then we need to force loading of
  ;; the rest of the file so all the variables will be defined.
  (when (not (featurep 'ido-completing-read+))
    (require 'ido-completing-read+))
  (if (or (ido-cr+-active)
          (not ido-cr+-replace-completely))
      ;; ido-cr+ has either already activated or isn't going to
      ;; activate, so just run the function as normal
      ad-do-it
    ;; Otherwise, we need to activate ido-cr+.
    (setq ad-return-value (apply #'ido-completing-read+ (ad-get-args 0)))))

;; Fallback on magic C-f and C-b
;;;###autoload
(defvar ido-context-switch-command nil
  "Variable holding the command used for switching to another completion mode.

This variable is originally declared in `ido.el', but it is not
given a value (or a docstring). This documentation comes from a
re-declaration in `ido-completing-read+.el' that initializes it
to nil, which should suppress some byte-compilation warnings in
Emacs 25. Setting another package's variable is not safe in
general, but in this case it should be, because ido always
let-binds this variable before using it, so the initial value
shouldn't matter.")

(defadvice ido-magic-forward-char (before ido-cr+-fallback activate)
  "Allow falling back in ido-completing-read+."
  (when (ido-cr+-active)
    ;; `ido-context-switch-command' is already let-bound at this
    ;; point.
    (setq ido-context-switch-command #'ido-fallback-command)))

(defadvice ido-magic-backward-char (before ido-cr+-fallback activate)
  "Allow falling back in ido-completing-read+."
  (when (ido-cr+-active)
    ;; `ido-context-switch-command' is already let-bound at this
    ;; point.
    (setq ido-context-switch-command #'ido-fallback-command)))

;;; Workaround for https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/93

(defadvice ido-select-text (around fix-require-match-behavior activate)
  "Fix ido behavior when `require-match' is non-nil.

Standard ido will allow C-j to exit with an incomplete completion
even when `require-match' is non-nil. Ordinary completion does
not allow this. In ordinary completion, RET on an incomplete
match is equivalent to TAB, and C-j selects the first match.
Since RET in ido already selects the first match, this advice
sets up C-j to be equivalent to TAB in the same situation."
  (if (and
       ;; Only override C-j behavior if...
       ;; We're using ico-cr+
       (ido-cr+-active)
       ;; Require-match is non-nil
       (with-no-warnings ido-require-match)
       ;; A default was provided, or ido-text is non-empty
       (or (with-no-warnings ido-default-item)
           (not (string= ido-text "")))
       ;; Only if current text is not a complete choice
       (not (member ido-text (with-no-warnings ido-cur-list))))
      (progn
        (ido-cr+--debug-message
         "Overriding C-j behavior for require-match: performing completion instead of exiting with current text. (This might still exit with a match if `ido-confirm-unique-completion' is nil)")
        (ido-complete))
    ad-do-it))

(provide 'ido-completing-read+)

;;; ido-completing-read+.el ends here
