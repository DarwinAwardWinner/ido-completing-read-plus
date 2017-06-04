;;; ido-completing-read+.el --- A completing-read-function using ido  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ryan C. Thompson

;; Filename: ido-completing-read+.el
;; Author: Ryan Thompson
;; Created: Sat Apr  4 13:41:20 2015 (-0700)
;; Version: 4.0
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

(defconst ido-completing-read+-version "4.0"
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

(defvar ido-cr+-assume-static-collection nil
  "If non-nil, ido-cr+ will assume that the collection is static.

This is used to avoid unnecessary work in the case where the
collection is a function, since a function collection could
potentially change the set of completion candidates
dynamically.")

(defvar ido-cr+-dynamic-collection nil
  "Stores the collection argument if it is a function.

This allows ido-cr+ to update the set of completion candidates
dynamically.")

(defvar ido-cr+-no-default-action 'prepend-empty-string
  "Controls the behavior of ido-cr+ when DEF is nil and REQUIRE-MATCH is non-nil.

Possible values:

- `prepend-empty-string': The empty string will be added to the
  front of COLLECTION, making it the default. This is the
  standard behavior since it mimics the semantics of
  `completing-read-default'.

- `append-empty-string': The empty string will be added to the
  end of COLLECTION, thus keeping the original default while
  making the empty string available as a completion.

- `nil': No action will be taken.

- Any other value: The value will be interpreted as a 1-argument
  function, which will receive the current collection as its
  argument and return the collection with any necessary
  modifications applied.

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

(defcustom ido-cr+-function-blacklist
  '(read-file-name-internal
    read-buffer
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/60
    todo-add-category
    gnus-emacs-completing-read
    gnus-iswitchb-completing-read
    grep-read-files
    magit-builtin-completing-read
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/39
    Info-read-node-name
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/44
    tmm-prompt)
  "Functions & commands for which ido-cr+ should be disabled.

Each entry can be either a symbol or a string. A symbol means to
fall back specifically for the named function. A regular
expression means to fall back for any function whose name matches
that regular expression. When ido-cr+ is called through
`completing-read', if any function in the call stack of the
current command matches any of the blacklist entries, ido-cr+
will be disabled for that command. Additionally, if the
collection in the call to `completing-read' matches any of the
blacklist entries, ido-cr+ will be disabled.

Note that using specific function names is generally preferable
to regular expressions, because the associated function
definitions will be compared directly, so if the same function is
called by another name, it should still trigger the fallback. For
regular expressions, only name-based matching is possible."
  :group 'ido-completing-read-plus
  :type '(repeat (choice (symbol :tag "Function or command name")
                         (string :tag "Regexp"))))

(defcustom ido-cr+-function-whitelist
  nil
  "Functions & commands for which ido-cr+ should be enabled.

If this variable is non-nil, ido-cr+'s whitelisting mode will be
enabled, and will be disabled for all functions unless they match
one of the entries. Matching is done in the same manner as
`ido-cr+-function-blacklist', and blacklisting takes precedence
over whitelisting."
  :group 'ido-completing-read-plus
  :type '(repeat (choice (symbol :tag "Function or command name")
                         (string :tag "Regexp"))))

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

(defsubst ido-cr+-default-was-provided ()
  "Returns non-nil if ido-cr+ was passed a non-nil default argument."
  (and (nth 6 ido-cr+-orig-completing-read-args)))

(defun ido-cr+--called-from-completing-read ()
  "Returns non-nil if the most recent call to ido-cr+ was from `completing-read'."
  (equal (cadr (backtrace-frame 1 'ido-completing-read+))
         'completing-read))

(defun ido-cr+-function-is-blacklisted (fun)
  (cl-loop
   for entry in ido-cr+-function-blacklist
   if (cond
       ;; Nil: Never matches anything
       ((null entry)
        nil)
       ;; Symbol: Compare names and function definitions
       ((symbolp entry)
        (or (eq entry fun)
            (eq (indirect-function entry)
                (indirect-function fun))))
       ;; String: Do regexp matching against function name if it is a
       ;; symbol
       ((stringp entry)
        (and (symbolp fun)
             (string-match-p entry (symbol-name fun))))
       ;; Anything else: invalid blacklist entry
       (t
        (ido-cr+--debug-message "Ignoring invalid entry in ido-cr+-function-blacklist: `%S'" entry)
        nil))
   return entry
   ;; If no blacklist entry matches, return nil
   finally return nil))

(defun ido-cr+-function-is-whitelisted (fun)
  (if (null ido-cr+-function-whitelist)
      ;; Empty whitelist means everything is whitelisted
      t
    (cl-loop
     for entry in ido-cr+-function-whitelist
     if (cond
         ;; Nil: Never matches anything
         ((null entry)
          nil)
         ;; Symbol: Compare names and function definitions
         ((symbolp entry)
          (or (eq entry fun)
              (eq (indirect-function entry)
                  (indirect-function fun))))
         ;; String: Do regexp matching against function name if it is a
         ;; symbol
         ((stringp entry)
          (and (symbolp fun)
               (string-match-p entry (symbol-name fun))))
         ;; Anything else: invalid whitelist entry
         (t
          (ido-cr+--debug-message "Ignoring invalid entry in ido-cr+-function-whitelist: `%S'" entry)
          nil))
     return entry
     ;; If no whitelist entry matches, return nil
     finally return nil)))

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
               initial-input hist def inherit-input-method))
        ;; Need to save this since activating the minibuffer once will
        ;; clear out any temporary minibuffer hooks, which need to get
        ;; restored before falling back.
        (orig-minibuffer-setup-hook minibuffer-setup-hook)
        ;; If collection is a function, save it for later, unless
        ;; instructed not to
        (ido-cr+-dynamic-collection
         (when (and (not ido-cr+-assume-static-collection)
                    (functionp collection))
           collection))
        ;; If the whitelist is empty, everything is whitelisted
        (whitelisted (not ido-cr+-function-whitelist)))
    (condition-case sig
        (progn
          ;; Check a bunch of fallback conditions
          (when inherit-input-method
            (signal 'ido-cr+-fallback
                    '("ido cannot handle non-nil INHERIT-INPUT-METHOD")))
          (when (bound-and-true-p completion-extra-properties)
            (signal 'ido-cr+-fallback
                    '("ido cannot handle non-nil `completion-extra-properties'")))

          ;; Check for black/white-listed collection function
          (when (functionp collection)
            ;; Blacklist
            (when (ido-cr+-function-is-blacklisted collection)
              (if (symbolp collection)
                  (signal 'ido-cr+-fallback
                          (list (format "collection function `%S' is blacklisted" collection)))
                (signal 'ido-cr+-fallback
                        (list "collection function is blacklisted"))))
            ;; Whitelist
            (when (and (not whitelisted)
                       (ido-cr+-function-is-whitelisted collection))
              (ido-cr+--debug-message
               (if (symbolp collection)
                   (format "Collection function `%S' is whitelisted" collection)
                 "Collection function is whitelisted"))
              (setq whitelisted t)))

          ;; Expand all currently-known completions.
          (setq collection (all-completions "" collection predicate))
          ;; No point in using ido unless there's a collection
          (when (and (= (length collection) 0)
                     (not ido-cr+-dynamic-collection))
            (signal 'ido-cr+-fallback '("ido is not needed for an empty collection")))
          ;; Check for excessively large collection
          (when (and ido-cr+-max-items
                     (> (length collection) ido-cr+-max-items))
            (signal 'ido-cr+-fallback
                    (list
                     (format
                      "there are more than %i items in COLLECTION (see `ido-cr+-max-items')"
                      ido-cr+-max-items))))

          ;; If called from `completing-read', check for
          ;; black/white-listed commands/callers
          (when (ido-cr+--called-from-completing-read)
            ;; Check calling command
            (when (ido-cr+-function-is-blacklisted this-command)
              (signal 'ido-cr+-fallback
                      (list "calling command `%S' is blacklisted" this-command)))
            (when (and (not whitelisted)
                       (ido-cr+-function-is-whitelisted this-command))
              (ido-cr+--debug-message "Command `%S' is whitelisted" this-command)
              (setq whitelisted t))
            ;; Check every function in the call stack starting after
            ;; `completing-read' until to the first
            ;; `funcall-interactively' (for a call from the function
            ;; body) or `call-interactively' (for a call from the
            ;; interactive form, in which the function hasn't actually
            ;; been called yet, so `funcall-interactively' won't be on
            ;; the stack.)
            (cl-loop for i upfrom 1
                     for caller = (cadr (backtrace-frame i 'completing-read))
                     while caller
                     while (not (memq (indirect-function caller)
                                      '(internal--funcall-interactively
                                        (indirect-function 'call-interactively))))
                     if (ido-cr+-function-is-blacklisted caller)
                     do (signal 'ido-cr+-fallback
                                (list (if (symbolp caller)
                                          (format "calling function `%S' is blacklisted" caller)
                                        "a calling function is blacklisted")))
                     if (and (not whitelisted)
                             (ido-cr+-function-is-whitelisted caller))
                     do (progn
                          (ido-cr+--debug-message
                           (if (symbolp caller)
                               (format "Calling function `%S' is whitelisted" caller)
                             "A calling function is whitelisted"))
                          (setq whitelisted t))))

          (unless whitelisted
            (signal 'ido-cr+-fallback
                    (list "no functions or commands matched the whitelist for this call")))

          ;; In ido, the semantics of "default" are simply "put it at
          ;; the front of the list". Furthermore, ido has certain
          ;; issues with a non-nil DEF arg. Specifically, it can't
          ;; handle list defaults or providing both DEF and
          ;; INITIAL-INPUT. So, just pre-process the collection to put
          ;; the default(s) at the front and then set DEF to nil in
          ;; the call to ido to avoid these issues.
          (unless (listp def)
            ;; Ensure DEF is a list
            (setq def (list def)))
          (when def
            (setq collection (append def (cl-set-difference collection def
                                                            :test #'equal))
                  def nil))

          ;; If DEF was nil and REQUIRE-MATCH was non-nil, then we need to
          ;; add the empty string as the first option, because RET on
          ;; an empty input needs to return "". (Or possibly we need
          ;; to take some other action based on the value of
          ;; `ido-cr+-no-default-action'.)
          (when (and require-match
                     ido-cr+-no-default-action
                     (not (ido-cr+-default-was-provided)))
            (cl-case ido-cr+-no-default-action
              (nil
               ;; Take no action
               t)
              (prepend-empty-string
               (ido-cr+--debug-message "Adding \"\" as the default completion since no default was provided.")
               (setq collection (cons "" collection)))
              (append-empty-string
               (ido-cr+--debug-message "Adding \"\" as a completion option since no default was provided.")
               (setq collection (append collection '(""))))
              (otherwise
               (ido-cr+--debug-message "Running custom action function since no default was provided.")
               (setq collection (funcall ido-cr+-no-default-action collection)))))

          ;; Check for a specific bug
          (when (and ido-enable-dot-prefix
                     (version< emacs-version "26.1")
                     (member "" collection))
            (signal 'ido-cr+-fallback
                    '("ido cannot handle the empty string as an option when `ido-enable-dot-prefix' is non-nil; see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997")))

          ;; Finally ready to do actual ido completion
          (prog1
              (let ((ido-cr+-minibuffer-depth (1+ (minibuffer-depth)))
                    ;; Reset these for the next call to ido-cr+
                    (ido-cr+-no-default-action 'prepend-empty-string)
                    (ido-cr+-assume-static-collection nil))
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
       (let (;; Reset `minibuffer-setup-hook' to original value
             (minibuffer-setup-hook orig-minibuffer-setup-hook)
             ;; Reset these for the next call to ido-cr+
             (ido-cr+-no-default-action 'prepend-empty-string)
             (ido-cr+-assume-static-collection nil))
         (ido-cr+--explain-fallback sig)
         (run-hooks 'ido-cr+-before-fallback-hook)
         (apply ido-cr+-fallback-function ido-cr+-orig-completing-read-args))))))

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
       ;; Current text is not a complete choice
       (not (member ido-text (with-no-warnings ido-cur-list))))
      (progn
        (ido-cr+--debug-message
         "Overriding C-j behavior for require-match: performing completion instead of exiting with current text. (This might still exit with a match if `ido-confirm-unique-completion' is nil)")
        (ido-complete))
    ad-do-it))

(defadvice ido-exhibit (before ido-cr+-update-dynamic-collection activate)
  "Maybe update the set of completions when ido-text changes."
  (when ido-cr+-dynamic-collection
    (let ((prev-ido-text ido-text)
          (current-ido-text (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))
      (when (not (string= prev-ido-text current-ido-text))
        (let ((current-match (car ido-matches))
              (def (nth 6 ido-cr+-orig-completing-read-args))
              (predicate (nth 2 ido-cr+-orig-completing-read-args)))
          (setq ido-cur-list
                (delete-dups
                 (append
                  (all-completions
                   current-ido-text
                   ido-cr+-dynamic-collection
                   predicate)
                  (all-completions
                   ""
                   ido-cr+-dynamic-collection
                   predicate))))
          (unless (listp def)
            (setq def (list def)))
          (when def
            (setq ido-cur-list
                  (append def (cl-set-difference ido-cur-list def
                                                 :test #'equal))))
          (when (and current-match (member current-match ido-cur-list))
            (setq ido-cur-list (ido-chop ido-cur-list current-match))))
        (ido-cr+--debug-message "Updated completion candidates for dynamic collection because `ido-text' changed from %S to %S. `ido-cur-list' now has %s elements" prev-ido-text current-ido-text (length ido-cur-list))))))

;; Interoperation with minibuffer-electric-default-mode: only show the
;; default when the input is empty and the empty string is the selected
(defadvice minibuf-eldef-update-minibuffer (around ido-cr+-compat activate)
  "This advice allows minibuffer-electric-default-mode to work with ido-cr+."
  (if (ido-cr+-active)
      (unless (eq minibuf-eldef-showing-default-in-prompt
                  (and (string= (car ido-cur-list) "")
                       (string= ido-text "")))
        ;; Swap state.
        (setq minibuf-eldef-showing-default-in-prompt
              (not minibuf-eldef-showing-default-in-prompt))
        (overlay-put minibuf-eldef-overlay 'invisible
                     (not minibuf-eldef-showing-default-in-prompt)))
    ad-do-it))

(provide 'ido-completing-read+)

;;; ido-completing-read+.el ends here
