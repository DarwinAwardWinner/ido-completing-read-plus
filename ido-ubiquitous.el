;;; ido-ubiquitous.el --- Use ido (nearly) everywhere. -*- lexical-binding: t -*-

;; Copyright (C) 2011-2015 Ryan C. Thompson

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Version: 3.0
;; Created: 2011-09-01
;; Keywords: convenience, completion, ido
;; EmacsWiki: InteractivelyDoThings
;; Package-Requires: ((emacs "24.1"))
;; Filename: ido-ubiquitous.el

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; If you use the excellent `ido-mode' for efficient completion of
;; file names and buffers, you might wonder if you can get ido-style
;; completion everywhere else too. Well, that's what this package
;; does! ido-ubiquitous is here to enable ido-style completion for
;; (almost) every function that uses the standard completion function
;; `completing-read'.

;; To use this package, call `ido-ubiquitous-mode' to enable the mode,
;; or use `M-x customize-variable ido-ubiquitous-mode' it to enable it
;; permanently. Once the mode is enabled, most functions that use
;; `completing-read' will now have ido completion. If you decide in
;; the middle of a command that you would rather not use ido, just C-f
;; or C-b at the end/beginning of the input to fall back to non-ido
;; completion (this is the same shortcut as when using ido for buffers
;; or files).

;; Note that `completing-read' has some quirks and complex behavior
;; that ido cannot emulate. Ido-ubiquitous attempts to detect some of
;; these quirks and avoid using ido when it sees them. So some
;; functions will not have ido completion even when this mode is
;; enabled. Some other functions have ido disabled in them because
;; their packages already provide support for ido via other means (for
;; example, org-mode and magit). See `M-x customize-group
;; ido-ubiquitous' and read about the override variables for more
;; information.

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

(defconst ido-ubiquitous-version "3.0"
  "Currently running version of ido-ubiquitous.

Note that when you update ido-ubiquitous, this variable may not
be updated until you restart Emacs.")

(eval-when-compile
  (when (or (not (boundp 'completing-read-function))
            (< emacs-major-version 24))
    (error "Could not find required variable `completing-read-function'. Are you using Emacs version 24 or higher? If you have Emacs 23 or lower, please downgrade to ido-ubiquitous version 1.7.")))

(require 'ido)
(require 'advice)
(require 'cl-lib)
(require 'ido-completing-read+)
;; Only exists in emacs 24.4 and up; we don't use this library
;; directly, but we load it here so we can test if it's available,
;; because if it isn't we need enable a workaround.
(require 'nadvice nil 'noerror)

;;; Internal utility functions

(defun ido-ubiquitous--as-string (sym-or-str)
  "Return name of symbol, return string as is."
  (if (symbolp sym-or-str)
      (symbol-name sym-or-str)
    sym-or-str))

(defun ido-ubiquitous--as-symbol (sym-or-str)
  "Return string as symbol, return symbol as is."
  (if (symbolp sym-or-str)
      sym-or-str
    (intern sym-or-str)))

;;; Custom widget definitions

;; We need to define some custom widget types for use in the override
;; variables.

(define-widget 'lazy-notag 'lazy
  "Like lazy widget, but does not display its tag, only its value."
  :format "%v")

;; Define matcher functions and widgets for match specifications
(defvar ido-ubiquitous-match-spec-widget-types nil
  "List of widget names for match specs.")
(defvar ido-ubiquitous-spec-matchers nil
  "Alist of functions for matching function specs against function names.")
(cl-loop for (widget-name widget-tag key field-type matcher) in
         '((exact-match "Exact match" exact string string=)
           (prefix-match "Prefix match" prefix string string-prefix-p)
           (regexp-match "Regexp match" regexp regexp string-match-p))
         do (define-widget (ido-ubiquitous--as-symbol widget-name) 'lazy-notag widget-tag
              :menu-tag widget-tag
              :type `(list :tag ,widget-tag :format "%v"
                           (const :format ""
                                  :tag ,widget-tag
                                  ,key)
                           (,field-type :tag ,widget-tag)))
         do (add-to-list 'ido-ubiquitous-match-spec-widget-types
                         widget-name 'append)
         do (add-to-list 'ido-ubiquitous-spec-matchers
                         (cons key matcher) 'append))

(define-widget 'ido-ubiquitous-match-spec 'lazy-notag
  "Choice of exact, prefix, or regexp match."
  :type `(choice :tag "Match type"
                 ,@ido-ubiquitous-match-spec-widget-types))

(define-widget 'ido-ubiquitous-command-override-spec 'lazy-notag
  "Choice of override action plus match specification."
  :type '(cons :tag "Override rule"
               (choice :tag "For matching commands"
                       (const :menu-tag "Disable"
                              :tag "Disable ido-ubiquitous"
                              disable)
                       (const :menu-tag "Enable"
                              :tag "Enable ido-ubiquitous in normal default mode"
                              enable)
                       (const :menu-tag "Enable old-style default"
                              :tag "Enable ido-ubiquitous in old-style default mode"
                              enable-old))
               ido-ubiquitous-match-spec))

(define-widget 'ido-ubiquitous-function-override-spec 'lazy-notag
  "Choice of override action and function name. (Exact match only.)"
  :type '(list :tag "Override rule"
               (choice :tag "Do the following"
                       (const :menu-tag "Disable"
                              :tag "Disable ido-ubiquitous"
                              disable)
                       (const :menu-tag "Enable"
                              :tag "Enable ido-ubiquitous in normal default mode"
                              enable)
                       (const :menu-tag "Enable old-style default"
                              :tag "Enable ido-ubiquitous in old-style default mode"
                              enable-old))
               (const :format "" exact)
               (string :tag "For function")))

;;; Custom Declarations

(defgroup ido-ubiquitous nil
  "Use ido for (almost) all completion."
  :group 'ido
  :group 'ido-completing-read-plus)

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "ido-ubiquitous 0.8")
;;;###autoload
(define-obsolete-function-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "ido-ubiquitous 0.8")

;;;###autoload
(define-minor-mode ido-ubiquitous-mode
  "Use `ido-completing-read' instead of `completing-read' almost everywhere.

If this mode causes problems for a function, you can customize
when ido completion is or is not used by customizing
`ido-ubiquitous-command-overrides' or
`ido-ubiquitous-function-overrides'."
  nil
  :global t
  :group 'ido-ubiquitous
  ;; Actually enable/disable the mode by setting
  ;; `completing-read-function'.
  (setq completing-read-function
        (if ido-ubiquitous-mode
            #'completing-read-ido-ubiquitous
          ido-cr+-fallback-function)))

;; Variables for functionality that has moved to ido-completing-read+
(define-obsolete-variable-alias
  'ido-ubiquitous-max-items
  'ido-cr+-max-items
  "ido-ubiquitous 3.0")
(define-obsolete-variable-alias
  'ido-ubiquitous-fallback-completing-read-function
  'ido-cr+-fallback-function
  "ido-ubiquitous 3.0")

(define-obsolete-variable-alias
  'ido-ubiquitous-enable-compatibility-globally
  'ido-ubiquitous-enable-old-style-default
  "ido-ubiquitous 2.0")

(defcustom ido-ubiquitous-default-state 'enable
  "Default ido-ubiquitous mode of operation for commands with no override.

This can be set to one of three options:

  * `enable': use normal ido completion;
  * `enable-old': use ido completion, but with emulation of the
    old-style default selection of `completing-read';
  * `disable': use non-ido completion.

Command-specific and function-specific overrides are available to
override this default for specific commands/functions. See
`ido-ubiquitous-command-overrides' and
`ido-ubiquitous-function-overrides'.

The `enable-old' option swaps the behavior of RET and C-j but
only for the first keypress after beginning completion.
Specifically, on the first keypress, RET will return an empty
string and C-j will return the first item on the list. The
purpose of this is to emulate a legacy compatibility quirk of
`completing-read'. From the `completing-read' docstring:

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

This odd behavior is required for compatibility with an old-style
usage pattern whereby the default was requested by returning an
empty string. In this mode, the caller receives the empty string
and handles the default case manually, while `completing-read'
never has any knowledge of the default. This is a problem for
ido, which normally returns the first element in the list, not an
empty string, when the input is empty and you press RET. Without
knowledge of the default, it cannot ensure that the default is
first on the list, so returning the first item is not the correct
behavior. Instead, it must return an empty string like
`completing-read'.

The `disable' mode is available as a default, which seems
counterintuitive. But this allows you, if you so desire, to
enable ido-ubiquitous selectively for only a few specifc commands
using overrides and disable it for everything else."
  :type '(choice :tag "Default mode"
                 (const :menu-tag "Disable"
                        :tag "Disable ido-ubiquitous"
                        disable)
                 (const :menu-tag "Enable"
                        :tag "Enable ido-ubiquitous in normal default mode"
                        enable)
                 (const :menu-tag "Enable old-style default"
                        :tag "Enable ido-ubiquitous in old-style default mode"
                        enable-old))
  :group 'ido-ubiquitous)

(make-obsolete-variable
 'ido-ubiquitous-enable-old-style-default
 "This variable no longer has any effect. Set
`ido-ubiquitous-default-state' to `enable-old' instead."
 "ido-ubiquitous 3.0")

(defconst ido-ubiquitous-default-command-overrides
  '(;; If you want ido for M-x, install smex
    (disable exact "execute-extended-command")
    ;; Wanderlust uses new-style default
    (enable prefix "wl-")
    ;; Info functions use old-style default selection
    (enable-old prefix "Info-")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/4
    (enable exact "webjump")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/28
    (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/37
    ;; Org and Magit already support ido natively
    (disable prefix "org-")
    ;; https://github.com/bbatsov/prelude/issues/488
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/44
    ;; tmm implements its own non-standard completion mechanics
    (disable prefix "tmm-")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/47
    ;; theme functions don't need old-style compatibility
    (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit\\)-theme\\'")
    )
  "Default value of `ido-ubiquitous-command-overrides'.

You can restore these using the command `ido-ubiquitous-restore-default-overrides'.")

(defconst ido-ubiquitous-default-function-overrides
  '((disable exact "read-file-name")
    (disable exact "read-file-name-internal")
    (disable exact "read-buffer")
    (disable exact "gnus-emacs-completing-read")
    (disable exact "gnus-iswitchb-completing-read")
    (disable exact "grep-read-files")
    (disable exact "magit-builtin-completing-read")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/36
    (enable exact "bookmark-completing-read")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/4
    (enable-old exact "webjump-read-choice")
    (enable-old exact "webjump-read-url-choice")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/9
    (disable exact "isearchp-read-unicode-char")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/37
    (disable exact "org-completing-read")
    (disable exact "org-completing-read-no-i")
    (disable exact "org-iswitchb-completing-read")
    (disable exact "org-icompleting-read")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/38
    (enable exact "read-char-by-name")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/39
    (disable exact "Info-read-node-name")
    ;; https://github.com/purcell/emacs.d/issues/182#issuecomment-44212927
    (disable exact "tmm-menubar")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/58
    ;; https://github.com/mooz/js2-mode/issues/181
    (enable exact "imenu--completion-buffer"))
  "Default value of `ido-ubiquitous-function-overrides'.

You can restore these using the command `ido-ubiquitous-restore-default-overrides'.")

(defcustom ido-ubiquitous-command-overrides ido-ubiquitous-default-command-overrides
  "List of command override specifications for ido-ubiquitous

Each override specification describes how ido-ubiquitous should
behave one or many commands. A specification has the
form `(BEHAVIOR MATCH-TYPE MATCH-TEXT)'. BEHAVIOR is one of the
following:

  * `disable': ido-ubiquitous should not be used at all for the
    specified commands;
  * `enable': ido-ubiquitous may be used with the specified
    commands, without emulating the old-style default selection
    of `completing-read';
  * `enable-old': ido-ubiquitous may be used with the specified
    commands, and should emulate the old-style default selection
    of `completing-read'.

MATCH-TYPE affects how MATCH-TEXT is interpreted, as follows:

  * `exact': the specification only affects the one command
    whose name is MATCH-TEXT;
  * `prefix': the specification affects any command whose name
    starts with MATCH-TEXT (This is useful for specifying a
    certain behavior for an entire package);
  * `regexp': the specification affects any command whose name
    matches MATCH-TEXT (with MATCH-TEXT being interpreted as a
    regular expression)

MATCH-TEXT should be a string.

Since this variable's has a somewhat complex structure, it is
recommended that you set this variable through Customize.

Note that this variable only affects *commands*, which are
functions marked as interactive. See
`ido-ubiquitous-function-overrides' for how to modify the
behavior of ido-ubiquitous for arbitrary functions.

If you need to add a new specification to this list, please also
file a bug report at https://github.com/DarwinAwardWinner/ido-ubiquitous/issues"
  :type '(repeat ido-ubiquitous-command-override-spec)
  :group 'ido-ubiquitous)

(defmacro ido-ubiquitous-with-override (override &rest body)
  "Eval BODY with specicified OVERRIDE in place.

The OVERRIDE argument is evaluated normally, so if it is a
literal symbol, it must be quoted.

See `ido-ubiquitous-command-overrides' for valid override types."
  (declare (indent 1))
  ;; Eval override
  `(let ((ido-ubiquitous-next-override ,override))
     ,@body))

(defun ido-ubiquitous-apply-function-override (func override)
  "Set the override property on FUNC to OVERRIDE and set up advice to apply the override."
  (setq func (ido-ubiquitous--as-symbol func)
        override (ido-ubiquitous--as-symbol override))
  (put func 'ido-ubiquitous-override override)
  (when override
    (let ((docstring
           (format "Override ido-ubiquitous behavior in %s if its `ido-ubiquitous-override' property is non-nil." func)))
      (eval
       `(defadvice ,func (around ido-ubiquitous-override activate)
          ,docstring
          (ido-ubiquitous-with-override
           (get ',func 'ido-ubiquitous-override)
           ad-do-it))))))

(defun ido-ubiquitous-set-function-overrides (sym newval)
  "Custom setter function for `ido-ubiquitous-function-overrides'.

In addition to setting the variable, this also sets up advice on
each function to apply the appropriate override."
  ;; Unset all previous overrides
  (when (boundp sym)
    (let ((oldval (eval sym)))
      (cl-loop for (_action _match-type func) in oldval
               do (ido-ubiquitous-apply-function-override func nil))))
  ;; Ensure that function names are strings, not symbols
  (setq newval
        (cl-loop for (action match-type func) in newval
                 collect (list action match-type
                               (ido-ubiquitous--as-string func))))
  (set-default sym newval)
  ;; set new overrides
  (cl-loop for (action _match-type func) in (eval sym)
           do (ido-ubiquitous-apply-function-override func action)))

(defcustom ido-ubiquitous-function-overrides ido-ubiquitous-default-function-overrides
  "List of function override specifications for ido-ubiquitous

Function override specifications have a similar structure to
command override specifications (see
`ido-ubiquitous-command-overrides'). A function override
specification has the form `(BEHAVIOR MATCH-TYPE MATCH-TEXT)'.
However, `MATCH-TYPE' may ONLY be `exact'; No other match type is
supported.

If you need to add a new specification to this list, please also file a
bug report at https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

Setting this variable directly has no effect. You must set it
through Customize."
  :type '(repeat ido-ubiquitous-function-override-spec)
  :set 'ido-ubiquitous-set-function-overrides
  :group 'ido-ubiquitous)

(defcustom ido-ubiquitous-allow-on-functional-collection nil
  "Allow ido completion when COLLECTION is a function.

The `completing-read' function allows its COLLECTION argument to
be a function instead of a list of choices. Some such functions
simply return a list of completions and are suitable for use with
ido, but others implement more complex behavior and will result
in incorrect behavior if used with ido. Since there is no way to
tell the difference, this preference defaults to nil, which means
that ido-ubiquitous will not work when COLLECTION is a function
unless there is a specific override in effect. To disable this
safeguard and GUARANTEE ERRORS on some functions, you may set
this to non-nil, but this is not recommended."
  :type 'boolean
  :group 'ido-ubiquitous)

;;; ido-ubiquitous core

;; These variable are used to make ido-ubiquitous work properly in the
;; case that `completing-read' is called recursively (which is
;; possible when `enable-recursive-minibuffers' is non-nil.)
(defvar ido-ubiquitous-enable-next-call nil
  "If non-nil, then the next call to `ido-completing-read' is by ido-ubiquitous.")
(defvar ido-ubiquitous-enable-this-call nil
  "If non-nil, then the current call to `ido-completing-read' is by ido-ubiquitous.")
(defvar ido-ubiquitous-next-override nil
  "This holds the override to be applied on the next call to `completing-read'.

It's value can be nil or one of the symbols `disable', `enable', or
`enable-old'.

You should not modify this variable directly. Instead use the
macro `ido-ubiquitous-with-override'.")
(defvar ido-ubiquitous-active-override nil
  "This holds the override being applied to the current call to `completing-read'.

It's value can be nil or one of the symbols `disable', `enable', or
`enable-old'.

You should not modify this variable directly. Instead use the
macro `ido-ubiquitous-with-override'.")
(defvar ido-ubiquitous-active-state nil
  "This holds the ido-ubiquitous mode of operation for the current call to `completing-read'.

It's value can be nil or one of the symbols `disable', `enable', or
`enable-old'.

You should not modify this variable directly.")

(defadvice ido-completing-read (around ido-ubiquitous activate)
  "Enable ido-ubiquitous features if this call was done through ido-ubiquitous.

This advice ensures that `ido-ubiquitous-enable-this-call' is set
properly while `ido-completing-read' is executing. This variable
is used to determine whether to enable certain behaviors only for
ido-ubiquitous, not for ordinary ido completion."
  ;; Set "this" and clear "next" so it doesn't apply to nested calls.
  (let* ((ido-ubiquitous-enable-this-call ido-ubiquitous-enable-next-call)
         (ido-ubiquitous-enable-next-call nil))
    ad-do-it))

;; Signal used to trigger fallback
(define-error 'ido-ubiquitous-fallback "ido-ubiquitous-fallback")

(defun completing-read-ido-ubiquitous
    (prompt collection &optional predicate
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
        (orig-args
         (list prompt collection predicate require-match
               initial-input hist def inherit-input-method)))
    (condition-case nil
        (let* (;; Set the active override and clear the "next" one so it
               ;; doesn't apply to nested calls.
               (ido-ubiquitous-active-override ido-ubiquitous-next-override)
               (ido-ubiquitous-next-override nil)
               ;; Apply the active override, if any
               (ido-ubiquitous-active-state
                (or ido-ubiquitous-active-override
                    ido-ubiquitous-default-state
                    'enable)))
          ;; If ido-ubiquitous is disabled this time, fall back
          (when (eq ido-ubiquitous-active-state 'disable)
            (signal 'ido-ubiquitous-fallback nil))
          ;; Handle a collection that is a function: either expand
          ;; completion list now or fall back
          (when (functionp collection)
            (if (or ido-ubiquitous-allow-on-functional-collection
                    (memq ido-ubiquitous-active-override
                          '(enable enable-old)))
                (setq collection (all-completions "" collection predicate))
              (signal 'ido-ubiquitous-fallback nil)))
          (let ((ido-ubiquitous-enable-next-call t))
            (ido-completing-read+
             prompt collection predicate require-match
             initial-input hist def inherit-input-method)))
      ;; Handler for ido-ubiquitous-fallback signal
      (ido-ubiquitous-fallback
       (message "Ido-ubiquitous falling back")
       (apply ido-cr+-fallback-function orig-args)))))
(define-obsolete-function-alias #'completing-read-ido #'completing-read-ido-ubiquitous
  "ido-ubiquitous 3.0")

;;; Old-style default support

(defvar ido-ubiquitous-initial-item nil
  "The first item selected when ido starts.

This is initialized to the first item in the list of completions
when ido starts, and is cleared when any character is entered
into the prompt or the list is cycled. If it is non-nil and still
equal to the first item in the completion list when ido exits,
then if `ido-ubiquitous-active-state' is `enable-old', ido
returns an empty string instead of the first item on the list.")

(defadvice ido-read-internal (before clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-make-choice-list (after set-initial-item activate)
  (setq ido-ubiquitous-initial-item
        (when (and ad-return-value (listp ad-return-value))
          (car ad-return-value))))

(defadvice ido-next-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-prev-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defun ido-ubiquitous-should-use-old-style-default ()
  "Returns non nil if ido-ubiquitous should emulate old-style default.

This function simply encapsulates all the checks that need to be
done in order to decide whether to swap RET and C-j. See
`ido-ubiquitous-default-state' for more information."
  (and
   ;; Only if completing a list, not a buffer or file
   (eq ido-cur-item 'list)
   ;; Only if this call was done through ido-ubiquitous
   ido-ubiquitous-enable-this-call
   ;; Only if default is nil
   (null ido-default-item)
   ;; Only if input is empty
   (string= ido-text "")
   ;; Only if old-style default enabled
   (eq ido-ubiquitous-active-state 'enable-old)
   ;; Only if `ido-ubiquitous-initial-item' hasn't been cleared
   ido-ubiquitous-initial-item
   ;; Only if initial item hasn't changed
   (string= (car ido-cur-list)
            ido-ubiquitous-initial-item)))

(defadvice ido-exit-minibuffer (around old-style-default-compat activate)
  "Emulate a quirk of `completing-read'.

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

See `ido-ubiquitous-default-state', which controls whether this
advice has any effect."
  (condition-case nil
      (if (ido-ubiquitous-should-use-old-style-default)
          (ido-select-text)
        ad-do-it)
    (error ad-do-it))
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-select-text (around old-style-default-compat activate)
  "Emulate a quirk of `completing-read'.

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

See `ido-ubiquitous-default-state', which controls whether this
advice has any effect."
  (condition-case nil
      (if (ido-ubiquitous-should-use-old-style-default)
          (ido-exit-minibuffer)
        ad-do-it)
    (error ad-do-it))
  (setq ido-ubiquitous-initial-item nil))

;;; Overrides

(defun ido-ubiquitous-restore-default-overrides (&optional save)
  "Re-add the default overrides for ido-ubiquitous.

This will ensure that the default overrides are all present and
at the head of the list in `ido-ubiquitous-command-overrides' and
`ido-ubiquitous-function-overrides'. User-added overrides will
not be removed, but they may be masked if one of the default
overrides affects the same functions.

With a prefix arg, also save the above variables' new values for
future sessions."
  (interactive "P")
  (let ((setter (if save
                    'customize-save-variable
                  'customize-set-variable)))
    (cl-loop for (var def) in '((ido-ubiquitous-command-overrides
                                 ido-ubiquitous-default-command-overrides)
                                (ido-ubiquitous-function-overrides
                                 ido-ubiquitous-default-function-overrides))
             do (let* ((curval (eval var))
                       (defval (eval def))
                       (newval (delete-dups (append defval curval))))
                  (funcall setter var newval)))
    (message (if save
                 "ido-ubiquitous: Restored default command and function overrides and saved for future sessions."
               "ido-ubiquitous: Restored default command and function overrides for current session only. Call again with prefix to save for future sessions."))))

(defun ido-ubiquitous-spec-match (spec symbol)
  "Returns t if SPEC matches SYMBOL (which should be a function name).

See `ido-ubiquitous-command-overrides'."
  (when (and symbol (symbolp symbol))
    (cl-destructuring-bind (type text) spec
      (let ((matcher (cdr (assoc type ido-ubiquitous-spec-matchers)))
            (text (ido-ubiquitous--as-string text))
            (symname (ido-ubiquitous--as-string symbol)))
        (when (null matcher)
          (error "ido-ubiquitous: Unknown match spec type \"%s\". See `ido-ubiquitous-spec-matchers' for valid types." type))
        (funcall matcher text symname)))))

(defun ido-ubiquitous-get-command-override (cmd)
  "Return the override associated with the command CMD.

If there is no override set for CMD in
`ido-ubiquitous-command-overrides', return nil."
  (when (and cmd (symbolp cmd))
    (cl-loop for (action . spec) in ido-ubiquitous-command-overrides
             when (memq action '(disable enable enable-old nil))
             when (ido-ubiquitous-spec-match spec cmd)
             return action
             finally return nil)))

;;; Workaround for https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/24

;; When `call-interactively' is advised, `called-interactively-p'
;; always returns nil. So we redefine it (and `interactive-p') to test
;; the correct condition.

(defsubst ido-ubiquitous--looks-like-advised-orig (func)
  "Returns t if FUNC is a symbol starting with \"ad-Orig-\".

Such symbols are used to store the original definitions of
functions that have been advised by `defadvice' or similar."
  (and (symbolp func)
       (string-prefix-p "ad-Orig-" (symbol-name func))))

(defsubst ido-ubiquitous--looks-like-call-interactively (func)
  "Returns t if FUNC looks like the function `call-interactively'.

FUNC \"looks like\" `call-interactively' if it is the literal
symbol `call-interactively', or the value of `(symbol-function
'call-interactively)', or a symbol whose `symbol-function' is the
same as that of `call-interactively'.

This function is used to determine whether a given function was
\"called by\" `call-interactively' and therefore was called
interactively."
  (when func
    (eq (symbol-function 'call-interactively)
        (if (symbolp func)
            (symbol-function func)
          func))))

(defun ido-ubiquitous--backtrace-from (fun)
  "Return all backtrace frames, starting with the one for FUN.

FUN may be a list of functions, in which case the first one found
on the stack will be used."
  (let ((stack
         (cl-loop for i upfrom 0
                  for frame = (backtrace-frame i)
                  while frame
                  collect frame))
        (funcs (if (functionp fun)
                   (list fun)
                 fun)))
    (while (and stack
                (not (memq (cl-cadar stack) funcs)))
      (setq stack (cdr stack)))
    stack))

(defun ido-ubiquitous--clean-advice-from-backtrace (stack)
  "Takes a stack trace and cleans all evidence of advice.

Specifically, for each call to a function starting with
\"ad-Orig-\", that call and all prior calls up to but not
including the advised function's original name are deleted from
the stack."
  (let ((skipping-until nil))
    (cl-loop for frame in stack
             for func = (cadr frame)
             ;; Check if we found the frame we we're skipping to
             if (and skipping-until
                     (eq func skipping-until))
             do (setq skipping-until nil)
             ;; If we're looking at an the original form of an advised
             ;; function, skip until the real name of that function.
             if (and (not skipping-until)
                     (ido-ubiquitous--looks-like-advised-orig func))
             do (setq skipping-until
                      (intern
                       (substring (symbol-name func)
                                  (eval-when-compile (length "ad-Orig-")))))
             unless skipping-until collect frame)))

(defsubst ido-ubiquitous--interactive-internal ()
  "Eqivalent of the INTERACTIVE macro in the Emacs C source.

This is an internal function that should never be called
directly.

See the C source for the logic behind this function."
  (and (not executing-kbd-macro)
       (not noninteractive)))

(defun ido-ubiquitous--interactive-p-internal ()
  "Equivalent of C function \"interactive_p\".

This is an internal function that should never be called
directly.

See the C source for the logic behind this function."
  (let ((stack
         ;; We clean advice from the backtrace. This ensures that we
         ;; get the right answer even if `call-interactively' has been
         ;; advised.
         (ido-ubiquitous--clean-advice-from-backtrace
          (cdr
           (ido-ubiquitous--backtrace-from
            '(called-interactively-p interactive-p))))))
    ;; See comments in the C function for the logic here.
    (while (and stack
                (or (eq (cl-cadar stack) 'bytecode)
                    (null (caar stack))))
      (setq stack (cdr stack)))
    ;; Top of stack is now the function that we want to know
    ;; about. Pop it, then check if the next function is
    ;; `call-interactively', using a more permissive test than the default.
    (ido-ubiquitous--looks-like-call-interactively (cl-cadadr stack))))

(defadvice call-interactively (around ido-ubiquitous activate)
  "Implements the behavior specified in `ido-ubiquitous-command-overrides'."
  (ido-ubiquitous-with-override
   (ido-ubiquitous-get-command-override (ad-get-arg 0))
   ad-do-it))

;; Work around `called-interactively-p' in Emacs 24.3 and earlier,
;; which always returns nil when `call-interactively' is advised.
(when (not (and (featurep 'nadvice)
                (boundp 'called-interactively-p-functions)))

  (defadvice interactive-p (around ido-ubiquitous activate)
    "Return the correct result when `call-interactively' is advised.

This advice completely overrides the original definition."
    (condition-case nil
        (setq ad-return-value
              (and (ido-ubiquitous--interactive-internal)
                   (ido-ubiquitous--interactive-p-internal)))
      ;; In case of error in the advice, fall back to the default
      ;; implementation
      (error ad-do-it)))

  (defadvice called-interactively-p (around ido-ubiquitous activate)
    "Return the correct result when `call-interactively' is advised.

This advice completely overrides the original definition."
    (condition-case nil
        (setq ad-return-value
              (and (or (ido-ubiquitous--interactive-internal)
                       (not (eq kind 'interactive)))
                   (ido-ubiquitous--interactive-p-internal)))
      ;; In case of error in the advice, fall back to the default
      ;; implementation
      (error ad-do-it))))

;;; Other

(defun ido-ubiquitous-initialize ()
  "Do initial setup for ido-ubiquitous.

This only needs to be called once when the file is first loaded."
  ;; Clean up old versions of ido-ubiquitous advice if they exist
  (ignore-errors (ad-remove-advice 'completing-read 'around 'ido-ubiquitous))
  (ignore-errors (ad-remove-advice 'ido-completing-read 'around 'detect-replacing-cr))
  (ignore-errors (ad-remove-advice 'ido-magic-forward-char 'before 'ido-ubiquitous-fallback))
  (ignore-errors (ad-remove-advice 'ido-magic-backward-char 'before 'ido-ubiquitous-fallback))
  (ignore-errors (ad-remove-advice 'ido-exit-minibuffer 'around 'compatibility))
  (ad-activate-all)
  ;; Make sure the mode is turned on/off as specified by the value of
  ;; the mode variable
  (ido-ubiquitous-mode (if ido-ubiquitous-mode 1 0)))
(ido-ubiquitous-initialize)

(provide 'ido-ubiquitous)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ido-ubiquitous.el ends here
