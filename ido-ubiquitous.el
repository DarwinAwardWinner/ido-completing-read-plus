;;; ido-ubiquitous.el --- Use ido (nearly) everywhere.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Version: 2.0
;; Created: 2011-09-01
;; Keywords: convenience
;; EmacsWiki: InteractivelyDoThings

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You may have seen the `ido-everywhere' variable in ido.el and got
;; excited that you could use ido completion for everything. Then you
;; were probably disappointed when you realized that it only applied
;; to *file names* and nothing else. Well, ido-ubiquitous is here to
;; fulfill the original promise and let you use ido completion for
;; (almost) any command that uses `completing-read' to offer you a
;; choice of several alternatives.

;; This even works in M-x, but for that, you might prefer the "smex"
;; package instead.

;; As of version 0.7, this package also makes a small modification to
;; ido's behavior so as to support a strange corner case of
;; `completing-read' that some functions rely on. Since the goal of
;; this package is to replace `completing-read' everywhere instead of
;; just selectively (as ido itself does), compatibility with all the
;; quriks of `completing-read' is important here.

;; If you find a case where enabling ido-ubiquitous causes a command
;; not to work correctly, please report it by creating an issue on
;; GitHub: https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Check for required feature
(eval-when-compile
  (when (not (boundp 'completing-read-function))
      (error "Could not find variable `completing-read-function'. Are you using Emacs version 24 or higher? If you have Emacs 23 or lower, please downgrade to ido-ubiquitous version 1.6.")))

(require 'ido)
(require 'advice)
(require 'cl)

;;; Custom Declarations

;;;###autoload
(defgroup ido-ubiquitous nil
  "Use ido for (almost) all completion."
  :group 'ido)

;;;###autoload
(define-minor-mode ido-ubiquitous-mode
  "Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can force the
  function to use the original completing read by using the macro
  `ido-ubiquitous-disable-in'. For example, if a
  function `foo' cannot work with ido-style completion, evaluate
  the following (for example by putting it in your .emacs file):

    (ido-ubiquitous-disable-in foo)"

  nil
  :global t
  :group 'ido-ubiquitous
  ;; Handle warning about ido disabled
  (when ido-ubiquitous-mode
    (unless (bound-and-true-p ido-mode)
      (if after-init-time
	  (ido-ubiquitous-warn-about-ido-disabled)
	(add-hook 'after-init-hook 'ido-ubiquitous-warn-about-ido-disabled))))
  ;; Ensure emacs 23 code disabled
  (ignore-errors
    (ad-disable-advice 'completing-read 'around 'ido-ubiquitous-legacy)
    (ad-activate 'completing-read))
  ;; Actually enable/disable the mode
  (setq completing-read-function
	(if ido-ubiquitous-mode
	    'completing-read-ido
	  ido-ubiquitous-fallback-completing-read-function)))

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")
;;;###autoload
(define-obsolete-function-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")

;;;###autoload
(defcustom ido-ubiquitous-fallback-completing-read-function
  ;; Initialize to the current value of `completing-read-function',
  ;; unless that is already set to the ido completer, in which case
  ;; use `completing-read-default'.
  (if (eq completing-read-function 'completing-read-ido)
      'completing-read-default
    completing-read-function)
  "Alternate completing-read function to use when ido is not wanted.

This will be used for functions that are incompatibile with ido
or if ido cannot handle the completion arguments.

If you turn off ido-ubiquitous mode, `completing-read-function'
will be set to this."
  :type '(choice (const :tag "Standard emacs completion"
			completing-read-default)
		 (function :tag "Other function"))
  :group 'ido-ubiquitous)

;;;###autoload
(defcustom ido-ubiquitous-command-exceptions '()
  "List of commands that should not be affected by `ido-ubiquitous'.

Even when `ido-ubiquitous' mode is enabled, these commands will
continue to use `completing-read' instead of
`ido-completing-read'.

Only *interactive* commands should go here. To disable
ido-ubiquitous in non-interactive functions, customize
`ido-ubiquitous-function-exceptions'.

Note: this feature depends on the variable `this-command' being
properly set to the name of the currently executing command.
Depending on how the command is onvoked, this may or may not
happen, so this feature may simply not work in some cases."
  ;; This isn't actually a hook, but it is a list of functions, so
  ;; that's close enough.
  :type 'hook
  :group 'ido-ubiquitous)

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous-exceptions
  'ido-ubiquitous-command-exceptions "0.4")

(defvar ido-ubiquitous-default-function-exceptions
  '(read-file-name
    read-file-name-internal
    read-buffer
    gnus-emacs-completing-read
    gnus-iswitchb-completing-read
    man
    grep-read-files)
  "Default value of `ido-ubiquitous-function-exceptions'")

(defun ido-ubiquitous-set-function-exceptions (sym newval)
  ;; Loop through all functions, enabling or disabling ido-ubiquitous
  ;; as appropriate.
  (mapatoms
   (lambda (sym)
     (if (memq sym newval)
	 (eval `(ido-ubiquitous-disable-in ,sym))
       (eval `(ido-ubiquitous-enable-in ,sym)))))
  ;; Set the new value
  (set-default sym newval))

(defmacro ido-ubiquitous-disable-in (func)
  "Disable ido-ubiquitous in FUNC."
  (put func 'ido-ubiquitous-disable t)
  (let ((docstring
         (format "Disable ido-ubiquitous in %s if its `idu-ubiquitous-disable' property is non-nil." func)))
    `(defadvice ,func (around disable-ido-ubiquitous activate)
       ,docstring
       (let ((ido-ubiquitous-mode
	      (and ido-ubiquitous-mode
		   (not (get ',func 'ido-ubiquitous-disable)))))
	 ad-do-it))))

(define-obsolete-function-alias
  'disable-ido-ubiquitous-in
  'ido-ubiquitous-disable-in
  "0.4")

(defmacro ido-ubiquitous-enable-in (func)
  "Re-enable ido-ubiquitous in FUNC.

  This reverses the effect of a previous call to
  `ido-ubiquitous-disable-in' (if no such call was made, this is
  a no-op.)"
  `(when (get ',func 'ido-ubiquitous-disable)
     (put ,func 'ido-ubiquitous-disable nil)))

(define-obsolete-function-alias
  'enable-ido-ubiquitous-in
  'ido-ubiquitous-enable-in
  "0.4")

;;;###autoload
(defcustom ido-ubiquitous-function-exceptions
  ido-ubiquitous-default-function-exceptions
  "List of functions in which to disable ido-ubiquitous.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

Setting this variable directly has no effect. You must set it
through Customize."
  :group 'ido-ubiquitous
  :type 'hook
  :options '(read-file-name
	     read-file-name-internal
	     read-buffer
	     gnus-emacs-completing-read
	     gnus-iswitchb-completing-read
	     man
	     grep-read-files)
  :set 'ido-ubiquitous-set-function-exceptions)

;;; Ido-ubiquitous core

(defvar ido-ubiquitous-next-call-replaces-completing-read nil
  "If t, then the next call to ido-completing-read is by ido-ubiquitous.")
(defvar ido-ubiquitous-this-call-replaces-completing-read nil
  "If t, then the current call to ido-completing-read is by ido-ubiquitous.")
(defvar ido-ubiquitous-disable-for-one-command nil
  "If t, disable ido-ubiquitous for the next command.")


(defun completing-read-ido (prompt collection &optional predicate
                                   require-match initial-input
                                   hist def inherit-input-method)
  "Ido-based method for reading from the minibuffer with completion.
See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'."
  (let ((comp-read-fun
	 (cond
	  (ido-ubiquitous-disable-for-one-command
	   (prog1
	       ido-ubiquitous-fallback-completing-read-function
	     (setq ido-ubiquitous-disable-for-one-command nil)))
	  ((or inherit-input-method          ; Can't handle this arg
	       (bound-and-true-p completion-extra-properties) ; Can't handle this
	       (not ido-mode)
	       (not ido-ubiquitous-mode)
	       (memq this-command ido-ubiquitous-command-exceptions))
	   ido-ubiquitous-fallback-completing-read-function)
	  ((let ((allcomp (all-completions "" collection predicate)))
	     ;; Only use ido completion if there are actually any completions
	     ;; to offer.
	     (if allcomp
		 (prog1 'ido-ubiquitous-completing-read
		   (setq collection allcomp
			 predicate nil))
	       ido-ubiquitous-fallback-completing-read-function))))))
    (funcall comp-read-fun
	     prompt collection predicate
	     require-match initial-input
	     hist def inherit-input-method)))

(defun ido-ubiquitous-completing-read (&rest args)
  "Wrapper for `ido-completing-read' that enables ido-ubiquitous features."
  (let ((ido-ubiquitous-next-call-replaces-completing-read t))
    (apply 'ido-completing-read args)))

(defadvice ido-completing-read (around detect-replacing-cr activate)
  "Detect whether this call was done through ido-ubiquitous.

If so enable, extra features for ido-ubiquitous that handle
special cases that `ido-completing-read' needs to handle to more
fully emulate `completing-read'. This allows ido-ubiquitous extra
features to avoid interfering with the normal operation of ido."
  (let* ((ido-ubiquitous-this-call-replaces-completing-read ido-ubiquitous-next-call-replaces-completing-read)
         (ido-ubiquitous-next-call-replaces-completing-read nil))
    (when ido-ubiquitous-this-call-replaces-completing-read
      ;; If DEF is a list, prepend it to CHOICES and set DEF to just the
      ;; car of the default list.
      (when (and def (listp def))
        (setq choices (delete-dups (append def choices))
              def (car def)))
      ;; Work around a bug in ido when both INITIAL-INPUT and DEF are provided
      ;; More info: https://github.com/technomancy/ido-ubiquitous/issues/18
      (let ((initial (cond ((null initial-input) "")
                           ((stringp initial-input) initial-input)
                           ((consp initial-input) (car initial-input))
                           (t initial-input)))
            (deflist (if (listp def)
                         def
                       (list def))))
        (when (and deflist initial
                   (stringp initial)
                   (not (string= initial "")))
          ;; Both default and initial input were provided. So keep the
          ;; initial input and preprocess the choices list to put the
          ;; default at the head, then proceed with default = nil.
          (setq choices (delete-dups (append deflist (remove def choices)))
                def nil))))
    ad-do-it))

;;; Ido-ubiquitous interactive command exceptions

;; The following advices should allow ido-ubiquitous to apply to the
;; interactive forms of commands as well as their bodies.
(defadvice call-interactively (around ido-ubiquitous-command-exceptions activate)
  (let ((ido-ubiquitous-disable-for-one-command
	 (memq function ido-ubiquitous-command-exceptions)))
    ad-do-it))

(defadvice command-execute (around ido-ubiquitous-command-exceptions activate)
  (let ((ido-ubiquitous-disable-for-one-command
	 (and (commandp cmd t)
	      (memq cmd ido-ubiquitous-command-exceptions))))
    ad-do-it))

;;; Ido-ubiquitous function exceptions

;;; Ido-ubiquitous compatibility with old completing-read

;;;###autoload
(defgroup ido-ubiquitous-compatibility nil
  "Use ido for (almost) all completion."
  :group 'ido-ubiquitous)

(defcustom ido-ubiquitous-enable-compatibility-globally t
  "Allow ido to emulate a quirk of `completing-read'.

From the `completing-read' docstring:

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

If this variable is non-nil, then ido-ubiquitous will attempt to
emulate this behavior. Specifically, if RET is pressed
immediately upon entering completion, an empty string will be
returned instead of the first element in the list. This behavior
is only enabled when ido is being used as a substitute for
`completing-read', and not when it is used directly.

This odd behavior is required for compatibility with an old-style
usage pattern whereby the default was requested by returning an
empty string. In this mode, the caller receives the empty string
and handles the default case manually, while `completing-read'
never has any knowledge of the default. This is a problem for
ido, which always returns the first element in the list when the
input is empty. Without knowledge of the default, it cannot
ensure that the default is first on the list, so returning the
first item is not the correct behavior. Instead, it must return
an empty string like `completing-read'.

When this mode is enabled, you can still select the first item on
the list by prefixing \"RET\" with \"C-u\".

If you want to enable compatibility selectively for specific
commands or functions, see the other options in the
`ido-ubiquitous-compatibility' group."
  :type 'boolean
  :group 'ido-ubiquitous-compatibility)

;;;###autoload
(defcustom ido-ubiquitous-command-compatibility-list '()
  "List of commands in which to enable old-style compatibility.

See `ido-ubiquitous-enable-compatibility-globally' for a
description of the compatibility behavior. If ido doesn't
properly select the default for a command, try adding it to this
list.

Only *interactive* commands should go here. To disable
compatibility mode in non-interactive functions, customize
`ido-ubiquitous-function-compatibility-exceptions'."
  :type 'hook
  :group 'ido-ubiquitous-compatibility)

(defmacro ido-ubiquitous-enable-compatibility-in (func)
  "Enable ido-ubiquitous old-style compatibility in FUNC."
  (let ((docstring
         (format "Enable ido-ubiquitous old-style compatibility in %s if its `idu-ubiquitous-enable-compatibility' property is non-nil." func)))
    `(progn
       (defadvice ,func (around disable-ido-ubiquitous activate)
	 ,docstring
	 (let ((ido-ubiquitous-enable-compatibility-globally
		(or ido-ubiquitous-enable-compatibility-globally
		    (get ',func 'ido-ubiquitous-enable-compatibility))))
	   ad-do-it))
       (put func 'ido-ubiquitous-enable-compatibility t))))

(defmacro ido-ubiquitous-disable-compatibility-in (func)
  "Enable ido-ubiquitous old-style compatibility in FUNC.

  This reverses the effect of a previous call to
  `ido-ubiquitous-enable-compatibility-in' (if no such call was
  made, this is a no-op.)"
  `(when (get ',func 'ido-ubiquitous-enable-compatibility)
     (put ,func 'ido-ubiquitous-enable-compatibility nil)))

(defun ido-ubiquitous-set-function-compatibility-list (sym newval)
  ;; Loop through all functions, enabling or disabling ido-ubiquitous
  ;; as appropriate.
  (mapatoms
   (lambda (sym)
     (if (memq sym newval)
	 (eval `(ido-ubiquitous-enable-compatibility-in ,sym))
       (eval `(ido-ubiquitous-disable-compatibility-in ,sym)))))
  ;; Set the new value
  (set-default sym newval))

;;;###autoload
(defcustom ido-ubiquitous-function-compatibility-list
  '()
  "List of functions in which to enable old-style compatibility.

See `ido-ubiquitous-enable-compatibility-globally' for a
description of the compatibility behavior. If ido doesn't
properly select the default selection for a function, try adding
it to this list.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues"
  :group 'ido-ubiquitous-compatibility
  :type 'hook
  :set 'ido-ubiquitous-set-function-compatibility-list)

(defvar ido-ubiquitous-initial-item nil
  "The first item selected when ido starts.")

(defadvice ido-read-internal (before clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-make-choice-list (after set-initial-item activate)
  (when (and ad-return-value (listp ad-return-value))
    (setq ido-ubiquitous-initial-item (car ad-return-value))))

(defadvice ido-next-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-prev-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-exit-minibuffer (around compatibility activate)
  "Emulate a quirk of `completing-read'.

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

See `ido-ubiquitous-enable-compatibility-globally', which
controls whether this advice has any effect."
  (if (and (eq ido-cur-item 'list)
           ;; Only enable if we are replacing `completing-read'
           ido-ubiquitous-this-call-replaces-completing-read
	   ;; Compatibiliy enabled
           (or ido-ubiquitous-enable-compatibility-globally
	       (memq this-command ido-ubiquitous-command-compatibility-list))
           ;; Input is empty
           (string= ido-text "")
           ;; Default is nil
           (null ido-default-item)
           ;; Prefix disables compatibility
           (not current-prefix-arg)
           (string= (car ido-cur-list)
                    ido-ubiquitous-initial-item))
      (ido-select-text)
    ad-do-it)
  (setq ido-ubiquitous-initial-item nil))

(defadvice call-interactively (around ido-ubiquitous-oldstyle-compatibility activate)
  (let ((ido-ubiquitous-enable-compatibility-globally
	 (or ido-ubiquitous-enable-compatibility-globally
	     (memq function ido-ubiquitous-command-compatibility-list))))
    ad-do-it))

(defadvice command-execute (around ido-ubiquitous-oldstyle-compatibility activate)
  (let ((ido-ubiquitous-enable-compatibility-globally
	 (or ido-ubiquitous-enable-compatibility-globally
	     (memq function ido-ubiquitous-command-compatibility-list))))
    ad-do-it))

;;; Other

(defun ido-ubiquitous-initialize ()
  "Do initial setup for ido-ubiquitous.

This only needs to be called once when the file is first loaded."
  ;; Clean up old versions of ido-ubiquitous that defined advice on
  ;; `completing-read' instead of modifying
  ;; `completing-read-function'.
  (when (ad-find-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-remove-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-activate 'completing-read))
  ;; Make sure all exceptions are activated
  (ido-ubiquitous-set-function-exceptions
   'ido-ubiquitous-function-exceptions
   ido-ubiquitous-function-exceptions)
  (ido-ubiquitous-set-function-compatibility-list
   'ido-ubiquitous-function-compatibility-list
   ido-ubiquitous-function-compatibility-list)
  ;; Make sure the mode is turned on/off as specified by the value of
  ;; the mode variable
  (ido-ubiquitous-mode (if ido-ubiquitous-mode 1 0)))
(ido-ubiquitous-initialize)

(defun ido-ubiquitous-warn-about-ido-disabled ()
  "Warn if ido-ubiquitous is enabled without ido.

Don't warn if emacs is still initializing, since ido-ubiquitous
could be enabled first during init."
  (if (and after-init-time
	   (not (bound-and-true-p ido-mode)))
      (warn "Ido-ubiquitous-mode enabled without ido mode. Ido-ubiquitous requires ido mode to be enabled.")))

(provide 'ido-ubiquitous) ;;; ido-ubiquitous.el ends here
