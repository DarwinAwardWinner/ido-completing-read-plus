;;; ido-ubiquitous-test.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ryan C. Thompson

;; Filename: ido-ubiquitous-test.el
;; Author: Ryan C. Thompson
;; Created: Tue Oct  6 20:52:45 2015 (-0700)

;; This file is NOT part of GNU Emacs.

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

;; This is a series of macros to facilitate the testing of completion
;; non-interactively by simulating input.

(require 'ido-ubiquitous)
(require 'ert)
(require 'cl-macs)

(defmacro keyboard-quit-to-error (&rest body)
  "Evaluate BODY but signal an error on `keyboard-quit'."
  `(condition-case nil
       (progn ,@body)
     (quit
      (error "Caught `keyboard-quit'"))))

(defmacro with-simulated-input (keys &rest body)
  "Eval body with KEYS as simulated input.

This macro is intended for testing normally interactive functions
by simulating input. If BODY tries to read more input events than
KEYS provides, `keyboard-quit' is invoked (by means of appending
multple C-g keys to KEYS). This is to ensure that BODY will never
block waiting for input, since this macro is intended for
noninteractive use. As such, BODY should not invoke
`keyboard-quit' under normal operation, and KEYS should not
include C-g, or this macro will interpret it as reading past the
end of input."
  ;; It would be better to detect end-of-input by overriding
  ;; `read-event' to throw an error, since theoretically C-g could be
  ;; rebound to something other than `keyboard-quit'. But apparently
  ;; some functions read input directly in C code, and redefining
  ;; `read-event' has no effect on those. So the suboptimal solution
  ;; is to rely on C-g.
  (declare (indent 1))
  `(let* ((key-sequence (listify-key-sequence (kbd ,keys)))
          (C-g-key-sequence
           (listify-key-sequence
            ;; We *really* want to trigger `keyboard-quit' if we reach
            ;; the end of the input.
            (kbd "C-g C-g C-g C-g C-g C-g C-g")))
          (unread-command-events
           (append key-sequence C-g-key-sequence)))
     (when (member (car C-g-key-sequence) key-sequence)
       (error "KEYS must include C-g"))
     (condition-case nil
         ,@body
       (quit
        (error "Reached end of simulated input while evaluating body")))))

(defmacro with-mode (mode arg &rest body)
  "Eval (MODE ARG), then body, then restore previous status of MODE.

This will only work on modes that respect the normal conventions
for activation and deactivation."
  (declare (indent 2))
  (let* ((orig-status (eval mode))
         (restore-arg (if orig-status 1 0)))
    `(unwind-protect
         (progn
           (,mode ,arg)
           ,@body)
       (,mode ,restore-arg))))

(defmacro with-ido-ubiquitous-standard-env (&rest body)
  "Execute BODY with standard ido-ubiquitous settings.\n\nAll ido-ubiquitous and ido-cr+ options will be let-bound to their\ndefault values, and `ido-ubiquitous-mode' will be enabled."
  (declare (indent 0))
  (let*
      ((ido-ubiquitous-options
        '(ido-ubiquitous-allow-on-functional-collection
          ido-ubiquitous-command-overrides
          ido-ubiquitous-debug-mode
          ido-ubiquitous-default-state
          ido-ubiquitous-function-overrides
          ido-cr+-fallback-function
          ido-cr+-max-items
          ido-cr+-replace-completely))
       (idu-bindings
        (loop for var in ido-ubiquitous-options collect
              (list var
                    (list 'quote
                          (default-value var))))))
    `(with-mode ido-ubiquitous-mode 1
       (let ,idu-bindings ,@body))))

(defun collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  (completion-table-dynamic (lambda (string) (all-completions string collection))))
  "Test whether observed ido-ubiquitous behavior matches OVERRIDE."
  (if (eq override 'disable)
      (progn
        (should
         ;; Verify that we get standard completion
         (string=
          "g"
          (with-simulated-input "g RET"
            (completing-read "Prompt: " '("blue" "yellow" "green")))))
        ;; Test is disabled because of an apparent bug in Emacs:
        ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21644

        ;; ;; Match is required, so with standard completion the input should
        ;; ;; be incomplete and throw an error.
        ;; (should-error
        ;;  (with-simulated-input "g RET"
        ;;    (completing-read "Prompt: " '("blue" "yellow" "green") nil t))
        ;;  :type 'error)
        )
    ;; Common tests whenever ido-ubiquitous is enabled in any way
    (should
     ;; Verify that ido completion is active
     (string=
      "green"
      (with-simulated-input "g RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    ;; Verify that C-j is working correctly
    (should
     (string=
      "g"
      (with-simulated-input "g C-j"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    (let ((collection '("brown" "blue" "yellow" "green")))
      (should
       (member
        (with-simulated-input "b RET"
          (completing-read "Prompt: " collection))
        (all-completions "b" collection))))
    (case override
      (enable
       ;; Test for new style
       (should
        (string=
         "blue"
         (with-simulated-input "RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       (should
        (string=
         ""
         (with-simulated-input "C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))))
      (enable-old
       (should
        (string=
         ""
         (with-simulated-input "RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       (should
        (string=
         "blue"
         (with-simulated-input "C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       ;; Verify that doing other stuff reverts RET and C-j to standard
       ;; meanings
       (should
        (string=
         "blue"
         (with-simulated-input "g DEL RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       (should
        (string=
         "blue"
         (with-simulated-input "<right> <left> RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       (should
        (string=
         ""
         (with-simulated-input "g DEL C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t))))
       (should
        (string=
         ""
         (with-simulated-input "<right> <left> C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))))
      (otherwise (error "Unknown override %S" override)))))

(defun test-ido-ubiquitous-expected-mode-on-functional-collection (override)
  "Test whether observed ido-ubiquitous behavior on functional collection matches OVERRIDE."
  (cl-letf* ((original-completing-read (symbol-function #'completing-read))
             ((symbol-function #'completing-read)
              (lambda (prompt collection &rest args)
                (apply original-completing-read prompt
                       (collection-as-function collection)
                       args))))
    (test-ido-ubiquitous-expected-mode override)))

(ert-deftest ido-ubiquitous-test-simple ()
  "Test that basic ido-ubiquitous functionality is working."
  (with-ido-ubiquitous-standard-env
    (ido-ubiquitous-mode 1)
    (test-ido-ubiquitous-expected-mode 'enable)
    (ido-ubiquitous-mode 0)
    (test-ido-ubiquitous-expected-mode 'disable)))

(ert-deftest ido-ubiquitous-test-oldstyle ()
  (with-ido-ubiquitous-standard-env
    (let ((ido-ubiquitous-default-state 'enable-old))
      (test-ido-ubiquitous-expected-mode 'enable-old))))

(ert-deftest ido-ubiquitous-test-maxitems ()
  (with-ido-ubiquitous-standard-env
    (let ((ido-cr+-max-items -1))
      (test-ido-ubiquitous-expected-mode 'disable))))

(ert-deftest ido-ubiquitous-test-override ()
  (with-ido-ubiquitous-standard-env
    (ido-ubiquitous-with-override 'enable
      (test-ido-ubiquitous-expected-mode 'enable))
    (ido-ubiquitous-with-override 'enable-old
      (test-ido-ubiquitous-expected-mode 'enable-old))
    (ido-ubiquitous-with-override 'disable
      (test-ido-ubiquitous-expected-mode 'disable))))

(ert-deftest ido-ubiquitous-test-functional-collection ()
  (with-ido-ubiquitous-standard-env
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable)
    (ido-ubiquitous-with-override 'enable
      (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable))
    (ido-ubiquitous-with-override 'enable-old
      (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old))))

(ert-deftest ido-ubiquitous-require-match ()
  "Test whether require-match works."
  :expected-result :failed
  (should-error
   (with-simulated-input "b C-j"
     (ido-completing-read "Prompt: " '("blue" "brown" "yellow" "green") nil t))))

;; Functions to define overrides on for testing
(defun idu-no-override-testfunc ()
  (test-ido-ubiquitous-expected-mode 'enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable))
(defun idu-enabled-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable))
(defun idu-disabled-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'disable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable))
(defun idu-enabled-oldstyle-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'enable-old)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old))

;; commands to define overrides on for testing
(defun idu-no-override-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable)))
  (test-ido-ubiquitous-expected-mode 'enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable))
(defun idu-enabled-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable)))
  (test-ido-ubiquitous-expected-mode 'enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable))
(defun idu-disabled-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'disable)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable)))
  (test-ido-ubiquitous-expected-mode 'disable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable))
(defun idu-enabled-oldstyle-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable-old)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old)))
  (test-ido-ubiquitous-expected-mode 'enable-old)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old))

(ert-deftest ido-ubiquitous-test-command-and-function-overrides ()
  (let ((orig-func-overrides ido-ubiquitous-function-overrides)
        (orig-cmd-overrides ido-ubiquitous-command-overrides))
    (unwind-protect
        (progn
          (customize-set-variable
           'ido-ubiquitous-function-overrides
           (append ido-ubiquitous-function-overrides
                   '((enable exact "idu-enabled-testfunc")
                     (disable exact "idu-disabled-testfunc")
                     (enable-old exact "idu-enabled-oldstyle-testfunc"))))
          (loop for func in
                '(idu-no-override-testfunc
                  idu-enabled-testfunc
                  idu-disabled-testfunc
                  idu-enabled-oldstyle-testfunc)
                do (funcall func))
          (customize-set-variable
           'ido-ubiquitous-command-overrides
           (append ido-ubiquitous-command-overrides
                   '((enable exact "idu-enabled-testcmd")
                     (disable exact "idu-disabled-testcmd")
                     (enable-old exact "idu-enabled-oldstyle-testcmd"))))
          (loop for cmd in
                '(idu-no-override-testcmd
                  idu-enabled-testcmd
                  idu-disabled-testcmd
                  idu-enabled-oldstyle-testcmd)
                do (call-interactively cmd)))
      (customize-set-variable 'ido-ubiquitous-function-overrides orig-func-overrides)
      (customize-set-variable 'ido-ubiquitous-command-overrides orig-cmd-overrides))))

(ert-deftest ido-ubiquitous-test-fallback ()
  (with-ido-ubiquitous-standard-env
    (should
     ;; C-b/f not at beginning/end of input should not fall back
     (string=
      "green"
      (with-simulated-input "g C-b C-f RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; C-f at end of input should fall back
     (string=
      "g"
      (with-simulated-input "g C-f RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; Repeated C-b should not fall back
     (string=
      "green"
      (with-simulated-input "g C-b C-b C-b C-b RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; C-b at beginning of line should fall back (if previous action
     ;; was not also C-b)
     (string=
      "g"
      (with-simulated-input "g C-b x DEL C-b RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))))

(provide 'ido-ubiquitous-test)

;;; ido-ubiquitous-test.el ends here
