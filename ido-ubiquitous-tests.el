;;; ido-ubiquitous-tests.el ---  -*- "lexical-binding": t -*-

;; Copyright (C) 2015 Ryan C. Thompson

;; Filename: ido-ubiquitous-tests.el
;; Author: Ryan C. Thompson
;; Created: Tue Oct  6 20:52:45 2015 (-0700)
;; Version: 
;; Package-Requires: ()
;; URL: 
;; Keywords: 

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 

;; 

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

(ert-deftest ido-ubiquitous-test ()
  "Do some ido-ubiquitous tests"
  (with-mode ido-ubiquitous-mode 1
    (should
     (string=
      "green"
      (with-simulated-input "g RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    (should
     (string=
      "g"
      (with-simulated-input "g C-j"
        (completing-read "Prompt: " '("blue" "yellow" "green"))))))
  ;; Normal completing-read
  (with-mode ido-ubiquitous-mode 0
    ;; No match required
    (should
     (string=
      "g"
      (with-simulated-input "g RET"
        (completing-read "Prompt: " '("blue" "yellow" "green")))))
    ;; Match required, so input should be incomplete
    (should-error
     (with-simulated-input "g RET"
       (completing-read "Prompt: " '("blue" "yellow" "green") nil 'require-match))
     :type 'error))))

(provide 'ido-ubiquitous-tests)

;;; ido-ubiquitous-tests.el ends here
