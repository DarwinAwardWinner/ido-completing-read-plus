;;; -*- lexical-binding: t -*-

(require 'ido-completing-read+)
(require 'ert)
(require 'cl-lib)

;; This is a series of macros to facilitate the non-interactive
;; testing of interactive functions by simulating user input.

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
       (error "KEYS must not include C-g"))
     (condition-case nil
         (progn ,@body)
       (quit
        (error "Reached end of simulated input while evaluating body")))))

(defmacro with-mode (mode arg &rest body)
  "Eval (MODE ARG), then body, then restore previous status of MODE.

This will only work on modes that respect the normal conventions
for activation and deactivation."
  (declare (indent 2))
  `(let* ((orig-status ,mode)
          (restore-arg (if orig-status 1 0)))
     (unwind-protect
         (progn
           (,mode ,arg)
           ,@body)
       (message "Restoring mode %s to %s" ',mode restore-arg)
       (,mode restore-arg))))

(defmacro with-ido-cr+-standard-env (&rest body)
  "Execute BODY with standard ido-cr+ settings.

All ido-cr+ options will be let-bound to their default values,
and `ido-ubiquitous-mode' will be enabled. The values will all br
restored to what they were previously after BODY exits."
  (declare (indent 0))
  (let*
      ((options
        '(ido-cr+-debug-mode
          ido-cr+-fallback-function
          ido-cr+-max-items
          ido-cr+-function-blacklist
          ido-cr+-function-whitelist
          ido-confirm-unique-completion))
       (bindings
        (cl-loop for var in options collect
                 (list var
                       (list 'quote
                             (eval (car (get var 'standard-value))))))))
    `(with-mode ido-ubiquitous-mode 1
       (let ,bindings ,@body))))

(defmacro collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  `(completion-table-dynamic (lambda (string) (all-completions string ,collection))))

(ert-deftest ido-cr+-test-basic ()
  :tags '(ido ido-cr+)
  "Test that basic ido-cr+ functionality is working."
  (with-ido-cr+-standard-env
    ;; Verify that pressing RET selects a matching item
    (should
     (string=
      "green"
      (with-simulated-input "g RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))

    ;; Verify that pressing RET with multiple matches selects the
    ;; first one
    (should
     (string=
      "brown"
      (with-simulated-input "b RET"
        (ido-completing-read+ "Prompt: " '("brown" "blue" "yellow" "green")))))

    ;; Verify that cycling with <left> and <right> works, including
    ;; wrap-around
    (should
     (string=
      "blue"
      (with-simulated-input "b <right> <right> <right> <right> <left> RET"
        (ido-completing-read+ "Prompt: " '("brown" "blue" "yellow" "green")))))

    ;; Verify that RET or C-j on empty input returns "" when
    ;; REQUIRE-MATCH is t
    (should
     (string=
      ""
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t))))
    (should
     (string=
      ""
      (with-simulated-input "C-j"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t))))

    ;; Verify that DEF works, whether or not it is an element of
    ;; COLLECTION
    (should
     (string=
      "green"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil "green"))))
    (should
     (string=
      "green"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t nil nil "green"))))
    (should
     (string=
      "brown"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil nil nil nil "brown"))))
    (should
     (string=
      "brown"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil t nil nil "brown"))))
    (should
     (string=
      "brown"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil t nil nil '("brown" "green")))))

    ;; Verify that INITIAL-INPUT works
    (should
     (string=
      "green"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr"))))

    ;; Verify that INITIAL-INPUT and DEF don't interfere with each other
    (should
     (string=
      "green"
      (with-simulated-input "RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))))
    (should
     (string=
      "blue"
      (with-simulated-input "DEL DEL RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))))

    ;; Verify that ido-cr+ works on function collections
    (should
     (string=
      "green"
      (with-simulated-input "g RET"
        (ido-completing-read+ "Prompt: " (collection-as-function '("blue" "yellow" "green"))))))))

(ert-deftest ido-cr+-test-maxitems ()
  :tags '(ido ido-cr+)
  "Test whether the large-collection fallback works."
  (with-ido-cr+-standard-env
    ;; This should not fall back
    (should
     (string=
      "green"
      (with-simulated-input "g RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))
    (let ((ido-cr+-max-items -1))
      ;; This should fall back because the collection is too large
      (should
       (string=
        "g"
        (with-simulated-input "g RET"
          (ido-completing-read+ "Prompt: " '("blue" "yellow" "green"))))))))

(ert-deftest ido-cr+-test-blacklist ()
  :tags '(ido ido-cr+)
  "Test whether `ido-ubiquitous-function-blacklist' works."
  (with-ido-cr+-standard-env
    (cl-letf (((symbol-function 'blacklisted-command)
               (lambda (arg)
                 (interactive (list (completing-read "Prompt: " '("blue" "yellow" "green"))))
                 arg))
              ((symbol-function 'blacklisted-function)
               (lambda ()
                 (completing-read "Prompt: " '("blue" "yellow" "green"))))
              ((symbol-function 'cmd-that-calls-blacklisted-function)
               (lambda ()
                 (interactive)
                 (funcall 'blacklisted-function)))
              ((symbol-function 'blacklisted-collection)
               (collection-as-function '("blue" "yellow" "green"))))
      ;; First verify that they work normally before blacklisting them
      (should
       (string=
        "green"
        (with-simulated-input "g RET"
          (call-interactively 'blacklisted-command))))
      (should
       (string=
        "green"
        (with-simulated-input "g RET"
          (call-interactively 'cmd-that-calls-blacklisted-function))))
      (should
       (string=
        "green"
        (with-simulated-input "g RET"
          (ido-completing-read+ "Prompt: " 'blacklisted-collection))))
      ;; Now add them to the blacklist and try again
      (let ((ido-cr+-function-blacklist
             (append '(blacklisted-command
                       blacklisted-function
                       blacklisted-collection)
                     ido-cr+-function-blacklist)))
        ;; All should now have ido-cr+ disabled
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (call-interactively 'blacklisted-command))))
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (call-interactively 'cmd-that-calls-blacklisted-function))))
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (ido-completing-read+ "Prompt: " 'blacklisted-collection))))))))

(ert-deftest ido-cr+-test-whitelist ()
  :tags '(ido ido-cr+)
  "Test whether `ido-ubiquitous-function-whitelist' works."
  (with-ido-cr+-standard-env
    (cl-letf (((symbol-function 'whitelisted-command)
               (lambda (arg)
                 (interactive (list (completing-read "Prompt: " '("blue" "yellow" "green"))))
                 arg))
              ((symbol-function 'whitelisted-function)
               (lambda ()
                 (completing-read "Prompt: " '("blue" "yellow" "green"))))
              ((symbol-function 'cmd-that-calls-whitelisted-function)
               (lambda ()
                 (interactive)
                 (funcall 'whitelisted-function)))
              ((symbol-function 'whitelisted-collection)
               (lambda (string pred action)
                 (complete-with-action action '("blue" "yellow" "green") string pred))))
      ;; Now add them to the whitelist
      (let ((ido-cr+-function-whitelist
             (append '(whitelisted-command
                       whitelisted-function
                       whitelisted-collection)
                     ido-cr+-function-whitelist)))
        ;; All should now have ido-cr+ enabled
        (should
         (string=
          "green"
          (with-simulated-input "g RET"
            (call-interactively 'whitelisted-command))))
        (should
         (string=
          "green"
          (with-simulated-input "g RET"
            (call-interactively 'cmd-that-calls-whitelisted-function))))
        (should
         (string=
          "green"
          (with-simulated-input "g RET"
            (ido-completing-read+ "Prompt: " 'whitelisted-collection)))))
      ;; Run again with nothing whitelisted
      (let ((ido-cr+-function-whitelist '(nil)))
        ;; All should now have ido-cr+ disabled
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (call-interactively 'whitelisted-command))))
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (call-interactively 'cmd-that-calls-whitelisted-function))))
        (should
         (string=
          "g"
          (with-simulated-input "g RET"
            (ido-completing-read+ "Prompt: " 'whitelisted-collection))))))))

(ert-deftest ido-cr+-require-match ()
  :tags '(ido ido-cr+)
  "Test whether REQUIRE-MATCH and DEF work as expected together."
  (with-ido-cr+-standard-env
    ;; "C-j" should be allowed to return an empty string even if
    ;; require-match is non-nil, as long as default is nil
    (should
     (string=
      ""
      (with-simulated-input "C-j"
        (ido-completing-read+
         "Prompt: "
         '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))))
    ;; "C-j" should NOT be allowed to return an empty string if
    ;; require-match and default are both non-nil.
    (should-error
     (with-simulated-input "C-j"
       (ido-completing-read+
        "Prompt: "
        '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t nil nil "yellow")))
    ;; Multiple presses of C-j won't just select the first match
    (should-error
     (with-simulated-input "b C-j C-j C-j"
       (ido-completing-read+
        "Prompt: "
        '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
    ;; First press of C-j should complete unique common prefix after the
    ;; first b, but then get stuck on the choice for the second b.
    (should-error
     (with-simulated-input "b C-j b C-j C-j"
       (ido-completing-read+
        "Prompt: "
        '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
    ;; This should complete to "blueberry" via 2 rounds of unique common
    ;; prefix completion, and then return on the 3rd "C-j"
    (should
     (string=
      "blueberry"
      (with-simulated-input "b C-j b C-j e C-j C-j"
        (ido-completing-read+
         "Prompt: "
         '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))))
    ;; The "C-j" should complete to "bluegrass" and return, because
    ;; `ido-confirm-unique-completion is nil.
    (should
     (string=
      "bluegrass"
      (with-simulated-input "b l u e g C-j"
        (ido-completing-read+
         "Prompt: "
         '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))))
    (let ((ido-confirm-unique-completion t))
      ;; Now the first "C-j" should complete to "bluegrass" but should
      ;; not return.
      (should-error
       (with-simulated-input "b l u e g C-j"
         (ido-completing-read+
          "Prompt: "
          '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
      ;; The first "C-j" should complete to "bluegrass", and the second
      ;; should return.
      (should
       (string=
        "bluegrass"
        (with-simulated-input "b l u e g C-j C-j"
          (ido-completing-read+
           "Prompt: "
           '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))))
    ;; Finally, a few tests for the expected wrong behavior without
    ;; ido-cr+. If ido.el ever fixes this bug, it will cause this test
    ;; to fail as a signal that the workaround can be phased out.
    (should
     (string=
      ""
      (with-simulated-input "C-j"
        (ido-completing-read
         "Prompt: "
         '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))))
    (should
     (string=
      "b"
      (with-simulated-input "b C-j"
        (ido-completing-read
         "Prompt: "
         '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))))))

(ert-deftest ido-cr+-test-fallback ()
  :tags '(ido ido-cr+)
  "Test whether manually invoking fallback works."
  (with-ido-cr+-standard-env
    (should
     ;; C-b/f not at beginning/end of input should not fall back
     (string=
      "green"
      (with-simulated-input "g C-b C-f RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; C-f at end of input should fall back
     (string=
      "g"
      (with-simulated-input "g C-f RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; Repeated C-b should not fall back
     (string=
      "green"
      (with-simulated-input "g C-b C-b C-b C-b RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))
    (should
     ;; C-b at beginning of line should fall back (if previous action
     ;; was not also C-b)
     (string=
      "g"
      (with-simulated-input "g C-b x DEL C-b RET"
        (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))))))

(ert-deftest ido-cr+-dot-prefix-empty-string ()
  :tags '(ido ido-cr+)
  "Test whether ido-ubiquitous successfully works around a bug in ido.

See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997 for more
information on this bug."
  (with-ido-cr+-standard-env
    (let ((ido-enable-dot-prefix t))
      (should
       (string=
        ""
        (with-simulated-input "RET"
          (ido-completing-read+ "Pick: " '("" "aaa" "aab" "aac")))))
      (should
       (string=
        "aab"
        (with-simulated-input "a a b RET"
          (ido-completing-read+ "Pick: " '("" "aaa" "aab" "aac"))))))))

(defun ido-cr+-run-all-tests ()
  (interactive)
  (ert "^ido-cr\\+-"))

(provide 'ido-ubiquitous-test)

;;; ido-ubiquitous-test.el ends here
