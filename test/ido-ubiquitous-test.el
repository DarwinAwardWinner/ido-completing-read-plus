;;; -*- lexical-binding: t -*-

(require 'ido-completing-read+)
(require 'ido-ubiquitous)
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
          ido-cr+-replace-completely
          ido-confirm-unique-completion))
       (idu-bindings
        (cl-loop for var in ido-ubiquitous-options collect
                 (list var
                       (list 'quote
                             (eval (car (get var 'standard-value))))))))
    `(with-mode ido-ubiquitous-mode 1
       (let ,idu-bindings ,@body))))

(defmacro collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  `(completion-table-dynamic (lambda (string) (all-completions string ,collection))))

(cl-defmacro should-with-tag (form &key tag)
  "Equivalent to `(should FORM)' but with a TAG on the output.

This is useful if the same `should' form will be called multiple
times in different contexts. Each test can pass a different tag
so it's clear in the ERT output which context is causing the
failure.

Note that although this is a macro, the TAG argument is evaluated normally."
  `(let ((tagvalue ,tag))
     (condition-case err
         (should ,form)
       (ert-test-failed
        (message "Error symbol: %S" (car err))
        (message "Error data: %S" (cdr err))
        (when tagvalue
          (setf (cadr err) (append (cadr err) (list :tag tagvalue))))
        (message "New error data: %S" (cdr err))
        (signal (car err) (cdr err))))))

(defun plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(cl-defmacro should-error-with-tag (form &rest other-keys &key tag &allow-other-keys)
  "Equivalent to `(should FORM)' but with a TAG on the output.
See `should-with-tag'.

Note that although this is a macro, the TAG argument is evaluated normally."
  (setq other-keys (plist-delete other-keys :tag))
  `(let ((tagvalue ,tag))
     (condition-case err
         (should-error ,form ,@other-keys)
       (ert-test-failed
        (message "Error symbol: %S" (car err))
        (message "Error data: %S" (cdr err))
        (when tagvalue
          (setf (cadr err) (append (cadr err) (list :tag tagvalue))))
        (message "New error data: %S" (cdr err))
        (signal (car err) (cdr err))))))

;; TODO: Add test for empty collection and too-large collection
(defun test-ido-ubiquitous-expected-mode (override &optional tag)
  "Test whether observed ido-ubiquitous behavior matches OVERRIDE."
  (declare (indent 1))
  (if (eq override 'disable)
      (progn
        (should-with-tag
         ;; Verify that we get standard completion
         (string=
          "g"
          (with-simulated-input "g RET"
            (completing-read "Prompt: " '("blue" "yellow" "green"))))
         :tag tag)
        (should-with-tag
         (string=
          "green"
          (with-simulated-input "g RET"
            (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
         :tag tag)
        ;; Standard completion should refuse to finish with incomplete
        ;; input if match is required
        (should-error-with-tag
         (with-simulated-input "b RET"
           (completing-read "Prompt: " '("brown" "blue" "yellow" "green") nil t))
         :type 'error
         :tag tag))
    ;; Common tests whenever ido-ubiquitous is enabled in any way
    (should-with-tag
     ;; Verify that ido completion is active
     (string=
      "green"
      (with-simulated-input "g RET"
        (completing-read "Prompt: " '("blue" "yellow" "green"))))
     :tag tag)
    ;; Verify that C-j is working correctly
    (should-with-tag
     (string=
      "g"
      (with-simulated-input "g C-j"
        (completing-read "Prompt: " '("blue" "yellow" "green"))))
     :tag tag)
    (let ((collection '("brown" "blue" "yellow" "green")))
      (should-with-tag
       (member
        (with-simulated-input "b RET"
          (completing-read "Prompt: " collection))
        (all-completions "b" collection))
       :tag tag))
    (cl-case override
      (enable
       ;; Test for new style
       (should-with-tag
        (string=
         "blue"
         (with-simulated-input "RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       (should-with-tag
        (string=
         ""
         (with-simulated-input "C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag))
      (enable-old
       (should-with-tag
        (string=
         ""
         (with-simulated-input "RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       (should-with-tag
        (string=
         "blue"
         (with-simulated-input "C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       ;; Verify that doing other stuff reverts RET and C-j to standard
       ;; meanings
       (should-with-tag
        (string=
         "blue"
         (with-simulated-input "g DEL RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       (should-with-tag
        (string=
         "blue"
         (with-simulated-input "<right> <left> RET"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       (should-with-tag
        (string=
         ""
         (with-simulated-input "g DEL C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag)
       (should-with-tag
        (string=
         ""
         (with-simulated-input "<right> <left> C-j"
           (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
        :tag tag))
      (otherwise (error "Unknown override %S" override)))))

(defvar original-completing-read (symbol-function #'completing-read))

(defun test-ido-ubiquitous-expected-mode-on-functional-collection (override &optional tag)
  "Test whether observed ido-ubiquitous behavior on functional collection matches OVERRIDE."
  (declare (indent 1))
  ;; This just temporarily replaces `completing-read' with a wrapper
  ;; that always converts the collection argument to an equivalent
  ;; function. That way, any use of `completing-read' will always see
  ;; a functional collection.
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt collection &rest args)
               (apply original-completing-read prompt
                      (collection-as-function collection)
                      args))))
    (test-ido-ubiquitous-expected-mode override tag)))

(ert-deftest ido-ubiquitous-test-simple ()
  :tags '(ido ido-ubiquitous)
  "Test that basic ido-ubiquitous functionality is working."
  (with-ido-ubiquitous-standard-env
    (ido-ubiquitous-mode 1)
    (test-ido-ubiquitous-expected-mode 'enable
      :simple-enable)
    (ido-ubiquitous-mode 0)
    (test-ido-ubiquitous-expected-mode 'disable
      :simple-disable)))

(ert-deftest ido-ubiquitous-test-oldstyle ()
  :tags '(ido ido-ubiquitous)
  "Test whether old-style completion works as expected."
  (with-ido-ubiquitous-standard-env
    (let ((ido-ubiquitous-default-state 'enable-old))
      (test-ido-ubiquitous-expected-mode 'enable-old
        :simple-oldstyle))))

(ert-deftest ido-ubiquitous-test-maxitems ()
  :tags '(ido ido-ubiquitous)
  "Test whether the large-collection fallback works."
  (with-ido-ubiquitous-standard-env
    (let ((ido-cr+-max-items -1))
      (test-ido-ubiquitous-expected-mode 'disable
        :maxitems))))

(ert-deftest ido-ubiquitous-test-override ()
  :tags '(ido ido-ubiquitous)
  "Test whether ido-ubiquitous overrides work."
  (with-ido-ubiquitous-standard-env
    (ido-ubiquitous-with-override 'enable
      (test-ido-ubiquitous-expected-mode 'enable
        :override-enable))
    (ido-ubiquitous-with-override 'enable-old
      (test-ido-ubiquitous-expected-mode 'enable-old
        :override-enable-old))
    (ido-ubiquitous-with-override 'disable
      (test-ido-ubiquitous-expected-mode 'disable
        :override-disable))))

(ert-deftest ido-ubiquitous-test-functional-collection ()
  :tags '(ido ido-ubiquitous)
  "Test whether ido-ubiquitous overrides work when collection is a function."
  (with-ido-ubiquitous-standard-env
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
      :collfunc)
    (ido-ubiquitous-with-override 'enable
      (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable
        :override-enable-collfunc))
    (ido-ubiquitous-with-override 'enable-old
      (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old
        :override-enable-old-collfunc))))

(ert-deftest ido-cr+-require-match ()
  :tags '(ido ido-cr+)
  "Test whether require-match works."
  (with-ido-ubiquitous-standard-env
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

;; Functions to define overrides on for testing
(defun idu-no-override-testfunc ()
  (test-ido-ubiquitous-expected-mode 'enable
    :func-override-none)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
    :func-override-none-collfunc))
(defun idu-enabled-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'enable
    :func-override-enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable
    :func-override-enable-collfunc))
(defun idu-disabled-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'disable
    :func-override-disable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
    :func-override-disable-collfunc))
(defun idu-enabled-oldstyle-testfunc (&rest args)
  (test-ido-ubiquitous-expected-mode 'enable-old
    :func-override-enable-old)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old
    :func-override-enable-old-collfunc))

;; commands to define overrides on for testing
(defun idu-no-override-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable
      :cmd-override-none)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
      :cmd-override-none-collfunc)))
  (test-ido-ubiquitous-expected-mode 'enable
    :cmd-override-none)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
    :cmd-override-non-collfunc))
(defun idu-enabled-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable
      :cmd-override-enable)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable
      :cmd-override-enable-collfunc)))
  (test-ido-ubiquitous-expected-mode 'enable
    :cmd-override-enable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable
    :cmd-override-enable-collfunc))
(defun idu-disabled-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'disable
      :cmd-override-disable)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
      :cmd-override-disable-collfunc)))
  (test-ido-ubiquitous-expected-mode 'disable
    :cmd-override-disable)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'disable
    :cmd-override-disable-collfunc))
(defun idu-enabled-oldstyle-testcmd (&rest args)
  (interactive
   (list
    (test-ido-ubiquitous-expected-mode 'enable-old
      :cmd-override-enable-old)
    (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old
      :cmd-override-enable-old-collfunc)))
  (test-ido-ubiquitous-expected-mode 'enable-old
    :cmd-override-enable-old)
  (test-ido-ubiquitous-expected-mode-on-functional-collection 'enable-old
    :cmd-override-enable-old-collfunc))

(ert-deftest ido-ubiquitous-test-command-and-function-overrides ()
  :tags '(ido ido-ubiquitous)
  "Test whether command- and function-specific overrides work."
  (let ((orig-func-overrides ido-ubiquitous-function-overrides)
        (orig-cmd-overrides ido-ubiquitous-command-overrides))
    (unwind-protect
        (with-ido-ubiquitous-standard-env
          (customize-set-variable
           'ido-ubiquitous-function-overrides
           (append ido-ubiquitous-function-overrides
                   '((enable exact "idu-enabled-testfunc")
                     (disable exact "idu-disabled-testfunc")
                     (enable-old exact "idu-enabled-oldstyle-testfunc"))))
          (cl-loop for func in
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
          (cl-loop for cmd in
                   '(idu-no-override-testcmd
                     idu-enabled-testcmd
                     idu-disabled-testcmd
                     idu-enabled-oldstyle-testcmd)
                   do (call-interactively cmd)))
      (customize-set-variable 'ido-ubiquitous-function-overrides orig-func-overrides)
      (customize-set-variable 'ido-ubiquitous-command-overrides orig-cmd-overrides))))

(ert-deftest ido-ubiquitous-test-fallback ()
  :tags '(ido ido-ubiquitous)
  "Test whether manually invoking fallback works."
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

(ert-deftest ido-ubiquitous-dot-prefix-empty-string ()
  :tags '(ido ido-ubiquitous)
  "Test whether ido-ubiquitous successfully works around a bug in ido.

See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997 for more
information on this bug."
  (with-ido-ubiquitous-standard-env
    (let ((ido-enable-dot-prefix t))
      (should
       (string=
        ""
        (with-simulated-input "RET"
          (completing-read "Pick: " '("" "aaa" "aab" "aac")))))
      (should
       (string=
        "aab"
        (with-simulated-input "a a b RET"
          (completing-read "Pick: " '("" "aaa" "aab" "aac"))))))))

(defun ido-ubiquitous-run-all-tests ()
  (interactive)
  (ert "^ido-\\(ubiquitous\\|cr\\+\\)-"))

(provide 'ido-ubiquitous-test)

;;; ido-ubiquitous-test.el ends here
