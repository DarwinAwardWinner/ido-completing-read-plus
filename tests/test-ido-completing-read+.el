;;; -*- lexical-binding: t -*-

(require 'ido-completing-read+)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)

(defun collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  (completion-table-dynamic (lambda (string) (all-completions string collection))))

(defun ido-cr+-test-save-custom-vars (vars)
  (cl-loop
   for var in vars
   if (not (custom-variable-p var))
   do (error "Variable `%s' is not a customizable variable" var)
   for curval = (symbol-value var)
   for stdval = (eval (car (get var 'standard-value)))
   for setter = (or (get var 'custom-set) 'set-default)
   do
   (progn
     ;; Save the current value
     (put var 'ido-cr+-test-saved-value curval)
     ;; Set it to the standard value, using it's custom setter
     ;; function
     (funcall setter var stdval))))

(defun ido-cr+-test-restore-custom-vars (vars)
  (cl-loop
   for var in vars
   for savedval = (get var 'ido-cr+-test-saved-value)
   for setter = (or (get var 'custom-set) 'set-default)
   do
   (progn
     ;; Set it to the saved value, using it's custom setter function
     (funcall setter var savedval)
     ;; Delete the saved value from the symbol plist
     (put var 'ido-cr+-test-saved-value nil))))

(describe "Within the `ido-completing-read+' package"

  ;; All these need to be saved before and restored after each each test
  :var (ido-mode
        ido-ubiquitous-mode
        ido-cr+-debug-mode
        ido-cr+-auto-update-blacklist
        ido-cr+-fallback-function
        ido-cr+-max-items
        ido-cr+-function-blacklist
        ido-cr+-function-whitelist
        ido-cr+-replace-completely
        ido-confirm-unique-completion
        ido-enable-flex-matching)

  ;; Reset all of these variables to their standard values before each
  ;; test
  (before-each
    (ido-cr+-test-save-custom-vars
     '(ido-mode
       ido-ubiquitous-mode
       ido-cr+-debug-mode
       ido-cr+-auto-update-blacklist
       ido-cr+-fallback-function
       ido-cr+-max-items
       ido-cr+-function-blacklist
       ido-cr+-function-whitelist
       ido-cr+-replace-completely
       ido-confirm-unique-completion
       ido-enable-flex-matching))
    ;; Now enable ido-mode and ido-ubiquitous-mode
    (ido-mode 1)
    (ido-ubiquitous-mode 1))

  ;; Restore the saved value after each test
  (after-each
    (ido-cr+-test-restore-custom-vars
     '(ido-mode
       ido-ubiquitous-mode
       ido-cr+-debug-mode
       ido-cr+-auto-update-blacklist
       ido-cr+-fallback-function
       ido-cr+-max-i
       tems
       ido-cr+-function-blacklist
       ido-cr+-function-whitelist
       ido-cr+-replace-completely
       ido-confirm-unique-completion
       ido-enable-flex-matching)))

  (describe "the `ido-completing-read+' function"

    (it "should complete with a matching item on RET"
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
       :to-equal "green"))
    (it "should complete with the first match when multiple matches are available"
      (expect
       (with-simulated-input "b RET"
         (ido-completing-read+ "Prompt: " '("brown" "blue" "yellow" "green")))
       :to-equal "brown"))
    (it "should allow <left> and <right> to cycle completions, with wrap-around"
      (expect
       (with-simulated-input "b <right> <right> <right> <right> <left> RET"
         (ido-completing-read+ "Prompt: " '("brown" "blue" "yellow" "green")))
       :to-equal
       "blue"))

    (it "should return \"\" when RET or C-j is pressed on an empty input even when REQUIRE-MATCH is non-nil"
      ;; No REQUIRE-MATCH
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
       :to-equal "blue")
      (expect
       (with-simulated-input "C-j"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
       :to-equal "")
      ;; Again, with REQUIRE-MATCH
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t))
       :to-equal "")
      (expect
       (with-simulated-input "C-j"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t))
       :to-equal ""))

    ;; Verify that DEF works, whether or not it is an element of
    ;; COLLECTION
    (it "should accept all the same forms of DEF as `completing-read-default'"
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil "green"))
       :to-equal "green")
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t nil nil "green"))
       :to-equal "green")
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil nil nil nil "brown"))
       :to-equal "brown")
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil t nil nil "brown"))
       :to-equal "brown")
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil t nil nil '("brown" "green")))
       :to-equal "brown"))

    ;; Verify that INITIAL-INPUT works
    (it "should work with INITIAL-INPUT"
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr"))
       :to-equal "green"))

    ;; Verify that INITIAL-INPUT and DEF don't interfere with each other
    (it "should properly handle both INITIAL-INPUT and DEF at the same time"
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))
       :to-equal "green")
      (expect
       (with-simulated-input "DEL DEL RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))
       :to-equal "blue"))

    ;; Verify that ido-cr+ works on function collections
    (it "should work when COLLECTION is a function"
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read+ "Prompt: " (collection-as-function '("blue" "yellow" "green"))))
       :to-equal "green"))

    (describe "when `ido-cr+-max-items' is set"
      (it "should not trigger a fallback for small collections"
        (expect
         (with-simulated-input "g RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
         :to-equal "green"))
      (it "should trigger a fallback for large collections"
        (expect
         ;; With max-items negative, all collections are considered "too
         ;; large"
         (let ((ido-cr+-max-items -1))
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " '("blue" "yellow" "green"))))
         :to-equal "g")))

    (describe "when REQUIRE-MATCH is non-nil"
      (it "should still allow exiting with an empty string if DEF is nil"
        (expect
         (with-simulated-input "C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
         :to-equal ""))
      ;; "C-j" should NOT be allowed to return an empty string if
      ;; require-match and default are both non-nil.
      (it "should not alow exiting with an empty string if DEF is non-nil"
        (expect
         (lambda ()
           (with-simulated-input "C-j"
             (ido-completing-read+
              "Prompt: "
              '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t nil nil "yellow")))
         :to-throw))
      (it "shouldn't allow C-j to select an ambiguous match"
        (expect
         (lambda ()
           (with-simulated-input "b C-j C-j C-j"
             (ido-completing-read+
              "Prompt: "
              '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
         :to-throw)
        ;; First press of C-j should complete to "blue" after the
        ;; first b, but then get stuck on the choice for the second b.
        (expect
         (lambda ()
           (with-simulated-input "b C-j b C-j C-j"
             (ido-completing-read+
              "Prompt: "
              '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
         :to-throw))
      (it "should allow exiting with an unambiguous match"
        (expect
         (with-simulated-input "b C-j b C-j e C-j C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow" "green") nil t))
         :to-equal "blueberry")
        ;; The "C-j" should complete to "bluegrass" and return, because
        ;; `ido-confirm-unique-completion is nil.
        (expect
         (with-simulated-input "b l u e g C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
         :to-equal "bluegrass"))
      (it "should require an extra C-j to exit when `ido-confirm-unique-completion' is non-nil"
        (setq ido-confirm-unique-completion t)
        ;; Now the first "C-j" should complete to "bluegrass" but should
        ;; not return.
        (expect
         (lambda ()
           (with-simulated-input "b l u e g C-j"
             (ido-completing-read+
              "Prompt: "
              '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t)))
         :to-throw)
        ;; The first "C-j" should complete to "bluegrass", and the second
        ;; should return.
        (expect
         (with-simulated-input "b l u e g C-j C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
         :to-equal "bluegrass"))

      ;; Finally, a test for the expected wrong behavior without
      ;; ido-cr+. If ido.el ever fixes this bug, it will cause this test
      ;; to fail as a signal that the workaround can be phased out.
      (it "should return a non-match when ordinary `ido-completing-read' is used"
        (expect
         (with-simulated-input "b C-j"
           (ido-completing-read
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
         :to-equal "b")))

    (describe "with manual fallback shortcuts"
      (it "should not fall back when C-b or C-f is used in the middle of the input"
        (expect
         ;; C-b/f not at beginning/end of input should not fall back
         (with-simulated-input "g C-b C-f RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
         :to-equal "green"))
      (it "should fall back on C-f at end of input"
        (expect
         ;; C-f at end of input should fall back
         (with-simulated-input "g C-f RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
         :to-equal "g"))
      (it "should not fall back from repeated C-b that hits the start of input"
        (expect
         ;; Repeated C-b should not fall back
         (with-simulated-input "g C-b C-b C-b C-b RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
         :to-equal "green"))
      (it "should fall back on C-b at beginning of input (if previous action was not C-b)"
        (expect
         ;; C-b at beginning of line should fall back (if previous action
         ;; was not also C-b)
         (with-simulated-input "g C-b x DEL C-b RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "green")))
         :to-equal "g")))

    (describe "with a work workaround for an bug with `ido-enable-dot-prefix'"
      ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997
      ;; for more information on this bug.
      (before-each
        (setq ido-enable-dot-prefix t))
      (it "should not throw an error when `ido-enable-dot-prefix' is non-nil and \"\" is in the collection"
        (expect
         (with-simulated-input "RET"
           (ido-completing-read+ "Pick: " '("" "aaa" "aab" "aac")))
         :to-equal "")
        (expect
         (with-simulated-input "a a b RET"
           (ido-completing-read+ "Pick: " '("" "aaa" "aab" "aac")))
         :to-equal "aab")))

    (describe "with dynamic collections"
      (before-all
        (setq my-dynamic-collection
              (completion-table-dynamic
               (lambda (text)
                 (cond
                  ;; Sub-completions for "hello"
                  ((s-prefix-p "hello" text)
                   '("hello" "hello-world" "hello-everyone" "hello-universe"))
                  ;; Sub-completions for "goodbye"
                  ((s-prefix-p "goodbye" text)
                   '("goodbye" "goodbye-world" "goodbye-everyone" "goodbye-universe"))
                  ;; General completions
                  (t
                   '("hello" "goodbye" "helicopter" "helium" "goodness" "goodwill")))))))
      (after-all
        (setq my-dynamic-collection nil))
      (before-each
        (setq ido-enable-flex-matching t
              ido-confirm-unique-completion nil))

      (it "should allow selection of dynamically-added completions"
        (expect
         (with-simulated-input "hello- RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal "hello-world"))

      (it "should allow ido flex-matching of dynamically-added completions"
        (expect
         (with-simulated-input "hello-ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world"))
      (it "should do a dynamic update when pressing TAB"
        (expect
         (with-simulated-input "h TAB -ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world"))
      (it "should do a dynamic update when idle"
        (expect
         (with-simulated-input
             '("h"
               (wsi-simulate-idle-time (1+ ido-cr+-dynamic-update-idle-time))
               "-ld RET")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world"))
      (it "should do a dynamic update when there is only one match remaining"
        (expect
         (with-simulated-input "hell-ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world"))
      (it "should not exit with a unique match if new matches are dynamically added"
        (expect
         (with-simulated-input '("hell TAB -ld RET")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world"))
      (it "should exit with a match that is still unique after dynamic updating"
        (expect
         (with-simulated-input '("helic TAB")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "helicopter"))))

  (describe "ido-ubiquitous-mode"
    ;; Set up a test command that calls `completing-read'
    (before-all
      (setf (symbol-function 'test-command)
            (lambda ()
              (interactive)
              (completing-read "Prompt: " '("blue" "yellow" "green")))))
    ;; Delete the test command
    (after-all
      (setf (symbol-function 'test-command) nil))

    ;; Verify that the mode can be activated
    (it "should enable itself properly"
      (expect
       (progn
         (ido-ubiquitous-mode 1)
         (with-simulated-input "g RET"
           (command-execute 'test-command)))
       :to-equal "green"))
    (it "should disable itself properly"
      (expect
       (progn
         (ido-ubiquitous-mode 0)
         (with-simulated-input "g RET"
           (command-execute 'test-command)))
       :to-equal "g"))

    (describe "with `ido-cr+-function-blacklist'"
      (before-all
        (setf (symbol-function 'blacklisted-command)
              (lambda (arg)
                (interactive (list (completing-read "Prompt: " '("blue" "yellow" "green"))))
                arg)
              (symbol-function 'blacklisted-function)
              (lambda ()
                (completing-read "Prompt: " '("blue" "yellow" "green")))
              (symbol-function 'cmd-that-calls-blacklisted-function)
              (lambda ()
                (interactive)
                (funcall 'blacklisted-function))
              (symbol-function 'blacklisted-collection)
              (collection-as-function '("blue" "yellow" "green"))))
      (after-all
        (setf (symbol-function 'blacklisted-command) nil
              (symbol-function 'blacklisted-function) nil
              (symbol-function 'cmd-that-calls-blacklisted-function) nil
              (symbol-function 'blacklisted-collection) nil))
      ;; First verify that they work normally before blacklisting them
      (describe "when the blacklist is empty"
        (it "should not affect a non-blacklisted command"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'blacklisted-command))
           :to-equal "green"))
        (it "should not affect a non-blacklisted function"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'cmd-that-calls-blacklisted-function))
           :to-equal "green"))
        (it "should not affect a non-blacklisted collection"
          (expect
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " 'blacklisted-collection))
           :to-equal "green")))

      (describe "when the specified functions are blacklisted"
        (before-each
          (setq ido-cr+-function-blacklist
                (append '(blacklisted-command
                          blacklisted-function
                          blacklisted-collection)
                        ido-cr+-function-blacklist)))
        (it "should prevent ido in a blacklisted command"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'blacklisted-command))
           :to-equal "g"))
        (it "should prevent ido in a blacklisted function"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'cmd-that-calls-blacklisted-function))
           :to-equal "g"))
        (it "should prevent ido with a blacklisted collection"
          (expect
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " 'blacklisted-collection))
           :to-equal "g"))))

    (describe "with `ido-cr+-function-whitelist'"
      (before-all
        (setf (symbol-function 'whitelisted-command)
              (lambda (arg)
                (interactive
                 (list
                  (completing-read "Prompt: " '("blue" "yellow" "green"))))
                arg)
              (symbol-function 'whitelisted-function)
              (lambda ()
                (completing-read "Prompt: " '("blue" "yellow" "green")))
              (symbol-function 'cmd-that-calls-whitelisted-function)
              (lambda ()
                (interactive)
                (funcall 'whitelisted-function))
              (symbol-function 'whitelisted-collection)
              (lambda (string pred action)
                (complete-with-action action '("blue" "yellow" "green") string pred))))
      (after-all
        (setf (symbol-function 'whitelisted-command) nil
              (symbol-function 'whitelisted-function) nil
              (symbol-function 'cmd-that-calls-whitelisted-function) nil
              (symbol-function 'whitelisted-collection) nil))
      (describe "when the whitelist is inactive (i.e. everything is whitelisted)"
        (before-each
          (setq ido-cr+-function-whitelist nil))
        (it "should enable ido in a command"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'whitelisted-command))
           :to-equal "green"))
        (it "should enable ido in a function"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'cmd-that-calls-whitelisted-function))
           :to-equal "green"))
        (it "should enable ido for a collection"
          (expect
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " 'whitelisted-collection))
           :to-equal "green")))
      (describe "when the specified functions are whitelisted"
        (before-each
          (setq ido-cr+-function-whitelist
                (append '(whitelisted-command
                          whitelisted-function
                          whitelisted-collection)
                        ido-cr+-function-whitelist)))
        (it "should enable ido in a whitelisted command"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'whitelisted-command))
           :to-equal "green"))
        (it "should enable ido in a whitelisted function"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'cmd-that-calls-whitelisted-function))
           :to-equal "green"))
        (it "should enable ido for a whitelisted collection"
          (expect
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " 'whitelisted-collection))
           :to-equal "green")))
      (describe "when the whitelist is active but empty (i.e. nothing whitelisted)"
        (before-each
          (setq ido-cr+-function-whitelist (list nil)))
        (it "should prevent ido in a command"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'whitelisted-command))
           :to-equal "g"))
        (it "should prevent ido in a function"
          (expect
           (with-simulated-input "g RET"
             (call-interactively 'cmd-that-calls-whitelisted-function))
           :to-equal "g"))
        (it "should prevent ido for a collection"
          (expect
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " 'whitelisted-collection))
           :to-equal "g"))))))

;; (defun ido-cr+-run-all-tests ()
;;   (interactive)
;;   (ert "^ido-cr\\+-"))

;;; test-ido-completing-read+.el ends here
