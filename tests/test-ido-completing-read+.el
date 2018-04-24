;;; -*- lexical-binding: t -*-

(require 'ido)
(require 'minibuf-eldef)
(require 'ido-completing-read+)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)

;; Note: Currently unused, but potentially useful in the future
(defun ido-cr+-maybe-chop (items elem)
  "Like `ido-chop', but a no-op if ELEM is not in ITEMS.

Normal `ido-chop' hangs infinitely in this case."
  (cl-loop
   with new-tail = ()
   for remaining on items
   for next = (car remaining)
   if (equal next elem)
   return (nconc remaining new-tail)
   else collect next into new-tail
   finally return items))

(defun collection-as-function (collection)
  "Return a function equivalent to COLLECTION.

The returned function will work equivalently to COLLECTION when
passed to `all-completions' and `try-completion'."
  (completion-table-dynamic (lambda (string) (all-completions string collection))))

(defun shadow-var (var &optional temp-value)
  "Shadow the value of VAR.

This will push the current value of VAR to VAR's
`shadowed-values' property, and then set it to TEMP-VALUE. To
reverse this process, call `unshadow-var' on VAR. Vars can
be shadowed recursively, and must be unshadowed once for each
shadowing in order to restore the original value. You can think
of shadowing as dynamic binding with `let', but with manual
control over when bindings start and end.

If VAR is a Custom variable (see `custom-variable-p'), it will be
set using `customize-set-variable', and if TEMP-VALUE is nil it
will be replaces with VAR's standard value.

 Other variables will be set with `set-default', and a TEMP-VALUE
 of nil will not be treated specially.

`shadow-var' only works on variables declared as special (i.e.
using `defvar' or similar). It will not work on lexically bound
variables."
  (unless (special-variable-p var)
    (error "Cannot shadow lexical var `%s'" var))
  (let* ((use-custom (custom-variable-p var))
         (setter (if use-custom 'customize-set-variable 'set-default))
         (temp-value (or temp-value
                         (and use-custom
                              (eval (car (get var 'standard-value)))))))
    ;; Push the current value on the stack
    (push (symbol-value var) (get var 'shadowed-values))
    (funcall setter var temp-value)))

(defun var-shadowed-p (var)
  "Return non-nil if VAR is shadowed by `shadow-var'."
  ;; We don't actually want to return that list if it's non-nil.
  (and (get var 'shadowed-values) t))

(defun unshadow-var (var)
  "Reverse the last call to `shadow-var' on VAR."
  (if (var-shadowed-p var)
      (let* ((use-custom (custom-variable-p var))
             (setter (if use-custom 'customize-set-variable 'set-default))
             (value (pop (get var 'shadowed-values))))
        (funcall setter var value))
    (error "Var is not shadowed: %s" var)))

(defun fully-unshadow-var (var)
  "Reverse *all* calls to `shadow-var' on VAR."
  (when (var-shadowed-p var)
    (let* ((use-custom (custom-variable-p var))
           (setter (if use-custom 'customize-set-variable 'set-default))
           (value (car (last (get var 'shadowed-values)))))
      (put var 'shadowed-values nil)
      (funcall setter var value))))

(defun fully-unshadow-all-vars (&optional vars)
  "Reverse *all* calls to `shadow-var' on VARS.

If VARS is nil, unshadow *all* variables."
  (if vars
      (mapc #'fully-unshadow-var vars)
    (mapatoms #'fully-unshadow-var))
  nil)

(defmacro shadow-vars (varlist)
  "Shadow a list of vars with new values.

VARLIST describes the variables to be shadowed with the same
syntax as `let'.

See `shadow-var'."
  (declare (indent 0))
  (cl-loop
   with var = nil
   with value = nil
   for binding in varlist
   if (symbolp binding)
   do (setq var binding
            value nil)
   else
   do (setq var (car binding)
            value (cadr binding))
   collect `(shadow-var ',var ,value) into exprs
   finally return `(progn ,@exprs)))

(defmacro unshadow-vars (vars)
  "Un-shadow a list of VARS.

This is a macro for consistency with `shadow-vars', but it will
also accept a quoted list for the sake of convenience."
  (declare (indent 0))
  (when (eq (car vars) 'quote)
    (setq vars (eval vars)))
  `(mapc #'unshadow-var ',vars))

(defmacro with-temp-info-buffer (&rest body)
  "Create a temporary info buffer and exeluate BODY forms there."
  (declare (indent 0))
  `(let ((temp-bufname (generate-new-buffer-name " *temp-info*")))
     (unwind-protect
         (save-excursion
           (info nil (generate-new-buffer-name " *temp-info*"))
           ,@body)
       (when (get-buffer temp-bufname)
         (kill-buffer temp-bufname)))))

(describe "Within the `ido-completing-read+' package"

  ;; Reset all of these variables to their standard values before each
  ;; test, saving the previous values for later restoration.
  (before-each
    (shadow-vars
     ((ido-mode t)
      (ido-ubiquitous-mode t)
      (ido-cr+-debug-mode t)
      ido-cr+-auto-update-blacklist
      ido-cr+-fallback-function
      ido-cr+-max-items
      ido-cr+-function-blacklist
      ido-cr+-function-whitelist
      ido-cr+-nil-def-alternate-behavior-list
      ido-cr+-replace-completely
      ido-confirm-unique-completion
      ido-enable-flex-matching
      ido-enable-dot-prefix
      (minibuffer-electric-default-mode t)))

    ;; Suppress all messages during tests
    (spy-on 'message))

  ;; Restore the saved values after each test
  (after-each
    (fully-unshadow-all-vars))

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

    (it "should work with `minibuffer-electric-default-mode'"
      (let ((eldef-was-showing nil))
        ;; No REQUIRE-MATCH, so electric default should not show
        (with-simulated-input
            '("blu DEL DEL DEL"
              (setq eldef-was-showing minibuf-eldef-showing-default-in-prompt)
              "RET")
          (ido-completing-read+ "Prompt (default green): " '("blue" "yellow" "green")))
        (expect eldef-was-showing :not :to-be-truthy)
        ;; With REQUIRE-MATCH, so electric default should show
        (with-simulated-input
            '("blu DEL DEL DEL"
              (setq eldef-was-showing minibuf-eldef-showing-default-in-prompt)
              "RET")
          (ido-completing-read+ "Prompt (default green): " '("blue" "yellow" "green") nil t))
        (expect eldef-was-showing :to-be-truthy)))

    (it "should accept all the same forms of DEF as `completing-read-default'"
      ;; DEF in COLLECTION
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil "green"))
       :to-equal "green")
      ;; Same, with REQUIRE-MATCH
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t nil nil "green"))
       :to-equal "green")
      ;; DEF not in COLLECTION
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil "brown"))
       :to-equal "brown")
      ;; Same, with REQUIRE-MATCH
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t nil nil "brown"))
       :to-equal "brown")
      ;; List DEF, partially in COLLECTION
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil t nil nil '("brown" "green")))
       :to-equal "brown"))

    (it "should work with INITIAL-INPUT"
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr"))
       :to-equal "green"))

    (it "should properly handle a cons INITIAL-INPUT"
      (expect
       (with-simulated-input "ee RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil (cons "gr" 2)))
       :to-equal "green"))

    (it "should properly handle both INITIAL-INPUT and DEF at the same time"
      (expect
       (with-simulated-input "RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))
       :to-equal "green")
      (expect
       (with-simulated-input "DEL DEL RET"
         (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil "gr" nil "blue"))
       :to-equal "blue"))

    (it "should work when COLLECTION is a function"
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read+ "Prompt: " (collection-as-function '("blue" "yellow" "green"))))
       :to-equal "green"))

    (it "should fall back when COLLECTION is empty"
      (spy-on 'ido-completing-read :and-call-through)
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read+ "Prompt: " nil))
       :to-equal "g")
      (expect 'ido-completing-read :not :to-have-been-called))

    (it "should replace `ido-completing-read' when `ido-cr+-replace-completely' is non-nil"
      (customize-set-variable 'ido-cr+-replace-completely t)
      (spy-on 'ido-completing-read+ :and-call-through)
      (expect
       (with-simulated-input "g RET"
         (ido-completing-read "Prompt: " '("blue" "yellow" "green")))
       :to-equal "green")
      (expect 'ido-completing-read+ :to-have-been-called))

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
         (with-simulated-input "C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t nil nil "yellow"))
         :to-throw))

      (it "shouldn't allow C-j to select an ambiguous match"
        (expect
         (with-simulated-input "b C-j C-j C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
         :to-throw)
        ;; First press of C-j should complete to "blue" after the
        ;; first b, but then get stuck on the choice for the second b.
        (expect
         (with-simulated-input "b C-j b C-j C-j C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow" "green") nil t))
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
         (with-simulated-input "b l u e g C-j"
           (ido-completing-read+
            "Prompt: "
            '("bluebird" "blues" "bluegrass" "blueberry" "yellow ""green") nil t))
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

    (describe "when INHERIT-INPUT-METHOD is non-nil"

      (before-each
        (spy-on 'ido-completing-read :and-call-through))

      (it "should not fall back if `current-input-method' is nil"
        (expect
         (let ((current-input-method nil))
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil nil t))
           :to-equal "green"))
        (expect 'ido-completing-read :to-have-been-called))

      (it "should fall back if `current-input-method' is non-nil"
        (expect
         (let ((current-input-method 'ucs))
           (with-simulated-input "g RET"
             (ido-completing-read+ "Prompt: " '("blue" "yellow" "green") nil nil nil nil nil t))
           :to-equal "green"))
        (expect 'ido-completing-read :not :to-have-been-called)))

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

    (describe "with a workaround for an bug with non-nil `ido-enable-dot-prefix'"
      ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997
      ;; for more information on this bug.
      (before-each
        (setq ido-enable-dot-prefix t))

      (it "should not throw an error when \"\" is in the collection"
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
              ido-confirm-unique-completion nil)
        (spy-on 'ido-cr+-update-dynamic-collection
                :and-call-through))

      (it "should allow selection of dynamically-added completions"
        (expect
         (with-simulated-input "hello- RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should allow ido flex-matching of dynamically-added completions"
        (expect
         (with-simulated-input "hello-ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should do a dynamic update when pressing TAB"
        (expect
         (with-simulated-input "h TAB -ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should do a dynamic update when idle"
        (expect
         (with-simulated-input
             '("h"
               (wsi-simulate-idle-time (1+ ido-cr+-dynamic-update-idle-time))
               "-ld RET")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should do a dynamic update when there is only one match remaining"
        (expect
         (with-simulated-input "hell-ld RET"
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should not exit with a unique match if new matches are dynamically added"
        (expect
         (with-simulated-input '("hell TAB -ld RET")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "hello-world")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should exit with a match that is still unique after dynamic updating"
        (expect
         (with-simulated-input '("helic TAB")
           (ido-completing-read+ "Say something: " my-dynamic-collection))
         :to-equal
         "helicopter")
        (expect 'ido-cr+-update-dynamic-collection
                :to-have-been-called))

      (it "should suppress errors raised by dynamic completion updates"
        (let ((collection
               (completion-table-dynamic
                (lambda (text)
                  (cond
                   ((equal text "")
                    '("hello" "goodbye" "helicopter" "helium" "goodness" "goodwill"))
                   (t (error "This collection throws an error on a nonempty prefix"))))))
              ;; The test framework uses the debugger to catch error
              ;; stack traces, but we want to run this code as if it
              ;; was not being debugged.
              (debug-on-error nil))
          (expect
           (with-simulated-input '("hell TAB RET")
             (ido-completing-read+ "Say something: " collection))
           :to-equal
           "hello")))

      (it "should respect `ido-restrict-to-matches' when doing dynamic updates"
        (assume (version<= "25" emacs-version))
        (let ((collection
               (list "aaa-ddd-ggg" "aaa-eee-ggg" "aaa-fff-ggg"
                     "bbb-ddd-ggg" "bbb-eee-ggg" "bbb-fff-ggg"
                     "ccc-ddd-ggg" "ccc-eee-ggg" "ccc-fff-ggg"
                     "aaa-ddd-hhh" "aaa-eee-hhh" "aaa-fff-hhh"
                     "bbb-ddd-hhh" "bbb-eee-hhh" "bbb-fff-hhh"
                     "ccc-ddd-hhh" "ccc-eee-hhh" "ccc-fff-hhh"
                     "aaa-ddd-iii" "aaa-eee-iii" "aaa-fff-iii"
                     "bbb-ddd-iii" "bbb-eee-iii" "bbb-fff-iii"
                     "ccc-ddd-iii" "ccc-eee-iii" "ccc-fff-iii")))
          ;; Test the internal function
          (expect
           (ido-cr+-apply-restrictions
            collection
            (list (cons nil "bbb")
                  (cons nil "eee")))
           :to-equal '("bbb-eee-ggg" "bbb-eee-hhh" "bbb-eee-iii"))
          ;; First verify it without a dynamic collection
          (expect
           (with-simulated-input "eee C-SPC bbb C-SPC ggg RET"
             (ido-completing-read+
              "Pick: " collection nil t nil nil (car collection)))
           :to-equal "bbb-eee-ggg")
          (expect
           (with-simulated-input "eee C-SPC aaa C-u C-SPC ccc C-u C-SPC ggg RET"
             (ido-completing-read+
              "Pick: " collection nil t nil nil (car collection)))
           :to-equal "bbb-eee-ggg")
          ;; Now test the same with a dynamic collection
          (expect
           (with-simulated-input "eee C-SPC bbb C-SPC ggg RET"
             (ido-completing-read+
              "Pick: " (collection-as-function collection) nil t nil nil (car collection)))
           :to-equal "bbb-eee-ggg")
          (expect
           (with-simulated-input "eee C-SPC aaa C-u C-SPC ccc C-u C-SPC ggg RET"
             (ido-completing-read+
              "Pick: " (collection-as-function collection) nil t nil nil (car collection)))
           :to-equal "bbb-eee-ggg"))))

    (describe "with unusual inputs"
      (it "should accept a COLLECTION of symbols"
        (expect
         (with-simulated-input "g RET"
           (ido-completing-read+ "Prompt: " '(blue yellow green)))
         :to-equal "green"))

      (it "should accept a mix of strings and symbols in COLLECTION"
        (expect
         (with-simulated-input "g RET"
           (ido-completing-read+ "Prompt: " '(blue "yellow" green)))
         :to-equal "green"))

      (it "should accept symbols in DEF"
        (expect
         (with-simulated-input "RET"
           (ido-completing-read+ "Prompt: " '("blue" "yellow" "brown") nil t nil nil '(brown "green")))
         :to-equal "brown"))

      (it "should accept an alist COLLECTION"
        (expect
         (with-simulated-input "RET"
           (ido-completing-read+
            "Prompt: "
            '(("blue" . blue-value)
              ("yellow" . yellow-value)
              (green . green-value))
            nil nil nil nil "green"))
         :to-equal "green"))

      (it "should accept a hash table COLLECTION"
        (expect
         (with-simulated-input "RET"
           (let ((collection (make-hash-table)))
             (puthash "blue" 'blue-value collection)
             (puthash "yellow" 'yellow-value collection)
             (puthash 'green 'green-value collection)
             (ido-completing-read+ "Prompt: " collection nil nil nil nil "green")))
         :to-equal "green"))

      (it "should accept an obarray COLLECTION"
        (expect
         (with-simulated-input "forward-char RET"
           (ido-completing-read+ "Prompt: " obarray #'commandp
                                 t nil nil "backward-char"))
         :to-equal "forward-char"))))

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
      (describe "when the specified functions are not blacklisted"

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
           :to-equal "g")))

      (describe "when updating ido-cr+"

        (before-each
          (spy-on 'ido-cr+-update-blacklist :and-call-through))

        (it "should update the blacklist when `ido-cr+-auto-update-blacklist' is t"
          (assume ido-cr+-function-blacklist)
          (let ((orig-blacklist ido-cr+-function-blacklist))
            (customize-set-variable 'ido-cr+-auto-update-blacklist t)
            (customize-set-variable 'ido-cr+-function-blacklist nil)
            (ido-cr+-maybe-update-blacklist)
            (expect 'ido-cr+-update-blacklist :to-have-been-called)
            (expect ido-cr+-function-blacklist :to-have-same-items-as orig-blacklist)))
        (it "should not update the blacklist when `ido-cr+-auto-update-blacklist' is nil"
          (assume ido-cr+-function-blacklist)
          (let ((orig-blacklist ido-cr+-function-blacklist))
            (customize-set-variable 'ido-cr+-auto-update-blacklist nil)
            (customize-set-variable 'ido-cr+-function-blacklist nil)
            (ido-cr+-maybe-update-blacklist)
            (expect 'ido-cr+-update-blacklist :not :to-have-been-called)
            (expect ido-cr+-function-blacklist :to-have-same-items-as nil)))

        (it "should notify about blacklist updates when `ido-cr+-auto-update-blacklist' is `notify'"
          (assume ido-cr+-function-blacklist)
          (spy-on 'display-warning)
          (let ((orig-blacklist ido-cr+-function-blacklist))
            (customize-set-variable 'ido-cr+-auto-update-blacklist 'notify)
            (customize-set-variable 'ido-cr+-function-blacklist nil)
            (ido-cr+-maybe-update-blacklist)
            (expect 'ido-cr+-update-blacklist :not :to-have-been-called)
            (expect 'display-warning :to-have-been-called)
            (expect ido-cr+-function-blacklist :to-have-same-items-as nil)))))

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
           :to-equal "g"))))

    (describe "with `ido-cr+-nil-def-alternate-behavior-list'"
      (before-all
        (setf (symbol-function 'def-nil-command)
              (lambda (arg)
                (interactive
                 (list
                  (completing-read "Prompt: " '("blue" "yellow" "green") nil t)))
                arg)
              (symbol-function 'def-nil-function)
              (lambda ()
                (completing-read "Prompt: " '("blue" "yellow" "green") nil t))
              (symbol-function 'cmd-that-calls-def-nil-function)
              (lambda ()
                (interactive)
                (funcall 'def-nil-function))
              (symbol-function 'def-nil-collection)
              (lambda (string pred action)
                (complete-with-action action '("blue" "yellow" "green") string pred))))

      (after-all
        (setf (symbol-function 'def-nil-command) nil
              (symbol-function 'def-nil-function) nil
              (symbol-function 'cmd-that-calls-def-nil-function) nil
              (symbol-function 'def-nil-collection) nil))

      (describe "when the specified functions are not in the list"
        (before-each
          (setq ido-cr+-nil-def-alternate-behavior-list nil))

        (it "should use empty string default in a command"
          (expect
           (with-simulated-input "RET"
             (call-interactively 'def-nil-command))
           :to-equal ""))

        (it "should use empty string default in a function"
          (expect
           (with-simulated-input "RET"
             (call-interactively 'cmd-that-calls-def-nil-function))
           :to-equal ""))

        (it "should use empty string default for a collection"
          (expect
           (with-simulated-input "RET"
             (ido-completing-read+ "Prompt: " 'def-nil-collection nil t))
           :to-equal "")))

      (describe "when the specified functions are in the list"
        (before-each
          (setq ido-cr+-nil-def-alternate-behavior-list
                (append '(def-nil-command
                           def-nil-function
                           def-nil-collection)
                        ido-cr+-nil-def-alternate-behavior-list)))

        (it "should not use empty string default in a command"
          (expect
           (with-simulated-input "RET"
             (call-interactively 'def-nil-command))
           :to-equal "blue"))

        (it "should not use empty string default in a function"
          (expect
           (with-simulated-input "RET"
             (call-interactively 'cmd-that-calls-def-nil-function))
           :to-equal "blue"))

        (it "should not use empty string default for a collection"
          (expect
           (with-simulated-input "RET"
             (ido-completing-read+ "Prompt: " 'def-nil-collection nil t))
           :to-equal "blue"))))

    ;; Test is currently disabled pending additional information
    (xit "should not hang or error when deleting characters in `org-refile' (issue #152)"
      (expect
       (progn
         (ido-ubiquitous-mode 1)
         (save-excursion
           (with-temp-buffer
             (org-mode)
             (insert (s-trim "
    * Heading 1
    ** Subheading 1.1
    ** Subheading 1.2
    ** Subheading 1.3
    * Heading 2
    * Heading 3
    "))
             (goto-char (point-max))
             ;; TODO Figure out what else needs to be set up to call
             ;; `org-refile'
             (with-simulated-input
                 "Heading DEL DEL DEL DEL DEL RET"
               (command-execute 'org-refile)))))
       :not :to-throw)))

  (describe "regressions should not occur for"
    (it "issue #151: should not hang or error when cycling matches in `Info-menu'"
      (expect
       (progn
         (ido-ubiquitous-mode 1)
         (with-temp-info-buffer
           (with-simulated-input
               '("emacs"
                 (ido-next-match)
                 (wsi-simulate-idle-time 5)
                 (ido-next-match)
                 (wsi-simulate-idle-time 5)
                 (ido-next-match)
                 (wsi-simulate-idle-time 5)
                 (ido-next-match)
                 (wsi-simulate-idle-time 5)
                 "RET")
             (command-execute 'Info-menu))))
       :not :to-throw))

    (it "issue #153: should preserve the selected item when doing a deferred dynamic update"
      (expect
       (with-simulated-input
           '("Emacs"
             (ido-next-match)
             (wsi-simulate-idle-time 5)
             "RET")
         (ido-completing-read+
          "Choose: "
          (collection-as-function '("Emacs" "Emacs A" "Emacs B" "Emacs C"))))
       :to-equal "Emacs A"))))

;;; test-ido-completing-read+.el ends here
