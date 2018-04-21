;;; -*- lexical-binding: t -*-

;; This file contains tests specifically for ido-cr+ interoperating
;; with flx-ido. These tests were split out from the main test file,
;; which unfortunately means that they brought a lot of the
;; scaffolding from the main test file with them. This might get
;; cleaned up at a later date. Putting these in a separate file is
;; necessary so that the main test suite can be both with and without
;; flx-ido loaded.

(require 'ido)
(require 'flx-ido)
(require 'minibuf-eldef)
(require 'ido-completing-read+)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)

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
      flx-ido-mode
      (minibuffer-electric-default-mode t)))

    ;; Suppress all messages during tests
    (spy-on 'message))

  ;; Restore the saved values after each test
  (after-each
    (fully-unshadow-all-vars))

  (describe "the `ido-completing-read+' function"

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

      (describe "with flx-ido-mode"
        (before-each
          (flx-ido-mode 1)
          (flx-ido-reset))

        (it "should allow selection of dynamically-added completions"
          (expect
           (with-simulated-input "hello-w RET"
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

        (it "should respect `ido-restrict-to-matches' when doing dynamic updates"
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
            ;; Now test the same with a dynamic collection
            (expect
             (with-simulated-input "eee C-SPC bbb C-SPC ggg RET"
               (ido-completing-read+
                "Pick: " (collection-as-function collection) nil t nil nil (car collection)))
             :to-equal "bbb-eee-ggg")))))))

;;; test-ido-completing-read+.el ends here
