;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-


(when (featurep 'ido-completing-read+)
  (error "Undercover loaded too late"))
(message "Setting up undercover")
(require 'undercover)
(defadvice undercover--edebug-files (before echo-instrumented-files activate)
  (message "Undercover instrumenting the following files: %S" files))
(undercover "ido-completing-read+.el")
