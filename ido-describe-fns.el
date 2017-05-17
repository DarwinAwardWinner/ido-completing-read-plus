;;; ido-describe-fns.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Ryan C. Thompson

;; Filename: ido-describe-fns.el
;; Author: Ryan C. Thompson
;; Created: Tue May 16 18:23:13 2017 (-0400)
;; Version: 3.17
;; Package-Requires: ((emacs "26.1") (ido-completing-read+ 3.17) (ido-ubiquitous 3.17))
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Keywords: ido, completion, convenience

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 

;; This package is an extension for ido-ubiquitous that implements ido
;; completion for the new `describe-*' family of commands. In recent
;; Emacs versions, these commands no longer work with ido-ubiquitous
;; because they now use a function-based collection argument to
;; implement auto-loading of the file corresponding to the prefix you
;; entered in order to offer completions of symbols from that file.

;; Note that there is no separate mode to enable. If
;; `ido-ubiquitous-mode' is already enabled, then simply loading this
;; package will enable it as well.

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

(defconst ido-describe-fns-version "3.17"
  "Currently running version of ido-describe-fns.

Note that when you update ido-describe-fns, this variable may not
be updated until you restart Emacs.")

(require 'ido)
(require 'ido-completing-read+)
(require 'ido-ubiquitous)

;;;###autoload
(defvar ido-descfns-enable-this-call nil
  "If non-nil, then the current call to `ido-completing-read+' is by `ido-descfns-read-from-help-symbol-completion-table'.")

(defvar ido-descfns-orig-predicate nil
  "Original predicate from the current completion call.")

(defun ido-descfns-maybe-load-prefixes (string)
  "Load any files needed to complete the current input.

This function auto-loads new files in the same way as
`help--symbol-completion-table', but without doing any
completion.

Returns non-nil if any new files were loaded."
  (let ((old-load-history load-history)
        (prefixes (radix-tree-prefixes (help-definition-prefixes) string)))
    (help--load-prefixes prefixes)
    (not (eq load-history old-load-history))))

(defun ido-descfns-post-self-insert-hook ()
  "Maybe load new files and update possible ido completions.

Has no effect unless `ido-descfns-enable-this-call' is non-nil."
  (when (and ido-descfns-enable-this-call
             (ido-descfns-maybe-load-prefixes ido-text))
    (with-no-warnings
      (setq ido-cur-list
            (all-completions "" obarray ido-descfns-orig-predicate)))))

(defun ido-descfns-setup ()
  (add-hook 'post-self-insert-hook #'ido-descfns-post-self-insert-hook)
  (add-hook 'minibuffer-exit-hook #'ido-descfns-cleanup))

(defun ido-descfns-cleanup ()
  (remove-hook 'post-self-insert-hook #'ido-descfns-post-self-insert-hook)
  (remove-hook 'minibuffer-exit-hook #'ido-descfns-cleanup)
  (remove-hook 'ido-setup-hook #'ido-descfns-setup))

;; This advice-based implementation is required for reentrancy
(defadvice ido-completing-read+ (around ido-descfns activate)
  (let ((ido-descfns-enable-this-call
         (and ido-ubiquitous-mode
              (eq collection 'help--symbol-completion-table)))
        ;; Each call gets its own private copy of these hooks
        (ido-setup-hook ido-setup-hook)
        (minibuffer-exit-hook minibuffer-exit-hook)
        (post-self-insert-hook post-self-insert-hook))
    ;; Clean up our copy of the hook in case of recursive completion.
    (ido-descfns-cleanup)
    (when ido-descfns-enable-this-call
      ;; Convert the initial collection into something ido-cr+ will
      ;; accept
      (setq collection obarray
            ido-descfns-orig-predicate predicate)
      (add-hook 'ido-setup-hook #'ido-descfns-setup))
    ad-do-it
    (ido-descfns-cleanup)))

(add-hook 'ido-cr+-before-fallback-hook #'ido-descfns-cleanup)

(provide 'ido-describe-fns)

;;; ido-describe-fns.el ends here
