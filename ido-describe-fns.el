;;; ido-describe-fns.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Ryan C. Thompson

;; Filename: ido-describe-fns.el
;; Author: Ryan C. Thompson
;; Created: Tue May 16 18:23:13 2017 (-0400)
;; Version: 3.17
;; Package-Requires: ((emacs "26") (ido-completing-read+ 3.17) (ido-ubiquitous 3.17))
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Keywords: ido, completion, convenience

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 

;; This package is an extension for ido-ubiquitous that implements ido
;; completion for the new implementation of the `describe-*' family of
;; commands. In Emacs 26 and above, these commands no longer work with
;; ido-ubiquitous because they now use a function-based collection
;; argument to implement auto-loading of the file corresponding to the
;; prefix you entered in order to offer completions of symbols from
;; that file.

;; Note that there is no separate mode to enable. If
;; `ido-ubiquitous-mode' is already enabled, then simply loading this
;; package will enable it as well.

;; This package has no effect in Emacs versions earlier than 26, so it
;; is safe to install it unconditionally in an Emacs config shared by
;; multiple Emacs versions.

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

(defun ido-descfns-maybe-load-prefixes (string)
  "Load any files needed to complete the current input.

This function auto-loads new files in the same way as
`help--symbol-completion-table', but without doing any
completion.

Returns non-nil if any new files were loaded."
  (when (bound-and-true-p help-definition-prefixes)
    (let* ((old-load-history load-history)
           (prefixes (radix-tree-prefixes (help-definition-prefixes) string)))
      (help--load-prefixes prefixes)
      (not (eq load-history old-load-history)))))

;; `ido-exhibit' is the ido post-command hook
(defadvice ido-exhibit (before ido-descfns activate)
  "Maybe load new files and update possible ido completions.

Has no effect unless `ido-descfns-enable-this-call' is non-nil."
  (when (and (ido-ubiquitous-active)
             (ido-descfns-maybe-load-prefixes ido-text))
    (with-no-warnings
      (setq ido-cur-list
            (all-completions "" obarray (nth 2 ido-cr+-orig-completing-read-args)))
      (ido-ubiquitous--debug-message "ido-describe-fns loaded new files. `ido-cur-list' now has %i items" (length ido-cur-list)))))

(provide 'ido-describe-fns)

;;; ido-describe-fns.el ends here
