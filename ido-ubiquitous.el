;;; ido-ubiquitous.el --- Use ido (nearly) everywhere.

;; Copyright unknown; taken from
;; http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; EmacsWiki edit history is currently unavailable for proper crediting.

;; Author: Unknown
;; URL: http://www.emacswiki.org/emacs/InteractivelyDoThings#toc13
;; Version: 0.1
;; Created: 2011-09-01
;; Keywords: convenience
;; EmacsWiki: InteractivelyDoThings

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ido)

;;;###autoload
(defvar ido-ubiquitous-enabled t
  "If non-nil, use ido-completing-read instead of completing-read if possible.
    
  Set it to nil using let in around-advice for functions where the
  original completing-read is required.  For example, if a function
  foo absolutely must use the original completing-read, define some
  advice like this:
    
  (defadvice foo (around original-completing-read-only activate)
    (let (ido-ubiquitous-enabled) ad-do-it))")

;;;###autoload
(defadvice completing-read (around use-ido-when-possible activate)
  (if (or (not ido-ubiquitous-enabled) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(provide 'ido-ubiquitous) ;;; ido-ubiquitous.el ends here

