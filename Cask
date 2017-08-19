;; -*- mode: emacs-lisp -*-

(source gnu)
(source melpa)

(package-file "ido-completing-read+.el")

(depends-on "s")
(depends-on "memoize" "1.1")

(development
 (depends-on "flx-ido")
 (depends-on "with-simulated-input"
             :git "https://github.com/DarwinAwardWinner/with-simulated-input.git"
             :files ("*.el"))
 (depends-on "buttercup"
             :git "https://github.com/DarwinAwardWinner/emacs-buttercup.git"
             :branch "expect-closures"
             :files ("*.el"
                     ("bin" "bin/*")))
 (depends-on "undercover"))
