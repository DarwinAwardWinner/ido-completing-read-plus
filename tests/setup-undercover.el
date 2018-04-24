(require 'undercover)
(undercover "*.el"
            (:exclude "test-*.el"))
