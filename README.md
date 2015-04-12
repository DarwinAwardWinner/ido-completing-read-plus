# ido-ubiquitous

Gimme some ido... everywhere! This pacakge replaces stock emacs
completion with ido completion wherever it is possible to do so
without breaking things.

Get it from http://marmalade-repo.org/packages/ido-ubiquitous

Note that ido-ubiquitous is not enabled for org mode or magit mode,
because those modes have their own built-in support for ido, which you
should enable instead. Also, to enable ido for all file and buffer
completion, customize `ido-everywhere`.

ido-ubiquitous version 3.0 is a major update, including a split into
two packages, and some of the configuration options have changed in
non-backwards-compatible ways. If you have customized ido-ubiquitous,
be sure to check out `M-x customize-group ido-ubiquitous` and `M-x
customize-group ido-completing-read+` after updating to 3.0 and make
sure the new settings are to your liking.
