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

# How to enable ido in as many places as possible

If you are using this package, you probably want to enable ido
everywhere that it is possible to do so. Here are all the place to
enable ido that I'm aware of.

## Ido itself

First, enable `ido-mode` and `ido-everywhere`.

    (ido-mode 1)
    (ido-everywhere 1)

## Smex

Smex allows you to use ido for completion of commands in M-x. First
install the [smex](https://github.com/nonsequitur/smex) package, then
follow the directions to set up key-bindings for it.

## ido completion in org-mode and magit

Org-mode and magit have their own support for ido:

    (setq org-completion-use-ido t)
    (setq magit-completing-read-function 'magit-ido-completing-read)

## ido-ubiquitous (this package)

Install this package and then turn on `ido-ubiquitous-mode`:

    (ido-ubiquitous-mode 1)
