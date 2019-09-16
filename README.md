# ido-completing-read+ (formerly ido-ubiquitous) #

[![MELPA Stable](https://stable.melpa.org/packages/ido-completing-read+-badge.svg)](https://stable.melpa.org/#/ido-completing-read%2B)
[![Join the chat at https://gitter.im/DarwinAwardWinner/ido-ubiquitous](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/DarwinAwardWinner/ido-ubiquitous?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/DarwinAwardWinner/ido-completing-read-plus.svg?branch=master)](https://travis-ci.org/DarwinAwardWinner/ido-completing-read-plus)
[![Coverage Status](https://coveralls.io/repos/github/DarwinAwardWinner/ido-completing-read-plus/badge.svg?branch=master)](https://coveralls.io/github/DarwinAwardWinner/ido-completing-read-plus?branch=master)

This package replaces stock emacs completion with ido completion
wherever it is possible to do so without breaking things (i.e. what
you were probably hoping for when you set `ido-everywhere` to `t`).

Get it from MELPA: https://stable.melpa.org/#/ido-completing-read+

## Version 4.0 changes ##

Long-time users should know that ido-completing-read+ version 4.0 is a
major update. The previously separate ido-ubiquitous package has been
merged into ido-completing-read+, which now provides all the features
of both packages. The distinction between "new" and "old" default
selection styles has been eliminated and replaced by a new variable
`ido-cr+-nil-def-alternate-behavior-list` (see [FAQ][1] for details),
and the override system has been accordingly simplified into just a
blacklist and a whitelist. If you have previously customized any
ido-ubiquitous options, be sure to check out

    `M-x customize-group ido-completing-read+`
    
after updating to 4.0 and make sure the new settings are to your
liking.

The short-lived ido-describe-fns package has likewise been subsumed
into this one.

[1]: #why-does-ret-sometimes-not-select-the-first-completion-on-the-list--why-is-there-an-empty-entry-at-the-beginning-of-the-completion-list--what-happened-to-old-style-default-selection

# How to enable ido in as many places as possible #

If you are using this package, you probably want to enable ido
everywhere that it is possible to do so. Here are all the places to
enable ido that I'm aware of. (Note that most of these variables/modes
can also be set/enabled via `M-x customize-variable` if you prefer
that.)

## Ido itself ##

First, enable `ido-mode` and `ido-everywhere`.

```elisp
(ido-mode 1)
(ido-everywhere 1)
```

## ido-completing-read+ (this package) ##

Install this package [from MELPA](https://melpa.org/#/ido-completing-read+)
and then turn on `ido-ubiquitous-mode`:

```elisp
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
```

## Amx ##

Amx, another of my packages, allows you to use alternate completion
systems like ido for commands in M-x, with enhancements like putting
your most-used commands at the front of the list. First install
[amx](https://melpa.org/#/amx) from MELPA, then turn on `amx-mode`:

```elisp
(require 'amx)
(amx-mode 1)
```

## ido-yes-or-no ##

If you want to use ido for yes-or-no questions, even though it's
massive overkill, install my [ido-yes-or-no package from MELPA](http://melpa.org/#/ido-yes-or-no), and then enable the mode:

```elisp
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)
```

## ido for `describe-face` and certain other commands ##

Some commands, such as `describe-face`, use `completing-read-multiple`
instead of `completing-read`. You can get ido completion for these
commands with `crm-custom-mode`, which replaces
`completing-read-multiple` with repeated calls to `completing-read`,
which would then use ido thanks to ido-ubiquitous-mode. First, install
the [crm-custom](https://github.com/DarwinAwardWinner/crm-custom)
package [from MELPA](http://melpa.org/#/crm-custom), then enable the
mode:

```elisp
(require 'crm-custom)
(crm-custom-mode 1)
```

Make sure to read and understand the FAQ entry below about the empty
entry at the beginning of the completion list before using this mode,
or using it will likely be very confusing.

## Packages with built-in ido support ##

Lastly, some packages already provide their own interfaces to ido, so
ido-completing-read+ specifically avoids interfering with these. If
you use any of the following packages, you need to enable ido for each
of them separately.

* [Magit](https://magit.vc/): `(setq magit-completing-read-function 'magit-ido-completing-read)`
* [Gnus](http://www.gnus.org/): `(setq gnus-completing-read-function 'gnus-ido-completing-read)`
* [ESS](https://ess.r-project.org/): `(setq ess-use-ido t)`

## icomplete-mode ##

For any case where ido cannot be used, there is another older mode
called `icomplete-mode` that integrates with standard emacs completion
and adds some ido-like behavior. It is built in to emacs, so no
installation is necessary. Just load the file and enable the mode:

```elisp
(require 'icomplete)
(icomplete-mode 1)
```

# Frequently asked questions #

## How does ido-ubiquitous-mode decide when to replace `completing-read`? <br/> Why don't some commands use ido completion? ##

Emacs' `completing-read` is a complex function with many complex
features. Not all of these features are supported by ido, so it is
impossible to always replace `completing-read` with ido completion.
Trying to use ido when these features are requested can cause
confusing and unexpected behavior or even completely break the
completion system. So, ido-completing-read+ tries to get out of the
way whenever it detects that these features might be used by a given
call to `completing-read`. Furthermore, it's not always possible to
detect based on the arguments to `completing-read` whether such
ido-incompatible features are being used or not, so
ido-completing-read+ also comes with a blacklist of functions that are
known not to work with ido. You can inspect this blacklist using

    M-x describe-variable ido-cr+-function-blacklist

If you want to know why a certain command isn't getting ido
completion, you can enable `ido-cr+-debug-mode` and then run the
command. There should then be a line in the `*Messages*` buffer that
explains the reason for disabling ido completion.

## Why does RET sometimes not select the first completion on the list? <br/> Why is there an empty entry at the beginning of the completion list? <br/> What happened to old-style default selection? ##

The simplest way to think about this is that if the command that
called `completing-read` didn't specify a default, then the default is
the empty string. In other words, `""` is the default default.

Previous versions of ido-ubiquitous-mode gave special consideration to
cases where a default value was not provided to `completing-read` and
the user pressed RET without entering any text. The expected behavior
is that `completing-read` should return the empty string in this case,
which indicates to the calling function that the user did not select
any completion. This conflicts with the standard ido behavior of
selecting the first available completion when pressing RET, and this
conflict was previously resolved by having two different modes that
differed in their handling of RET on an empty input. Now there is only
one mode, and the no-default case is handled by acting as if the empty
string was specified as the default, which more closely matches the
behavior of standard emacs completion. Since you, the user, have no
way of knowing how `completing-read` was called, you can tell when
this is occurring by watching for the appearance of an empty
completion at the front of the list. Compare:

If the command specifies apple as the default when calling
`completing-read`, the prompt will look like this, and pressing RET
will select "apple":

    Pick a fruit: {apple | banana | cherry | date}
    
However, if the command does not specify any default, an extra empty
option is displayed before the first option, and pressing RET will
select this empty option and return "":

    Pick a fruit: { | apple | banana | cherry | date}

To select "apple" instead, you must first press the right arrow key
once, or type an "a", before pressing RET.

However, some commands don't take this quirk of `completing-read` into
account and don't expect it to ever return an empty string when
`require-match` is non-nil. You can accommodate these functions by
adding them to `ido-cr+-nil-def-alternate-behavior-list`.

## How can I troubleshoot when ido-completing-read+ isn't doing what I want? ##

First, invoke the `ido-cr+-debug-mode` command. Then, run the command
or code that you are having trouble with, and when the completion
prompt appears, make a selection to complete the process. Then,
examine the Messages buffer, where ido-completing-read+ will explain
which mode of operation it selected and why. Based on this, you can
add an entry to `ido-cr+-function-blacklist`, or take some other
appropriate action.

Updates to ido-completing-read+ may include new blacklist entries, but
Emacs will not edit your override variables if you have already
customized them. So, if you have recently upgraded
ido-completing-read+, remember to invoke `ido-cr+-update-blacklist` to
add in any new overrides. By default, ido-completing-read+ will remind
you to do this whenever a new version adds to the blacklist. For more
information, see:

    M-x describe-variable ido-cr+-auto-update-blacklist

## Where can I report bugs? ##

If you end up adding any blacklist entries, please report them at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues so I can
incorporate them into the defaults for future versions. You can also
report any bugs you find in ido-completing-read+.

## I'm getting some weird warnings from ido-completing-read+ when Emacs starts. ##

I've gotten numerous reports about nonsensical warnings produced by
this package, such as "free variable" warnings about variables that
are most definitely properly declared, or warnings that only appear
when ido-completing-read+ is loaded after another unrelated package.
For many of these warnings, I've never been able to discover the cause
or consistently reproduce the warnings myself, and I've given up
trying to figure it out. Please don't report any new bugs about
variable warnings *unless* you can tell me how to consistently
reproduce them starting from `emacs -Q`. If you are an Emacs expert
who knows how to fix these warnings, please let me know.

You can see the bug reports about weird warnings
[here](https://github.com/DarwinAwardWinner/ido-ubiquitous/issues?utf8=%E2%9C%93&q=label%3Abizarre-unexplainable-scoping-issues+).

## What is the "bleeding-edge" branch? ##

All users should just use the master branch, or better yet, install
from MELPA. The bleeding-edge branch is where I test experimental and
unfinished features. Because ido-completing-read+ hooks deeply into
the bowels of Emacs, a bug in ido-completing-read+ could easily freeze
or crash Emacs entirely. Additionally, some bug only show up when
ido-completing-read+ is installed and compiled as a package. So I test
every new feature myself for some time on this branch before pushing
to the master branch. If you report a bug, I might develop a fix for
it on the bleeding edge branch and ask then you to try this branch.
Otherwise, normal users don't need to think about this branch.

## Running the tests

This package comes with a test suite. If you want to run it yourself,
first install the [cask](http://cask.readthedocs.io/en/latest/)
dependency manager. Then, from the package directory, run `cask
install` to install all the development dependencies, in
particular
[buttercup](https://github.com/jorgenschaefer/emacs-buttercup).
Finally, to run the tests, execute `cask exec buttercup -L .`. Please
run this test suite before submitting any pull requests, and note in
the pull request whether any of the tests fail.
