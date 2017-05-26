# ido-ubiquitous #

[![MELPA Stable](https://stable.melpa.org/packages/ido-ubiquitous-badge.svg)](https://stable.melpa.org/#/ido-ubiquitous)
[![Join the chat at https://gitter.im/DarwinAwardWinner/ido-ubiquitous](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/DarwinAwardWinner/ido-ubiquitous?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Gimme some ido... everywhere! This package replaces stock emacs
completion with ido completion wherever it is possible to do so
without breaking things.

Get it from MELPA: https://stable.melpa.org/#/ido-ubiquitous

## Version 3.0 changes ##

Long-time users should know that ido-ubiquitous version 3.0 is a major
update, including a split into two packages, and some of the
configuration options have changed in non-backwards-compatible ways.
If you have customized any options ido-ubiquitous, be sure to check
out `M-x customize-group ido-ubiquitous` and `M-x customize-group
ido-completing-read+` after updating to 3.0 and make sure the new
settings are to your liking.

# How to enable ido in as many places as possible #

If you are using this package, you probably want to enable ido
everywhere that it is possible to do so. Here are all the places to
enable ido that I'm aware of. (Note that most of these variables/modes
can also be set/enabled via `M-x customize-variable` if you prefer
that.)

## Ido itself ##

First, enable `ido-mode` and `ido-everywhere`.

    (ido-mode 1)
    (ido-everywhere 1)

## ido-ubiquitous (this package) ##

Install this package [from MELPA](http://melpa.org/#/ido-ubiquitous)
and then turn on `ido-ubiquitous-mode`:

    (require 'ido-ubiquitous)
    (ido-ubiquitous-mode 1)

## ido-describe-fns ##

Newer versions of Emacs have a special feature where functions like
`describe-variable` and `describe-function` can automatically load the
file whose prefix you have typed in order to offer the symbols defined
in that package as completions. Unfortunately, this non-standard
completion with a dynamically updating set of options is incompatible
with ido. The ido-describe-fns package allows ido to work with these
functions again. Simply install it and then do

    (require 'ido-describe-fns)

If `ido-ubiquitous-mode` is enabled, then simply loading this package
will enable it as well.

## Smex ##

Smex allows you to use ido for completion of commands in M-x, with
enhancements like putting your most-used commands at the front of the
list. First install the [smex](https://github.com/nonsequitur/smex)
package, then follow the directions to load it and replace your normal
M-x key-binding with smex:

    (require 'smex) ; Not needed if you use package.el
    (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                      ; when Smex is auto-initialized on its first run.
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; This is your old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(These directions are the same ones given in the smex README file.)

## ido-yes-or-no ##

If you want to use ido for yes-or-no questions, even though it's
massive overkill, install my [ido-yes-or-no package from MELPA](http://melpa.org/#/ido-yes-or-no), and then enable the mode:

    (require 'ido-yes-or-no)
    (ido-yes-or-no-mode 1)

## ido for `describe-face` and certain other commands ##

Some commands, such as `describe-face`, use `completing-read-multiple`
instead of `completing-read`. You can get ido completion for these
commands with `crm-custom-mode`, which replaces
`completing-read-multiple` with repeated calls to `completing-read`,
which would then use ido thanks to ido-ubiquitous-mode. First, install
the [crm-custom](https://github.com/DarwinAwardWinner/crm-custom)
package [from MELPA](http://melpa.org/#/crm-custom), then enable the
mode:

    (require 'crm-custom)
    (crm-custom-mode 1)

Remember that when using this mode, completion for affected commands
will continue to read additional items until you use C-j to enter an
empty input, which terminates the completion. (Due to this quirk, I do
not find this mode to be very useful in conjunction with ido, but it
does work.)

## Packages with built-in ido support ##

Lastly, some packages already provide their own interfaces to ido, so
ido-ubiquitous specifically avoids interfering with these. If you use
any of the following packages, you need to enable ido for each of them
separately.

* [Magit](https://magit.vc/): `(setq magit-completing-read-function 'magit-ido-completing-read)`
* [Gnus](http://www.gnus.org/): `(setq gnus-completing-read-function 'gnus-ido-completing-read)`
* [ESS](https://ess.r-project.org/): `(setq ess-use-ido t)`

## icomplete-mode ##

For any case where ido cannot be used, there is another older mode
called `icomplete-mode` that integrates with standard emacs completion
and adds some ido-like behavior. It is built in to emacs, so no
installation is necessary. Just load the file and enable the mode:

    (require 'icomplete)
    (icomplete-mode 1)

# Frequently asked questions #

## How does ido-ubiquitous decide when to replace `completing-read`? <br/>  Why don't some commands use ido completion? ##

Emacs' `completing-read` is a complex function with many advanced
features and some quirks that are only maintained for backwards
compatibility. Not all of these features are supported by ido, so it
is impossible to always replace `completing-read` with ido completion.
Trying to use ido when these features are requested can completely
break the completion system. So, by default ido-ubiquitous tries to
get out of the way whenever it detects that these features might be
used by a given call to `completing-read`. Furthermore, it's not
always possible to detect based on the arguments to `completing-read`
whether such ido-incompatible features are being used or not.
ido-ubiquitous uses a series of heuristics to determine whether
unsupported features *might* be used, and also supports an override
feature to correct it when the heuristics get things wrong.

Each time a function invokes `completing-read`, ido-ubiquitous selects
one of 3 modes:

* `enable`: use ido completion;
* `enable-old`: use ido completion, but with a compatibility fix for
old-style default selection; and
* `disable`: no ido completion.

The following describes in detail how ido-ubiquitous selects the
appropriate mode, and what to do when you think it is making the wrong
choice.

### Disabling ido completion when the collection is very large ###

Ido can get slow on very large collections, so by default
ido-ubiquitous disables itself for collections with more than 30,000
items in them. This rule takes precedence over anything else, even
overrides that explicitly enable ido completion for a command.

You can change the large collection threshold by customizing
`ido-cr+-max-items`.

### Disabling ido completion when the collection is a function ###

One feature of `completing-read` is that the collection argument can
be a function. This function could simply return a list of all
possible completions, in which case it would be safe to use ido, or it
could implement a completely new completion system, in which case
using ido would interfere with this new completion system (for an
example of this, see the `tmm` command). But ido-ubiquitous cannot
tell by looking at the function which kind it is, so it errs on the
side of caution and disables itself whenever the collection is a
function, unless an override exists telling it that the command is
safe for ido completion. You can turn off this safeguard by
customizing `ido-ubiquitous-allow-on-functional-collection`. Be aware
that enabling this will likely break completion entirely in any
command that uses this feature to implement non-standard completion.

If you run across a command that unexpectedly uses normal Emacs
completion instead of ido completion, it's likely that either this or
the previous option is to blame.

### Disabling or enabling ido completion by overrides ###

If ido-ubiquitous is not running for a command where is should be due
to the functional collection rule, you can add an override for that
command by using `M-x customize-variable
ido-ubiquitous-command-overrides`. Conversely, if ido completion
causes problems for a command, you can also use this to disable
ido-ubiquitous for that command.

## What is old-style default selection? ##

The `enable-old` mode of operation is required because the old way for
`completing-read` to indicate that the user simply pressed RET and
selected the default option was to return an empty string. When this
old-style mode is used, `completing-read` doesn't even know what the
default is *supposed* to be -- the calling code handles all of that.
But in ido, simply pressing RET will return the first item of the
list, not an empty string. The way to enter an empty string in ido is
C-j. The `enable-old` mode enables ido completion, but swaps the
meaning of C-j and RET if you haven't entered any text or cycled the
options yet (once you do either of those, C-j and RET regain their
standard meanings). This allows you to select the default by pressing
RET as soon as the completion prompt appears, as intended (C-j would
select the first item).

Unfortunately, there is no way for ido-ubiquitous to detect when a
command is using this old-style default selection, so instead it uses
a built-in set of overrides telling it about commands that are known
to use old-style defaults. If you discover a command where pressing
RET or C-j at an empty prompt is not doing what you expect it to,
there's a good chance that you need to add an `enable-old` override
for that command to `ido-ubiquitous-command-overrides`. Luckily, since
this is an obsolete usage pattern, it is unlikely that any Elisp
functions written since 1990 or so will need to be added to this list.
Hopefully all uses of old-style completion will eventually be
eliminated, and with them, the need for this feature of
ido-ubiquitous.

## How can I troubleshoot when ido-ubiquitous isn't doing what I want? ##

First, invoke the `ido-ubiquitous-debug-mode` and `ido-cr+-debug-mode`
commands. Then, with these two modes active, run the command(s) that
you are having trouble with, and when the completion prompt appears,
make a selection to complete the process. Then, examine the Messages
buffer, where ido-ubiquitous will explain which mode of operation it
selected and why. Based on this, you can add an override to
`ido-ubiquitous-command-overrides`. If you are not familiar with the
structure of this variable, I recommend using `M-x customize-variable`
to edit it, which will help you get it right. If ido completion was
skipped ido completion because the collection was too large, try
giving `ico-cr+-max-items` a larger value, or set it to nil if your
computer is fast enough to handle any size of collection.

Updates to ido-ubiquitous often include new overrides, but Emacs will
not edit your override variables if you have already customized them.
So, if you have recently upgraded ido-ubiquitous, remember to invoke
`ido-ubiquitous-restore-default-overrides` to add in any new
overrides. (In the future, this process may become automatic:
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/90)

## Where can I report bugs or suggest new overrides? ##

If you end up adding any overrides, please also report them at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues so I can
incorporate them into the defaults for future versions. You can also
report any bugs you find in ido-ubiquitous.

## I'm getting some weird warnings from ido-ubiquitous when Emacs starts. ##

I've gotten numerous reports about nonsensical warnings produced by
ido-ubiquitous, such as "free variable" warnings about variables that
are most definitely properly declared, or warnings that only appear
when ido-ubiquitous is loaded after another unrelated package. For
many of these warnings, I've never been able to discover the cause or
reproduce the warnings myself, and I've given up trying to figure it
out. Please don't report any new bugs about variable warnings *unless*
you can tell me how to consistently reproduce them starting from
`emacs -Q`. If you are an Emacs expert who knows how to fix these
warnings, please let me know.

You can see the bug reports about weird warnings
[here](https://github.com/DarwinAwardWinner/ido-ubiquitous/issues?utf8=%E2%9C%93&q=label%3Abizarre-unexplainable-scoping-issues+).

## What is the "bleeding-edge" branch? ##

All users should just use the master branch, or better yet, install
from MELPA. The bleeding-edge branch is where I test experimental and
unfinished features. Because ido-ubiquitous hooks deeply into the
bowels of Emacs, a bug in ido-ubiquitous could easily freeze or crash
Emacs entirely. Additionally, some bug only show up when
ido-ubiquitous is installed and compiled as a package. So I test every
new feature myself for some time on this branch before pushing to the
master branch. If you report a bug, I might develop a fix for it on
the bleeding edge branch and ask then you to try this branch.
Otherwise, normal users don't need to think about this branch.

# ido-completing-read+ #

As of version 3.0, most of the core functionality of ido-ubiquitous
has been spun off into a separate library called ido-completing-read+,
or "ido-cr+" for short. ido-cr+ incorporates all the features of
ido-ubiquitous that are actually just generic improvements to ido that
should probably always be enabled. It implements these fixes in a
single function `ido-completing-read+`, which should be suitable as a
drop-in replacement for either `ido-completing-read` or
`completing-read`. Notably, unlike the original `ido-completing-read`,
`ido-completing-read+` should always do something useful, by falling
back to normal completion when ido completion won't work.
Additionally, it allows you to manually fall back using C-f and C-b,
in the same way you can use those keys to switch between file and
buffer completion in ido. As a user, you don't really need to know
anything about ido-cr+. However, if you are writing an Emacs package
and would like to incorporate ido completion, you may wish to use
ido-cr+ to get more robust completion with fewer weird edge cases.

# Running the tests #

Since ido-ubiquitous is a package that is fundamentally about user
interaction, testing it is a bit tricky. In particular, it cannot be
tested if Emacs in running in batch mode, because there are is no way
to read user input. Nevertheless, ido-ubiquitous has a test suite,
which must be run in a special way to avoid running Emacs in batch
mode. First, install the [cask](http://cask.readthedocs.io/en/latest/)
dependency manager. Then, from the ido-ubiqutous directory, run `cask
install` to install all the development dependencies, in
particular [ert-runner](https://github.com/rejeep/ert-runner.el).
Finally, to run the tests, you must instruct ert-runner not to use
batch mode using either the `--win` option. (The `--no-win` option
also works.) So the command to run the tests is `cask exec ert-runner
--win`. An emacs window will briefly appear (or, if you use
`--no-win`, emacs will briefly launch in the terminal) and the tests
will run, and finally the test results will be printed. You should see
something like this:

```
    .........

    Ran 9 tests in 0.371 seconds
````

Please run this test suite before submitting any pull requests.
