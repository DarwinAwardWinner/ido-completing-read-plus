# ido-ubiquitous

[![Join the chat at https://gitter.im/DarwinAwardWinner/ido-ubiquitous](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/DarwinAwardWinner/ido-ubiquitous?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Gimme some ido... everywhere! This pacakge replaces stock emacs
completion with ido completion wherever it is possible to do so
without breaking things.

Get it from MELPA: http://melpa.org/#/ido-ubiquitous

## Version 3.0 changes

ido-ubiquitous version 3.0 is a major update, including a split into
two packages, and some of the configuration options have changed in
non-backwards-compatible ways. If you have customized ido-ubiquitous,
be sure to check out `M-x customize-group ido-ubiquitous` and `M-x
customize-group ido-completing-read+` after updating to 3.0 and make
sure the new settings are to your liking.

# How to enable ido in as many places as possible

If you are using this package, you probably want to enable ido
everywhere that it is possible to do so. Here are all the places to
enable ido that I'm aware of. (Note that most of these variables can
also be set via `M-x customize-variable` if you prefer that.)

## Ido itself

First, enable `ido-mode` and `ido-everywhere`.

    (ido-mode 1)
    (ido-everywhere 1)

## ido-ubiquitous (this package)

Install this package and then turn on `ido-ubiquitous-mode`:

    (require 'ido-ubiquitous)
    (ido-ubiquitous-mode 1)

## Smex

Smex allows you to use ido for completion of commands in M-x, with
enhancements like putting your most-used commands at the front of the
list. First install the [smex](https://github.com/nonsequitur/smex)
package, then follow the directions to set up key-bindings for it.

## ido-yes-or-no

If you want to use ido for yes-or-no questions, even though it's
massive overkill, install the ido-yes-or-no package (soon to be
available from MELPA):
https://github.com/DarwinAwardWinner/ido-yes-or-no

## ido for `describe-face` and certain other commands

Some commands, such as `describe-face`, use `completing-read-multiple`
instead of `completing-read`. You can get ido completion for these
commands with `crm-custom-mode`, which replaces
`completing-read-multiple` with repeated calls to `completing-read`,
which would then use ido thanks to ido-ubiquitous-mode. First, install
the [crm-custom](https://github.com/DarwinAwardWinner/crm-custom)
package from MELPA: http://melpa.org/#/crm-custom

Then enable the mode:

    (crm-custom-mode 1)

Rememebr that when using this mode, completion for affected commands
will continue to read additional items until you use C-j to enter an
empty input, which terminates the completion. (Due to this quirk, I do
not find this mode to be very useful in conjunction with ido, but it
does work.)

## Packages with built-in ido support

Finally, some packages implement their own completion customizations,
and ido-ubiquitous specifically avoids interfering with these, so you
need to enable them separately.

* Org Mode: `(setq org-completion-use-ido t)`
* Magit: `(setq magit-completing-read-function 'magit-ido-completing-read)`
* Gnus: `(setq gnus-completing-read-function 'gnus-ido-completing-read)`

(You can also use `M-x customize-variable` to set all of these
options.)

## "But some commands still aren't using ido! What gives?"

There are some features of `completing-read` that ido cannot handle,
and by default ido-ubiquitous tries to get out of the way whenever it
detects that these features might be used. But the detection is not
perfect and errs on the side of caution, so ido may be disabled for
commands where it is actually perfectly safe. If you find a command
that you think should be using ido but isn't, you can try customizing
`ido-ubiquitous-command-overrides` to tell ido-ubiquitous that certain
commands are safe for ido completion, or you can customize
`ido-ubiquitous-allow-on-functional-collection` and
`ido-ubiquitous-max-items` to turn off the saveguards and always force
ido in ambiguous cases. Be aware, this could cause certain commands
not to work correctly or at all.

# How ido-ubiquitous decides when to replace `completing-read`

Emacs' `completing-read` is a complex function with many advanced
features and some quirks that are only maintained for backwards
compatibility. Not all of these features are supported by ido, so it
is impossible to always replace `completing-read` with ido completion.
Furthermore, it's not always possible to detect based on the arguments
to `completing-read` whether such ido-incompatible features are being
used or not. ido-ubiquitous uses a series of heuristics to determine
whether unsupported features *might* be used, and also supports an
override feature to correct it when the heuristics get things wrong.

Each time `completing-read` is invoked, ido-ubiquitous selects one of
3 modes:

* `enable`: use ido completion;
* `enable-old`: use ido completion, but with a compatibility fix for
old-style default selection; and
* `disable`: no ido completion.

The following describes how ido-ubiquitous selects the appropriate
mode, and what to do when you think it is making the wrong choice.

## When the collection is a function

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
the following option is to blame.

## When the collection is very large

Ido can get slow on very large collections, so by default
ido-ubiquitous disables itself for collections with more than 30,000
items in them. You can change this threshold by customizing
`ido-cr+-max-items`.

## Old-style default selection

The `enable-old` mode of operation is required because the old way for
`completing-read` to indicate that the user simply pressed RET and
selected the default option was to return an empty string. When this
old-style mode is used, `completing-read` doesn't even know what the
default is supposed to be -- the calling code handles all of that. But
in ido, simply pressing RET will return the first item of the list,
not an empty string. The way to enter an empty string in ido is C-j.
The `enable-old` mode enables ido completion, but swaps the meaning of
C-j and RET if you haven't entered any text or cycled the options yet
(once you do either of those, C-j and RET regain their standard
meanings). This allows you to select the default by pressing RET as
soon as the completion prompt appears, as intended (C-j would select
the first item).

Unfortuantely, there is no way for ido-ubiquitous to detect when a
command is using this old-style default selection, so instead it uses
a built-in set of overrides telling it about commands that are known
to use old-style defaults. If you discover a command where pressing
RET or C-j at an empty prompt is not doing what you expect it to,
there's a good chance that you need to add an `enable-old` override
for that command. Luckily, since this is an obsolete usage pattern, it
is unlikely that any Elisp functions written since 1990 or so will
need to be added to this list.

## "A command is not working the way I expect it to! What should I do?"

First, invoke the `ido-ubiquitous-debug-mode` and `ido-cr+-debug-mode`
commands (ido-cr+ is a lower-level package underlying ido-ubiquitous
and implementing generic improvements to `ido-completing-read`). Then,
with these two modes active, run the offending command. Then examine
the Messages buffer, where ido-ubiquitous will explain which mode of
operation it selected and why. Based on this, you can add an override
to `ido-ubiquitous-command-overrides`. If you are not familiar with
the structure of this variable, I recommend using `M-x
customize-variable` to edit it, which will help you get it right. If
ido completion was skipped ido completion because the collection was
too large, try giving `ico-cr+-max-items` a larger value, or set it to
nil if your computer is fast enough to handle any size of collection.

New versions often include new overrides, but Emacs will not edit your
override variables if you have already customized them. So, if you
have recently upgraded ido-ubiquitous, remember to invoke
`ido-ubiquitous-restore-default-overrides` to add in any new
overrides.

If you do add any overrides yourself, please also report them at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues so I can
add them to the default set for future versions.
