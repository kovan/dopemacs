![Image](../../blob/master/dopemacs.png?raw=true)
![Image](../../blob/master/dopemacs2.png?raw=true)

dopemacs
========

Emacs configuration that aims to add lots of enabled-by-default,
non-intrusive useful features while keeping traditional Emacs
keybindings and workflow.

Basically it consists just of a bunch of modules from ELPA, MELPA and
Marmalade that are downloaded the first time Emacs is started, and a
bunch of customize-variable statements that provide a better (IMHO)
defaults. So it very easy to customize. It contains as little elisp as
possible on its own and it is an aim of the project to keep it that
way.

**Installation:**

- Clone the repository to ~/.emacs.d

**Requirements:**

- Emacs >= 24

**F.A.Q**
- *Flycheck is not working for language X.* You probably need to install the appropiate syntax checker in your system, see https://github.com/flycheck/flycheck#quick-start.
- *error: Package `xxxx' is not available for installation*. Sometimes the repositories give errors. Try relanunching Emacs a couple times and if it still doesn't work, file a bug.

You should also check:
- https://github.com/bbatsov/prelude
- https://github.com/rdallasgray/graphene
- https://github.com/xiaohanyu/oh-my-emacs


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/kovan/dopemacs/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

