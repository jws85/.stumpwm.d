# Introduction

This is my StumpWM config, oriented around:

 - Vim-ish keybindings
 - Greatly decreased use of prefix keys

I stripped out all of the old prefix key stuff in favor of hanging
most things off the *top-map* and instead chording with the Super key,
like with most other window managers.  I prefer to let my other apps
(*cough*emacs*cough*) have Control and Meta/Alt.

Moving around frames is Super-hjkl.  Moving windows between them
is Super-HJKL.

Switching between groups is Super-[number].  Moving windows between
groups is Super-Shift-[number].

# Other niceties

I have StumpWM handling dbus notifications and notification area
stuff using stumpwm-contrib modules.

StumpWM also groks TTF fonts (rather than really old bitmap fonts
which go for an `a e s t h e t i c` that I don't want to go whole hog
on); this had to be added by a contrib module as well.

# Installation

This will take a 10,000 ft. view on the process, because there's a
bit to it.

Install SBCL, then install [https://www.quicklisp.org/beta/](Quicklisp)
in it.  Then install the following Lisp packages via Quicklisp:

- stumpwm
- swank
- dbus (for notify)
- xml-emitter (for notify)
- clx-truetype (for ttf-fonts)
- xembed (for stumptray)

You will probably run into issues here.  Install stuff as needed.  For
instance, on Ubuntu 18.04, dbus will not install without
`libfixposix-dev`.  I don't even know what that's for.  Whatever.

Somehow rig your setup so that your login manager dumps you into an
.xinitrc or .xsession.  That file should have something like:

```shell
#!/bin/bash
exec sbcl --load $HOME/.stumpwm.d/kickstart.lisp
```

Now try to log in.  If it fails, oh well, fix something and try again?

You can put your autolaunching programs in the .xinitrc/xsession; I
prefer that to starting them from StumpWM.
