emacs-config
============

This repo is designed to be overlaid on top of the .emacs.d directory.

It takes care of several concerns:

* loading packages with a package manager, bootstrapping properly if they're not yet loaded
  (i.e. on a fresh machine)
  
* per-machine configuration.  The elisp/per-machine directory may optionally hold
  a .el file named as (system-name).el.  Replace system-name with whatever your
  system's name returns within emacs when you execute the (system-name) method.
  See snappy.local.el for an example.
  
* handling clojure with cider.

* handling lisp with slime.  Please note I consistently hit an issue with
  the package manager's default way of compiling the .emacs.d/elpa/slime*/contrib
  directory.  This shows up as slime failing to load slime-fancy or other slime
  modules after M-x slime.  It is easily fixed by changing to the
  .emacs.d/elpa/slime*/contrib directory, deleting all the .elc files in there,
  and then byte compiling the directory once again.
  
  See elisp/per-machine/snappy.local.el for declaring all the lisps on your
  system.  If you do this, then the slime "multiple lisps" startup approach
  will work (default keybinding: C-u - M-x slime).  The slime setup I use
  will filter those lisps so that only lisp exes found on your system are available.

For best performance, please ensure the elisp/ directory is byte compiled.
