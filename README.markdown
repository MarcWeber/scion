
Introduction
============

[Scion][home] is a Haskell library that aims to provide Haskell source
code inspection and transformation functionality as well as various
other features that may be useful for an IDE.

Most of Scion's functionality is based on the GHC API.  Scion tries to
be front-end agnostic; it provides both a Haskell API and servers for
non-Haskell clients such as Emacs (no Vim, volunteers required).

  [home]: http://code.google.com/p/scion-lib/


Installation
============

(For developer builds see section "Hacking" below.)

Scion requires [GHC 6.10.1][ghc] or later.  All other dependencies
should be on [Hackage][hackage] and can be installed using
[cabal-install][ci]:

    $ cd dir/to/scion
    $ cabal install

Scion supports various configuration flags which are useful when
working on Scion itself.

  [ghc]: http://haskell.org/ghc/download.html
  [hackage]: http://hackage.haskell.org/packages/hackage.html
  [ci]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall



Usage
=====

TODO

Emacs
-----

TODO

    $ cd <scion>
    $ cabal install -femacs
    $ ./.cabal/bin/emacs_server

Emacs:

    (add-to-list 'load-path "<scion>/emacs")
    (require 'scion)
    
    (add-hook 'haskell-mode-hook 'my-scion-hook)
    (defun my-scion-hook ()
      (scion-mode 1))
    
    M-x scion-connect
    M-x scion-open-cabal-project
    M-x scion-load-library

    C-c i l  -- insert language pragma
    C-c i p  -- insert pragma
    C-c i m  -- insert (external) module name


Vim:
    ensure :echo has('python')
    returns 1

    add to your ~/.vimrc (TODO make this lazy so that python is only loaded when required!):

      py scionConnectionSetting = ('socket', ("localhost",4005))
      set runtimepath+=<path to scion repo/vim_runtime_path/>

    :e some_hs_file.hs
    :OpenCabalProject

    :LoadComponent library
    or
    :LoadComponent executable:cabal_executable_name

    At this point you should already get some compilation errors.

    use
    :BackgroundTypecheckFile

    before
    :ThingAtPoint
    You should see something like:
      {'Just': 'print :: [Char] -> IO ()'}
    
    Have a look at vim_runtime_path/ftplugin/haskell.vim to see a list of all
    commands which are implemented yet.

Bug Reports
===========

Please send bug reports or feature requests to the [Issue tracker][issues].

  [issues]: http://code.google.com/p/scion-lib/issues/list

Discussion
==========

For discussions about Scion use the [scion-lib-devel][ml] mailing list.

  [ml]: http://groups.google.com/group/scion-lib-devel


Hacking
=======

The main repository for Scion is hosted on [Github][gh].  Get it via

    $ git clone git://github.com/nominolo/scion

Send patches or pull requests to nominolo (email address at googlemail
dot com).  Note that, if you fork the project on Github your fork
won't take up additional space on your account.

  [gh]: http://github.com


Building
--------

For development it is probably easier to use the GNU make than Cabal
directly.  The makefile includes a file called `config.mk` which is
not present by default.  You can use the provided `config.mk.sample`
and edit it:

    $ cp config.mk.sample config.mk
    $ edit config.mk

After that, the makefile takes care of the rest.

    $ make           # configure and build
    $ make install   # configure, build, and install

If you don't have the dependencies, yet, and have `cabal-install`, the
following may be helpful (If it's not in the path, adjust `config.mk`
accordingly):

    $ make cabal-install

(This also installs Scion, but that shouldn't interfere with hacking.)


Using an in-place GHC
---------------------

GHC 6.10.1 has a couple of problems.  For example, not all error
messages are reported using the GHC API but instead are printed to
stdout/stderr.  Some parts also call `exitWith` directly.  GHC's HEAD
branch has some of these bugs fixed and may contain new features not
present in the stable branch.  If you want to compile against an
inplace GHC, the following steps should work:

 1. On windows, make sure that Cabal finds the inplace gcc

        $ cd /path/to/ghc
        $ cp `which gcc` ghc/

    (Adjust to version of GCC that GHC was compiled with.)

 2. Set the `GHC_PATH` variable to the correct path to for your
    system.  Make sure *not* to set `HC`, `PKG`, or `HADDOCK`, they
    will automatically be set to point to the inplace versions.

 3. Use `make` or `make cabal-install` as above.



KNOWN PITFALLS
------------------------------
If you get an error message like this:
  "scion_server: mkTopLevEnv: not interpreted main:Main"
then you should rm [Ss]etup.hi [Ss]etup.o in the project directory.
