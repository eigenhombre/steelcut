# Steelcut

![build](https://github.com/eigenhombre/steelcut/actions/workflows/build.yml/badge.svg)

<img src="https://github.com/eigenhombre/steelcut/blob/c3b3cc2979850d0db9f793c4e18d9323be2b92c3/steel.jpg?raw=true" width="400">

An approximate port to Common Lisp of
[Oatmeal](https://github.com/eigenhombre/oatmeal/), a Common Lisp
project generator.

# Features

The program generates new Common Lisp projects.  The idea is to make it
as easy as possible to generate, test, build and install a new Common Lisp
program as easily as possible.

- Projects can be generated and built using
  [SBCL](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp), a
    free, fast and multi-platform Lisp system
  - Generated projects have selectable features as outlined in [Feature Selection](#feature-selection), below
- Build stand-alone binaries for the current architecture with one
  command (`make`)
- Install using `make install`
- Run unit tests for current project `make test`
- Run tests and build sample project within Docker (`make docker`)
  - Do the above in the GitHub Actions CI system (see generated
    `build.yml`)

# Status

Pretty alpha.  I'm using it, probably nobody else is.

Tested only on `(and SBCL (or (Mac M1) (and Docker Ubuntu))))`.

# Building

## Prerequisites

- SBCL
- Make
- `BINDIR` defined as an environment variable, directory is created,
  and on your `PATH`.  I use `$HOME/bin`.
- `LISP_HOME` also exists as a directory and defined.

To build `steelcut`, check out this repo and `cd` to it.  Then,

    $ make
    # Assuming BINDIR defined and on your PATH; I use ~/bin ...:
    $ make install

# Example

## Usage

    $ steelcut
    Usage: steelcut <appname>
    $

Assuming you want to call your new project `myproject`:

## Generate the New Project

    $ steelcut myproject
    Project myproject created.  Thanks for using steelcut!
    $ cd $LISP_HOME/myproject
    $

## Build It

    $ make
    ./build.sh
    This is SBCL 2.2.6, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    To load "myproject":
      Load 1 ASDF system:
        myproject
    ; Loading "myproject"
    [package myproject]
    [undoing binding stack and other enclosing state... done]
    [performing final GC... done]
    [saving current Lisp image into myproject:
    writing 1840 bytes from the read-only space at 0x300000000
    writing 1840 bytes from the static space at 0x300200000
    writing 0 bytes from the immobile space at 0x300300000
    writing 42270720 bytes from the dynamic space at 0x7003000000
    done]
    $

## Install It

    $ make install
    test -n "/Users/myusername/bin"  # $BINDIR
    cp myproject /Users/myusername/bin
    $

## Run It

    $ myproject
    Thanks for using myproject!
    $

## Feature Selection

Some features in generated projects are optional, with de-selectable
defaults.  Examples:

Select library only with no other defaults:

    $ steelcut myapp -ci -docker -cl-oju

Build a Web service app which accepts JSON requests:

    $ steelcut myapp +webserver +json +args

`+` selects an option, and is optional. `-` deselects a (presumably default)
option.

This is a work in progress.  The following features are implemented:

- `docker` (enabled by default)
- CI GitHub Actions build (enabled by default; implies `docker`)
- `cmd` (uses [Paul Rodriguez's library](https://github.com/ruricolist/cmd);
  disabled by default.
- `args`: argument processing via Steve Losh's [Adopt](https://github.com/sjl/adopt/)
  (in progress)

Planned additional selectable features:

    :csv         :json        :time     :yaml
    :webclient   :webserver   :xml

# Testing Steelcut

There are several unit tests which rely on generating example projects
in temporary directories and checking properties of the resulting
files.  =make docker= runs a test script =test-all-features= which
generates and tests all single and double combinations of features.
**This is best run in Docker** to avoid polluting your =$LISP_HOME=
with lots of temporary projects.

# License

MIT License

Copyright (c) 2022-2025 John Jacobsen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
