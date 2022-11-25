# Steelcut

![build](https://github.com/eigenhombre/steelcut/actions/workflows/build.yml/badge.svg)

<img src="/steel.jpg" width="400">

A port to Common Lisp of
[Oatmeal](https://github.com/eigenhombre/oatmeal/), a Common Lisp
project generator.

# Features

This program provides a simple template for new Common Lisp projects,
including:
- Projects can be generated and built using
  [SBCL](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp), a
  free, fast and multi-platform Lisp system
- Build stand-alone binaries for the current architecture with one
  command (`make`)
- Install on current path (`make install`)
- Run unit tests for current project (`make test`)
- Run tests and build sample project within Docker (`make docker`)
  - Do the above in the GitHub Actions CI system (see generated
    `build.yml`)

# Status

Very very alpha.  I'm using it, probably nobody else is.

Tested only on (+ SBCL (or (Mac M1) (+ Docker Ubuntu)))).

# Building

## Prerequisites

- SBCL
- Make

Check out this repo.  Make sure `LISP_HOME` is defined and you're in
that directory. Then,

    make
    # Assuming BINDIR defined and on your PATH; I use ~/bin ...:
    make install

# Example

Assuming you want to build a project `myproject`,

    $ steelcut
    Usage: steelcut <appname>
    $ steelcut myproject
    Project myproject created.  Thanks for using steelcut!
    $ cd $LISP_HOME/myproject
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
    $ make install
    test -n "/Users/jacobsen/bin"  # $BINDIR
    cp myproject /Users/jacobsen/bin
    $ cd
    $ myproject
    Thanks for using myproject!
    $

# License

MIT License

Copyright (c) 2022 John Jacobsen

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
