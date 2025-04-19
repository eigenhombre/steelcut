# Steelcut

![build](https://github.com/eigenhombre/steelcut/actions/workflows/build.yml/badge.svg)

<img src="https://github.com/eigenhombre/steelcut/blob/c3b3cc2979850d0db9f793c4e18d9323be2b92c3/steel.jpg?raw=true" width="400">

An approximate port to Common Lisp of
[Oatmeal](https://github.com/eigenhombre/oatmeal/), a Common Lisp
project generator.

# Features

The program generates new Common Lisp projects.  The idea is to make it
as easy as possible to generate, test, build and install a new Common Lisp
program.

- Projects can be generated and built using
  [SBCL](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp), a
    free, fast and multi-platform Lisp system.
- Generated projects have selectable features as outlined in [Feature
  Selection](#feature-selection), below.
- Build stand-alone binaries for the current architecture with one
  command (`make`).
- Install using `make install`.
- Run unit tests for current project with `make test`.
- Run tests and build sample project within Docker (`make docker`)
  - Do the above in the GitHub Actions CI system (see generated
    `build.yml`).
  - Smoke-test all single features and all two-feature combinations
    to avoid obvious conflicts, e.g. symbol collisions.

# Status

Pretty alpha.  I'm using it, probably nobody else is.  See
[Prior Art](#prior-art) for some other Common Lisp project
generators.

Tested only on `(and SBCL (or (Mac M1) (and Docker Ubuntu))))`.

# Building Steelcut

## Prerequisites

- SBCL
- Make
- `BINDIR` defined as an environment variable, directory is created,
  and on your `PATH`.  I use `$HOME/bin`.
- `LISP_HOME` also exists as a directory and defined.

To build `steelcut`, check out the current repo and `cd` to it.  Then,

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

Some features in generated projects are optional, and the default
features can be turned off.  Examples:

Generate vanilla app only with no extra dependencies:

    $ steelcut myapp -ci -docker -cl-oju

Generate an app which takes command line arguments and can invoke
shell commands:

    $ steelcut myapp +cmd +args

`+` selects an option, and is optional. `-` deselects a (presumably default)
option.

The following features are implemented, with more planned:

### `docker` (enabled by default)

A Dockerfile is generated, and the project can be built, tested and run
in Docker with the `make docker` command.

### `ci` (enabled by default; implies `docker`)

A GitHub Actions build file is generated, which runs `make docker`.

### `cl-oju` (enabled by default)

Brings in [`cl-oju`](https://github.com/eigenhombre/cl-oju), a simple
compatibility library providing Common Lisp equivalents of several core
Clojure functions.

### `cmd` (disabled by default)

Uses [Paul Rodriguez's library](https://github.com/ruricolist/cmd) for
interacting with the shell.

### `csv` (disabled by default)

Use [`cl-csv`](https://github.com/AccelerationNet/cl-csv) to
facilitate reading and writing of CSV data.

### `args` (disabled by default)

Provides argument processing via Steve Losh's [Adopt](https://github.com/sjl/adopt/).

### `yaml` (disabled by default)

YAML parsing and generation by Fernando Borretti's
[cl-yaml](https://github.com/eudoxia0/cl-yaml/).
(NOTE: you must have `libyaml-dev` installed for this to work!)

Each option provides necessary dependencies and an example of usage in the
generated project.

To handle the case where the user forgets to supply the application
name, `steelcut` will not allow you to use a supported feature name as
the project name.

Planned additional selectable features:

    :csv         :json        :time
    :webclient   :webserver   :xml

# Testing Steelcut

There are several unit tests which rely on generating example projects
in temporary directories and checking properties of the resulting
files.  `make docker` runs a test script `test-all-features` which
generates and tests all single and double combinations of features.
**This is best run in Docker** to avoid polluting your `$LISP_HOME`
with lots of temporary projects.

# Prior Art

There are quite a few Common Lisp project generators out there; some
look more opinionated than others, and/or have specific focus geared
towards games, web applications, etc.  I haven't tried any of them.

- Alexander Artemenko: [`project-templates`](https://github.com/40ants/project-templates)
- Andrew Kravchuk: [`cookiecutter-lisp-game`](https://github.com/lockie/cookiecutter-lisp-game)
- `vindarel`: [`cl-cookieweb`](https://github.com/vindarel/cl-cookieweb)
- Anthony Green: [`make-like`](https://github.com/container-lisp/make-like)
- Rajasegar Chandran: [`cl-webapp-seed`](https://github.com/rajasegar/cl-webapp-seed)
- Mikel Evins: [`electron-lisp-boilerplate`](https://github.com/mikelevins/electron-lisp-boilerplate)
- Sebastian Carlos: [`cl-yasboi`](https://github.com/sebastiancarlos/cl-yasboi)

(Thanks to Alexander Artemenko for supplying this list.)

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
