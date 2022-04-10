<!-- omit in toc -->
# DECA Starter Project
This DECA starter project should make it easier to get started with Clash on the
[Arrow DECA development kit](https://www.arrow.com/en/products/deca/arrow-development-tools).

<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
  - [Stack (Windows, Linux, MacOS) [recommended]](#stack-windows-linux-macos-recommended)
  - [Cabal (Linux, MacOS)](#cabal-linux-macos)
  - [REPL](#repl)
  - [IDE support](#ide-support)
- [Running the project on the DECA](#running-the-project-on-the-deca)
  - [Creating the FPGA bitstream](#creating-the-fpga-bitstream)
  - [Programming the FPGA](#programming-the-fpga)
- [Project overview](#project-overview)
  - [deca.cabal](#namecabal)
  - [cabal.project](#cabalproject)
  - [stack.yaml](#stackyaml)
  - [src/](#src)
  - [syn/](#syn)
  - [tests/](#tests)
- [Change the license](#change-the-license)

# Getting this project
Stack users can run `stack new my-clash-project clash-lang/deca`. Cabal users can
[download a zip](https://raw.githubusercontent.com/clash-lang/clash-starters/main/deca.zip)
containing the project.

# Building and testing this project
There's a number of ways to build this project on your machine. The recommended
way of doing so is using _Stack_, whose instructions will follow directly after
this section.

## Stack (Windows, Linux, MacOS) [recommended]
Install Stack using your package manager or refer to the
[How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install)
section of the [Stack manual](https://docs.haskellstack.org/en/stable/README/).

Build the project with:

```bash
stack build
```

To compile the project to VHDL, run:

```bash
stack run clash -- DECA --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/DECA.hs`.

## Cabal (Linux, MacOS)
**The following instructions only work for Cabal >=3.0 and GHC >=8.4.**

First, update your cabal package database:

```bash
cabal update
```

You only have to run the update command once. After that, you can keep
rebuilding your project by running the build command:

```bash
cabal build
```

To compile the project to VHDL, run:

```bash
cabal run clash -- DECA --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/DECA.hs`.

## REPL
Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
as a quick way to try things, similar to Python's `python` or Ruby's `irb`.
Stack users can open the REPL by invoking:

```
stack run clashi
```

Cabal users use:

```
cabal run clashi
```

## IDE support
We currently recommend Visual Studio Code in combination with the _Haskell_ plugin.
All you need to do is open this folder in VSCode; it will prompt you to install
the plugin.

# Running the project on the DECA

## Creating the FPGA bitstream
First, compile the project to VHDL by running:

```bash
stack run clash -- DECA --vhdl
```

Now start [Quartus Prime](https://fpgasoftware.intel.com/?edition=lite); once started,
in the menu bar, click `File -> Open Project` and open `syn/deca.qpf`.
In the menu bar, click: `Processing -> Start Compilation`.
This can take up to a minute depending on your machine.
If everything worked as it was supposed to the last messages in the logs should be in
the spirit of:

```
Info (332101): Design is fully constrained for setup requirements
Info (332101): Design is fully constrained for hold requirements
Info: Quartus Prime Timing Analyzer was successful. 0 errors, 2 warnings
	Info: Peak virtual memory: 550 megabytes
	Info: Processing ended: Tue Jun  1 09:51:50 2021
	Info: Elapsed time: 00:00:01
	Info: Total CPU time (on all processors): 00:00:01
Info (293000): Quartus Prime Full Compilation was successful. 0 errors, 11 warnings
```

## Programming the FPGA
After synthesis has finished, it is time to program our FPGA board.
Connect the FPGA board to a USB port, and start the programmer from the menu
bar: `Tools -> Programmer`. Press the `Start` button on the left to program your FPGA
and wait until the progress bar says `100% (Successful)`.

Once programmed, you should be able to operate the DECA devkit as seen here:
[![IMAGE ALT TEXT](http://img.youtube.com/vi/zsChH7q03mg/0.jpg)](http://www.youtube.com/watch?v=zsChH7q03mg "Clash DECA starter")

# Project overview
This section will give a tour of all the files present in this starter project.
It's also a general introduction into Clash dependency management. It's not an
introduction to Clash itself though. If you're looking for an introduction to
Clash, read ["Clash.Tutorial" on Hackage](https://hackage.haskell.org/package/clash-prelude).

```
deca
├── bin
│   ├── Clash.hs
│   └── Clashi.hs
├── syn
│   └── deca_ext.sdc
|   └── deca.qpf
|   └── deca.qsf
├── src
│   └── DECA.hs
├─── tests
│   ├── Tests
│   │   └── DECA.hs
│   ├── doctests.hs
│   └── unittests.hs
├── cabal.project
├── deca.cabal
└── stack.yaml
```

## deca.cabal
This is the most important file in your project. It describes how to build your
project. Even though it ends in `.cabal`, Stack will use this file too. It
starts of with meta-information:

```yaml
cabal-version:       2.4
name:                deca
version:             0.1
license:             BSD-2-Clause
author:              Author <author@example.com>
maintainer:          Author <author@example.com>
```

If you decide to publish your code on [Hackage](https://hackage.haskell.org/),
this will show up on your package's front page. Take note of the license, it's
set to `BSD-2-Clause` by default, but this might bee too liberal for your project.
You can use any of the licenses on [spdx.org/licenses](https://spdx.org/licenses/).
If none of those suit, remove the `license` line, add `license-file: LICENSE`,
and add a `LICENSE` file of your choice to the root of this project. Moving on:

```yaml
common common-options
  default-extensions:
    BangPatterns
    BinaryLiterals
    ConstraintKinds
    [..]
    QuasiQuotes

    -- Prelude isn't imported by default as Clash offers Clash.Prelude
    NoImplicitPrelude
```

Clash's parent language is Haskell and its de-facto compiler, GHC, does a lot of
heavy lifting before Clash gets to see anything. Because using Clash's Prelude
requires a lot of extensions to be enabled to be used, we enable them here for
all files in the project. Alternatively, you could add them where needed using
`{-# LANGUAGE SomeExtension #-}` at the top of a `.hs` file instead. The next
section, `ghc-options`, sets warning flags (`-Wall -Wcompat`) and flags that
make GHC generate code Clash can handle.

Note that this whole section is a `common` "stanza". We'll use it as a template
for any other definitions (more on those later). The last thing we add to the
common section is some build dependencies:

```yaml
  build-depends:
    base,
    Cabal,

    -- clash-prelude will set suitable version bounds for the plugins
    clash-prelude >= 1.6.3 && < 1.8,
    ghc-typelits-natnormalise,
    ghc-typelits-extra,
    ghc-typelits-knownnat
```

These dependencies are fetched from [Hackage](https://hackage.haskell.org/),
Haskell's repository for packages. Next up is a `library` stanza. It defines
where the source is located, in our case `src/`, and what modules can be found
there. In our case that's just a single module, `DECA`.

```yaml
library
  import: common-options
  hs-source-dirs: src
  exposed-modules:
    DECA
  default-language: Haskell2010
```

Note that extra dependencies could be added by adding a `build-depends` line to
this section.

The following section defines a testsuite called _doctests_. Doctests are tests
that are defined in the documentation of your project. We'll see this in action
in [src/](#src).

```yaml
test-suite doctests
  import:           common-options
  type:             exitcode-stdio-1.0
  default-language: Haskell2010
  main-is:          doctests.hs
  hs-source-dirs:   tests

  build-depends:
    base,
    deca,
    process,
    doctest >= 0.16.1 && < 0.18
```

Last but not least, another testsuite stanza is defined:

```yaml
test-suite test-library
  import: common-options
  default-language: Haskell2010
  hs-source-dirs: tests
  type: exitcode-stdio-1.0
  ghc-options: -threaded
  main-is: unittests.hs
  other-modules:
    Tests.DECA
  build-depends:
    deca,
    QuickCheck,
    hedgehog,
    tasty >= 1.2 && < 1.5,
    tasty-hedgehog,
    tasty-th
```

These testsuites are executed when using `stack test` or `cabal test --enable-tests`.
Note that Cabal swallows the output if more than one testsuite is defined, as
is the case here. You might want to consider running the testsuites separately.
More on tests in [/tests](#tests).

## cabal.project
A `cabal.project` file is used to configure details of the build, more info can
be found in the [Cabal user documentation](https://cabal.readthedocs.io/en/latest/cabal-project.html).
We use it to make Cabal always generate GHC environment files, which is a
feature Clash needs when using Cabal.

```haskell
packages:
  deca.cabal

write-ghc-environment-files: always
```

`cabal.project` can be used to build multi-package projects, by extending `packages`.

## stack.yaml
While Cabal fetches packages straight from Hackage (with a bias towards the
latest versions), Stack works through _snapshots_. Snapshots are an index of
packages from Hackage know to work well with each other. In addition to that,
they specify a GHC version. These snapshots are curated by the community and
FP Complete and can be found on [stackage.org](https://www.stackage.org/).

```yaml
resolver: lts-18.27

extra-deps:
  - clash-prelude-1.6.3
  - clash-lib-1.6.3
  - clash-ghc-1.6.3
```

This project uses [lts-18.27](https://www.stackage.org/lts-18.27), which
includes Clash 1.4.7. We've added the extra-deps section to make sure Stack
fetches the latest version of Clash, 1.6.3, instead. The point of this exercise
is to make reproducible builds. Or in other words, if a `stack build` works
now, it will work in 10 years too.

Note: If you need a newer Clash version, simply change the version bounds in
`deca.cabal` and follow the hints given by Stack.

## src/
This is where the Haskell source code of the project lives, as specified in
`deca.cabal`. It contains a single file, `DECA.hs`.

One aspect that's worth highlighting is that in this file there is the following
definition:

```haskell
-- | Changes the LED mode
--
-- >>> flipMode Rotate
-- Complement
-- >>> flipMode Complement
-- Rotate
flipMode :: LedMode -> LedMode
flipMode Rotate = Complement
flipMode Complement = Rotate
```

Where the example (`>>> flipMode Rotate`) in the comments gets executed by the
_doctests_ defined for this project and checked for consistency with the result
in the documentation (`Complement`).

## syn/
This contains the Quartus projects, it expects that you already generated the
VHDL.

## tests/
Most of this directory is scaffolding, with the meat of the tests being defined
in `tests/Tests/DECA.hs`. Writing good test cases is pretty hard: edge cases are
easy to forget both in the implementation and tests. To this end, it's a good
 idea to use _fuzz testing_. In this project we use [Hedgehog](https://hedgehog.qa/):

```haskell
import DECA (LedMode (..), flipMode)

prop_flipTwiceOriginal :: H.Property
prop_flipTwiceOriginal = H.property $ do
  a <- H.forAll (Gen.enum Rotate Complement)
  a === flipMode (flipMode a)
```

This test generates an LED mode, `a`, enumerating from the `Rotate` mode to the
`Complement` mode. It then tests whether flipping the LED mode twice give you
back the original LED mode.

All functions called `prop_*` are collected automatically:

```haskell
tests :: TestTree
tests = $(testGroupGenerator)
```

We can run the tests using `stack test` or `cabal run test-library --enable-tests`:

```
.
  Tests.DECA
    flipTwiceOriginal: OK
        ✓ flipTwiceOriginal passed 100 tests.

All 1 tests passed (0.00s)
```

# Change the license
By default `deca.cabal` sets its `license` field to `BSD-2-Clause`. You
might want to change this.
