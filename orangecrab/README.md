<!-- omit in toc -->
# OrangeCrab Starter Project

This project contains some demo applications for targeting the [OrangeCrab FPGA r0.2.1](https://orangecrab-fpga.github.io/orangecrab-hardware/docs/r0.2.1) in combination with the OrangeCrab Pmod / Debug Expander board and an [Adafruit FT232H JTAG Debugger](https://learn.adafruit.com/adafruit-ft232h-breakout).

It can to be used as a rapid prototyping template or as a starter for larger project setups.

 Read [Simple Starter Project - Project overview](https://github.com/clash-lang/clash-starters/blob/main/simple/README.md#project-overview) for more information on the various files.

<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
  - [Stack (Windows, Linux, MacOS) [recommended]](#stack-windows-linux-macos-recommended)
  - [Cabal (Linux, MacOS)](#cabal-linux-macos)
  - [REPL](#repl)
  - [IDE support](#ide-support)
- [Running the project on the OrangeCrab](#running-the-project-on-the-orangecrab)
  - [Prerequisites](#prerequisites)
  - [Build Architecture](#build-architecture)
  - [Make](#make)
    - [Configuration](#configuration)
    - [Usage](#usage)
  - [Demo Application](#demo-application)
    - [Blink](#blink)
- [Change the license](#change-the-license)

# Getting this project
Stack users can run `stack new my-clash-project clash-lang/orangecrab`. Cabal users can [download a zip](https://raw.githubusercontent.com/clash-lang/clash-starters/main/orangecrab.zip) containing the project.

# Building and testing this project
There's a number of ways to build this project on your machine. The recommended way of doing so is using _Stack_, whose instructions will follow directly after this section.

## Stack (Windows, Linux, MacOS) [recommended]
Install Stack using your package manager or refer to the [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install) section of the [Stack manual](https://docs.haskellstack.org/en/stable/README/).

Build the project with:

```bash
stack build
```

To run the tests defined in `tests/`, use:

```bash
stack test
```

To compile the project to Verilog, run:

```bash
stack run clash -- Blink --verilog
```

You can find the HDL files in `verilog/`. The source can be found in `src/Blink.hs`.

## Cabal (Linux, MacOS)
**The following instructions only work for Cabal >=3.0 and GHC >=8.4.**

First, update your cabal package database:

```bash
cabal update
```

You only have to run the update command once. After that, you can keep rebuilding your project by running the build command:

```bash
cabal build
```

To run the tests defined in `tests/`, use:

```bash
cabal run test-library
cabal run doctests
```

To compile the project to Verilog, run:

```bash
cabal run clash -- Blink --verilog
```

You can find the HDL files in `verilog/`. The source can be found in `src/Blink.hs`.

## REPL
Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) as a quick way to try things, similar to Python's `python` or Ruby's `irb`. Stack users can open the REPL by invoking:

```
stack run clashi
```

Cabal users use:

```
cabal run clashi
```

## IDE support
We currently recommend Visual Studio Code in combination with the _Haskell_ plugin. All you need to do is open this folder in VSCode; it will prompt you to install the plugin.

# Running the project on the OrangeCrab

## Prerequisites
The project requires `yosys`, `nextpnr`, and `prjtrellis`. You will need to install them (they are all part of [the releases of the nightly builds of OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build/releases)).

## Build Architecture

The setup uses a combination of the Clash stage documented above and `make`, where
* Clash and GHC are used to compile your project and generate Verilog files, and
* `make` is used to run RTL synthesis, Place and Route, and to program the FPGA.

## Make

A straightforward [`Makefile`](Makefile) is used to handle any additional calls which are not covered by the cabal build system yet.

### Configuration

The `Makefile` includes the additional configuration file [`build.cfg`](build.cfg), which can be used to link all of the required external build tools. To this end, simply copy the file to `build.cfg.local` on your local machine and modify it towards your personal needs. Any changes made to the copy will be preferred in comparison to original `build.cfg`. Just leave out the overrides, if you like to stick with the defaults instead.

Some references on where to get all of the required external tooling are also given as part of the configuration file.

The particular module that contains the top entity can be configured via overriding the `NAME` parameter in the `build.cfg.local`. Just set it to the name of the file in the `src` directory that shall be built (without the `.hs` ending). See the `Makefile` for the pre-configured default. Similarly, the `TOP` parameter configures the name of the top entity in that module. This is either `topEntity` or a function annotated with a [`Synthesize`](https://hackage-content.haskell.org/package/clash-prelude/docs/Clash-Annotations-TopEntity.html#t:TopEntity) annotation. Please note that the `Makefile` does not support Clash designs with multiple `Synthesize` annotations in the same design. Put differently, the top entity cannot (directly or transitively) depend upon a function containing a `Synthesize` annotation. Also, the `t_name` parameter of a `Synthesize` annotation needs to have the same value as the actual name of the function it annotates.

### Usage

The `Makefile` offers the following targets (to be invoked via `make TARGET`) where each command depends on the output generated by all of the former ones according to the order of the list below. The `bitstream` target is the `make` default.

| TARGET      | Description                                         |
| ----------- | --------------------------------------------------- |
| `synth`     | runs RTL synthesis                                  |
| `netlist`   | runs Place and Route                                |
| `bitstream` | generates a bitstream to be transferred to the FPGA |
| `upload`    | copies the generated bitstream to FPGA              |

The output of each target is stored in a corresponding sub-directory within an automatically created `_build` folder. The `synth` step also copies the needed HDL files from the `verilog/` dir that was created by Clash into a directory named `01-hdl`. Use `make clean`, if you like to rebuild everything in the `_build` folder from scratch (`make clean` does **not** run `stack/cabal clean`).

## Demo Application

### Blink

Simple blinking example using the RGB LED on the OrangeCrab r0.2.1 board. Running `make upload` should flash the OrangeCrab with the example.

# Change the license
By default `orangecrab.cabal` sets its `license` field to `BSD-2-Clause`. You might want to change this.
