# Gotta Go Fast

A command line utility for practicing typing and measuring your WPM and
accuracy. Written with [brick](https://github.com/jtdaugherty/brick).

## Installation

From source:

    $ git clone https://github.com/hot-leaf-juice/gotta-go-fast
    $ cd gotta-go-fast
    $ stack install

## Usage

Pass `gotta-go-fast` the name of a file, and it will extract a random 20 line
section for you to type – or the whole thing if the file is short enough.
You’ll get a WPM and accuracy report for your trouble.

    $ gotta-go-fast README.md

Errors are highlighted in red.

![screenshot](img/screenshot.png)

Pass `gotta-go-fast` more than one file, and it will choose one at random.

    $ gotta-go-fast src/*

## Configuration

The maximum number of lines to sample, and the width at which to wrap them, can
be changed by setting the `--height` and `--width` options. Run `gotta-go-fast
--help` for details.
