# 16-bit Lisp interpreter (work in progress)

Jake Herrmann  
CS 301 Fall 2018

TODO: toc

## Project goals

The **final goal** is a bootable, 16-bit Lisp interpreter for x86 real mode.

The **end of semester goal** is a partial implementation of the interpreter. In
particular, the interpreter should be able to:

- Construct the following Lisp objects: ints, symbols, pairs, and the empty
  list.
- Read an input expression and convert it into a Lisp object representing the
  expression's abstract syntax tree.
- Print Lisp objects.

## Background

Before starting this project I had not written an interpreter for any language.
I found it too difficult to tackle the problem in 16-bit assembly, so I decided
to write the first version in C and target 64-bit Linux. This was a good
decision because it allowed me to focus on writing an interpreter without the
added challenge of doing so in 16-bit assembly.

After implementing [a Lisp in C](https://notabug.org/jtherrmann/lisp-in-c), I
had a much better idea of how to do the same in 16-bit assembly. So far, it has
been surprisingly straightforward to translate my C code into NASM.

## Progress report

I have met my goals for the end of the semester...TODO

TODO: document everything documented / to document for C lisp

## Getting started

Known to work on Debian GNU/Linux 9.5 (stretch).

1. Make sure `nasm` and `qemu` are installed.
2. Clone this repo, `cd` into `lisp/`, and run:

        nasm -f bin -o lisp.bin lisp.asm
        qemu-system-x86_64 lisp.bin
