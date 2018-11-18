# 16-bit Lisp interpreter (work in progress)

Jake Herrmann  
CS 301 Fall 2018

TODO: toc

## Project goals

The **final goal** is a bootable, 16-bit Lisp interpreter for x86 real mode.

The **end of semester goal** is a partial implementation of the interpreter.
The partial interpreter should be able to:

- Construct the following Lisp objects: ints, symbols, pairs, and the empty
  list.
- Read an input expression and convert it into a Lisp object representing the
  expression's abstract syntax tree (AST).
- Print Lisp objects.

Evaluating expressions is not an explicit goal for the end of the semester, but
I may begin implementing expression evaluation if I have time.

## Background

Before starting this project I had not written an interpreter for any language.
I found it too difficult to tackle the problem in 16-bit assembly, so I decided
to write the [first version](https://notabug.org/jtherrmann/lisp-in-c) in C and
target 64-bit Linux. This was a good decision because it allowed me to focus on
writing an interpreter without the added challenge of doing so in 16-bit
assembly. After implementing a Lisp in C, I had a much better idea of how to do
the same in 16-bit NASM.

## Progress report

I have met my [goals](#project-goals) for the end of the semester and have
begun implementing expression evaluation. The interpreter currently supports
the following kinds of expressions:

TODO: document ints, symbols, pairs, empty list, quote, define, and placeholder
expressions; others? (see `eval`)

TODO: document everything documented / to document for C lisp

## Getting started

Known to work on Debian GNU/Linux 9.5 (stretch).

1. Make sure `nasm` and `qemu` are installed.
2. Clone this repo, `cd` into `lisp/`, and run:

        nasm -f bin -o lisp.bin lisp.asm
        qemu-system-x86_64 lisp.bin
