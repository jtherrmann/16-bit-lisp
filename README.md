# 16-bit Lisp interpreter (work in progress)

Jake Herrmann  
CS 301 Fall 2018

TODO: toc

TODO: make presentation slides public and link from README

## Introduction

Originally, my final goal for the project was a bootable, 16-bit Lisp
interpreter for x86 real mode.

Before starting this project I had not written an interpreter for any language.
I found it too difficult to tackle the problem in 16-bit assembly, so I decided
to write the first version in C and target 64-bit Linux. After implementing an
interpreter in C, I had a much better idea of how to approach the problem using
16-bit assembly.

I decided on the following goals for the end of the semester:

- Construct the following Lisp objects: ints, symbols, pairs, and the empty
  list.
- Parse input: read an input expression and convert it into a Lisp object
  representing the expression's abstract syntax tree.
- Print Lisp objects.

I decided that evaluating expressions should not be an explicit goal for the
end of the semester. However, I finished the above goals in a reasonable amount
of time and was able to begin implementing expression evaluation.

[The C version](https://notabug.org/jtherrmann/lisp-in-c) is mostly complete
and should be considered the complete roadmap for this project. The C version
also has garbage collection, which is not a goal for the 16-bit assembly
version.

## Getting started

Known to work on Debian GNU/Linux 9.5 (stretch).

1. Make sure `nasm` and `qemu` are installed.
2. Clone this repo, `cd` into `lisp/`, and run:

        nasm -f bin -o lisp.bin lisp.asm
        qemu-system-x86_64 lisp.bin

## Interpreter commands

The interpreter recognizes a handful of special commands:

- `:free` prints the current number of free Lisp objects.
- `:freelist` prints the current list of free Lisp objects, where each object
  is printed as its position in the list followed by its memory address.
- `:genv` prints the current global environment (a list of name-value pairs).
- `:help` prints the available interpreter commands.
- `:keymap` toggles between QWERTY and Dvorak.
- `:restart` reboots the computer.

## Objects

Not yet implemented:

- bools (`#t` and `#f`)

### Ints

An int is an integer and evaluates to itself:

    > 5
    5
    > 123
    123

### Symbols

A symbol evaluates to the object to which it's bound:

    > (define x 3)
    > x
    3
    > (quote x)
    x
    > (eval (quote x))
    3

### Pairs and lists

A pair is an object with two data members, car and cdr:

    > (define p (cons 1 2))
    > p
    (1 . 2)
    > (car p)
    1
    > (cdr p)
    2

A list is the empty list, `()`, or any pair whose cdr is a list. The empty list
evaluates to itself, while a non-empty list evaluates as a function
application:

    > ()
    ()
    > (define expr (quote (cons 1 2)))
    > expr
    (cons 1 2)
    > (eval expr)
    (1 . 2)

A non-list pair cannot be evaluated:

    > (define expr (cons (quote x) (cons (quote y) (quote z))))
    > expr
    (x y . z)
    > (eval expr)
    Invalid expression:

      (x y . z)

    (x y . z) is not a list
    Invalid expression:

      (eval expr)

    #<function: eval> signaled an error

### Functions

A function evaluates to itself and can be applied to some number of arguments:

    > cons
    #<function: cons>
    > (eval cons)
    #<function: cons>
    > (cons 1 2)
    (1 . 2)

## Special forms

Not yet implemented:

- `cond`: a conditional expression.
- `lambda`: an anonymous function definition.

### define

special form: **define** *name* *definition*

Binds the symbol *name* to the result of evaluating *definition*.

    > (define x (quote foo))
    > (define y (quote bar))
    > (cons x y)
    (foo . bar)

### quote

special form: **quote** *object*

Evaluates to *object*.

    > (quote hello)
    hello
    > (quote (1 2 3))
    (1 2 3)

## Builtin functions

The available builtin functions can be displayed by printing the global
environment with `:genv`.

There are currently only four builtin functions:

- `cons` constructs a pair.
- `car` returns the first element of a pair.
- `cdr` returns the second element of a pair.
- `eval` evaluates an object as an expression.

Not yet implemented:

- Arithmetic operators
- Logical operators
- Comparison functions
- Type predicates

## Error handling

The interpreter detects and handles various kinds of errors. Examples:

Parse errors:

    > (quote (1 2 3)
                    ^
    Parse error: incomplete list

Invalid expressions:

    > (define 1 2)
    Invalid expression:

      (define 1 2)

    1 is not a symbol

An attempt to use `cond` or `lambda`:

    > (lambda (x) x)
    Invalid expression:

      (lambda (x) x)

    Special form 'lambda' not yet implemented

Type errors:

    > (car ())
    Type error: () is not a pair
    Invalid expression:

      (car ())

    #<function: car> signaled an error

No free memory:

    > :free
    free objects: 4
    > (quote (1 2 3))
    Error: no free memory for new object
    Lisp has crashed.

    Press any key to reboot.
