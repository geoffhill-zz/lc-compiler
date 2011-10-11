lc-compiler
===========

EECS 322 Compiler Construction  
Northwestern University

Geoff Hill \<GeoffreyHill2012@u.northwestern.edu\>  
Copyright 2011 Geoff Hill.  
Licensed under 2-clause BSD. See LICENSE for details.

This is a compiler from the LC higher-level language to x86 machine code.

## Input: The LC language

As a language, LC is purely of teaching interest. It has no ability to
interact with the host environment, except to print values, allocate memory,
and report errors. Some features of the language include:

*   First-class functions (functions as values)
*   Higher order functions
*   Recursive function definitions
*   Proper tail calls

The compiler uses the following stages of compilation through intermediate
languages:

*   L5 -> L4
    *   [Lambda lifting](http://en.wikipedia.org/wiki/Lambda_lifting)
    *   High arity function calls
    *   Function inlining optimizations
    *   Recursive function call optimizations
*   L4 -> L3
    *   [A-Normalization](http://en.wikipedia.org/wiki/Administrative_normal_form)
*   L3 -> L2
    *   Expression flattening
    *   Type tagging
*   L2 -> L1
    *   Liveness analysis
    *   Register allocation and spilling
*   L1 -> ASM
    *   Redundant calculation optimizations
    *   Function calling conventions (normal and tail calls)
    *   Interaction with runtime system

## Output: x86 Code

The compiler is written in Racket and requires Racket 5.1 or greater and GNU as
to run. Produces valid binaries for 32-bit Linux.

Compiled code generally runs 10x-200x faster than code interpreted by the Racket
Redex-based LC interpreter supplied in class for programs that tun in constant space.
Since closures compile to higher arity functions, and higher arity functions
allocate memory, many closures do not run in constant space.

