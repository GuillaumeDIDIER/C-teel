# C-teel
C-teel (pronounced steel) is compiler from a subset of C to x86_64 written in Rust.
It is a school project unsuitable for use in production.

## Features
 - Complete Mini-C (as defined by INF564 at École polytechnique)
 - Supports functions with more than 6 parameters

## Known issues

 - The character counting logic is flawed. And under some circumstances,
 the line counting logic can be too. This is due to the parser backtracking
 when parsing certain constructions, and could be avoided by clever factorisations
 of the grammar.
 - `if` and `while` conditions use inefficient constructs.
 (The condition is wholly evaluated and only then is `testq` used on the result to determine jump, instead of selecting the right jcc to use)
 - register allocation use simple heuristics which can sometimes be inefficient.



## Licence

All rights reserved.

## Usage

```
C-teel 0.1.0
guillaume.didier@polytechnique.edu
Compiles Badly a subset of C to x86_64

USAGE:
    c-teel [FLAGS] <INPUT>

FLAGS:
        --full          Fully compiles the source file
    -h, --help          Prints help information
        --open-only     Only open the source file (sanity check of the compiler io)
        --parse-only    Only parse the source file
        --type-only     Only parse and type the source file
    -V, --version       Prints version information

ARGS:
    <INPUT>    Name of the source file to use
```

## Installation

With rust and cargo (its package manager installed) :

```
git clone https://github.com/GuillaumeDIDIER/C-teel
cd C-teel
cargo build
./target/debug/c-teel ...
```

## Code organisation

The code is written in rust, a multi paradigm language with a strong typing system and feature from functional language.
Hence the architecture is closer to the suggested one in OCaml than in Java.

All the code is contained under `src/`

### General

`src/bin/` contains the executable programs,



`src/lib.rs` is the top level file of the library.

### Frontend

It is split in two modules :

 - `src/parse` contains the lexer and parser written using nom (a parser combinator).
 (The two of them are rather coupled, but `lexer.rs` contains the lexing primitives,
 and `parser.rs` combines these to parse the syntax tree.)

 - `src/typing` contains the typer. `typer.rs`is where the transformation from parsing ast is done into typed ast.

In those modules :
 - `mod.rs`is the module file, which declares the submodules in the other files.
 - `ast.rs` contains the definition of the abstract syntax trees output by the pass.

### Backend

Each of the pass of the backend is organized similarly :

 - `mod.rs`is again the module file, which declares the submodules in the other files.

 - `tree.rs` defines the data structures.

 - `builder.rs` contains the logic that creates the data structures from the preceding intermediate representation.

 - Extra files exists in specific passes.

There are four passes in the backend

 - `src/rtl` contains the first pass which outputs RTL (Register Transfer Language) code.

 - `src/ertl` contains the second pass which converts it to ERTL (Explicit RTL) code. In this pass, liveness computation is also done, in `liveness.rs`.

 - `src/ltl` contains the third pass which outputs LTL (Location Transfer Language) code. At the beginning of this passe interference computation and register allocation is done, in `ìnterference.rs`.

 - `src/output`is the last pass, which linearises the LTL code into x86_64 assembly. `tree.rs` is instead named `asm.rs`.

### Further information

Please see the relevant `mod.rs` and files for more comment on the implementation.
