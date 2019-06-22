# Infinity Language Base Library

The Infinity Language base library consists of a Type-system, Lexer, Parser, and expression Evaluator.

# Building

The following external dependencies are required:

- CMake 3.10+
- C++17 Compiler
- GNU MP
- GNU MPFR
- LLVM 8

## Linux / MacOS

From the source directory run the following commands

```bash
git submodule update --init --recursive
mkdir build && cd build
cmake ..
cmake --build .
```

# Components

## Type System

A type system incorporating features of refinement, dependent and uniqueness typing.

## Lexer

Simple lexer.

## Parser

Recursive-decent parser. Does partial type checking when parsing.

## Evaluator

Slow-as-shit AST walking expression Evaluator.

Currently supports:

- Function definitions
- C++ Function binding (FFI)
- Arbitrary-precision arithmetic
