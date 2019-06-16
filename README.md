# Infinity Language Base Library

The Infinity Language base library consists of a Type-system, Lexer, Parser, and expression Evaluator.

# Building

The following external dependencies are required:

- CMake 3.10+
- C++17 Compiler
- GNU MP
- GNU MPFR

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

A hand written lexer.

## Parser

A hand written recursive-decent parser.

## Evaluator

Slow-as-shit run-time expression Evaluator.