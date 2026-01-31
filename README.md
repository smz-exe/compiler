# SPL Compiler

A simple compiler for SPL (Simple Programming Language) targeting x86_64.

## Structure

```
src/           # Compiler source
examples/      # Sample programs
build/         # Compiler output (generated)
```

### Compiler Components

| File | Description |
| ---- | ----------- |
| `src/lexer.mll` | Lexer (OCamllex) |
| `src/parser.mly` | Parser (OCamlyacc) |
| `src/ast.ml` | Abstract Syntax Tree |
| `src/types.ml` | Type definitions |
| `src/table.ml` | Symbol table |
| `src/semant.ml` | Semantic analysis |
| `src/emitter.ml` | x86_64 code generation |
| `src/sim.ml` | Main driver |

## Usage (Docker)

Requires Docker for x86_64 environment on Apple Silicon.

```bash
./run.sh make                              # Build compiler
./run.sh "./src/simc examples/sample.spl"  # Compile program
./run.sh ./build/sample                    # Run program
./run.sh                                   # Interactive shell
```

## Usage (Native x86_64)

```bash
make                              # Build compiler
./src/simc examples/sample.spl    # Compile → build/sample
./build/sample                    # Run
```

## Compiler Options

```bash
./src/simc <input.spl>            # Output to build/<name>
./src/simc <input.spl> -o <path>  # Custom output path
```
