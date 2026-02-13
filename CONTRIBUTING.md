# Contributing to HIAX

Thank you for your interest in contributing to HIAX! We welcome contributions from everyone.

## Getting Started

### Prerequisites

You need **Zig 0.16.0-dev** (or newer) to build HIAX.

### Building

1.  Clone the repository:

    ```bash
    git clone https://github.com/your-username/hiax.git
    cd hiax
    ```

2.  Build the project:
    ```bash
    zig build
    ```
    This will generate the executable in `zig-out/bin/hiax`.

### Running Tests

We have several test files to verify both the interpreter and transpiler.

1.  **Interpreter Mode**:

    ```bash
    zig-out/bin/hiax test.hix
    zig-out/bin/hiax complex_test.hix
    ```

2.  **Transpiler Mode**:
    ```bash
    zig-out/bin/hiax --build test.hix
    zig-out/bin/hiax --build complex_test.hix
    ```

### Code Structure

- `src/main.zig`: Entry point.
- `src/lexer.zig`: Tokenizer (handles bilingual keywords).
- `src/parser.zig`: Parses tokens into AST.
- `src/ast.zig`: Abstract Syntax Tree definitions.
- `src/evaluator.zig`: Interpreter logic.
- `src/codegen.zig`: Transpiler logic (generates Zig code).
- `src/object.zig`: Runtime object representation.

## How to Contribute

1.  Fork the repository.
2.  Create a new branch: `git checkout -b feature/my-feature`.
3.  Make your changes.
4.  Run tests to ensure everything works.
5.  Commit your changes: `git commit -m 'Add new feature'`.
6.  Push to the branch: `git push origin feature/my-feature`.
7.  Open a Pull Request.

## Coding Style

- Use `zig fmt` to format your code.
- Keep code clean and readable.

## Need Help?

Check out `docs/ENGINE.md` for internal architecture details or open a discussion on GitHub.
