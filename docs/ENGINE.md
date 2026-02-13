# HIAX Engine Internal Architecture ⚙️

HIAX is powered by a custom-built engine written in **Zig**, a modern systems programming language known for its focus on robustness, optimality, and maintainability.

## 1. Why the Zig Engine?

Traditional languages like Python use a C-based interpreter which can be slow and memory-heavy. HIAX uses Zig for several critical reasons:

- **Spatial Memory Safety**: Prevents common bugs like buffer overflows.
- **Extreme Speed**: Zig is as fast as C, ensuring HIAX can handle large data processing tasks.
- **Comptime Power**: HIAX uses Zig's compile-time features to optimize the bilingual keyword mapping.

## 2. Compilation vs Interpretation

HIAX offers two modes of execution:

- **Standalone Engine**: A fast tree-walking interpreter for quick prototyping.
- **Transpiler Mode**: Converts HIAX code into optimized Zig code, which is then compiled into a native binary via the Zig compiler (`zig build`).

## 3. Memory Management

The HIAX engine uses an **Arena Allocator** strategy.

- When your HIAX program runs, all AST nodes and objects are allocated in a high-speed memory block.
- Once execution finishes, the entire block is freed at once. This virtually eliminates individual "leaks" and makes the engine incredibly fast.

## 4. Bilingual Parser Logic

The engine's parser is designed with an "Aliasing Layer."

- The Lexer recognizes tokens like `dhor` and `var` as the same internal ID.
- This means there is **zero performance penalty** for using Hindi/Assamese keywords.

## 5. Roadmap: LLM Integration

We are integrating the HIAX engine with native `C` bindings to LLM libraries (like llama.cpp).

- **The Vision**: The `socho` keyword will call directly into the engine's internal LLM bridge, bypassing slow Python wrappers.

---

_Built with ❤️ using Zig._
