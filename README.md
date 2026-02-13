# HIAX Language

**HIAX** (Hindi-Assamese-Extension) is a bilingual, high-performance, general-purpose programming language built on top of the **Zig** engine. It bridges the gap between local languages (Hindi/Assamese) and professional software engineering, allowing you to write code in English, Hindi/Assamese, or a mix of both!

> **Note**: The current version is optimized for **Windows**. Linux and macOS support is currently experimental.

---

## Why HIAX?

1.  **Bilingual Core**: Write code in English OR Hindi/Assamese. Per-file or per-line mixing is supported.
2.  **Zig-Powered Engine**: Built on Zig 0.16.0-dev for extreme performance and memory safety.
3.  **Two Modes**:
    - **Interpreter**: Instant feedback, great for scripts and learning.
    - **Transpiler**: Compiles HIAX to Zig, then to a native optimized binary (comparable to C++ speed).
4.  **AI-Ready**: Designed with future AI agent capabilities in mind (inference keywords coming soon).

---

## Installation & Build

### Prerequisites

- **OS**: Windows, Linux, or macOS.
- **Zig Compiler**: You need **Zig 0.16.0-dev** (or newer).
  - Download from: [ziglang.org/download](https://ziglang.org/download/)
  - Add `zig` to your System PATH.

### Building HIAX from Source

1.  Clone the repository:

    ```powershell
    git clone https://github.com/AajoriGroups/HIAX.git
    cd hiax
    ```

2.  Build the release executable:

    ```powershell
    zig build -Doptimize=ReleaseFast
    ```

    - The executable will be creating in `zig-out/bin/hiax.exe` (Windows) or `zig-out/bin/hiax` (Linux/Mac).

---

## Usage

You can use HIAX in two ways:

### 1. Interpreter Mode (Run directly)

Useful for development, testing, and scripts.

```powershell
# Run a HIAX file
./zig-out/bin/hiax examples/hello.hix

# View Help
./zig-out/bin/hiax --help

# List Bilingual Keywords
./zig-out/bin/hiax --list
```

### 2. Transpiler Mode (Build to Native Binary)

Compiles your HIAX code to Zig, then builds a standalone executable.

```powershell
# Compile and Run
./zig-out/bin/hiax --build examples/complex_test.hix
```

_Note: This requires `zig` to be in your PATH._

---

## Language Guide

### Bilingual Keywords

HIAX supports English and Hindi/Assamese keywords interchangeably.

| English          | Native (Hindi/Assamese) | Description                           |
| :--------------- | :---------------------- | :------------------------------------ |
| `var`            | `dhor`                  | Declare a variable                    |
| `echo` / `print` | `bol`                   | Print to standard output              |
| `if`             | `jodi`                  | Conditional start                     |
| `else`           | `na`                    | Conditional else                      |
| `while`          | `ghur`                  | While Loop                            |
| `for`            | -                       | For Loop                              |
| `function`       | `kaam`                  | Define a function                     |
| `return`         | _implicit_              | Return value (keyword support coming) |
| `class`          | `shreni`                | Define a class                        |
| `new`            | `naya` / `noya`         | Instantiate a class                   |
| `this`           | `iswa`                  | Current instance reference            |
| `int`            | `ank`                   | Integer type                          |
| `string`         | `txt`                   | String type                           |

### Examples

Check out the `examples/` folder for a step-by-step learning path:

1.  [Basics (Variables & Printing)](examples/01_basics.hix)
2.  [Control Flow (If/Else, Loops)](examples/02_control_flow.hix)
3.  [Functions](examples/03_functions.hix)
4.  [Object Oriented Programming](examples/04_oop.hix)

#### 1. Hello World (Mixed)

#### 1. Hello World (Mixed)

```hix
dhor message = "Namaste, World!";
bol(message);

var english_msg = "Hello!";
print(english_msg);
```

#### 2. Functions & Logic

```hix
kaam add(a, b) {
    return a + b;
}

dhor x = 10;
jodi (x > 5) {
    bol("X is big");
} na {
    bol("X is small");
}
```

#### 3. Loops

```hix
// While Loop
dhor i = 0;
ghur (i < 5) {
    bol(i);
    i = i + 1;
}

// For Loop
for (dhor j = 0; j < 5; j = j + 1) {
    bol(j);
}
```

#### 4. Object-Oriented Programming

```hix
shreni Greeter {
    kaam greet(name) {
        bol("Hello, " + name);
    }
}

dhor g = noya Greeter();
g.greet("HIAX User");
```

---

## Roadmap: The AI-Friendly Future

HIAX is evolving to be the primary language for AI Agents:

- **`socho`**: Built-in keyword for AI inference/prompts.
- **Semantic Types**: Native support for embeddings and vector similarity.
- **Agent Classes**: Specialized `shreni Agent` types with integrated system prompts.
- **Safe Sandboxing**: Secure execution environment for AI-generated code.
- **Garbage Collection**: Mark-and-Sweep GC for long-running applications.

---

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

## License

HIAX is licensed under the [MIT License](LICENSE).
