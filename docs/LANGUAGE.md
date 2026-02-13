# HIAX Language Specification

This document provides a technical overview of the HIAX syntax, types, and Object-Oriented features.

## 1. Syntax Overview

HIAX uses a C-style syntax with braces `{}` for blocks and semicolons `;` for statement termination. It is case-sensitive.

### Comments

```hix
// This is a single line comment
```

## 2. Variables and Types

Variables are declared using `var` or `dhor`.

### Integer (`int` / `ank`)

Standard 64-bit signed integers.

```hix
dhor x = 10;
var y = 20;
```

### String (`string` / `txt`)

UTF-8 compatible strings.

```hix
dhor name = "HIAX Language";
```

### String Concatenation

Strings can be concatenated with other strings or integers using `+`.

```hix
dhor name = "Agent " + 007;
bol(name); // Output: Agent 7
```

## 3. Operators

- **Arithmetic**: `+` (Add), `-` (Subtract - _planned_), `*` (Multiply - _planned_), `/` (Divide - _planned_)
- **Comparison**: `>` (Greater), `<` (Less), `==` (Equal), `!=` (Not Equal)
- **Assignment**: `=`

## 4. Control Flow

### Conditionals (`if` / `jodi`, `else` / `na`)

```hix
jodi (x > 5) {
    bol("Greater than 5");
} na {
    bol("Smaller or equal");
}
```

### Loops (`while` / `ghur`)

```hix
dhor i = 0;
ghur (i < 5) {
    bol(i);
    i = i + 1;
}
```

## 5. Object-Oriented Programming

### Classes (`class` / `shreni`)

Classes are defined with the `shreni` keyword. Methods are defined with `kaam`.

```hix
shreni Calculator {
    kaam add(a, b) {
        bol(a + b);
    }
}
```

### Instantiation (`new` / `naya` / `noya`)

Objects are created using the `naya` or `noya` keyword.

```hix
dhor calc = noya Calculator();
calc.add(5, 10);
```

### Self Reference (`this` / `iswa`)

Use `iswa` to access instance methods or fields within a class.

```hix
shreni Person {
    kaam greet() {
        bol("Hello!");
    }

    kaam welcome() {
        iswa.greet();
    }
}
```

## 6. Built-in Functions

### `echo` / `bol`

Prints a value to the standard output.

```hix
bol("Output message");
```
