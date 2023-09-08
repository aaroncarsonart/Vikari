# Vikari

Vikari is an original, gradually typed, object-oriented programming language. Generally, it favors operators over keywords. This project is an interpreter that currently supports a subset of the Vikari language. Its initial focus is to support development of simple terminal programs.

### Learn the Language

The [demo][0] contains an exhaustive set of Vikari source files demonstrating each implemented feature. A website also is coming soon to extensively document all of Vikari's language design and to demonstrate usage each of its features.

# License

Vikari is an open-source project licensed under the [Apache License 2.0][1]. Make cool stuff! And if you do, please send me a link: [atonement.crystals@gmail.com][2].

# Features

Vikari aims to be a general-purpose language. But its initial scope limits it to be suitable for simple scripts. The project's structure is based on design principles outlined in [part II][3] of [Crafting Interpreters][4]. Thus is it structured in terms of a [lexer][5], [parser][6], and [tree-walk interpreter][7]. This limits Vikari in terms of execution speed. But optimization is not an initial design goal.

The parser utilizes [recursive descent][8] to parse the language's [grammar production rules][9]. Vikari's [grammar][10] is modeled in an [EBNF][11]-like format for implemented language features [here](grammar.ebnf).

### Implemented Features

The following language features are fully implemented: _numeric types, arithmetic expressions, grouping, print statements, statement separators, variable declarations, assignments, comments, line continuations, boolean types, null types, logical operators, and equality operators_. Other features of the interpreter also include syntax error and compilation warning reporting, different phases of execution, REPL mode, and more. See the [Usage](#Basic Usage) section for details.

### Planned Features

The total list of all planned language features is too multitudinous to exhaustively list here. But to name a few: _control flow structures (conditional statements, loops, error handling), collection types (arrays, lists, maps, sets), characters and strings, functions, type declarations, inheritance, parameterized types, multiple base types (interfaces, enums, records, libraries), and modules._ As well as more operators, and a standard library.

# Dependencies

- [Java 17 JDK][12]
- [Maven][13]

# Build

Build the project with Maven.

```zsh
mvn install
```

# Basic Usage

Run the project by executing the jar and passing it the path to a Vikari file.

```zsh
java -jar target/Vikari-<version_number>.jar <path_to_vikari_file>
```

You can also use the [vikari.sh](vikari.sh) script. It will automatically execute your code with the most recent version of Vikari in the target directory.

```zsh
chmod +x vikari.sh
./vikari.sh <path_to_vikari_file>
```

I recommend aliasing one of these two commands as `vikari`.  From my `.zshrc`:

```zsh
alias vikari="java -jar `ls -r ${VIKARI_PROJECT_DIR}/target/Vikari* | head -n1`"
```

### Run the REPL

`-r` starts Vikari in REPL mode. Use it to sequentially execute Vikari statements and view the result of each statement. Configure the REPL with commands prefixed by `!`. `!help` prints a list of all commands.

```
vikari -r
vikari> val << 42
val:Integer = 42
vikari> val * 2
84
vikari> [val '= null] ^ [val = 42]
true
vikari> !exit
```

# Advanced Usage

Vikari has many options. See `-h` or `--help` for basic usage, or [USAGE.md](USAGE.md) for more details.

## Addendum: Design Notes

### Terminology

Vikari is an invented word meaning _error_. To err is human. Errors are inevitable. What matters most is not that we never make them, but that we learn how to recover from and correct our past mistakes. This process of undoing error is called Atonement. This is a central philosophy behind both the design and implementation of Vikari.

The language is modeled in terms of Atonement Crystals, and Atonement Fields. As components of the language, Atonement Crystals are simply objects, and Atonement Fields are a hash map of string to object. Each crystal is defined by a field. Everything in Vikari is a crystal.

Vikari source files end with the suffix `.DNR` for type definitions or `.dnr` for script files. In terms of the language, DNR is an initialism meaning _default namespace region_. Regions are sections of code in Vikari which denote scope. Thus does each DNR file denote a unique default namespace for the code it contains.

The names for these language constructs have their roots in the author's metaphysical model for understanding reality.

### The Metaphysical Model

Existence is consciousness. The running program of existence is called Atonement. The units of meaning in consciousness which constitute the structure of this program and the output of its execution are called Atonement Crystals. Each crystal's structure is defined by its Atonement Field. These fields are defined entirely in terms of other crystals.

It is called Atonement because consciousness must atone with itself for the original error of believing it has somehow separated from itself, and the resulting immense fear that was then projected outward. This projection organized itself into the Atonement Crystals to facilitate the undoing of itself. The purpose of Atonement is thus to end the separation by undoing the fear that upholds the projection.

DNR is the name for the thought of separation that must be undone. It is the original error. As an initialism, it means _Dahlrei Na Rapnir_, which are invented words meaning _awareness of terror_. DNR is the language in which Atonement is written. Thus is the original error now directly wielded by consciousness through consciousness to undo the error in consciousness.

### A Dialect of DNR

If DNR is meant to be the programming language for all of existence, then Vikari is meant to be a dialect of DNR which implements only a small subset of the total scope of what DNR fully entails. This is also a part of the reasoning behind the Vikari source file suffix.

[0]: demo
[1]: https://choosealicense.com/licenses/apache-2.0/
[2]: mailto:atonement.crystals@gmail.com?subject=Vikari
[3]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
[4]: https://craftinginterpreters.com/
[5]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/Lexer.java
[6]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/Parser.java
[7]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/TreeWalkInterpreter.java
[8]: https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing
[9]: https://craftinginterpreters.com/representing-code.html#rules-for-grammars
[10]: https://craftinginterpreters.com/representing-code.html#context-free-grammars
[11]: https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
[12]: https://openjdk.org/projects/jdk/17/
[13]: https://maven.apache.org/download.cgi
