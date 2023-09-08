# Vikari: Advanced Usage

These examples use a `vikari` alias as suggested in [Basic Usage](README.md#basic-usage).

# Help

`-h` or `--help` prints a message demonstrating basic usage of all program options and their arguments, along with a brief explanation.

# Version

`-v` or `--version` prints the current version of Vikari.

# Source Code

The file naming convention for Vikari source files match against one of the two following formats: `TypeName.DNR` and `script_name.dnr`.

Type definition files will be required to use the first format, following type naming conventions with an uppercase file extension. While script files will be required to use the second format, following variable naming conventions and a lowercase file extension.

But for now, every file is a script. And so any Vikari source file can use one of the two naming conventions.

### Execute a File

`-f` or `--file` explicitly sets the path for a file to execute. This option can be omitted if the file is the last argument.

```zsh
vikari -f demo/numbers.dnr
vikari demo/variable_declarations.dnr
```

### Execute a Type

`-t` or `--type` is the same as `-f` but the path separator is `::` instead of `/` and the file extension is omitted.

```zsh
vikari -t demo::whitespace
```

This option will be preferred when type definition files with package regions are implemented, as it is more in line with the rest of Vikari's syntax.


### Execute a Code String

`-c` or `--code` specifies a string of code to execute. Quoting this string can become complicated, as Vikari uses `"`, `'`, and `` ` ``  as operators. Separate statements with `,` and use the print operator `:` to produce output.

```zsh
vikari -c "int:Integer << 1024, :int / [256 * 2]:"
```

# Lexer Phase

The [Lexer][0] accepts text as input, and produces a list of [tokens][3] (crystals) for each statement as output. To only run the Lexer, use `-l` or `--lex` to report any syntax errors checked for at this phase. Alternatively, use `-L` or `--Lex` to additionally configure the printing of lexed tokens in the program output using an argument of character flags.

The following command prints a verbose list of each token's type name with statement numbers.

```zsh
vikari -L pnv -c "int:Integer << 2, :[int + 9.0]:"
```

This produces the following output:

```
----
Lex:
----
[1]: Reference("int"), TypeLabelOperator(":"), TypeReference("Integer"), LeftAssignmentOperator("<<"), Integer("2")
[2]: TypeLabelOperator(":"), LeftSquareBracket("["), Reference("int"), AddOperator("+"), Double("9.0"), RightSquareBracket("]"), TypeLabelOperator(":")
```

All language tokens can be lexed. Note that overloaded operators are always lexed as one specific type. So the print statement operator `:` will instead be described as a type label operator during this phase.

# Parser Phase

The [Parser][1] accepts the Lexer output as input, and produces an [AST][4] in the form of statements composed of expressions. To run code through the Lexer and Parser, use `-p` or `--parse` to report any syntax errors checked at both the Lexer and Parser phases. Alternatively, use `-P` or `--Parse` to additionally configure the printing of each parsed statement and expression in the program output using an argument of character flags.

The following command prints a verbose list of each statement and expression type with statement numbers:

```zsh
vikari -P pnv -c "int:Integer << 2, :[int + 9.0]:"
```

This produces the same output as `-L`, but also with the following additional parser output:

```
------
Parse:
------
[1]: VariableDeclarationStmt:[int:Integer, LeftAssignmentOperator:[<<], LiteralExpr:[2]]
[2]: PrintStmt:[PrintExpr:[PrintStatementOperator:[:], GroupingExpr:[BinaryExpr:[VariableExpr:[int], AddOperator:[+], LiteralExpr:[9.0]]]], PrintExpr:[PrintStatementOperator:[:]]]
```

Only fully implemented language features can be parsed without producing syntax errors. So features like functions and type declarations will not currently successfully pass this phase.

# Execute Phase

The [Interpreter][2] accepts the Parser output as input, and executes the AST directly by walking the tree and evaluating each expression in each statement. To run code through the Lexer, Parser, and Interpreter, use `-e` or `--execute` to report any syntax errors checked at both the Lexer and Parser phases, and then also execute the code afterwards if there were no errors. Alternatively, use `-E` or `--Execute` to also configure the printing of Lexer and Parser output in addition to running the program afterwards.

Mirroring the previous two examples, use the following command to execute all three phases of the interpreter:

```zsh
vikari -E pnv -c "int:Integer << 2, :[int + 9.0]:"
```

This prints the same output of the Lexer and Parser phases as the previous two examples, but also executes the program and prints the following output:

```
--------
Execute:
--------
11.0
```

The `Lex`, `Parse`, and `Execute` headers are only included with the program output if the printing of token or statement values are requested with the `p` flag in as argument for `-L`, `-P`, or `-E`.

If no phase option is given, the `Execute` phase is selected by default. So if the additional Lexer and Parser output is not desired, simply use `-f`, `-t`, or `-c` to execute the program with normal output.

# Syntax Errors

Any Syntax errors will always be automatically reported, preventing the program from being executed. To only check for syntax errors and not execute a program, use `-l` or `-p` to validate code through that phase. `-L` and `-P` can also be used, but require the additional argument of character flags such as `pnv` to configure the reporting of Lexer and Parser output. `-l` and `-p` omit this part.

### Lexer Phase Validation

Use `-l` to check any valid language construct against only the Lexer phase.

```zsh
vikari -lc ':``foo' 
```

This reports an error for an unterminated string:

```
--------------
Syntax Errors:
--------------
<repl>:1:2:
    :``foo
     ^
    Missing closing capture quotation ``.
```

### Parser Phase Validation

Use `-p` to validate fully supported language features against both the Lexer and the Parser.

```zsh
vikari -pc ":[foo"
```

This reports errors for the use of an undeclared variable, and an unclosed grouping:

```
--------------
Syntax Errors:
--------------
<repl>:1:3:
    :[foo
      ^
    Undefined variable reference.

<repl>:1:5:
    :[foo
         ^
    Expected `]` after expression.
```

# Enabling Warnings

Use `-w` or `--warnings` to include compilation warnings.

```zsh
vikari -wlc ":2 + 5~"
```

This reports a warning for the trailing line continuation:

```
---------------------
Compilation Warnings:
---------------------
<repl>:1:7:
    :2 + 5~
          ^
    Unnecessary line continuation at end of statement.
```

`-w` prevents execution of the program if any warnings are reported.

### Alternate Warning Flag

Alternatively, use the character flag `w` instead with `-L`, `-P`, or `-E` respectively to first check for and report any warnings, but then to also execute the program anyways afterwards.

```zsh
vikari -E w -c ":2 + 5~"
```

This produces the following output:

```
---------------------
Compilation Warnings:
---------------------
<repl>:1:7:
    :2 + 5~
          ^
    Unnecessary line continuation at end of statement.

---------------
Program Output:
---------------
7
```

# REPL

`-r` or `--repl` starts Vikari in REPL mode.

```zsh
vikari -r
vikari>
```

REPL mode is also started if Vikari is executed with no arguments.

### REPL Commands

REPL commands are prefixed by `!` and are case-insensitive.

- `!HELP` displays a list of all REPL commands.
- `!CLEAR` clears the REPL state. All declared variables are cleared.
- `!VERBOSE` toggles printing the result of declarations, assignments, and expressions.
- `!WARNINGS` toggles compilation warnings.
- `!EXIT` and `!QUIT` both exit REPL mode.

Additionally:

- `Ctrl + C` cancels the current in-progress line of input, and starts a new line.
- `Ctrl + D` will exit the REPL, but only if the current line of input is clear.

# Log Levels

Vikari logs to `~/.vikari/vikari.log`. To specify a log level, use `-g` or `--log-level` along with a string argument specifying the log level. One of:

```
ALL, TRACE, DEBUG, INFO, WARN, ERROR, FATAL, OFF
```

If not specified, the default log level is `TRACE`.

### Program Id

Each program has a unique program id to help identify it in the logs. The last used program id is tracked in `~/.vikari/vikari.program_id` in a thread-safe manner.

[0]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/Lexer.java
[1]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/Parser.java
[2]: src/main/java/com/atonementcrystals/dnr/vikari/interpreter/TreeWalkInterpreter.java
[3]:https://craftinginterpreters.com/scanning.html
[4]: https://craftinginterpreters.com/representing-code.html#implementing-syntax-trees