# Scala-Compiler

This project utilizes scala to lex and parse the WHILE language and then compiling the language into Java Byte Code using the Jasmin compiler.

## Getting Started

This project will require to have ammonite installed on your machine. You can find the installation instructions [here](http://ammonite.io/#Ammonite-REPL).

### Prerequisites

You will need to have the following installed on your machine:
- Scala 2.13.7 (NOT scala 3)
- Ammonite 2.4.0
- Jasmin Jar File (included in the repo)
- some WHILE programs (included in the repo, can write your own following the syntax :))
- Depending on your OS, you may need to change `beginning` prelude variable from `ldc 10` for Linux/MacOS to `ldc 13` for Windows (this has been commented in the code in `compiler.sc`)

### Commands to run the project


#### Compiler

To run the project directly from the command line, you can run the following commands:

```bash
amm compiler.sc run "WHILE_FILE_NAME.while"
```

To compile the WHILE program into a .j file, you can run the following commands:

```bash
amm compiler.sc main "WHILE_FILE_NAME.while"
```

You can then run the .j file using the Jasmin compiler:

```bash
java -jar jasmin.jar "WHILE_FILE_NAME.j"
```

#### Parser

To parse a WHILE program at command line, you can run the following commands:

```bash
amm parser.sc "WHILE_FILE_NAME.while"
```

#### Lexer

To lex a WHILE program at command line, you can run the following commands:

```bash
amm lexer.sc "WHILE_FILE_NAME.while"
```

