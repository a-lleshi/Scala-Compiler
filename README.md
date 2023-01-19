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

#### Interpreter



#### Parser


#### Lexer

