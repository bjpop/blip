# Blip, a bytecode compiler and interpreter for Python 3 

Blip compiles Python 3 source files to bytecode. The output bytecode
is compatible with the CPython interpreter.

For example, given a Python 3 source file called foo.py, the command:

```
   blip -c foo.py
```

produces a bytecode file called `foo.pyc`. The bytecode can be executed
by passing it as an argument to a CPython interpreter:

```
   python3 foo.pyc
```

or you can pass it as an argument to blip:

```
   blip foo.pyc
```

If you omit the `-c` argument then blip will compile the Python code
to bytecode and then immediately execute it:

```
   blip foo.py
```

If you don't provide a Python file as input to blip it will start
a REPL where you can type in Python statements and have them
immediately executed:

```
   blip
```

The blip source tree also includes code for a program called readpyc,
which can be used for pretty printing the contents of `.pyc` files:

```
   readpyc foo.pyc
```

# Usage

```
usage: blip [options] [<input file>]
  [--version]               Show the version number of blip.
  [-h,--help]               Display a help message.
  [--magic <magic number>]  Magic number to include in pyc file header.
  [--dumpScope]             Dump the variable scope.
  [--dumpAST]               Dump the abstract syntax tree.
  [-c,--compile]            Compile .py to .pyc but do not run the program.
  [<input file>]            Name of the input Python file, either .py or .pyc
```

Each version of CPython uses a magic number in the .pyc file header.
This is to prevent the wrong version of CPython from being used to
interpret a given .pyc file. The current version of blip generates
bytecode which is compatible with Python 3.3, and automatically
uses the correct magic number for that version. If you want to
use a different magic number you can specify it on the command line
with the --magic argument. You might need to consult the source
code of CPython to find out what magic number to use for a given
CPython version.

# License and Copyright

Blip is distributed as open source software under the terms of the BSD
License (see the file LICENSE in the top directory).

Author: Bernie Pope, copyright 2012 onwards. 

# Building and installing

Blip uses the cabal infrastructure for configuring, building and installation.
However, for convenience a Makefile is provided to orchestrate the process.

To build (in the top directory of the source tree): 

```
cabal build all
```

To install blip:
```
cabal install --overwrite-policy=always blip
```

To install readpyc:
```
cabal install --overwrite-policy=always readpyc 
```

# Testing

Blip uses shelltestrunner for regression testing. Tests can be run like so:

   make test

Shelltestrunner can be installed from Hackage:

   cabal install shelltestrunner

Blip is a work-in-progress, so some tests might fail.
