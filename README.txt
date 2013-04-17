Blip, a bytecode compiler for Python 3 
--------------------------------------

Blip compiles Python 3 source files to bytecode. The output bytecode
is compatible with the CPython interpreter.

For example, given a Python 3 source file called foo.py, the command:

   blip foo.py

produces a bytecode file called foo.pyc. The bytecode can be executed
by passing it as an argument to a CPython interpreter:

   python3 foo.pyc

You can specify more than one python source file on the command line;
blip will compile them all in sequence, stopping at the first error
encountered.

The blip source tree also includes code for a program called readpyc,
which can be used for pretty printing the contents of .pyc files:

   readpyc foo.pyc

Usage
-----

usage: blip [options] [--] [PYTHON_FILES ...]
  [--version]               Show the version number of blip.
  [-h,--help]               Display a help message.
  [--magic <magic number>]  Magic number to include in pyc file header.
  [--dumpScope]             Dump the variable scope.
  [--dumpAST]               Dump the abstract syntax tree.

Each version of CPython uses a magic number in the .pyc file header.
This is to prevent the wrong version of CPython from being used to
interpret a given .pyc file. The current version of blip generates
bytecode which is compatible with Python 3.3, and automatically
uses the correct magic number for that version. If you want to
use a different magic number you can specify it on the command line
with the --magic argument. You might need to consult the source
code of CPython to find out what magic number to use for a given
CPython version.

License and Copyright
---------------------

Blip is distributed as open source software under the terms of the BSD
License (see the file LICENSE in the top directory).

Author: Bernie Pope, copyright 2012, 2013.

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

Building and installing
-----------------------

Blip uses the cabal infrastructure for configuring, building and installation.
However, for convenience a Makefile is provided to orchestrate the process.

To build and install, use:

   make install

To clean, use:

   make clean

Alternatively, if you want to use cabal-dev then use the command:

   make dev

Testing
-------

Blip uses shelltestrunner for regression testing. Tests can be run like so:

   make test

Shelltestrunner can be installed from Hackage:

   cabal install shelltestrunner

Blip is a work-in-progress, so some tests might fail.

Directory structure
-------------------

---- lib                                 
     |
     |---- src
           |
           |---- Blip     # library code for manipulating .pyc files


---- compiler             
     |
     |---- src            # the bytecode compiler program


---- readpyc
     |
     |---- src            # a program for pretty printing .pyc files


---- test
     |
     |---- regression
	   |
	   |---- features     # tests for particular language features
	   |
	   |---- programs     # Python programs

--------------------------------------------------------------------------------
