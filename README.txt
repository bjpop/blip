Blip, a bytecode compiler for Python 3 
--------------------------------------

Blip compiles Python 3 source files to bytecode. The output bytecode
is compatible with the CPython interpreter.

For example, given a Python 3 source file called foo.py, the command:

   blip foo.py

produces a bytecode file called foo.pyc. The bytecode can be executed
by passing it as an argument to a CPython interpreter:

   python foo.pyc

The blip source tree also includes code for a program called readpyc,
which can be used for pretty printing the contents of .pyc files:

   readpyc foo.pyc

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

--------------------------------------------------------------------------------
