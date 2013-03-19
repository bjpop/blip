# Copyright   : (c) 2012, 2013 Bernie Pope
# License     : BSD-style
# Maintainer  : florbitous@gmail.com 

# A convenience Makefile.
# Mostly a wrapper for cabal.

.PHONY : all
all:
	cabal configure
	cabal build

.PHONY : clean
clean:
	cabal clean

.PHONY : configure
configure:
	cabal configure

.PHONY : install
install:
	cabal install --disable-library-profiling --disable-executable-profiling --disable-documentation
