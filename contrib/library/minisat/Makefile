#
# The CIAO-Prolog interface to MiniSat
# Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
# 
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

CIAOCMD ?= ciao

CC=gcc
CIAO_INC_PATH=../../include/LINUXi686 # TODO: wrong, use ciao-config

C_OPT = -O3 -fomit-frame-pointer -s

minisat.po: ciao-minisat.o solver.o libminisat.so Makefile

libminisat.so:	ciao-minisat.o solver.o Makefile
	$(CC) $(C_OPT) -shared -o libminisat.so ciao-minisat.o solver.o

solver.o:	solver.c solver.h vec.h Makefile
	$(CC) $(C_OPT) -c -fpic -o solver.o -std=c99 solver.c

ciao-minisat.o:	ciao-minisat.c ciao-minisat.h Makefile
	$(CC) -I$(CIAO_INC_PATH) $(C_OPT) -c -fpic -o ciao-minisat.o ciao-minisat.c



##-----------------------------------------------------------------------------
clean:
	$(CIAOCMD) clean_tree "."




