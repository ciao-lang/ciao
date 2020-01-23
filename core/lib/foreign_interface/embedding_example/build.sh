#!/bin/sh

osarch=$(ciao_sysconf --osarch)
engdir=$(dirname $(which ciao_sysconf))/../eng/ciaoengine
config=$engdir/cfg/$osarch/config_sh
ciaolib=$engdir/objs/$osarch/libciaoengine.a

. $config

# ciaoc -c test.pl
ciaoc -o test.cpx -s test.pl
$CC -c $CFLAGS $CCSHARED test.c
$LD $LDFLAGS -o test test.o $ciaolib

