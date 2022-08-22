#!/bin/sh

#osarch=$(ciao_sysconf --osarch)
osarch=DEFAULT
engdir=$(dirname $(which ciao_sysconf))/../oc-cache/bin/ciaoloader.car
config=$engdir/cfg/$osarch/config_sh
ciaolib=$engdir/objs/$osarch/libarch.a

. $config

ciao oc:comp --staexec test.cpx test.pl
$CC -c $CFLAGS $CCSHARED test.c
$LD $LDFLAGS -o test test.o $ciaolib $LIBS

