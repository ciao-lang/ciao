#!/bin/sh

# TODO: allow running without setting any environment variable

#osarch=$(ciao_sysconf --osarch)
osarch=DEFAULT
engdir=$(dirname $(which ciao_sysconf))/../oc-cache/bin/ciaoloader.car
# Make sure that CIAOROOT and CIAOCACHE are defined
r=${CIAOROOT}
c="$r/build/oc-cache"
c=${CIAOCACHE:-$c}
CIAOCACHE="$c" CIAOCCONFIG=$engdir/cfg/$osarch ./test
