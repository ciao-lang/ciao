#!/bin/sh
# Debug native code (.car archive)

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Get .car path from first argument
_carbase=$1; shift; [ -x "${_carbase}" ] || exit 1

if [ -x ${_carbase}/arch ]; then
  true
else
  ${_base}/compile_native "${_carbase}" --debuglevel=debug || exit -1
fi
echo "{Type 'run' to start the program}"
CIAOCCONFIG=${_carbase}/configuration gdb --silent -d ${_carbase}/c/engine --args ${_carbase}/arch "$@" -C ${_carbase}/noarch ${CIAORTOPTS}

