#!/bin/sh
# Run executable (.car archive)

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Get .car path from first argument
_carbase=$1; shift; [ -x "${_carbase}" ] || exit 1

if [ -x ${_carbase}/arch ]; then
  true
else
  ${_base}/compile_native.sh "${_carbase}" || exit 1
fi
CIAOCCONFIG=${_carbase}/configuration ${_carbase}/arch "$@" -C ${_carbase}/noarch ${CIAORTOPTS}
