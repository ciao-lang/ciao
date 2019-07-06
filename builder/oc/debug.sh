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
  ${_base}/compile_native.sh "${_carbase}" --debuglevel=debug || exit -1
fi
echo "{Type 'run' to start the program}"
if command -v gdb > /dev/null 2>&1; then
    CIAOCCONFIG=${_carbase}/cfg/DEFAULT gdb --silent -d ${_carbase}/c/engine --args ${_carbase}/arch "$@" -C ${_carbase}/noarch ${CIAORTOPTS}
elif command -v lldb > /dev/null 2>&1; then
    CIAOCCONFIG=${_carbase}/cfg/DEFAULT lldb -- ${_carbase}/arch "$@" -C ${_carbase}/noarch ${CIAORTOPTS}
else
    echo "ERROR: no 'gdb' nor 'lldb' found" 1>&2
    exit -1
fi


