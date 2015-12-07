#!/bin/sh
# Wrapper for booting ciao_builder

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

$_base/builder/sh_boot/builder_boot.sh "$@"
