#!/bin/sh
#
#  car_exec.sh
#
#  Running .car-style executables
#
#  Copyright (C) 2015 Remy Haemmerle, Jose F. Morales, Ciao Developer team
#
#  Usage: see library(make_car_exec) for details
#
# ---------------------------------------------------------------------------

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------

# Select 'ciaoroot' based on the directory where this script lives
execname=`basename "$0"`
ciaoroot="$_base/$execname.car"

sh_boot_dir="$ciaoroot/builder/sh_boot"
sh_src_dir="$ciaoroot/builder/sh_src"
. "$sh_boot_dir/autoboot.sh"

boot_builddir="$ciaoroot/build-boot"
builddir="$ciaoroot/build"

# TODO: use ciao__DEFAULTLIBDIR from ciao.config_saved_sh file
f_saved_ciaosrc="$builddir/saved_ciaosrc"

# Verifies if the executable has changed location since the last execution time.
# If yes, then reconfigure paths.
prev_ciaosrc=
if [ -f "$f_saved_ciaosrc" ]; then prev_ciaosrc=`cat "$f_saved_ciaosrc"`; fi
if [ x"$prev_ciaosrc" != x"$ciaoroot" ]; then 
    ( builddir_configure_boot "$boot_builddir" && \
	"$ciaoroot/ciao-boot.sh" rescan-bundles "$ciaoroot" > /dev/null )
    printf "%s" "$ciaoroot" > "$f_saved_ciaosrc"
fi

# (for setup_env_vars)
# TODO: customize? (e.g., for debug or profile builds)
eng_name="ciaoengine"
eng_cfg=`. "$ciaoroot/build/ciao.config_saved_sh; echo $core__OS$core__ARCH`
setup_eng_vars "$builddir"
export CIAOENGINE="$bld_objdir/$eng_name"
export CIAOLIB="$ciaoroot/core"
exec "$builddir/bin/$execname" "$@"
