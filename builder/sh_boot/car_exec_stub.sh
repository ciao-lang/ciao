#!/bin/sh
#
#  car_exec_stub.sh
#
#  Stub loader for .car-style executables (see ciaoc_car.pl)
#
#  Copyright (C) 2015-2017 Ciao Developer Team
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

# Target name (for autoboot.sh messages)
# TODO: probably wrong (autoboot full bundles instead of execs instead?)
target_name="$execname"

builddir="$ciaoroot/build"

f_saved_ciaoroot="$builddir/saved_ciaoroot"
f_saved_env="$builddir/saved_env"

# Relocate build, if needed
saved_ciaoroot=
if [ -f "$f_saved_ciaoroot" ]; then
    saved_ciaoroot=`cat "$f_saved_ciaoroot"`
fi
if [ x"$saved_ciaoroot" != x"$ciaoroot" ]; then
    sh_boot_dir="$ciaoroot/builder/sh_boot"
    sh_src_dir="$ciaoroot/builder/sh_src"
    . "$sh_boot_dir/autoboot.sh"
    boot_builddir="$ciaoroot/build-boot"
    ( builddir_configure_boot "$boot_builddir" && \
	"$ciaoroot/ciao-boot.sh" rescan-bundles "$ciaoroot" > /dev/null )
    printf "%s" "$ciaoroot" > "$f_saved_ciaoroot"
    #
    # (for setup_env_vars)
    # TODO: customize? (e.g., for debug or profile builds, multiarchitecture?)
    eng_name="ciaoengine"
    eng_cfg=`. "$ciaoroot/build/bundlereg/ciao.bundlecfg_sh"; echo $core__OS$core__ARCH`
    setup_eng_vars "$builddir"
    cat > "$f_saved_env" <<EOF
export CIAOENGINE="$bld_objdir/$eng_name"
export CIAOROOT="$ciaoroot"
EOF
fi

. "$f_saved_env"
exec "$builddir/bin/$execname" "$@"
