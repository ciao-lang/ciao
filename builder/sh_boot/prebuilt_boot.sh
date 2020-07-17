#!/bin/sh
#
#  prebuilt_boot.sh
#
#  Boot from prebuilt sources (temporary! merge with other code)
#
#  Copyright (C) 2017 Jose F. Morales
#

# TODO: allow more valid commands (only local-install and install are supported)
# TODO: check that re-configuration is consistent w.r.t. prebuilt sources
# TODO: improve relocation
#  - patch binaries instead?
#  - prebuild in a better default location?
#  - use a single CIAOROOT env var (instead of CIAOENGINE,CIAOHDIR,...)
# TODO: touch all po,itf so that no recompilation happens
# TODO: mingw version?
# TODO: missing relocation of lib/compiler/header (new executables still require explicit CIAOENGINE env var)
# TODO: create exec stub (turn into static + lib)
# TODO: create small ciaoengine (dynlink to so)

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Detect ciaoroot
old_dir=`pwd`; cd "$_base/../.."; ciaoroot=`pwd`; cd "$old_dir"; old_dir=

cfg=$CIAO_PREBUILT_CFG # (must be passed)
if [ x"$cfg" = x"" ]; then
    cat <<EOF
ERROR: CIAO_PREBUILT_CFG not defined!

EOF
    exit 1
fi

fix_symlinks() {
    # normal_message "relocating symbolic links..."
    # Fix some symbolic links in build/ directory
    cd "$ciaoroot"
    # TODO: meta_sh, etc. are not patched
    . "$ciaoroot/build/eng/ciaoengine/cfg/$cfg/meta_sh" # (for eng_srcpath)
    local old=$eng_srcpath # old srcpath at build host
    # TODO:[optim_comp] fix path
    local new="$ciaoroot/core/engine" # new srcpath at this host
    local i f g
    for i in $(find "$ciaoroot/build" -type l); do
        f=$(readlink "$i")
        g=$(printf "%s" "$f" | sed -e "s;^$old;$new;")
        if [ "$f" != "$g" ]; then
            unlink "$i"
            ln -s "$g" "$i"
        fi
    done
}

# Rebuild some usermods to fix absolute paths
# TODO: avoid absolute paths in usermods
fix_usermods() {
    # usermods and mods with dependency to usermods
    rm -f "$ciaoroot"/build/cache/core.library.toplevel.toplevel_scope.* \
          "$ciaoroot"/build/cache/core.library.toplevel.toplevel.*
    "$ciaoroot"/builder/sh_boot/builder_boot.sh build core.shell
}

# Rebuild ciao-env and patch it to enable relocation (explicit
# definitions for CIAOENGINE,CIAOHDIR,CIAOROOT)
# TODO: a single CIAOROOT should be enough (or patching binaries)
fix_ciao_env() {
    "$ciaoroot"/builder/sh_boot/builder_boot.sh build core.ciao_env
    f="$ciaoroot"/build/bin/ciao-env
    mv "$f" "$f".bak
    cat "$f".bak | sed '-e' 's;^reloc=no$;reloc=yes;' > "$f"
    chmod a+x "$f"
    rm -f "$f".bak
}

# Check that this is a valid command for a prebuilt distribution
case $1 in
    local-install) shift; cmd=install ;;
    install) shift; cmd=install ;;
    *) cat <<EOF
ERROR: Command '$1' not allowed when using a prebuilt distribution.

If this is what you really need, please consider '--no-prebuilt' to
build from sources.

EOF
       exit 1
       ;;
esac

cd "$ciaoroot" # (needed for installation)
"$ciaoroot"/builder/sh_boot/builder_boot.sh rescan-bundles # (fix paths)
"$ciaoroot"/builder/sh_boot/builder_boot.sh configure "$@"
fix_ciao_env
fix_symlinks
fix_usermods
exec "$ciaoroot"/builder/sh_boot/builder_boot.sh "$cmd"
