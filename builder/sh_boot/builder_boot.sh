#!/bin/sh
#
#  builder_boot.sh
#
#  A (sh) portable wrapper for ciao_builder that can self-compile and boot.
#
#  Copyright (C) 2015 Jose F. Morales, Ciao Developer team
#

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------
# Imports

old_dir=`pwd`; cd "$_base/../.."; ciaoroot=`pwd`; cd "$old_dir"; old_dir=
sh_boot_dir="$ciaoroot/builder/sh_boot"
sh_src_dir="$ciaoroot/builder/sh_src"

. "$sh_boot_dir/autoboot.sh"
. "$sh_boot_dir/scan_bootstrap_opts.sh"

# ---------------------------------------------------------------------------
# Some paths for source code locations and build directories

# Default CIAOLIB
default_ciaolib="$ciaoroot/core"
# The pre-compiled bootstrap ciaoc
boot_ciaoc="$ciaoroot/core/bootstrap/ciaoc.sta"
# Default engine
eng_name="ciaoengine" # TODO: allow a different one for bootstrap?

# Builddir where ciao_builder is compiled
boot_builddir="$ciaoroot/build-boot"
# The ciao_builder command-line module
builder_mod="$ciaoroot/builder/cmds/ciao_builder"

# Build configuration name for bootstrap
boot_eng_cfg() { printf "BOOT"; }

# ---------------------------------------------------------------------------
# Error message when there is no bootstrap (needed for autoboot.sh)

# Email where bugs must be sent
bugs_mail="ciao-bugs@clip.dia.fi.upm.es"

no_bootstrap_error() { # ARGS
    cat <<EOF
ERROR: Bootstrap is missing!

Please use 'help' (or 'help-all') for usage information.
EOF
    exit 1
}

failed_bootstrap_error() { # ARGS
    cat <<EOF
ERROR: Bootstrap could not be compiled!

If needed, use 'emergency-clean' to recover from inconsistent build
states and try 'boot-rebuild' to build the bootstrap manually.

If bootstrap compilation failed for your system, please consult online
documentation or report the bug to <$bugs_mail>.

EOF
    exit 1
}

# ---------------------------------------------------------------------------
# Message formatting
# NOTE: Synchronize with builder/src/messages_aux.pl

cmd_message() { # target [rest...]
    local target=$1
    shift
    echo "=> $target: $*" 1>&2
}

normal_message() {
    echo "   $*" 1>&2
}

# ---------------------------------------------------------------------------

# Clean the compilation output for all files, recursively
# TODO: Use *.glue.c, *.inline.c, *.co.pl, *.auto.pl, etc.
clean_tree() {
    # NOTE: this code has been optimized (usually it should take less
    #   than 10 seconds)
    #
    # We cannot use -print0 | xargs -0 because -print0 is not compatible
    # with Solaris, nor -print | xargs because it is buggy
    test x"$1" = x"" && return
    find "$1"/. -name ".svn" -prune -o \( \
	-name "*.po" -o -name "*.itf" -o -name "*.wam" \
	-o -name "*.asr" -o -name "*.ast" \
	-o -name "*.testout" \
	\
	-o -name "*.cpx" \
	-o -name "*_glue.c" -o -name "*_inline.c" \
	-o -name "*.o" -o -name "*.a" \
	-o -name "*.so" -o -name "*.dll" -o -name "*.dylib" \
	-o -name "*.class" \
	\
	-o -name "*.log" -o -name "*.err" \
	-o -name "tmpciao*" \
	\
	-o -name "*_auto.pl" \
	-o -name "*_co.pl" \
	-o -name "*_co.java" \
	\) \
	-exec /bin/rm -f {} \;
}

# Clean a whole builddir
clean_builddir() { # DIR
    local dir=$1
    shift

    # Safety check (clean operations are dangerous)
    if [ x"$dir" = x"" ]; then
	cat >&2 <<EOF
INTERNAL ERROR: missing argument in clean_builddir
EOF
	exit 1
    elif [ -x "$dir" -a '(' ! -r "$dir/bundlereg/ciao.bundlecfg_sh" ')' ]; then
	for i in "$dir/*"; do
	    cat >&2 <<EOF
INTERNAL ERROR: suspicuous directory in clean_builddir:

The directory "$dir" is not empty and does not look like a valid build
directory. For safety, this script is aborted.

If correct, please clean manually the contents of the specified directory.
EOF
	    exit 1
	done
    fi

    rm -rf "$dir"
}

# ---------------------------------------------------------------------------
# Commands that are directly implemented in this script (not in Prolog)
# (as cmd_* functions)

# Direct manipulation of the bootstrap (for developers)
cmd_boot_build() {
    eng_name=$eng_name eng_cfg=`boot_eng_cfg` \
        autoboot_autobuild "$boot_builddir" "$builder_mod"
}
cmd_boot_rebuild() {
    eng_name=$eng_name eng_cfg=`boot_eng_cfg` \
        autoboot_autobuild "$boot_builddir" "$builder_mod"
}
cmd_boot_clean() {
    normal_message "Cleaning the bootstrap"
    clean_builddir "$boot_builddir"
}

# Like 'distclean' but also removes bootstrap.
# (realclean = distclean + boot-clean)
cmd_realclean() {
    # Do a distclean (which removes configuration)
    eng_name=$eng_name eng_cfg=`boot_eng_cfg` \
        autoboot_exec "$boot_builddir" "$builder_mod" distclean
    # Finally, clean the bootstrap
    clean_builddir "$boot_builddir"
}

# Emergency clean
cmd_emergency_clean() {
    local d
    # TODO: Another option is to provide an explicit timestamp for
    #   recompilation in ciaoc.sta for @apl{ciao_builder} (so that it
    #   will always recompile itself).
    cat 1>&2 <<EOF
WARNING: Emergency clean WILL NOT 100% clean your sources!

This is an auxiliary command to recover from inconsistent build
states. If build still does not work, BACKUP all important untracked
files and use 'git clean -d -x ...'.

EOF
    # Cleaning the bootstrap
    clean_builddir "$boot_builddir"

    local rel_builddir="build" # TODO: hardwired here -- duplicated on the Prolog side
    local builddir="$ciaoroot/$rel_builddir"
    # Cleaning the system builddir (includes bundlereg)
    clean_builddir "$builddir"
    # Clean exec_header # TODO: it should be created somewhere else
    rm -f "$ciaoroot"/core/lib/compiler/header
    # Try to clean leftovers (mostly .po and .itf) of any previous
    # compilation in the whole source tree.
    for d in "$ciaoroot/"*; do
	if [ -d "$d" ] && \
	   [ x"`basename "$d"`" != x"third-party" ]; then
	    clean_tree "$d"
	fi
    done
    # Clean CIAOCACHEDIR (if exists)
    if [ ! -z "$CIAOCACHEDIR" ] && [ -x "$CIAOCACHEDIR" ]; then
	clean_tree "$CIAOCACHEDIR"
    fi
}

# ---------------------------------------------------------------------------
# Main

# Parse the first argument as $cmd. It replaces '-' by '_' in the
# name of the command.  Selects 'help' for '-h', '--help', or if no
# command is specified.
cmd="$1"
if [ -z "$cmd" ]; then
    # (no command, use 'help')
    cmd="help"
elif [ x"$cmd" = x"-h" -o \
       x"$cmd" = x"--help" ]; then
    cmd="help"
    shift
else
    shift
fi
cmd=`echo "$cmd" | sed -e s:-:_:g`

# Select $autobuild for the given $cmd
case "$cmd" in
    # Actions for which bootstrap is build automatically
    help|help_all|boot_build|configure|local_install|global_install)
	autobuild=yes
	;;
    # Actions for which bootstrap is rebuild automatically
    boot_rebuild)
	autobuild=rebuild
	;;
    # Actions that assume that the bootstrap is already built
    *)
	autobuild=no
	;;
esac
# Replace help->help_boot
case "$cmd" in
    help|help_all)
	cmd="$cmd"_boot
	;;
esac

# ('crossp' added from "$sh_boot_dir/autoboot.sh")

# Select $ciaoaliaspath for ciao_builder compilation and execution
# (needed for bootstrapping since bundles are not scanned)
cross_ciaoroot=`crossp "$ciaoroot"`
default_ciaoaliaspath="ciaobld=$cross_ciaoroot/builder/src"

# If (re)build is necessary, pre-fetch configuration options for
# bootstrap compilation from the command line arguments.
if [ x"$autobuild" = x"no" ]; then
    true
else
    scan_bootstrap_opts "$@"
fi

# Execute the command
case "$cmd" in
    # Special commands implemented in this script
    boot_build|boot_rebuild|boot_clean|realclean|emergency_clean)
	eval cmd_"$cmd"
	;;
    # Actions implemented in ciao_builder
    *)
	eng_name=$eng_name eng_cfg=`boot_eng_cfg` \
	    autoboot_exec "$boot_builddir" "$builder_mod" "$cmd" "$@"
esac

