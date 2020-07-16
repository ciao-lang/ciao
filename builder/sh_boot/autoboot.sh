#
#  autoboot.sh
#
#  Automatic build of bootstrappable executables
#
#  Copyright (C) 2015-2017 Jose F. Morales, Ciao Developer team
#
# ===========================================================================
#
# Usage: include this code in your script ('.' or 'source')
#
# Required input variables:
#   $sh_src_dir: directory for other builder scripts
#   $ciaoroot: environment variable pointing to the root of Ciao sources
#
#   $target_name: target name for cmd_message
#
#   $eng_name: engine name
#   $eng_cfg: engine configuration name (e.g., platform+debug/release)
#   $boot_ciaoc: location of static ciaoc.sta for bootstrap
#
#   + output of scan_bootstrap_opts.sh (boot__*, etc.)
#
# Required functions:
#   no_bootstrap_error: function invoked when bootstrap is missing
#   failed_bootstrap_error: invoked when bootstrap is could not be built 
#
# Exports:
#   autoboot_autobuild
#   autoboot_build
#   autoboot_exec
# ---------------------------------------------------------------------------

boot_cmd_message() {
    echo "=> $target_name: $*" 1>&2
}
boot_dmc_message() {
    true
}
boot_normal_message() {
    echo "   $*" 1>&2
}

# ---------------------------------------------------------------------------

build_car() { # [ARGS]
    ENG_CFG="$eng_cfg" "$sh_src_dir"/build_car.sh "$@"
}

# Execute $engdir+$cpxexec for boot
run_exec_boot() { # engdir cpxexec [ARGS] (and env)
    local engdir="$1"
    local cpxexec="$2"
    shift
    shift
    build_car run_exec_boot "$engdir" "$@" -C -b "$cpxexec" $engopts
}

# ---------------------------------------------------------------------------

# Location of (static) executable for 'mainmod' in 'builddir'
get_mainexec() { # builddir mainmod
    echo "$1/bin/`basename "$2"`.sta"
}

# Location of engdir for eng_name
get_mainengdir() { # builddir
    # (requires: $eng_name)
    local builddir="$1"
    shift
    if [ x"$eng_name" = x"" ] ; then
        cat >&2 <<EOF
INTERNAL_ERROR: missing eng_name in get_mainengdir
EOF
        exit 1
    fi
    printf "$builddir/eng/$eng_name"
}

# Build or check if 'mainmod' is already compiled, and depending on
# the value of $autobuild:
#
#  - $autobuild==no:      prompt an error if it does not exists
#  - $autobuild==yes:     build if it does not exists
#  - $autobuild==rebuild: rebuild (incrementally), even if it exists
#
autoboot_autobuild() { # builddir mainmod (requires: $autobuild)
    local mainexec=`get_mainexec "$1" "$2"`
    mainengdir=`get_mainengdir "$1"`

    if [ ! x"$autobuild" = x"rebuild" ] && \
       [ -f "$mainexec" ] && \
       build_car exists_exec "$mainengdir"; then
        return # Bootstrap is ready
    fi

    if [ x"$autobuild" = x"no" ]; then
        # TODO: detect (mark) in builddir if bootstrap
        #   compilation failed or it is just missing
        no_bootstrap_error
        exit 1
    fi
    # $autobuild==yes or $autobuild==rebuild

    if autoboot_build "$1" "$2"; then
        true
    else
        failed_bootstrap_error
    fi
}

# Build 'mainmod'
autoboot_build() { # builddir mainmod
    local builddir="$1"
    local mainexec=`get_mainexec "$1" "$2"`
    local mainname=`basename "$2"`
    boot_cmd_message "building [boot]"
#    boot_normal_message "compiling native code for `basename "$boot_ciaoc"` $boot__OS$boot__ARCH (engine)"
    mainengdir=`get_mainengdir "$builddir"`
    boot_normal_message "compiling $eng_name (engine for `basename $boot_ciaoc`)"
    build_car clone_boot "`dirname "$boot_ciaoc"`" "$mainengdir"
    build_car prebuild_boot_version_info "$mainengdir"
    build_car clean_config "$mainengdir" # force reconfigure
    if build_car build "$mainengdir" \
              --optim-level="$boot__OPTIM_LEVEL" \
              --debug-level="$boot__DEBUG_LEVEL" \
              --os="$boot__OS" \
              --arch="$boot__ARCH" \
              --m32="$boot__M32" \
              --m64="$boot__M64" \
              --custom-cc="$boot__CUSTOM_CC" \
              --custom-ld="$boot__CUSTOM_LD" \
              --extra-cflags="$boot__EXTRA_CFLAGS" \
              --extra-ldflags="$boot__EXTRA_LDFLAGS"; then
        true
    else
        return 1
    fi
    # Backup exec_header, create a dummy one
    # TODO: an option in ciaoc to skip exec_header instead
    local header="$ciaoroot/core/lib/compiler/header"
    if [ -f "$header" ]; then
        mv "$header" "$header".tmp
    fi
    touch "$header"
    #
    boot_normal_message "compiling $mainname (command) [using `basename "$boot_ciaoc"`]"
    #
    # Build a static executable using the bootstrap 'ciaoc'.
    # It must be static to avoid bootstrapping problems (the compiler
    # compiling its own libraries).
    #
    # TODO: CIAOCCACHE may allow non-static exec here
    #
    mkdir -p "$builddir/bin"
    mkdir -p "$builddir/cache" # (for out-of-tree builds)
    run_exec_boot "$mainengdir" "$boot_ciaoc" $boot__CIAOC_OPTS -s -x -o "$mainexec" "$2"
    # Restore previous exec_header (if it existed)
    rm "$header" # Delete dummy header
    if [ -f "$header".tmp ]; then
        mv "$header".tmp "$header"
    fi
    #
    # NOTE: We assume that the builddir for 'ciaoc.sta' (which
    #   contains the engine) is the same than the builddir for this
    #   executable (we share the engine). Sometimes we may need a
    #   different engine.
    #
    boot_normal_message "reusing $eng_name (engine for $mainname)"
    #
    boot_dmc_message "built [boot]"
}

# Do autobuild and execute the compiled 'mainmod'
autoboot_exec() { # builddir mainmod [ARGS]
    local builddir="$1"
    local mainmod="$2"
    local mainexec=`get_mainexec "$1" "$2"`
    shift
    shift
    autoboot_autobuild "$builddir" "$mainmod"
    mainengdir=`get_mainengdir "$builddir"`
    run_exec_boot "$mainengdir" "$mainexec" "$@"
}

