#!/bin/sh
# Compile native code (.car archive)

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ===========================================================================

# Get BLD_ENGDIR from from first argument

BLD_ENGDIR=$1; shift; [ -x ""${BLD_ENGDIR}"" ] || exit 1
# Make bld_engdir absolute
BLD_ENGDIR=$(e=${BLD_ENGDIR}/native_modules;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)
ENG_CFG=DEFAULT

bld_engdir=$BLD_ENGDIR
eng_cfg=$ENG_CFG

# ===========================================================================

c_files() {
    echo "engine__configuration.c"
    for m in `cat "$bld_engdir"/native_modules`; do
        if [ x"${m}" = x"engine__main" ]; then
            continue
        fi
        echo "${m}.c"
    done
}

eng_make() {
    local make="make -s"
    # Use gmake if available, otherwise expect make to be gmake
    if command -v gmake > /dev/null 2>&1; then make="gmake -s"; fi
    $make --no-print-directory -j$PROCESSORS \
          -C "$bld_objdir" \
          -f "$_base/../sh_src/engine.mk" \
          "$@" \
          BLD_CDIR="$bld_cdir" \
          BLD_OBJDIR="$bld_objdir" \
          ENG_NAME="$eng_name" \
          ENG_CFG_MK="$bld_cfgdir/config_mk"
}

# ---------------------------------------------------------------------------
# Definition of available options

section_opts="optimlevel debuglevel debug_trace rtchecks profile_insfreq profile_ins2freq profile_blockfreq profile_stats libdir compresslibs build_info with_cc"

defaultval_optimlevel=optimized
message_optimlevel=`cat <<EOF
  --optimlevel=[optimized]        Optimization level 

    optimized       -- Turn on optimization flags
    normal          -- Normal emulator (non-optimized code)

    For normal use, we recommend leaving it as "optimized".  But if
    you suspect that your compiler performs buggy optimizations (which
    should not be the case), turn optimization off.  This can be
    happen more easily in concurrent applicacions: if you write any
    thread-based program and unexpected results appear, try
    recompiling Ciao without optimization options first.
EOF
`

defaultval_debuglevel=nodebug
message_debuglevel=`cat <<EOF
  --debuglevel=[nodebug]          Level of debugging (CC)

    nodebug         -- Do not include debug information or messages
    debug           -- Emulator with C level debugging info available
                       plus extended C compilation warnings
    paranoid-debug  -- Emulator with C level debugging info available
                       plus paranoid C compilation warnings

    Enable debugging options in the C compiler
EOF
`

defaultval_debug_trace=no
message_debug_trace=`cat <<EOF
  --debug-trace=[no]

    yes
    no

    Enable debug trace (controled with runtime options)
EOF
`

defaultval_rtchecks=no
message_rtchecks=`cat <<EOF
  --rtchecks=[no]

    yes
    no

    Enable superfluous runtime checks (preconditions, postconditions,
    etc.) in the C code. A bug-free emulator should not violate any of
    these checks.
EOF
`

defaultval_profile_insfreq=no
message_profile_insfreq=`cat <<EOF
  --profile-insfreq=[no]

    yes
    no

    Profile instruction frequencies (note: execute a wamtime profile to
    ensure that the overhead compensation is correct)
EOF
`

defaultval_profile_ins2freq=no
message_profile_ins2freq=`cat <<EOF
  --profile-ins2freq=[no]

    yes
    no

    Profile instruction pairs frequencies (note: execute a wamtime profile to
    ensure that the overhead compensation is correct) (EXPERIMENTAL)
EOF
`

defaultval_profile_blockfreq=no
message_profile_blockfreq=`cat <<EOF
  --profile-blockfreq=[no]

    yes
    no

    Profile block frequencies (EXPERIMENTAL)
EOF
`

defaultval_profile_stats=no
message_profile_stats=`cat <<EOF
  --profile-stats=[no]

    yes
    no

    Collect statistics about the execution (load time of static bytecode,
    used memory, garbage collections, etc.)
EOF
`

defaultval_libdir=""
message_libdir=`cat <<EOF
  --libdir=DIRECTORY              Default LIBDIR
EOF
`

defaultval_compresslibs=no
message_compresslibs=`cat <<EOF
  --compresslibs=[no]             Compressed bytecode

    yes             -- yes
    no              -- no

    If you want to compile the Ciao libraries with COMPRESSED BYTECODE
    then set the following variable to "yes". Libraries generated this
    way will be smaller at the sake of a slightly slower usage, both
    in their load as when employed to create an executable.
EOF
`

defaultval_build_info=yes
message_build_info=`cat <<EOF
  --build-info=[yes]              Include build info

    yes             -- Include build info (cc version, hostname, date, ...)
    no              -- Do not include build info
EOF
`

defaultval_with_cc=""
message_with_cc=`cat <<EOF
  --with-cc=[]                    Specify a different C compiler
EOF
`

# ---------------------------------------------------------------------------
# Parse command-line options

parse_config_opts() {
    for option in "$@"; do
        case "$option" in
            --*=*)
                name="`echo "$option" | sed -e 's/=.*//;s/--//'`"
                if test -n "`echo $name | sed 's/[a-z0-9-]//g'`"; then
                    echo "{configuration error: invalid option name $name}" 1>&2
                    exit 1
                fi
                name="`echo "$name" | sed -e 's/-/_/g'`" # replace - by _
                value="`echo \"$option\" | sed 's/[^=]*=//'`"
                eval "value_$name='$value'" ;;

            --*)
                name="`echo "$option" | sed -e 's/--//'`"
                if test -n "`echo $name | sed 's/[a-z0-9-]//g'`"; then
                    echo "{configuration error: invalid option name $name}" 1>&2
                    exit 1
                fi
                name="`echo "$name" | sed -e 's/-/_/g'`" # replace - by _
                eval "option_$name=yes" ;;

            *)
                nonoption="$nonoption $option" ;;
        esac
    done

    for s_o in $section_opts; do
        # Set default value of s_o if not set by the user
        eval test -n '"${value_'${s_o}'}"' || \
            eval value_${s_o}='"${defaultval_'${s_o}'}"'
    done
}

emit_configuration_c() {
    DEFAULT_LIBDIR="$value_libdir"
    # Build info
    INCLUDE_BUILD_INFO="$value_build_info"
    VERSION_DIR="$bld_engdir"/version
    VERSIONTAG="`cat ${VERSION_DIR}/GlobalVersion` [optim_comp]"
    if test x"${INCLUDE_BUILD_INFO}" = x"yes"; then
        BUILD_DATE=": `LC_ALL=C LANG=C date`"
        # TODO: include revision info (merge with trunk/)
        #    COMMIT_ID="`cat ${VERSION_DIR}/svnrev`"
        #    if test x"${COMMIT_ID}" = x""; then
        #       COMMIT_ID_INFO=""
        #    else
        #       COMMIT_ID_INFO=" (r${COMMIT_ID})"
        #    fi
        #    BUILD_INFO="Ciao ${VERSIONTAG}${COMMIT_ID_INFO}${BUILD_DATE}\n"
        BUILD_INFO="Ciao ${VERSIONTAG}${BUILD_DATE}\n"
    else
        BUILD_INFO="Ciao ${VERSIONTAG}\n"
    fi
    cat <<EOF
char *emulator_version = "${BUILD_INFO}";
char *emulator_architecture = "${CIAOARCH}";
char *emulator_os = "${CIAOOS}";
char *emulator__so_cc = "${CC}";
char *emulator__so_ld = "${LD}";
char *emulator__so_cc_opts = "${CFLAGS} ${CCSHARED}";
char *emulator__so_ld_opts = "${LDSHARED}";
char *emulator__so_libs = "";
char *ciao_versiontag = "${VERSIONTAG}";
char *installibdir = "${DEFAULT_LIBDIR}";
EOF
}
emit_define() {
    if test x"$1" = x"yes"; then
        echo "#define $2 1"
    fi
}
emit_configuration_h() {
    cat <<EOF
#if !defined(__CONFIGURATION_H__)
#define __CONFIGURATION_H__
`emit_define "${value_rtchecks}" "USE_RTCHECKS"`
`emit_define "${value_use_threads}" "USE_THREADS"`
`emit_define "${value_debug_trace}" "DEBUG_TRACE"`
`emit_define "${value_profile_insfreq}" "PROFILE_INSFREQ"`
`emit_define "${value_profile_ins2freq}" "PROFILE_INS2FREQ"`
`emit_define "${value_profile_blockfreq}" "PROFILE_BLOCKFREQ"`
`emit_define "${value_profile_stats}" "PROFILE_STATS"`
`${CONFIGURE}`
#endif /* __CONFIGURATION_H__ */
EOF
}

# ===========================================================================

eng_name=arch # TODO: see ensure_eng_config_loaded

# bld_hdir="$bld_engdir/include"
# bld_cdir="$bld_engdir/src"
bld_hdir="$bld_engdir/c"
bld_cdir="$bld_engdir/c/engine"
# bld_objdir="$bld_engdir/o"
bld_cfgdir="$bld_engdir/cfg/$eng_cfg"
bld_objdir="$bld_engdir/objs/$eng_cfg"

H_PATH="-I$bld_hdir -I$bld_cfgdir"
COMPILER_VERSION=`cat "$bld_engdir"/compiler_version`
REQUIRE64=`cat "$bld_engdir"/require64`

old_dir=`pwd`; cd "$_base/../.."; ciaoroot=`pwd`; cd "$old_dir"; old_dir=
sh_src_dir="$ciaoroot/builder/sh_src"

# TODO: add H_PATH
# TODO: use CIAOOPTS (for experiments)

eng_build() { # (configure options)
    mkdir -p "$bld_cfgdir"
    mkdir -p "$bld_objdir"

    # TODO: see scan_bootstrap_opts.sh
    boot__EXTRA_CFLAGS="-Werror -DWITH_COMPILER_VERSION=$COMPILER_VERSION $H_PATH"
    boot__EXTRA_LDFLAGS=
    boot__OS=`"$sh_src_dir"/config-sysdep/ciao_sysconf --os`
    boot__ARCH=`"$sh_src_dir"/config-sysdep/ciao_sysconf --arch`
    if test x"${REQUIRE64}" = x"yes"; then
        # TODO: see eng_maker:arch64/2
        # check that we are compiling for a 64 bit architecture
        case $boot__ARCH in
            Sparc64) true ;;
            x86_64)  true ;;
            ppc64)   true ;;
            *)       echo "{configuration error: This executable requires a 64 bit architecture}" 1>&2 && exit 1 ;;
        esac
    else
        # TODO: see eng_maker:arch64/2
        # set 32 bit compatibility mode for 64 bit architectures,
        # do nothing for 32 bit architectures
        case $boot__ARCH in
            Sparc64) boot__ARCH=Sparc ;;
            x86_64)  boot__ARCH=i686 ;;
            ppc64)   boot__ARCH=ppc ;;
        esac
    fi

    value_use_threads=yes
    parse_config_opts ${CIAOOPTS} "$@"

    # TODO: hack for compatibility (do it properly)
    cat > "$bld_hdir/ciao_prolog.h" <<EOF
#include <engine/engine__ciao_prolog.h>
EOF
    mkdir -p "$bld_hdir/ciao"
    cat > "$bld_hdir/ciao/datadefs.h" <<EOF
#include <engine/basiccontrol.native.h>
EOF
    cat > "$bld_hdir/ciao/support_macros.h" <<EOF
#include <engine/basiccontrol.native.h>
EOF

    # "$builddir/bundlereg/core.bundlecfg_sh"
    cat > "$bld_cfgdir/core.bundlecfg_sh" <<EOF
core__USE_THREADS=$value_use_threads
core__AND_PARALLEL_EXECUTION=no
core__PAR_BACK=no
core__TABLED_EXECUTION=no
core__OPTIM_LEVEL=$value_optimlevel
core__DEBUG_LEVEL=$value_debuglevel
#
core__ARCH=$boot__ARCH
core__OS=$boot__OS
core__CUSTOM_CC=$value_with_cc
core__CUSTOM_LD=$value_with_cc
core__EXTRA_CFLAGS="$boot__EXTRA_CFLAGS"
core__EXTRA_LDFLAGS="$boot__EXTRA_LDFLAGS"
EOF

    # Create a predefined 'meta_sh'
    cat > "$bld_cfgdir/meta_sh" <<EOF
eng_name="$eng_name"
eng_h_alias="ciao"
eng_srcpath="$ciaoroot/core/engine"
eng_use_stat_libs=no
eng_default_ciaoroot="$ciaoroot"
eng_addobj=
eng_addcfg=
eng_core_config="$bld_cfgdir/core.bundlecfg_sh"
EOF
    # Do sysdep configuration
    # (generate 'config_mk' and 'config_sh')
    "$sh_src_dir"/config-sysdep/config-sysdep.sh "$bld_engdir" "$eng_cfg"
    #
    CONFIGURE_DIR="${_base}"/configure

    . "$bld_cfgdir"/config_sh
    # Compile configure exec
    CONFIGURE="$bld_cfgdir/configure"
    ${CC} ${CFLAGS} ${LDFLAGS} -o ${CONFIGURE} \
          ${CONFIGURE_DIR}/configure.c \
          ${CONFIGURE_DIR}/engine__own_mmap.c
    mkdir -p "$bld_cfgdir" || exit -1
    mkdir -p "$bld_cfgdir"/engine
    emit_configuration_c > "$bld_cfgdir/engine/engine__configuration.c"
    emit_configuration_h > "$bld_cfgdir/engine/engine__configuration.h"
    rm ${CONFIGURE} # clean configure exec

    cp "$bld_cfgdir/engine/engine__configuration.c" "$bld_cdir/engine__configuration.c" # TODO: hardwired
    #
    CFILES=`c_files`
    cat > "$bld_cdir/eng_info_mk" <<EOF
ENG_STUBMAIN = engine__main.c
ENG_CFILES = $(echo $CFILES)
ENG_HFILES=
ENG_HFILES_NOALIAS=
EOF
    #
    rm -f "$bld_engdir/$eng_name"
    eng_make engexec
    ( cd "$bld_engdir" ; ln -s objs/"$eng_cfg"/"$eng_name" "$eng_name" )
    #
    eng_header
}

# ---------------------------------------------------------------------------

eng_header() {
    cat > "$bld_engdir"/run <<EOF
#!/bin/sh
# Run executable (.car archive)

# Physical directory where the script is located
_base=\$(e=\$0;while test -L "\$e";do d=\$(dirname "\$e");e=\$(readlink "\$e");\\
        cd "\$d";done;cd "\$(dirname "\$e")";pwd -P)
# Make sure that CIAOROOT and CIAOCACHE are defined
r=\${CIAOROOT:-"$CIAOROOT"}
c="\$r/build/oc-cache"
c=\${CIAOCACHE:-\$c}
CIAOROOT="\$r" CIAOCACHE="\$c" CIAOCCONFIG=\${_base}/cfg/DEFAULT \${_base}/arch "\$@" -C \${_base}/noarch \${CIAORTOPTS}
EOF
    chmod a+x "$bld_engdir"/run
}

# ---------------------------------------------------------------------------

eng_build "$@"
[ -x "$bld_engdir/$eng_name" ] || { \
  echo "{Compilation of $bld_engdir failed}" 1>&2; \
  exit 1; \
}
