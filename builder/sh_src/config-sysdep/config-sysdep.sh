#!/bin/sh
#
#  config-sysdep.sh
#
#  Generation of platform specific settings (compiler flags, C
#  libraries, etc.) from the selection of 'ciao configure'.
#
#  Copyright (C) 2015 Ciao Developer team
#
# ---------------------------------------------------------------------------
#
# Input (arguments):
#   build_engdir: directory of engine build
#   eng_cfg:      engine configuration name
# Input (other):
#   $build_engdir/cfg/$eng_cfg/meta_sh
#
# Output: $build_engdir/cfg/$eng_cfg/config_sh
#         $build_engdir/cfg/$eng_cfg/config_mk

# TODO: Merge with core_OC/engine/internals.cfg/

# ----------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ----------------------------------------------------------------------------

if [ $# -ne 2 ]; then
    echo "ERROR: Specify $build_engdir and $eng_cfg as arguments" >& 2
    exit 1
fi
build_engdir=$1
eng_cfg=$2

bld_cfgdir="$build_engdir/cfg/$eng_cfg"
bld_hdir="$build_engdir/include"
bld_objdir="$build_engdir/objs/$eng_cfg"

# Load meta_sh (defines variables: eng_name, eng_ciao_config, etc.)
. "$bld_cfgdir/meta_sh"

# Load configuration parameters
. "$eng_ciao_config"
CIAOOS=$core__OS
CIAOARCH=$core__ARCH

# Override core__OPTIM_LEVEL if needed
# TODO: If really needed, move to the Prolog part?
case "$core__DEBUG_LEVEL" in
    debug)
	core__OPTIM_LEVEL=normal # TODO: why?
	;;
    profile)
	core__OPTIM_LEVEL=debug # TODO: why?
	;;
    profile-debug)
	core__OPTIM_LEVEL=debug # TODO: why?
	;;
esac

# ===========================================================================
# Basic platform-dependent configuration options (architecture, OS,
# compiler, C flags for threads, sockets, etc.)

. "$_base"/cc_test_option

# Set CC (C compiler) and LD (linker)
if [ x"$core__CUSTOM_CC" != x"" ]; then
    # Use custom values for CC and LD
    CC="$core__CUSTOM_CC"
    if [ x"$core__CUSTOM_LD" != x"" ]; then
	LD="$core__CUSTOM_LD"
    else
	LD="$core__CUSTOM_CC"
    fi
elif [ x"$core__CUSTOM_LD" != x"" ]; then
    echo "Custom LD not valid without a custom CC" 1>&2
    exit 1
else
    # Detect based on OS and architecture
    case "$CIAOOS$CIAOARCH" in
	Solaris*)    CC=gcc; LD=ld ;;
	LINUXarmv5tel)  CC=arm-linux-gcc; LD=arm-linux-gcc ;; # TODO: Recover cross compilation
#	crossWin32i686)  CC=i386-mingw32-gcc; LD=i386-mingw32-gcc ;; # TODO: Recover cross compilation
	DARWIN*)        CC=clang; LD=clang ;;
	*)
	# The rest of the systems just use plain 'gcc'
	    CC=gcc; LD=gcc ;;
    esac
fi

if ! which "$CC" > /dev/null; then
    echo "\`$CC' does not seem a valid C compiler" 1>&2
    exit 1
fi
if ! which "$LD" > /dev/null; then
    echo "\`$LD' does not seem a valid C linker" 1>&2
    exit 1
fi

# Does this machine has dynamic linking abilities?
case "$CIAOOS$CIAOARCH" in
    *) FOREIGN_FILES_FLAG="-DFOREIGN_FILES" ;;
esac

# Libraries and flags to use threads
LD_THREAD_LIB=
THREAD_FLAG=
if test x"$core__USE_THREADS" = x"yes"; then
    case "$CIAOOS$CIAOARCH" in
	SolarisSparc*)
	    case "`/bin/uname -r`" in
		5.[123456]* ) SOLARIS_VERSION=pre_7 ;;
		5.* )         SOLARIS_VERSION=equal_post_7 ;;
	    esac
	    THREAD_FLAG="-D_REENTRANT -DTHREADS"
	    if test x"$SOLARIS_VERSION" = x"pre_7"; then
		LD_THREAD_LIB="-lpthread"
	    else
		LD_THREAD_LIB="-lpthread -lrt"
	    fi
	    ;;
	Solaris*)      LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	BSD*)          LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	DARWIN*)       THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUX*)        LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
        # Threads and locks in Win32: no threads, no locks so far.
	Win32*) THREAD_FLAG="-DTHREADS" ;;
    esac
fi

# C compiler options for generating shared libraries code
# Linker options for shared objects
case "$CIAOOS$CIAOARCH" in
    BSD*)        CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    DARWINppc)   CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -dynamiclib -undefined suppress" ;;
    DARWIN*)     CCSHARED="-fPIC" ; LDSHARED="-dynamiclib -undefined dynamic_lookup" ;;
    LINUX*)      CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    Solaris*)    CCSHARED= ; LDSHARED="-G" ;;
    # note: -fPIC and -rdynamic are unused in Windows
    Win32*)      CCSHARED= ; LDSHARED="-shared" ;;
esac
case "$CIAOOS$CIAOARCH" in
    *m32) CCSHARED="-m32 $CCSHARED"; LDSHARED="-m32 $LDSHARED" ;;
esac

# Extension of shared libraries
case "$CIAOOS$CIAOARCH" in
    DARWIN*) 	    SOSUFFIX=".dylib" ;;
    Win32*)         SOSUFFIX=".dll" ;;
    # Assume .so for anything else
    *)              SOSUFFIX=".so" ;;
esac

# Debugging and profiling
# Note: core__OPTIM_LEVEL is switched to 'debug' in profiling mode
# TODO: define different levels of debug/profile (wam level, C level, etc.)

DEBUG_FLAGS=
# Raise warning levels for questionable C code
case "$core__DEBUG_LEVEL" in
    paranoid-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -Wall -W -Wtraditional -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline"
	;;
    debug|profile-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -Wall"
	;;
    *) ;;
esac
# C level debugging information
case "$core__DEBUG_LEVEL" in
    paranoid-debug|debug|profile-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -g"
	;;
    *) ;;
esac
# Instrument for gprof profiling
case "$core__DEBUG_LEVEL" in
    profile|profile-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -pg"
	PROFILE_LD_FLAGS="-pg"
	;;
    *) ;;
esac
# WAM level debugging
case "$core__DEBUG_LEVEL" in
    paranoid-debug|debug|profile-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -DDEBUG"
	;;
    *) ;;
esac
# WAM level profiling
case "$core__DEBUG_LEVEL" in
    profile|profile-debug)
	DEBUG_FLAGS="$DEBUG_FLAGS -DPROFILE"
	;;
    *) ;;
esac

# C optimization flags

if cc_test_option -falign-functions=2; then
    ALIGN_FLAGS="-falign-loops=2 -falign-jumps=2 -falign-functions=2"
elif cc_test_option -malign-functions=2; then
    # Version =< 3.1 of gcc only admits the -m form
    ALIGN_FLAGS="-malign-loops=2 -malign-jumps=2 -malign-functions=2"
else
    # Other compilers like clang does not admit this flag
    ALIGN_FLAGS=""
fi
# TODO: Review ALIGN_FLAGS in those OS and architectures
case "$CIAOOS$CIAOARCH" in
    Solarisi686) ALIGN_FLAGS="" ;;
    *alpha)      ALIGN_FLAGS="" ;;
    *Sparc)      ALIGN_FLAGS="" ;;
    *Sparc64)    ALIGN_FLAGS="" ;;
    *Sparc64m32) ALIGN_FLAGS="" ;;
    *armv4l)     ALIGN_FLAGS="" ;;
    *armv5tel)   ALIGN_FLAGS="" ;;
esac

# Code generation options
OPTIM_FLAGS0="$ALIGN_FLAGS"
case "$CIAOOS$CIAOARCH" in
    # We are not using strict-aliasing to avoid problems in Solaris # TODO: wrong?
    Solarisi686)    OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    SolarisSparc64) OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    SolarisSparc64m32) OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    LINUXalpha)     OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    #
    *i686)        OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *x86_64)      OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *x86_64m32)   OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *ppc)       OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *ppc64)     OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *ppc64m32)  OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *Sparc)     OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *Sparc64)   OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *Sparc64m32) OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *arm)       OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *armv4l)    OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *armv5tel)  OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
esac
if test x"$core__OPTIM_LEVEL" = x"optimized"; then
    OPTIM_FLAGS="-fno-strict-aliasing -O2 $OPTIM_FLAGS0"
else
    # TODO: Why not "-O2" as in OptimComp? which one is wrong?
    OPTIM_FLAGS=""
fi
# Special cases where -fPIC is needed to allow compilation as shared library
case "$CIAOOS$CIAOARCH" in
    LINUXx86_64) OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
esac
# Select C standard
OPTIM_FLAGS="-Wall -std=gnu11 $OPTIM_FLAGS"

# Memory management primitives (own_malloc)
# See engine/configure.c: USE_MMAP includes USE_OWN_MALLOC
MEM_MNG_FLAGS=""
case "$CIAOOS$CIAOARCH" in
    # TODO: why?
    LINUXSparc)      MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    LINUXSparc64)    MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    LINUXSparc64m32) MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    # Do not use USE_MMAP for own_malloc in 64-bit architectures
    *Sparc64)        MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    *x86_64)         MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    *ppc64)          MEM_MNG_FLAG="-DUSE_OWN_MALLOC" ;;
    # (assume 32-bit)
    *) MEM_MNG_FLAG="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
esac

# Architecture options
# TODO: rename to ARCH_FLAGS like in OptimComp?
ARCHFLAGS=
LDARCHFLAGS=
case "$CIAOOS$CIAOARCH" in
    *m32) ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
esac
case "$CIAOOS$CIAOARCH" in
    # TODO: WHY??
    Solarisi686) ARCHFLAGS="-fPIC $ARCHFLAGS" ;;
    *) ;;
esac

# Linker specific options
# Note: LINUX linker specific options
#   "-rdynamic" allows the dynamic libraries symbols to be resolved
#   against
LDFLAGS0=
case "$CIAOOS$CIAOARCH" in
    BSD*)          LDFLAGS0="-rdynamic" ;;
    LINUX*)        LDFLAGS0="-rdynamic" ;;
    *) ;;
esac
# 
# Libs for sockets
SOCKETS_FLAG=
LD_SOCKETS_LIB=
case "$CIAOOS$CIAOARCH" in
    Win32*)
	case "`uname -s`" in
	    MINGW*)
		LD_SOCKETS_LIB="-lws2_32" ; 
		SOCKETS_FLAG="-D__USE_W32_SOCKETS" ; # needed for windows.h
		;;
	esac
	;;
esac

STAT_SOCKETS_LIB=
case "$CIAOOS$CIAOARCH" in
    Solaris*)     STAT_SOCKETS_LIB="-lsocket" ;;
    *) ;;
esac

# Other libs
case "$CIAOOS$CIAOARCH" in
    BSD*)         LIBS0="-lm" ;;
    Win32*)
	case "`uname -s`" in
	    MINGW*) LIBS0="-lws2_32" ;;
	    *) LIBS0= ;;
	esac
	;;
    # LIBS=-ldl
    DARWIN*)      LIBS0= ;;
    Solaris*)     LIBS0="-ldl -lm -lnsl" ;;
    *) LIBS0="-ldl -lm" ;;
esac

# ===========================================================================

# TODO: Recover? (add NOCONSOLEFLAG to LDFLAGS)

# # Hack to not show a default console in Windows (used by the engine
# # Makefile when compiling a static engine)
# case "$CIAOOS" in
#     Win32)
#         #NOCONSOLEFLAG=-mwindows
#         # Hack not to use Cygwin's but Microsoft libraries
# 	NOCONSOLEFLAG="-mno-cygwin" ;;
#     *) true
# esac

# In Windows we need an executable built without console support 
# ifeq ($CIAOOS,Win32)
# 	cd $bld_objdir &&	do_gmake clean
# 	do_gmake CONSOLEFLAG="$NOCONSOLEFLAG" dostateng
# 	/bin/mv $bld_objdir/$ENGINAME.sta $bld_objdir/$ENGINAME_nc.sta
# 	cd $bld_objdir &&	do_gmake clean
# endif

# ---------------------------------------------------------------------------

EXECSUFFIX=
case "$CIAOOS" in
    Win32) EXECSUFFIX=".exe" ;;
esac

# ---------------------------------------------------------------------------

CFLAGS="$OPTIM_FLAGS $DEBUG_FLAGS $THREAD_FLAG $FOREIGN_FILES_FLAG $SOCKETS_FLAG -D$CIAOOS -D$CIAOARCH $ARCHFLAGS $MEM_MNG_FLAG"
LDFLAGS="$LDARCHFLAGS $LDFLAGS0 $LDRPATH $PROFILE_LD_FLAGS"
LIBS="$LIBS0 $LD_THREAD_LIB $LD_SOCKETS_LIB $DEBUG_LIBS"
STAT_LIBS="$STAT_SOCKETS_LIB"

# Number of processors: use the real number
PROCESSORS=`"$_base"/available_processors $CIAOOS$CIAOARCH`

# ===========================================================================

# Add extra CFLAGS and LDFLAGS
CFLAGS="$CFLAGS $core__EXTRA_CFLAGS"
LDFLAGS="$LDFLAGS $core__EXTRA_LDFLAGS"
CCSHARED="$CCSHARED $core__EXTRA_CFLAGS"
LDSHARED="$LDSHARED $core__EXTRA_LDFLAGS"

# ===========================================================================
# [Extend basic configuration with sub-targets]
# TODO: Make it simpler and generalize to other libs.

gsl_cfg="$CIAOOS$CIAOARCH" # TODO: select as part of configuration (may not be the same as $eng_cfg due to debug/profile builds of different components)
config_gsl_file="$gsl_engdir/cfg/$gsl_cfg/config_sh"
if test -r "$config_gsl_file"; then
    . "$config_gsl_file"
    STAT_LIBS="$STAT_LIBS $GSL_STAT_LIBS"
fi

# ===========================================================================
# [Export some config flags to C preprocessor defs]
# TODO: Do it automatically?

if test x"$core__AND_PARALLEL_EXECUTION" = x"yes"; then
    ANDPARALLEL="-DANDPARALLEL"
elif test x"$core__AND_PARALLEL_EXECUTION" = x"visandor"; then
    ANDPARALLEL="-DANDPARALLEL -DVISANDOR"
elif test x"$core__AND_PARALLEL_EXECUTION" = x"no"; then
    ANDPARALLEL=""
fi

if test x"$core__PAR_BACK" = x"yes"; then
    PARBACK="-DPARBACK"
fi

if test x"$core__TABLED_EXECUTION" = x"yes"; then
    TABLING="-DTABLING"
fi

# Add CFLAGS for bundle subtargets
CFLAGS="$CFLAGS $ANDPARALLEL $PARBACK $TABLING"

# ===========================================================================

# Create an engine executable stub (using ENG_STUBMAIN) that links
# statically or dynamically against the engine code.
#
# TODO: It should be configurable by the user
case "$CIAOOS" in
    Win32) ENG_STUBMAIN_DYNAMIC=1 ;;
    *)     ENG_STUBMAIN_DYNAMIC=0 ;;
esac
#ENG_STUBMAIN_DYNAMIC=1

# ===========================================================================

# Add engine include dir to search path for build
CFLAGS="-I$bld_hdir $CFLAGS"

# ===========================================================================

# Flags for using the engine as a library (LIBENG_CFLAGS, LIBENG_LDFLAGS)
LIBENG_CFLAGS="$CFLAGS"
LIBENG_LDFLAGS="$LDFLAGS"

# TODO: This hardwires a build path; use patchelf (Linux) or
#   install_name_tool (Mac OS X) to fix during installation
LIBENG_RPATH="$bld_objdir"

if [ x"$LIBENG_RPATH" != x ]; then
    LIBENG_LDFLAGS="$LIBENG_LDFLAGS -Wl,-rpath -Wl,$LIBENG_RPATH"
fi

# ===========================================================================

# The first argument defines the string quoting character for the
# specified output kind. That ensures that values consisting on
# several words are correctly assigned (i.e., V="foo bar" or V=foo
# bar).

# Settings for C compiler (used in engine/Makefile)
dump_config_ccomp() { # $1 is the quotation char
    local q=$1
    cat <<EOF
# Exec and shared lib extensions
EXECSUFFIX=$q$EXECSUFFIX$q
SOSUFFIX=$q$SOSUFFIX$q
# C compiler, linker, and flags
CC=$q$CC$q
LD=$q$LD$q
CCSHARED=$q$CCSHARED$q
LDSHARED=$q$LDSHARED$q
CFLAGS=$q$CFLAGS$q
LDFLAGS=$q$LDFLAGS$q
LIBENG_CFLAGS=$q$LIBENG_CFLAGS$q
LIBENG_LDFLAGS=$q$LIBENG_LDFLAGS$q
ENG_STUBMAIN_DYNAMIC=$q$ENG_STUBMAIN_DYNAMIC$q
EOF
}

# All platform settings (used in build_engine.sh)
dump_config_platform() { # $1 is the quotation char
    local q=$1
    cat <<EOF
# Platform
CIAOOS=$q$CIAOOS$q
CIAOARCH=$q$CIAOARCH$q
PROCESSORS=$q$PROCESSORS$q
# C libraries
STAT_LIBS=$q$STAT_LIBS$q
LIBS=$q$LIBS$q
#
CIAODEBUG=$q$CIAODEBUG$q
EOF
    dump_config_ccomp "$q"
}

mkdir -p "$bld_cfgdir"
dump_config_platform "'" > "$bld_cfgdir/config_sh"
dump_config_ccomp "" > "$bld_cfgdir/config_mk"

