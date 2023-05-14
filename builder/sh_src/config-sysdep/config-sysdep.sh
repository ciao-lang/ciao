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
#   cardir:  path of .car file
#   eng_cfg: configuration name
# Input (other):
#   $cardir/cfg/$eng_cfg/meta_sh
#
# Output: $cardir/cfg/$eng_cfg/config_sh
#         $cardir/cfg/$eng_cfg/config_mk

# ----------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ----------------------------------------------------------------------------

if [ $# -ne 2 ]; then
    echo "ERROR: Specify cardir and eng_cfg as arguments" >& 2
    exit 1
fi
cardir=$1
eng_cfg=$2

bld_cfgdir="$cardir/cfg/$eng_cfg"
bld_srcdir="$cardir/src"
bld_hdir="$cardir/include"
bld_objdir="$cardir/objs/$eng_cfg"

# Load meta_sh (defines variables: eng_name, eng_core_config, etc.)
. "$bld_cfgdir/meta_sh"

# Load configuration parameters
. "$eng_core_config"

# Select target architecture
case "$eng_cross_os$eng_cross_arch" in
    EMSCRIPTENwasm32)
        CIAOOS=EMSCRIPTEN
        CIAOARCH=wasm32 # uname() in Emscripten
        ;;
    *)
        CIAOOS=$core__OS
        CIAOARCH=$core__ARCH
        ;;
esac

# Number of processors (for compilation): use the real number for the
# current OS and ARCH
PROCESSORS=`"$_base"/available_processors $core__OS$core__ARCH`

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

# Check if CC admits the given option (compiling a dummy file)
cc_test_option() { # Options
    local temp_base temp_c temp_exec temp_err ret
    if [ ! -z $CC ] && type $CC > /dev/null ; then 
	temp_base=${TMPDIR:-/tmp}/cc_option_test_$$
	temp_c=$temp_base.c
	temp_exec=$temp_base.exe
	temp_err=$temp_base.err
	echo 'int main() { return 0; }' > "$temp_c"
	"$CC" "$@" "$temp_c" -o "$temp_exec" 2> "$temp_err"
	if test -s "$temp_err" ; then
	    ret=1 # false
	else
	    ret=0 # true
	fi
        # Get rid of the now unneeded intermediate files
	rm -f "$temp_c" "$temp_exec" "$temp_err"
    else
	ret=1
    fi
    return $ret
}

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
        Solaris*)         CC=gcc; LD=ld ;;
        LINUXarmv5tel)    CC=arm-linux-gcc; LD=arm-linux-gcc ;; # TODO: Recover cross compilation
#       crossWin32i686)   CC=i386-mingw32-gcc; LD=i386-mingw32-gcc ;; # TODO: Recover cross compilation
        DARWIN*)          CC=clang; LD=clang ;;
        EMSCRIPTENwasm32) CC=emcc; LD=emcc ;; # Emscripten
        *)
            # The rest of the systems just use plain 'gcc'
            CC=gcc; LD=gcc ;;
    esac
fi

if ! command -v "$CC" > /dev/null 2>&1; then
    echo "\`$CC' does not seem a valid C compiler" 1>&2
    exit 1
fi
if ! command -v "$LD" > /dev/null 2>&1; then
    echo "\`$LD' does not seem a valid C linker" 1>&2
    exit 1
fi

# TODO: _REENTRANT is obsolete, remove?

# Override core__USE_THREADS if needed
case "$CIAOOS$CIAOARCH" in
    EMSCRIPTENwasm32) core__USE_THREADS=no ;;
esac

# Libraries and flags to use threads
LD_THREAD_LIB=
THREAD_FLAGS=
if test x"$core__USE_THREADS" = x"yes"; then
    THREAD_FLAGS="-D_REENTRANT"
    case "$CIAOOS$CIAOARCH" in
        SolarisSparc*)
            case "`/bin/uname -r`" in
                5.[123456]* ) SOLARIS_VERSION=pre_7 ;;
                5.* )         SOLARIS_VERSION=equal_post_7 ;;
            esac
            if test x"$SOLARIS_VERSION" = x"pre_7"; then
                LD_THREAD_LIB="-lpthread"
            else
                LD_THREAD_LIB="-lpthread -lrt"
            fi
            ;;
        Solaris*) LD_THREAD_LIB="-lpthread" ;;
        BSD*)     LD_THREAD_LIB="-lpthread" ;;
        LINUX*)   LD_THREAD_LIB="-lpthread" ;;
        EMSCRIPTEN*) LD_THREAD_LIB="-lpthread" ;;
    esac
fi

# Architecture options
# TODO: use host arch?
ARCHFLAGS=
LDARCHFLAGS=
case "$CIAOARCH" in
    i686)   ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
    ppc)    ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
    Sparc)  ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
    wasm32) ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
esac
case "$CIAOOS$CIAOARCH" in
    # TODO: WHY??
    Solarisi686) ARCHFLAGS="-fPIC $ARCHFLAGS" ;;
    *) ;;
esac

# C compiler options for generating shared libraries code
# Linker options for shared objects
case "$CIAOOS$CIAOARCH" in
    BSD*)      CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    DARWINppc) CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -dynamiclib -undefined suppress" ;;
    DARWIN*)   CCSHARED="-fPIC" ; LDSHARED="-dynamiclib -undefined dynamic_lookup" ;;
    LINUX*)    CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    EMSCRIPTEN*) CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    Solaris*)  CCSHARED= ; LDSHARED="-G" ;;
    # note: -fPIC and -rdynamic are unused in Windows
    Win32*)    CCSHARED= ; LDSHARED="-shared" ;;
esac

# Extension of executables
EXECSUFFIX=
case "$CIAOOS$CIAOARCH" in
    EMSCRIPTEN*) EXECSUFFIX=".js" ;;
    Win32*) EXECSUFFIX=".exe" ;;
esac

# Extension of shared libraries
case "$CIAOOS$CIAOARCH" in
    DARWIN*)        SOSUFFIX=".dylib" ;;
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
# TODO: EMSCRIPTENwasm32: add ASSERTIONS=2 for debugging
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

# C optimization flags

# TODO: review if this is needed now and what are the best options
ALIGN_FLAGS=""
# if cc_test_option "-falign-functions=2"; then # TODO: needed now? (only GCC)
#     ALIGN_FLAGS="-falign-functions=2 -falign-loops=2 -falign-jumps=2 $ALIGN_FLAGS"
# fi

# Code generation options
OPTIM_FLAGS0="-fomit-frame-pointer $ALIGN_FLAGS"
if test x"$core__OPTIM_LEVEL" = x"optimized"; then
    case "$CIAOOS$CIAOARCH" in
        EMSCRIPTENwasm32) OPTIM_FLAGS="-fno-strict-aliasing -Oz -O3 $OPTIM_FLAGS0" ;;
        *) OPTIM_FLAGS="-fno-strict-aliasing -O2 $OPTIM_FLAGS0"
    esac
else
    # TODO: Why not "-O2" as in OptimComp? which one is wrong?
    OPTIM_FLAGS=""
fi
# Special cases where -fPIC is needed to allow compilation as shared library
case "$CIAOOS$CIAOARCH" in
    LINUXx86_64)  OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
    LINUXaarch64) OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
    BSDx86_64)    OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
    EMSCRIPTENwasm32)  OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
esac
# Workaround bug in Darwin19/Xcode 11 (Catalina) # TODO: remove when they fix it
OPTIM_FLAGS="-fno-stack-check $OPTIM_FLAGS"
# Disable -Wcompound-token-split-by-macro flag (clang 12) -- see optim-comp
if cc_test_option "-Wno-compound-token-split-by-macro"; then # TODO: needed now? (only GCC)
    OPTIM_FLAGS="-Wno-compound-token-split-by-macro $OPTIM_FLAGS"
fi
# Select C standard
OPTIM_FLAGS="-Wall -Wstrict-prototypes -std=gnu11 $OPTIM_FLAGS"

# Linker specific options
# Note: LINUX linker specific options
#   "-rdynamic" allows the dynamic libraries symbols to be resolved
#   against
LDFLAGS0=
case "$CIAOOS$CIAOARCH" in
    BSD*)        LDFLAGS0="-rdynamic" ;;
    LINUX*)      LDFLAGS0="-rdynamic" ;;
    EMSCRIPTEN*) LDFLAGS0="-rdynamic" ;;
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
    EMSCRIPTENwasm32)  LIBS0="-lnodefs.js" ;; # add NODEFS support
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
#       NOCONSOLEFLAG="-mno-cygwin" ;;
#     *) true
# esac

# In Windows we need an executable built without console support 
# ifeq ($CIAOOS,Win32)
#       cd $bld_objdir &&       do_gmake clean
#       do_gmake CONSOLEFLAG="$NOCONSOLEFLAG" dostateng
#       /bin/mv $bld_objdir/$ENGINAME.sta $bld_objdir/$ENGINAME_nc.sta
#       cd $bld_objdir &&       do_gmake clean
# endif

# ===========================================================================
# Custom engine configuration flags (-D preprocessor and configure.h)

emit_cdef() {
    printf " -D%s" "$1"
}

emit_cdefs() {
    emit_cdef "$CIAOOS"
    emit_cdef "$CIAOARCH"
    # Threads
    if test x"$core__USE_THREADS" = x"yes"; then emit_cdef "USE_THREADS"; fi
    # (optim-comp)
    if test x"$core__LOWRTCHECKS" = x"yes"; then emit_cdef "USE_LOWRTCHECKS"; fi
    if test x"$core__DEBUG_TRACE" = x"yes"; then emit_cdef "DEBUG_TRACE"; fi
    if test x"$core__PROFILE_INSFREQ" = x"yes"; then emit_cdef "PROFILE_INSFREQ"; fi
    if test x"$core__PROFILE_INS2FREQ" = x"yes"; then emit_cdef "PROFILE_INS2FREQ"; fi
    if test x"$core__PROFILE_BLOCKFREQ" = x"yes"; then emit_cdef "PROFILE_BLOCKFREQ"; fi
    if test x"$core__PROFILE_STATS" = x"yes"; then emit_cdef "PROFILE_STATS"; fi
    # Enable dynamic linking
    case "$CIAOOS$CIAOARCH" in
        EMSCRIPTENwasm32) true ;;
        *) emit_cdef "FOREIGN_FILES" ;;
    esac
    # WAM level debugging
    case "$core__DEBUG_LEVEL" in
        paranoid-debug|debug|profile-debug) emit_cdef "DEBUG" ;;
    esac
    # WAM level profiling
    case "$core__DEBUG_LEVEL" in
        profile|profile-debug) emit_cdef "PROFILE" ;;
    esac
    # Memory management primitives (own_malloc)
    # See engine/configure.c: USE_MMAP includes USE_OWN_MALLOC
    case "$CIAOOS$CIAOARCH" in
        # TODO: why?
        LINUXSparc)      emit_cdef "USE_OWN_MALLOC" ;;
        LINUXSparc64)    emit_cdef "USE_OWN_MALLOC" ;;
        # Do not use USE_MMAP for own_malloc in 64-bit architectures
        *Sparc64)        emit_cdef "USE_OWN_MALLOC" ;;
        *x86_64)         emit_cdef "USE_OWN_MALLOC" ;;
        *wasm32)         true ;;
        *ppc64)          emit_cdef "USE_OWN_MALLOC" ;;
        *ppc64le)        emit_cdef "USE_OWN_MALLOC" ;;
        *aarch64)        emit_cdef "USE_OWN_MALLOC" ;;
        # (assume 32-bit)
        *) emit_cdef "USE_MMAP"; emit_cdef "ANONYMOUS_MMAP" ;;
    esac
    #
    case "$core__AND_PARALLEL_EXECUTION" in
        yes) emit_cdef "ANDPARALLEL" ;;
        visandor) emit_cdef "ANDPARALLEL"; emit_cdef "VISANDOR" ;;
    esac
    #
    if test x"$core__PAR_BACK" = x"yes"; then emit_cdef "PARBACK"; fi
    if test x"$core__TABLED_EXECUTION" = x"yes"; then emit_cdef "TABLING"; fi
}    
CDEFS=`emit_cdefs`

# ---------------------------------------------------------------------------

CFLAGS="$OPTIM_FLAGS $DEBUG_FLAGS $THREAD_FLAGS $SOCKETS_FLAG $ARCHFLAGS $CDEFS"
LDFLAGS="$LDARCHFLAGS $LDFLAGS0 $LDRPATH $PROFILE_LD_FLAGS"
CCSHARED="$ARCHFLAGS $CCSHARED" # TODO: misses OPTIM_FLAGS, etc.?
LDSHARED="$LDARCHFLAGS $LDSHARED"
LIBS="$LIBS0 $LD_THREAD_LIB $LD_SOCKETS_LIB $DEBUG_LIBS"
STAT_LIBS="$STAT_SOCKETS_LIB"

# Add extra CFLAGS and LDFLAGS
CFLAGS="$CFLAGS $core__EXTRA_CFLAGS"
LDFLAGS="$LDFLAGS $core__EXTRA_LDFLAGS"
CCSHARED="$CCSHARED $core__EXTRA_CFLAGS"
LDSHARED="$LDSHARED $core__EXTRA_LDFLAGS"

# ===========================================================================
# [Extend basic configuration]
# TODO: complete, add socket cfg here?

for f in $eng_addcfg; do
    if test -r "$f/config_sh"; then
        ADD_STAT_LIBS=
        . "$f/config_sh"
        STAT_LIBS="$STAT_LIBS $ADD_STAT_LIBS"
    fi
done

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

# Compile as dynamic library
case "$CIAOOS$CIAOARCH" in
    EMSCRIPTENwasm32) ENG_DYNLIB=0 ;;
    *)           ENG_DYNLIB=1 ;;
esac

# Fix computed size in executable
case "$CIAOOS$CIAOARCH" in
    EMSCRIPTENwasm32) ENG_FIXSIZE=0 ;;
    *)           ENG_FIXSIZE=1 ;;
esac

# ===========================================================================

# Add engine include dir to search path for build
CFLAGS="-I$bld_hdir $CFLAGS"

# ===========================================================================

# Patch configuration for Emscripten (EMSCRIPTENwasm32)

case "$CIAOOS$CIAOARCH" in
    EMSCRIPTENwasm32)
        # It needs source dir in the include path (probably a bug:
        # emcc does not locate included files from some symlinked .c
        # files)
        CFLAGS="-I$bld_srcdir $CFLAGS"
        # Optimization flags must be included during link
        LDFLAGS="$LDFLAGS $OPTIM_FLAGS"
        # Other hardwired options (see ciaowasm)
        # TODO: customize
        LDFLAGS="$LDFLAGS \
-s MAIN_MODULE=2 \
-s DEFAULT_LIBRARY_FUNCS_TO_INCLUDE=['\$Browser'] \
-s LZ4=1 \
-s FORCE_FILESYSTEM=1 \
-s TOTAL_MEMORY=20971520 \
-s ALLOW_MEMORY_GROWTH=1 \
-s EXPORTED_RUNTIME_METHODS='[\
  \"FS_createPath\",\
  \"addRunDependency\",\
  \"removeRunDependency\",\
  \"ccall\",\
  \"cwrap\"]'"
        ;;
esac

# ===========================================================================

# Flags for using the engine as a library (LIBENG_CFLAGS, LIBENG_LDFLAGS)
LIBENG_CFLAGS="$CFLAGS"
LIBENG_LDFLAGS="$LDFLAGS"

# TODO: This hardwires a build path; use patchelf (Linux) or
#   install_name_tool (macOS) to fix during installation
LIBENG_RPATH="$bld_objdir"

if [ x"$LIBENG_RPATH" != x ]; then
    LIBENG_LDFLAGS="$LIBENG_LDFLAGS -Wl,-rpath -Wl,$LIBENG_RPATH"
fi

# ===========================================================================

# The first argument defines the string quoting character for the
# specified output kind. That ensures that values consisting on
# several words are correctly assigned (i.e., V="foo bar" or V=foo
# bar).

# (more complete quoting, used only for some flags)
qflag() { # q str
    if [ "$1" = "" ]; then # (Make)
        # escape $ as $$
        printf "%s" "$2" | sed -e "s/\\\$/\$\$/g"
    else # (sh)
        # escape ' as '"'"', $ as \$
        printf "$1"
        printf "%s" "$2" | sed -e "s/'/'\"'\"'/g;s/\\\$/\\\\\$/g"
        printf "$1"
    fi
}

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
LDFLAGS=$(qflag "$q" "$LDFLAGS")
LIBENG_CFLAGS=$q$LIBENG_CFLAGS$q
LIBENG_LDFLAGS=$(qflag "$q" "$LIBENG_LDFLAGS")
ENG_STUBMAIN_DYNAMIC=$q$ENG_STUBMAIN_DYNAMIC$q
EOF
}

# All platform settings (used in build_car.sh)
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
#
ENG_DYNLIB=$q$ENG_DYNLIB$q
ENG_FIXSIZE=$q$ENG_FIXSIZE$q
EOF
    dump_config_ccomp "$q"
}

mkdir -p "$bld_cfgdir"
dump_config_platform "'" > "$bld_cfgdir/config_sh"
dump_config_ccomp "" > "$bld_cfgdir/config_mk"


