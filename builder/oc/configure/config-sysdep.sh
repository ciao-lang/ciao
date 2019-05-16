#!/bin/sh
# Input: CIAOOS CIAOARCH

source ${CONFIGURE_DIR}/cc_test_option

# Set CC (C compiler) and LD (linker)
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

# Does this machine has dynamic linking abilities?
case "$CIAOOS$CIAOARCH" in
    *) FOREIGN_FILES_FLAG="-DFOREIGN_FILES" ;;
esac

# Libraries and flags to use threads
LD_THREAD_LIB=
THREAD_FLAG=
if test x"$USE_THREADS" = x"yes"; then
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
	Solaris*) LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	BSD*)     LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	DARWIN*)  THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
	LINUX*)   LD_THREAD_LIB="-lpthread"; THREAD_FLAG="-D_REENTRANT -DTHREADS" ;;
        # Threads and locks in Win32: no threads, no locks so far.
	Win32*) THREAD_FLAG="-DTHREADS" ;;
    esac
fi

# Architecture options
# TODO: use host arch?
ARCHFLAGS=
LDARCHFLAGS=
case "$CIAOARCH" in
    i686)  ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
    ppc)   ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
    Sparc) ARCHFLAGS="-m32" ; LDARCHFLAGS="-m32" ;;
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
    DARWINppc) CCSHARED="-fPIC" ; LDSHARED="-flat_namespace -bundle -undefined suppress" ;;
    DARWIN*)   CCSHARED="-fPIC" ; LDSHARED="-dynamiclib -undefined dynamic_lookup" ;;
    LINUX*)    CCSHARED="-fPIC" ; LDSHARED="-shared" ;;
    Solaris*)  CCSHARED= ; LDSHARED="-G" ;;
    # note: -fPIC and -rdynamic are unused in Windows
    Win32*)    CCSHARED= ; LDSHARED="-shared" ;;
esac

# # POSIX locks (old Solaris versions)
# if test x"${CIAOOS}" = x"Solaris"; then
#   LD_LOCK_LIB="-lposix4"
# fi

# Debug options
if test x"${DEBUG_LEVEL}" = x"debug"; then
  OPTIM_LEVEL="debug"
fi

# Optimizations

if cc_test_option -falign-functions=2; then
    ALIGN_FLAGS="-falign-loops=2 -falign-jumps=2 -falign-functions=2"
elif cc_test_option -malign-functions=2; then
    # Version =< 3.1 of gcc only admits the -m form
    ALIGN_FLAGS="-malign-loops=2 -malign-jumps=2 -malign-functions=2"
else
    # Other compilers like clang does not admit this flag
    ALIGN_FLAGS=""
fi

# Code generation options
OPTIM_FLAGS0="$ALIGN_FLAGS"
case "$CIAOOS$CIAOARCH" in
    # We are not using strict-aliasing to avoid problems in Solaris # TODO: wrong?
    Solarisi686)    OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    SolarisSparc)   OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    SolarisSparc64) OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    LINUXalpha)     OPTIM_FLAGS0="$OPTIM_FLAGS0" ;;
    #
    *i686)     OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *x86_64)   OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *ppc)      OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *ppc64)    OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *Sparc)    OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *Sparc64)  OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *arm)      OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *armv4l)   OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
    *armv5tel) OPTIM_FLAGS0="-fomit-frame-pointer $OPTIM_FLAGS0" ;;
esac
if test x"${OPTIM_LEVEL}" = x"optimized"; then
    OPTIM_FLAGS="-fno-strict-aliasing -O2 $OPTIM_FLAGS0"
else
    OPTIM_FLAGS="-O2"
fi
# Special cases where -fPIC is needed to allow compilation as shared library
case "$CIAOOS$CIAOARCH" in
    LINUXx86_64) OPTIM_FLAGS="-fPIC $OPTIM_FLAGS" ;;
esac
# Select C standard
OPTIM_FLAGS="-Wall -Wstrict-prototypes -std=gnu11 $OPTIM_FLAGS"

# Memory management primitives (own_malloc)
MEM_MNG_FLAGS=""
case "$CIAOOS$CIAOARCH" in
    DARWINppc)         MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    DARWINi686)        MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXi686)         MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    LINUXppc)          MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc)      MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    SolarisSparc64)    MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
    Win32i686)         MEM_MNG_FLAGS="-DUSE_MMAP -DANONYMOUS_MMAP" ;;
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

# Libraries used when creating so files
case ${CIAOOS} in
    LINUX)             SOLIBS="-lc"
esac

# ---------------------------------------------------------------------------

CCSHARED="$ARCHFLAGS $CCSHARED" # TODO: misses OPTIM_FLAGS, etc.?
LDSHARED="$LDARCHFLAGS $LDSHARED"
LIBS="${LIBS0} ${LD_THREAD_LIB} ${LD_LOCK_LIB}"

