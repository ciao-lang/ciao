#!/bin/sh
#
#  clean_aux.sh
#
#  Auxiliary functions to clean results of compilation
#
#  Copyright (C) 2015 Jose F. Morales, Ciao Developer team
#
# ===========================================================================
#
# TODO: port to Prolog
#
#  - The list of suffixes should be given by the system (the previous
#    point would help).
#
#  - CIAOCACHEDIR may simplify some code
#
# ---------------------------------------------------------------------------

# Exit immediately if a simple command exits with a non-zero status
set -e

# Clean the compilation output for all files, recursively
# TODO: really clean backup files *~ and #*?
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
	\
	-o -name "*_glue.c" -o -name "*_inline.c" \
	-o -name "*.o" -o -name "*.a" \
	-o -name "*.so" -o -name "*.dll" -o -name "*.dylib" \
	\
	-o -name "*.log" -o -name "*.err" \
	-o -name "tmpciao*" \
	\
	-o -name "*_auto.pl" \
	-o -name "*_co.pl" \) \
	-exec /bin/rm -f {} \;
}

# Clean the compilation output for just one module
clean_mod() { # MOD
    test x"$1" = x"" && return
    /bin/rm -f \
           "$1.asr" \
           "$1.ast" \
           "$1.itf" \
           "$1.po" \
	   "$1""_""$CIAOOS$CIAOARCH"".o" \
	   "$1""_""$CIAOOS$CIAOARCH"".a" \
	   "$1""_""$CIAOOS$CIAOARCH"".so" \
           "$1""_""$CIAOOS$CIAOARCH"".dll" \
	   "$1""_""$CIAOOS$CIAOARCH"".dylib" \
           "$1""_""$CIAOOS$CIAOARCH""_glue.c" \
           "$1""_""$CIAOOS$CIAOARCH""_glue.o" \
	   "$1""_""$CIAOOS$CIAOARCH""_inline.c"
}

# Load configuration parameters
. "$ENG_CIAO_CONFIG"
CIAOOS=$core__OS
CIAOARCH=$core__ARCH

# Call whatever comes in arguments
"$@"
