#!/bin/sh
#
#  clean_aux.sh
#
#  Auxiliary functions to clean results of compilation
#
#  Copyright (C) 2015-2019 Jose F. Morales, Ciao Developer team
#
# ===========================================================================
#
# TODO: port to Prolog (unique calls are from ciaoc_aux.pl)
#
#  - The list of suffixes should be given by the system (the previous
#    point would help).
#
# ---------------------------------------------------------------------------

# Exit immediately if a simple command exits with a non-zero status
set -e

# Clean the compilation output for all files, recursively
# TODO:
#  - removing *_glue.c, *_inline.c, *_auto.c, *_co.pl may not be a good idea
#    (use different names)
clean_tree() {
    # NOTE: this code has been optimized (usually it should take less
    #   than 10 seconds)
    #
    # We cannot use -print0 | xargs -0 because -print0 is not compatible
    # with Solaris, nor -print | xargs because it is buggy
    test x"$1" = x"" && return
    find "$1"/. -name '.svn' -prune -o \( \
        -name '*.po' -o -name '*.itf' -o -name '*.wam' \
        -o -name '*.asr' -o -name '*.ast' \
        -o -name '*.testout' \
        \
        -o -name '*_glue.c' -o -name '*_inline.c' \
        -o -name '*.o' -o -name '*.a' \
        -o -name '*.so' -o -name '*.dll' -o -name '*.dylib' \
        \
        -o -name '*.log' -o -name '*.err' \
        -o -name 'tmpciao*' \
        \
        -o -name '*_auto.pl' \
        -o -name '*_co.pl' \) \
        -exec rm -f {} \;
}

# Clean the compilation output at some cachedir for some prefix
# TODO:
#  - removing *_glue.c, *_inline.c, *_auto.c, *_co.pl may not be a good idea
#    (use different names)
clean_cachedir() { # Dir Prefix
    # NOTE: this code has been optimized (usually it should take less
    #   than 10 seconds)
    #
    # We cannot use -print0 | xargs -0 because -print0 is not compatible
    # with Solaris, nor -print | xargs because it is buggy
    test x"$1" = x"" && return
    test x"$2" = x"" && return
    find "$1"/. -name "$2"'.*' -a \( \
        -name '*.po' -o -name '*.itf' -o -name '*.wam' \
        -o -name '*.asr' -o -name '*.ast' \
        -o -name '*.testout' \
        \
        -o -name '*_glue.c' -o -name '*_inline.c' \
        -o -name '*.o' -o -name '*.a' \
        -o -name '*.so' -o -name '*.dll' -o -name '*.dylib' \
        \
        -o -name '*.log' -o -name '*.err' \
        -o -name 'tmpciao*' \
        \
        -o -name '*_auto.pl' \
        -o -name '*_co.pl' \) \
        -exec rm -f {} \;
}

# Call whatever comes in arguments
"$@"
