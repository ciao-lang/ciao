#!/bin/bash
# Grep version on Ciao sources
#
# Author: Jose F. Morales
# ---------------------------------------------------------------------------

# Find in files with bundle version numbers
function find_vers() { # [paths]
    local d
    for d in "$@"; do
        find_vers_ "$d"
    done
}

# TODO: GlobalPatch is not shown (and it does ot match regexp)
function find_vers_() { # PATH
    # one line per file, end in \0 (useful for xargs)
    find "$1" \
         \( -name 'Manifest.pl' -o \
         -name '*.hooks.pl' -o \
         -name 'ciao-boot.sh' -o \
         -name 'lpdoc.js' -o \
         -name 'GlobalVersion' \) \
         -print0
    # other interesting files
}

# Something that looks like a version number
vers_regexp='[0-9][0-9]*\.[0-9][0-9]*'

# Other version numbers
function find_others() { # [paths]
    local d
    local f
    for d in "$@"; do
        for f in \
            core/compiler_oc/frontend_common.pl \
            core/engine/internals.pl \
            core/lib/compiler/c_itf.pl;
        do
            [ -r "$d"/"$f" ] && printf "%s\0" "$d"/"$f"
        done
    done
}

# Other versions
others_regexp='\(\<compiler_version\>\|\<poversion\>\|\<itf_version\>\)(.*[0-9]'

find_vers "$@" | xargs -0 grep -I -nH -e "$vers_regexp"
find_others "$@" | xargs -0 grep -I -nH -e "$others_regexp"
