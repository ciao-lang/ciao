#!/bin/bash
# Grep version on Ciao sources
#
# Author: Jose F. Morales
# ---------------------------------------------------------------------------

# Find in files with bundle version numbers
function find_source() { # [paths]
    local d
    for d in "$@"; do
	find_source_ "$d"
    done
}

# TODO: GlobalPatch is not shown (and it does ot match regexp)
function find_source_() { # PATH
    # one line per file, end in \0 (useful for xargs)
    find "$1" \
	 \( -name 'Manifest.pl' -o \
	 -name '*.hooks.pl' -o \
	 -name 'GlobalVersion' \) \
	 -print0
}

regexp='[0-9][0-9]*\.[0-9][0-9]*'

find_source "$@" | xargs -0 grep -I -nH -e "$regexp"
