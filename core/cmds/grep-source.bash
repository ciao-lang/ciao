#!/bin/bash
# Grep on Ciao sources
#
# Author: Jose F. Morales
#
#   This command performs a grep on Ciao source files (avoid .svn and
#   .git files and automatically generated files like .po, .itf, files
#   under build/).
#
# Usage:
#   grep-source.bash -e REGEXP [PATHS]
#
# TODO: 
#   - Nothing is cached or precomputed (so search is really slow)
#   - Search is not semantic nor syntactic.
#   - Merge with modgraph_grep.pl
# ---------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Exit immediately if a simple command exits with a non-zero status
set -e

# ---------------------------------------------------------------------------

. "$_base"/source-enum.bash

if [ x"$1" == x"-e" ]; then
    shift
    regexp=$1
    shift
else
    cat <<EOF
ERROR: Expected regexp (-e REGEXP)
EOF
    exit 1 # error
fi

find_source0 "$@" | xargs -0 grep -I -nH -e "$regexp"
