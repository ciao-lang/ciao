#!/bin/bash
# Detect wrong character encodings for Ciao sources.
#
# Author: Jose F. Morales
#
# A tool to detect wrong character encodings in Ciao source trees.
# Accepted MIME charsets:
#  - us-ascii: accepted
#  - iso-8859-1 (latin1): works in comments, strings but not in unquoted atoms
#
# We plan move all iso-8859-1 to utf-8 and make it an accepted encoding.
#
# TODO: detect Windows encodings? (detected with 'uchardet' but not as mime charset?)
#
# Usage:
#   charset-source.bash [-d DIR]
# ---------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Exit immediately if a simple command exits with a non-zero status
set -e

# ----------------------------------------------------------------

. "$_base"/source-enum.bash

if [ x"$1" == x"-d" ]; then
    shift
    dir=$1
    shift
else
    dir=$_base/../..
fi

## (we use 'file --mime-encoding' instead)
## # TODO: Do a partial port of https://github.com/chardet/chardet
## #   (based on Mozilla's algorithm for detecting languages and coding systems)
## #   to avoid using 'uchardet'
## 
## if ! which uchardet > /dev/null 2>&1; then
##     cat <<EOF
## ERROR: 'uchardet' command found
## 
## Please install it from source or with your favourite package
## management system (e.g., brew install uchardet).
## 
## EOF
## fi

function ignored_charset() {
    case $1 in
	"binary") return 0 ;; # TODO: only for empty files?
	"ERROR: (null)") return 0 ;; # TODO: good only for 1-char files
	*) return 1 ;;
    esac
}

function accepted_charset() {
    case $1 in
	"us-ascii") return 0 ;;
	*) return 1 ;;
    esac
}

cd "$dir"

# Charset
find_source -print | \
    while IFS= read i; do
	if [ -f "$i" ]; then
#	    chardet=`uchardet "$i" || echo UCHARDET-FAILED`
	    charset=`file -b --mime-encoding "$i" || true`
	    if ignored_charset "$charset"; then
		true
	    elif accepted_charset "$charset"; then
		if grep -l `printf '\r\n'` "$i" > /dev/null 2>&1; then
 		    echo "WARNING: DOS-style newlines: $i"
		fi
	    else
 		echo "WARNING: encoding '$charset' not accepted: $i"
	    fi
	fi
    done

# DOS-style newlines
# find_source -type f -print0 | xargs -0 grep -l `printf '\r\n'` | \
#     while IFS= read i; do
# 	echo "WARNING: DOS-style newlines: $i"
#     done
