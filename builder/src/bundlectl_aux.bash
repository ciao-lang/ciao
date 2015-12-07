#!/bin/bash

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------

old_dir=`pwd`; cd "$_base/../.."; ciaoroot=`pwd`; cd "$old_dir"; old_dir=
builder_src=$ciaoroot/builder/src
source "$builder_src"/compat.bash
source "$builder_src"/config.bash
source "$builder_src"/messages.bash
source "$builder_src"/compare_files.bash
# source "$builder_src"/archdump.bash

# ---------------------------------------------------------------------------

# POSIX flags to sanitize scripts
set -e # Exit on non-zero status command exit
set -u # Error on parameter expansion of unset variables

# ---------------------------------------------------------------------------

get_bundle_opts() {
    if [ ! -z "${CIAOC_BUNDLE_OPTS:-}" ]; then
	return
    fi
    CIAOC_BUNDLE_OPTS=`$_base/bundlectl ciaoc-bundle-opts`
    CIAOC_BUNDLE_MODS=`$_base/bundlectl ciaoc-bundle-mods`
}

ensure_dirs() {
    ensure_regression_dir
    resdir=$regression_dir/$REGRNAME
    mkdir -p "$resdir"
}

# ---------------------------------------------------------------------------

# TODO: refactor
save_question() {
    local qst_what
    qst_what=$1
    while [ -t ]; do
	printf "%s" "Really save results for ${qst_what}? (y/n) "
	read qst_ok
	if [ x"$qst_ok" == x"y" ]; then
	    return 0
	elif [ x"$qst_ok" == x"n" ]; then
	    return 1
	else
	    echo "Unrecognized answer. Please type 'y' for 'yes' or 'n' for 'no'."
	fi
    done
}

# path without .pl extension (if passed)
no_pl() { # path
    local b d
    b=`basename "$1" .pl`
    d=`dirname "$1"`
    if [ "$d" != "." ]; then
	b=$d/$b
    fi
    printf "%s" "$b"
}

# TODO: refactor
# Get absolute path of some compilation product
# (see internals:product_filename/3 predicate)
product_filename() { # relpath
    local f=$1
    local dir base
    if [[ "$f" == */* ]]; then
        dir=`cd "${f%/*}" && pwd`
        base=${f##*/}
    else
        dir=`pwd`
        base=$f
    fi
    if [ ! -z "${CIAOCACHEDIR:-}" ]; then
	printf "%s" "$CIAOCACHEDIR/"`printf "$dir" | sed 's/\./../g;s/\//./g'`".$base"
    else
	printf "%s" "$dir/$base"
    fi
}

# Absolute path to the file in the regression database
regr_filename() { # relpath
    local base=`basename $1`
    printf "%s" "$resdir"/"$base"-orig
}

# ---------------------------------------------------------------------------

do_buildmod() { # I
    ensure_dirs
    get_bundle_opts # (1)
    ciaoc ${CIAOC_BUNDLE_OPTS} -c "$1"
}

do_buildexec() { # I
    ensure_dirs
    get_bundle_opts # (1)
    ciaoc ${CIAOC_BUNDLE_OPTS} "$1" ${CIAOC_BUNDLE_MODS}
}

do_check() { # I
    ensure_dirs
    get_bundle_opts # (1)
    ciaoc ${CIAOC_BUNDLE_OPTS} -w "$1"
    do_briefcompare "$1"
}

do_briefcompare() { #I
    ensure_dirs
    local f=`product_filename "$1".wam`
    local orig=`regr_filename "$1".wam`
    difffiles "WAM code" "$f" "$orig" brief || true
}

do_compare() { #I
    ensure_dirs
    local f=`product_filename "$1".wam`
    local orig=`regr_filename "$1".wam`
    difffiles "WAM code" "$f" "$orig" compare || true
}

do_save() { #I
    ensure_dirs
    local f=`product_filename "$1".wam`
    local orig=`regr_filename "$1".wam`
    difffiles "WAM code" "$f" "$orig" save || true
}

do_check_all() {
    local I
    echo "Generating WAM code"
    for I in ${WAMCHECKMODS}; do
	echo -n "  ${I} "
	do_check ${I}
    done
}

do_compare_all() {
    local I
    echo "Comparing WAM code"
    for I in ${WAMCHECKMODS}; do
	echo -n "  ${I} "
	do_compare ${I}
    done
}

do_briefcompare_all() {
    local I
    echo "Comparing WAM code"
    for I in ${WAMCHECKMODS}; do
	echo -n "  ${I} "
	do_briefcompare ${I}
    done
}

do_save_all() {
    local I
    echo "Saving WAM code"
    for I in ${WAMCHECKMODS}; do
#	echo -n "  ${I} "
	do_save ${I}
    done
}

# ---------------------------------------------------------------------------

do_build_all() {
    local I
    echo "Building modules"
    for I in ${BUILDMODS}; do
	echo -n "  ${I} "
	do_buildmod ${I}
	echo
    done
    echo "Building execs"
    for I in ${RUNMODS}; do
	echo -n "  ${I} "
	do_buildexec ${I}
	echo
    done
}

# ---------------------------------------------------------------------------

do_run() { # EXEC
    local e=$1
    shift
    ./"$e" "$@"
}

# ---------------------------------------------------------------------------

do_bench_all() {
    local I
    echo "% Benchmarking code"
    for I in ${BENCHMODS}; do
	echo "%  ${I} "
	do_bench ${I}
    done
}

do_bench() { # I
    local dir base
    ensure_dirs
    get_bundle_opts
    ciaoc ${CIAOC_BUNDLE_OPTS} "$1" ${CIAOC_BUNDLE_MODS}
    dir=`dirname "$1"`
    base=`basename "$1"`
    pushd "$dir" > /dev/null
    for t in `./"$base" --enum-tests`; do
	./"$base" "${t}"
    done
    popd > /dev/null
}

# ---------------------------------------------------------------------------

# TODO: This is an approximation (may leave .so files, etc.)
do_clean_subdir() {
    local i b
    find . '(' -name '*.po' -or -name '*.itf' ')' -exec rm -f '{}' ';'
    # TODO: Prefix based solution instead? (it would remove old files too, and be faster)
    if [ ! -z "${CIAOCACHEDIR:-}" ]; then
	find . -name '*.pl' | while read i; do
            b=`no_pl "$i"`
            rm -f "`product_filename "$b".itf`"
            rm -f "`product_filename "$b".po`" 
	    done
    fi
}

do_clean() {
    local i b
    do_clean_subdir
    # Clean binaries for benchmarks
    for i in ${BENCHMODS}; do
        b=`no_pl "$i"`
	rm -f "$b"
    done
}

# ---------------------------------------------------------------------------

do_help() {
    cat <<EOF
Usage: `basename $0` ACTION

Where ACTION is one of:

  check
  compare
  briefcompare
  save
  bench
  clean
EOF
}

# ---------------------------------------------------------------------------

source "$CURR_BUNDLEDIR"/"$CURR_BUNDLENAME.hooks.bash"

# ---------------------------------------------------------------------------

ACTION=$1

case ${ACTION} in
    run) shift; do_run "$@" ;;
    build) do_build_all ;;
    check) do_check_all ;;
    compare) do_compare_all ;;
    briefcompare) do_briefcompare_all ;;
    save) do_save_all ;;
    bench) do_bench_all ;;
    clean) do_clean ;;
    *) do_help ;;
esac
