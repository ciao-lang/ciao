#!/bin/bash

#
# Tests for mtypes
#

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

# ===========================================================================

all_tests="test1 test2 test3 test4 test5 polymorph queens11 paritylist err1 err2 err3 err4 err5"
rt_tests="test2 test4 test5"
testdir=`pwd` # $_base

ensure_dirs() {
    ensure_regression_dir
    resdir=$regression_dir/mtypes
    outdir=$regression_dir/mtypes/out
    mkdir -p "$outdir"
    mkdir -p "$resdir"
}

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

copy_test() { # test
    local i=$1
    mkdir -p "$outdir/$i"
    cp "$testdir/$i/"*"".pl "$outdir/$i"
    cp "$testdir/rtdriver".pl "$outdir"
}

# TODO: "ciaoc -c" does not generate .itf/.po for m_*.pl modules, etc.

cmd.check() {
    local i f
    ensure_dirs
    for i in $all_tests; do
	echo "### Compiling $i ###"
	copy_test "$i"
	pushd "$outdir/$i" > /dev/null
	if time ciaoc -x -c "$i" > "$i".err 2>&1 && \
	    ciaoc -w "$i" > /dev/null 2>&1; then
	    f=`product_filename "$i.wam"`
	    if [ -f "$f" ]; then
		cp "$f" "$i".wam
	    else
		echo > "$i".wam
	    fi
	else
	    echo > "$i".wam
	fi
	popd > /dev/null
    done
}

cmd.eval() {
    local i j f
    ensure_dirs
    for i in $rt_tests; do
	echo "### Eval of $i ###"
	copy_test "$i"
	pushd "$outdir/$i" > /dev/null
	j="$i"rt
	if ciaoc -x "$j" > /dev/null 2>&1 && \
	   ciaoc -w "$j" > /dev/null 2>&1; then
	    f=`product_filename "$j.wam"`
	    if [ -f "$f" ]; then
		cp "$f" "$j".wam
	    else
		echo > "$j".wam
	    fi
	    ( time ./"$j"; ) 2>&1
	fi
	popd > /dev/null
    done
}

cmd.show_err() {
    local i
    ensure_dirs
    for i in $all_tests; do
	echo "### Err of $i ###"
	cat "$outdir/$i/$i.err"
    done
}

cmd.show_po() {
    local i j
    ensure_dirs
    for i in $all_tests; do
	echo "### Bytecode of $i ###"
	cat "$outdir/$i/$i.wam"
    done
    for i in $rt_tests; do
	j="$i"rt
	echo "### Bytecode of $j ###"
	cat "$outdir/$i/$j.wam"
    done
}

cmd.help() {
    cat <<EOF
Available commands:
  check      Compile all tests
  eval       Eval (run) all tests
  show_err   Show compiler output (warnings, errors, etc.) for all tests
  show_po    Show disassembled .po files for all tests
  all        Compile and run tests, compare results
  compare    Compare results
  briefcompare Compare results (brief)
  save       Save results
EOF
}

cmd.all() {
    ensure_dirs
    cmd.check
    cmd.eval > "$resdir"/e.txt
    cmd.show_err > "$resdir"/t.txt
    cmd.show_po > "$resdir"/w.txt
    cmd.briefcompare
}

cmd.compare() {
    ensure_dirs
    difffiles "t" "$resdir"/t.txt "$resdir"/t.txt-orig compare || true
    difffiles "w" "$resdir"/w.txt "$resdir"/w.txt-orig compare || true
    difffiles "e" "$resdir"/e.txt "$resdir"/e.txt-orig compare || true
}

cmd.briefcompare() {
    ensure_dirs
    difffiles "t" "$resdir"/t.txt "$resdir"/t.txt-orig brief || true
    difffiles "w" "$resdir"/w.txt "$resdir"/w.txt-orig brief || true
    difffiles "e" "$resdir"/e.txt "$resdir"/e.txt-orig brief || true
}

cmd.save() {
    ensure_dirs
    difffiles "t" "$resdir"/t.txt "$resdir"/t.txt-orig save || true
    difffiles "w" "$resdir"/w.txt "$resdir"/w.txt-orig save || true
    difffiles "e" "$resdir"/e.txt "$resdir"/e.txt-orig save || true
}

if [ $# == 0 ]; then
    cmd.help
    exit 1
fi

case $1 in
    check) cmd.check ;;
    "eval") cmd.eval ;;
    show_err) cmd.show_err ;;
    show_po) cmd.show_po ;;
    all) shift; cmd.all $* ;; # TODO: ugly
    compare) shift; cmd.compare $* ;; # TODO: ugly
    briefcompare) shift; cmd.briefcompare $* ;; # TODO: ugly
    save) shift; cmd.save $* ;; # TODO: ugly
    *) cmd.help ;;
esac
