#!/bin/bash

# ---------------------------------------------------------------------------
# A simple script to test the documentation output for several
# backends and manuals.

# Note:
#
# The testing code is far from perfect. A much cleaner and clever test
# code could be implemented.
#
# Its purpose is just to serve as an oracle to point out differences
# in lpdoc output. Comparison is textual and may give many false
# positives on detecting problems; so human help is required.
#
# The tests are neither optimal: some of them are huge, full manuals.
#
# Author: Jose F. Morales
# ---------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------
# Imports

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

# TODO: share these defs
# TODO: Find a method to extract this directory from Prolog code (or
#   share the same value)
rel_builddir="build"
ciao_builddir="$ciaoroot/$rel_builddir"

builddir_doc=$ciao_builddir/doc

ensure_dirs() {
    ensure_regression_dir
    resdir=$regression_dir/lpdoc
}

# ---------------------------------------------------------------------------

# This is a hack... it has the version number hardwired.
function get_manual_file() {
    case $1 in
	ciao) echo "ciao-1.15.0" ;;
	ciaopp_ref_man) echo "ciaopp-1.2.0" ;;
	ciaopp_doc) echo "ciaopp_internals-1.2.0" ;;
	lpdoc) echo "lpdoc-3.0.0" ;;
	# for testing
	ciaotest) echo "ciaotest" ;;
	singlelpdoc) echo "singlelpdoc" ;;
	*) echo ""
    esac
}

function get_manual_dir() {
    case $1 in
	ciao) echo "$ciaoroot/core" ;;
	ciaopp_ref_man) echo "$ciaoroot/ciaopp" ;;
	ciaopp_doc) echo "$ciaoroot/ciaopp" ;;
	lpdoc) echo "$ciaoroot/lpdoc" ;;
	# for testing
	ciaotest) echo "$ciaoroot/tests/lpdoc/ciaotest" ;;
	singlelpdoc) echo "$ciaoroot/tests/lpdoc/singlelpdoc" ;;
	*) echo ""
    esac
}

function get_manual_outdir() {
    case $1 in
	ciao) echo "$builddir_doc" ;;
	ciaopp_ref_man) echo "$builddir_doc" ;;
	ciaopp_doc) echo "$builddir_doc" ;;
	lpdoc) echo "$builddir_doc" ;;
	# for testing
	ciaotest) echo "$ciaoroot/tests/lpdoc/ciaotest" ;;
	singlelpdoc) echo "$ciaoroot/tests/lpdoc/singlelpdoc" ;;
	*) echo ""
    esac
}

function get_manual_cmd() {
    case $1 in
	ciao) echo "lpmake docs" ;;
	ciaopp_ref_man) echo "lpmake docs" ;;
	ciaopp_doc) echo "lpmake docs" ;;
	lpdoc) echo "lpmake docs" ;;
	# for testing
	ciaotest) cat <<EOF
lpdoc all
EOF
	    ;;
	singlelpdoc) cat <<EOF
lpdoc singlelpdoc.texi && \
lpdoc singlelpdoc.html && \
echo > singlelpdoc.infoindex && \
echo > singlelpdoc.info && \
echo > singlelpdoc.pdf && \
echo > singlelpdoc.dvi && \
echo > singlelpdoc.manl
EOF
	    ;;
	*) echo ""
    esac
}

# ---------------------------------------------------------------------------

#SUFFIXES="pdf dvi html manl texi info infoindex"

# Clean doc output
# TODO: use lpdoc realclean, etc.
function clean_docs() {
    cd "$manual_outdir"
    rm -f *.texic_gr *.texic_rr *.html_gr *.html_rr *.manl_gr *.manl_rr *.texic_dr *.html_dr *.ascii_dr *.manl_dr *.texic *.texi *.bbl *.blg *.el *.manl *.info *.infoindex || true
    rm -rf *.html || true
    rm -rf *.tmp-html || true
    rm -rf *.tmp-man || true
    rm -rf *.tmp-texinfo || true
}

# Generate the documentation for $manual
function check_docs() {
    ensure_dirs
    cd "$manual_dir"
    time eval "$manual_cmd"
}

# TODO: texi and html output is split into several files! concatenate them for comparison?

# Just compare the output of the documentation generation
function compare_docs() {
    ensure_dirs
    cd "$manual_outdir"
    echo "Comparing PDF (just size)"
    ls -la "$manual_file.pdf" "$resdir/$manual_file.pdf-saved" || true
    echo "Comparing DVI (just size)"
    ls -la "$manual_file.dvi" "$resdir/$manual_file.dvi-saved" || true
    echo "Comparing html"
    mydiff "$manual_file.html" "$resdir/$manual_file.html-saved" || true
    echo "Comparing manl"
    mydiff "$manual_file.manl" "$resdir/$manual_file.manl-saved" || true
    echo "Comparing texi"
    mydiff "$manual_file.texi" "$resdir/$manual_file.texi-saved" || true
    echo "Comparing info"
    mydiff "$manual_file.info" "$resdir/$manual_file.info-saved" || true
    echo "Comparing infoindex"
    mydiff "$manual_file.infoindex" "$resdir/$manual_file.infoindex-saved" || true
}

function mydiff() {
    if [ ! -z "${DIFFCMD:-}" ]; then
	diff -q $1 $2 || "${DIFFCMD}" $1 $2
    else
	# Be silent, just say if files differ
	diff -q $1 $2
    fi
}

# Save the output of the documentation generation
function save_docs() {
    ensure_dirs
    cd "$manual_outdir"
    echo "Saving PDF"
    cp "$manual_file.pdf" "$resdir/$manual_file.pdf-saved" || true
    echo "Saving DVI"
    cp "$manual_file.dvi" "$resdir/$manual_file.dvi-saved" || true
    echo "Saving html"
    cp "$manual_file.html" "$resdir/$manual_file.html-saved" || true
    echo "Saving manl"
    cp "$manual_file.manl" "$resdir/$manual_file.manl-saved" || true
    echo "Saving texi"
    cp "$manual_file.texi" "$resdir/$manual_file.texi-saved" || true
    echo "Saving info"
    cp "$manual_file.info" "$resdir/$manual_file.info-saved" || true
    echo "Saving infoindex"
    cp "$manual_file.infoindex" "$resdir/$manual_file.infoindex-saved" || true
}

# -----------------------------------------------------------------
# TODO: use lpdoc for those commands

# Open the HTML documentation
function open_html() {
    local top=`cat "$manual_outdir/$manual_file.html/"*".htmlmeta"`
    open "$manual_outdir/$manual_file.html/$top.html"
}

# Open the HTML documentation
function open_pdf() {
    open "$manual_outdir/$manual_file.pdf"
}

# Open the info documentation (with emacsclient)
function open_info() {
    # TODO: escape is wrong
    # TODO: fix 'lpdoc infoview', needs absolute file name
    emacsclient -n --eval "(info \"$manual_outdir/$manual_file.info\")"
}

# -----------------------------------------------------------------

cmd.help() {
    cat <<EOF
Usage: `basename $0` ACTION MANUAL

where ACTION is one of:

  check         Generate the documentation
  clean         Clean the documentation

  compare       Briefly compare the documentation output
  compare-diff  Compare the documentation output with diff (standard output)
  compare-meld  Compare the documentation output with meld (graphical interface)
  save          Save the documentation output

  open-html     Open the generated HTML documentation
  open-pdf      Open the generated PDF documentation
  open-info     Open the generated info documentation

  help          Show this message
EOF
}

# -----------------------------------------------------------------

function pick_manual() {
    manual_file=`get_manual_file "$manual"`
    if [ x"$manual_file" == x"" ]; then
	echo "Unrecognized manual \`$manual'"
	do_help
	exit -1
    fi
    manual_outdir=`get_manual_outdir "$manual"`
    manual_dir=`get_manual_dir "$manual"`
    manual_cmd=`get_manual_cmd "$manual"`
}

# -----------------------------------------------------------------

if [ $# != 2 ]; then
    cmd.help
    exit 1
fi

action=$1
shift
manual=$1
shift

case $action in
    check)      pick_manual ; check_docs ;;
    clean)        pick_manual ; clean_docs ;;
    #
    compare)      pick_manual ; compare_docs ;;
    compare-diff) DIFFCMD=diff; pick_manual ; compare_docs ;;
    compare-meld) DIFFCMD=meld; pick_manual ; compare_docs ;;
    save)         pick_manual ; save_docs ;;
    #
    open-html)    pick_manual ; open_html ;;
    open-pdf)     pick_manual ; open_pdf ;;
    open-info)    pick_manual ; open_info ;;
    #
    help)         cmd.help; exit -1 ;;
    *)            cmd.help; exit -1 ;;
esac

