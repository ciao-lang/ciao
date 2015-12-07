#!/bin/bash

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
#source "$builder_src"/car.bash
source "$builder_src"/archdump.bash

#oc_builder=$builder_src/oc_builder.bash

# ---------------------------------------------------------------------------
# Description of the generated engine

eng_name="ciaoengine"

# (automatically generated files)
hfiles_ciao="instrdefs.h absmachdef.h"
cfiles="wamloop.c eng_info_mk eng_info_sh"

# Paths to engine automatically generated source and objects
eng_cfg=`"$builder_src"/../../builder/sh_src/config-sysdep/ciao_sysconf --osarch`
dir_hfiles_ciao=$builder_src/../../build/eng/$eng_name/include/ciao
dir_cfiles=$builder_src/../../build/eng/$eng_name/src
dir_ofiles=$builder_src/../../build/eng/$eng_name/objs/$eng_cfg
# TODO: hardwired list of files!

# (binary .o files for regression of disassembled output)
get_ofiles() {
    local i
    for i in "$dir_ofiles"/*.o; do
	basename "$i"
    done
}
ofiles=`get_ofiles`

# ---------------------------------------------------------------------------
# Test for emugen

# TODO: integrate with other tests (and optim_comp) (see bench.bash)
set -e

# TODO: do not use save-no-ask by default, add parameters to save different
# versions (e.g. for different architectures, absmach options, etc.)
savecmd="save-no-ask"
#savecmd="save"

emugen_regrdir="$regression_dir/emugen/$eng_cfg"

function ensure_emugen_regrdir() {
    ensure_regression_dir
    mkdir -p "$emugen_regrdir"
}

function bringemu() {
    ensure_emugen_regrdir
    mkdir -p "$emugen_regrdir"
    pushd "$emugen_regrdir" > /dev/null
    mkdir -p curr
    mkdir -p prev
    # TODO: missing bytecode from ciaoc.sta
    for i in $hfiles_ciao; do
	cp "$dir_hfiles_ciao"/"$i" curr/"$i"
    done
    for i in $cfiles; do
	cp "$dir_cfiles"/"$i" curr/"$i"
    done
    for i in $ofiles; do
	archdump "$dir_ofiles"/"$i" > curr/"$i".s
    done
    popd > /dev/null
}

function compareemu() { # Args: mode
    local mode
    local ret
    mode=$1
    bringemu
    ret=0
    ensure_emugen_regrdir
    pushd "$emugen_regrdir" > /dev/null
    for i in $hfiles_ciao; do
	difffiles "generated C code $i" curr/$i prev/$i-0 $mode || ret=1
    done
    for i in $cfiles; do
	difffiles "generated C code $i" curr/$i prev/$i-0 $mode || ret=1
    done
    for i in $ofiles; do
	difffiles "disassembled gcc code $i.s" curr/$i.s prev/$i.s-0 $mode || ret=1
    done
    popd > /dev/null
    return $ret
}

function help() {
    cat <<EOF
Usage: `basename $0` ACTION

  Diff based regression testing of emulator generated C files and
  disassembled native code. Do 'ciao build core/engine'
  (and 'ciao clean core/engine' if needed) to generate the engine files.

Actions:

  save          Save the current engine files (for regression)
  compare       Compare current output with saved files
  briefcompare  
EOF
}

case $1 in
    save) compareemu $savecmd ;;
    briefcompare) compareemu brief ;;
    compare) compareemu full ;;
    *) help ;;
esac
