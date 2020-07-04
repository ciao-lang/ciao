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
source "$builder_src"/car.bash
source "$builder_src"/archdump.bash

# ---------------------------------------------------------------------------
# Test data

# TESTDATA provides:
#  - alltests (all test names)
#  - test_opts TEST: function which defines range, gccopts,
#    inputrepeat, inputfile, ciaoccopts, ciaoldopts
#
if [ ! -r ./TESTDATA ]; then
    echo "error: TESTDATA file not found in the current directory"
    exit 1
fi
source ./TESTDATA
if [ x"$regr_name" = x"" ]; then
    echo "error: regr_name is undefined"
    exit 1
fi

# ---------------------------------------------------------------------------
# Output directories

ensure_regression_dir
curr_dir=${regression_dir}/$regr_name/curr
prev_dir=${regression_dir}/$regr_name/prev
out_dir=${regression_dir}/$regr_name/out

# ---------------------------------------------------------------------------
# Options and benchmarks

total_c=0
total_improlog=0
ok_c=0
ok_improlog=0

catinput() {
    rm -f /tmp/buf
    for i in `seq 1 $2`; do
        cat $1 >> /tmp/buf
    done
    cat /tmp/buf
    rm -f /tmp/buf
}

if which gtime > /dev/null; then
    gtime=`which gtime`
else
    gtime=/usr/bin/time
fi

testexec() { # execname inputarg outputname timename
    if [ x"${inputfile}" == x ]; then
        "$gtime" -a -o ${4} ${1} ${2} > ${3}
    else
#       echo "catinput ${inputfile} ${inputrepeat} | "$gtime" -a -o ${4} ${1} ${2} > ${3}"
        catinput ${inputfile} ${inputrepeat} | "$gtime" -a -o ${4} ${1} ${2} > ${3}
    fi
}

has_pl_ext() {
    local base
    local basenopl
    base=`basename $1`
    basenopl=`basename $1 .pl`
    [ "${basenopl}".pl == "${base}" ]
}

warn_nonempty_log() {
    if [ -s "${1}" ]; then
        echo "Warning: log file ${1} is not empty!"
        cat "${1}" # show log
    fi
}

function bring_files() {
#    echo $out_dir/${1}.pl.code.car/c/engine/
#    ls -la $out_dir/${1}.pl.code.car/c/engine/
    mkdir -p ${curr_dir}
    if [ -r $out_dir/${1}.pl.code.car/native_modules ]; then
        cp $out_dir/${1}.pl.code.car/c/engine/${1}.native.c ${curr_dir}/
        cp $out_dir/${1}.pl.code.car/c/engine/${1}.native.h ${curr_dir}/
        cp $out_dir/${1}.pl.code.car/o/${1}.native.s ${curr_dir}/
        archdump $out_dir/${1}.pl.code.car/o/${1}.native.o > ${curr_dir}/${1}.native.o.s
        return 0
    else
        rm -f ${curr_dir}/${1}.native.c
        rm -f ${curr_dir}/${1}.native.h
        rm -f ${curr_dir}/${1}.native.s
        rm -f ${curr_dir}/${1}.native.o.s
        echo "No compilation output for ${i}"
        return 1
    fi
}

compare_files() {
    local mode
    local ret
    mode=${1}
    mkdir -p ${prev_dir}
    bring_files ${2} || return 0
    ret=0
    pushd "$_base" > /dev/null
    i=${2}.native.c; difffiles "generated C code ${i}" ${curr_dir}/${i} ${prev_dir}/${i}-0 ${mode} || ret=1
    i=${2}.native.h; difffiles "generated C header code ${i}" ${curr_dir}/${i} ${prev_dir}/${i}-0 ${mode} || ret=1
    i=${2}.native.s; difffiles "gcc assembled code ${i}" ${curr_dir}/${i} ${prev_dir}/${i}-0 ${mode} || ret=1
    i=${2}.native.o.s; difffiles "disassembled gcc code ${i}" ${curr_dir}/${i} ${prev_dir}/${i}-0 ${mode} || ret=1
    popd > /dev/null
    return ${ret}
}

check_one() {
    compile_one ${1} && compare_one brief ${1}
}

compile_one() {
    test_opts ${1}
    for i in ${1}.*pl ${1}.*gcc; do
        if [ ! -r ${i} ]; then
            continue
        fi
        if [ -r ${i}.SKIP ]; then
            echo "Skipping version ${i}"
            continue
        fi
        srcname=${i}
        execname=$out_dir/${i}.code
        logname=$out_dir/${i}.log
        if has_pl_ext ${i}; then
#           continue # enable to disable ImProlog
            echo -n "Compiling ImProlog version: ${i}"
            total_improlog=$((${total_improlog} + 1))
            touch ${srcname} # TODO: a kludge..
            if "$bin_dir"/ciao oc:comp --bootstrap ${execname} ${srcname} > ${logname} 2>&1; then
                echo " [OK]"
                warn_nonempty_log ${logname}
                "$bin_dir"/ciao oc:car-clean "$execname".car
                CIAOCCOPTS="${ciaoccopts}" CIAOLDOPTS="${ciaoldopts}" "$bin_dir"/ciao oc:car-build "$execname".car
                ok_improlog=$((${ok_improlog} + 1))
            else
                delete_exe ${execname}
                echo " [Failed] (see log in ${logname})"
                cat "${logname}" # show log
                continue
            fi
        else
            echo -n "Compiling C version: ${i}"
            total_c=$((${total_c} + 1))
#           gcc -S -x c ${gccopts} ${srcname}
            if gcc -x c ${gccopts} -o ${execname} ${srcname} > ${logname} 2>&1; then
#           echo "using clang and llvm"
#           if ccc -x c ${gccopts} -o ${execname} ${srcname} > ${logname} 2>&1; then
                ok_c=$((${ok_c} + 1))
                echo " [OK]"
                warn_nonempty_log ${logname}
            else
                echo " [Failed] (see log in ${logname})"
                cat "${logname}" # show log
                continue
            fi
        fi
    done
}

run_one() {
    test_opts ${1}
    for i in ${1}.*pl ${1}.*gcc; do
        srcname=${i}
        execname=$out_dir/${i}.code
        timename=$out_dir/${i}.time
        if has_pl_ext ${i}; then
#           continue # enable to disable ImProlog
            if [ -r ${execname}.car/noarch ]; then
                true
            else
                continue
            fi
            echo -n "Testing ImProlog version: ${i}"
            execcmd=${execname}.car/run
        else
            if [ -r ${execname} ]; then
                true
            else
                continue
            fi
            echo -n "Testing C version: ${i}"
            execcmd=${execname}
        fi
        for j in $range; do
            echo -n " (range $j)"
            echo "Bench $i" > ${timename}
            echo "Testing for range $j" >> ${timename}
            testexec ${execcmd} ${j} $out_dir/${srcname}.out ${timename}
        done
        echo
    done
}

compare_one() {
    local i
    for i in ${2}.*pl; do
        if [ ! -r ${i} ]; then
            continue
        fi
        if [ -r ${i}.SKIP ]; then
#           echo "Skipping version ${i}"
            continue
        fi
        srcname=${i}
        compare_files ${1} `basename ${srcname} .pl`
    done
}

input=${1}

print_summary() {
    echo "C versions: ${ok_c}/${total_c} successfully compiled"
    echo "ImProlog versions: ${ok_improlog}/${total_improlog} successfully compiled"
}

check_all() {
    local i
    for i in ${alltests}; do
        check_one ${i}
    done
    print_summary
}

compile_all() {
    local i
    for i in ${alltests}; do
        compile_one ${i}
    done
    print_summary
}

compare_all() {
    local i
    for i in ${alltests}; do
        compare_one ${1} ${i}
    done
}

run_all() {
    local i
    for i in ${alltests}; do
        run_one ${i}
    done
}

help() {
    cat <<EOF
Usage: `basename ${0}` ACTION

Where ACTION is, one of:

  check-all
  compile-all
  run-all
  compare-all
  briefcompare-all
  save-all

Or for a single test TEST:

  check-one TEST
  compile-one TEST
  run-one TEST
  compare-one TEST
  briefcompare-one TEST
  save-one TEST

Where TEST is one of:
${alltests}
EOF
}

#pushd "$_base" > /dev/null

mkdir -p $out_dir

case ${input} in
    check-all) check_all ;;
    compile-all) compile_all ;;
    compare-all) compare_all full ;;
    briefcompare-all) compare_all brief ;;
    save-all) compare_all save-no-ask ;;
    run-all) run_all ;;
    eval-all) compile_all && run_all ;;
    check-one) check_one ${2} ;;
    compile-one) compile_one ${2} ;;
    compare-one) compare_one full ${2} ;;
    briefcompare-one) compare_one brief ${2} ;;
    save-one) compare_one save-no-ask ${2} ;;
    run-one) run_one ${2} ;;
    *) help ;;
esac


