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

oc_builder=$builder_src/oc_builder.bash

# ---------------------------------------------------------------------------
# Output directories

ensure_regression_dir
curr_dir=${regression_dir}/shootout-c/curr
prev_dir=${regression_dir}/shootout-c/prev
out_dir=${regression_dir}/shootout-c/out

# ---------------------------------------------------------------------------

# TODO: benchmarks that takes an input file runs too fast (e.g. sumcol, called sum-file?), and it should not!
# TODO: read input parameter in common.pl
# TODO: improve syntax in ImProlog: merge lowpred and foreign props

#gccopts0="-pipe -Wall -O3 -fomit-frame-pointer -march=pentium4 -mfpmath=sse -msse2"
gccopts0="-pipe -Wall -O3 -fomit-frame-pointer -march=pentium4 -m32 -mfpmath=sse -msse2"

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

testexec() { # execname inputarg outputname timename
    if [ x"${inputfile}" == x ]; then
	/usr/bin/time -a -o ${4} ${1} ${2} > ${3}
    else
#	echo "catinput ${inputfile} ${inputrepeat} | /usr/bin/time -a -o ${4} ${1} ${2} > ${3}"
	catinput ${inputfile} ${inputrepeat} | /usr/bin/time -a -o ${4} ${1} ${2} > ${3}
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
    if [ -s ${1} ]; then
	echo "Warning: log file ${1} is not empty!"
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
#	    continue # enable to disable ImProlog
	    echo -n "Compiling ImProlog version: ${i}"
	    total_improlog=$((${total_improlog} + 1))
	    touch ${srcname} # TODO: a kludge..
	    if "$oc_builder" comp --bootstrap ${execname} ${srcname} > ${logname} 2>&1; then
		echo " [OK]"
		warn_nonempty_log ${logname}
		${execname}.car/clean
		CIAOCCOPTS="${ciaoccopts}" CIAOLDOPTS="${ciaoldopts}" ${execname}.car/compile_native
		ok_improlog=$((${ok_improlog} + 1))
	    else
		delete_exe ${execname}
		echo " [Failed] (see log in ${logname})"
		continue
	    fi
	else
	    echo -n "Compiling C version: ${i}"
	    total_c=$((${total_c} + 1))
#	    gcc -S -x c ${gccopts0} ${gccopts} ${srcname}
	    if gcc -x c ${gccopts0} ${gccopts} -o ${execname} ${srcname} > ${logname} 2>&1; then
#	    echo "using clang and llvm"
#	    if ccc -x c ${gccopts0} ${gccopts} -o ${execname} ${srcname} > ${logname} 2>&1; then
		ok_c=$((${ok_c} + 1))
		echo " [OK]"
		warn_nonempty_log ${logname}
	    else
		echo " [Failed] (see log in ${logname})"
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
#	    continue # enable to disable ImProlog
	    if [ -r ${execname}.car/run ]; then
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
#	    echo "Skipping version ${i}"
	    continue
	fi
	srcname=${i}
	compare_files ${1} `basename ${srcname} .pl`
    done
}

alltests="binarytrees chameneos fannkuch fasta hello knucleotide mandelbrot nbody nsievebits nsieve partialsums pidigits recursive regexdna revcomp spectralnorm sumcol threadring"

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

test_opts() {
    range=
    gccopts=
    inputrepeat=1
    inputfile=
    ciaoccopts=
    ciaoldopts=
    case ${1} in
	binarytrees) 
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    gccopts="-lm"
            #range="12 14 16"
	    range="16"
	    ;;
	chameneos)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    gccopts="-lpthread"
	    #range="10000 100000 1000000"
	    range="1000000"
	    ;;
	fannkuch)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    #range="9 10 11"
	    range="10"
	    ;;
	fasta)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    #range="250000"
	    range="250000"
	    ;;
	hello)
	    #range="1 50 100 150 200 "
	    range="200"
	    ;;
	knucleotide)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    #range="10000 100000 1000000"
	    range="1000000"
	    inputfile="input/knucleotide-input.txt"
	    ;;
	mandelbrot)
	    gccopts="-D_ISOC9X_SOURCE -lm"
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    #range="400 600 3000"
	    range="3000"
	    ;;
	nbody)
	    gccopts="-lm"
	    #range="200000 2000000 20000000"
	    range="2000000"
	    ;;
	nsievebits)
	    #range="9 10 11"
	    range="11"
	    ;;
	nsieve)
	    gccopts="-std=c99"
	    #range="8 9 10"
	    range="10"
	    ;;
	partialsums)
	    gccopts="-lm"
	    ciaoccopts="-march=pentium4 -mfpmath=sse -msse2"
	    #range="25000 250000 2500000"
	    range="2500000"
	    ;;
	pidigits)
	    gccopts="-lgmp"
	    ciaoccopts="-march=pentium4 -mfpmath=sse -msse2"
	    ciaoldopts="-lgmp"
	    #range="1500 2500 5000"
	    range="5000"
	    ;;
	recursive)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    #range="3 7 11"
	    range="11"
	    ;;
	regexdna)
	    ciaoccopts="-march=pentium4 -mfpmath=sse -msse2"
	    ciaoldopts="-lpcre"
	    gccopts="-lpcre"
	    #range="100000 300000 500000"
	    range="500000"
	    inputfile="input/regexdna-input.txt"
	    ;;
	revcomp)
	    #range="25000 250000 2500000"
	    range="2500000"
	    inputrepeat=2000
	    inputfile="input/revcomp-input.txt"
	    ;;
	spectralnorm)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    gccopts="-lm"
	    #range="500 3000 5500"
	    range="2000"
	    ;;
	sumcol)
	    ciaoccopts="-O3 -march=pentium4 -mfpmath=sse -msse2"
	    gccopts="-lm"
	    #range="1000 11000 21000"
	    range="21000"
	    inputrepeat=2000
	    inputfile="input/sumcol-input.txt"
	    ;;
	threadring)
	    range="500000"
	    gccopts="-lpthread"
	    ;;
	*)
	    echo "Bad tests name '${1}'"
	    return
	    ;;
    esac
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


