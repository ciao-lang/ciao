#!/bin/bash
# A simple multisystem performance benchmark

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------
# Imports

old_dir=`pwd`; cd "$_base/../.."; ciaoroot=`pwd`; cd "$old_dir"; old_dir=
builder_src=$ciaoroot/builder/src
# source "$builder_src"/compat.bash

oc_builder=$builder_src/oc_builder.bash

# ---------------------------------------------------------------------------

TESTS="\
    mtsys_boyer\
    mtsys_crypt\
    mtsys_deriv\
    mtsys_exp\
    mtsys_factorial\
    mtsys_fft\
    mtsys_fib\
    mtsys_knights\
    mtsys_nreverse\
    mtsys_poly\
    mtsys_primes\
    mtsys_qsort\
    mtsys_queens11\
    mtsys_query\
    mtsys_tak"

systems="yap hprolog sicstus swiprolog swiprolog_opt ciao ciao_1_6 ciao2 ciao3"

function do_tests { # arguments: system
    local sys
    sys=$1
    for i in $TESTS; do
        pushd "${ciaoroot}/tests/suite/$i" > /dev/null
        "$oc_builder" bench mtsys-evalmod-${sys} $i
        popd > /dev/null
    done
}

if [ $# == 0 ]; then
    cat <<EOF
Usage: tests_mtsys.bash SYSTEMS

where SYSTEMS is a subset of: $systems

This command allows the evaluation of a set of tests in several Prolog
systems.
EOF
    exit 1
fi

## TODO: Old documentation
#
#Other commands can be executed with:
#
#  cd TEST_DIR
#  ciao oc:bench ACTION
#
#where the available actions are:
#
# Compile and check the compiler output
#
#  checkmodsSUFFIX:     check all modules
#  checkmodSUFFIX NAME: check the module NAME
#
# Evaluate (execute and measure time)
#
#  evalmodsSUFFIX:      evaluate all modules
#  evalmodSUFFIX NAME:  evaluate the module NAME
#
# Save output
#
#  savemodsSUFFIX:      save output for all modules
#  savemodSUFFIX NAME:  save output for module NAME
#
# View differences
#
#  comparemodsSUFFIX:     view differences for all modules
#  comparemodSUFFIX NAME: view differences for the module NAME
#
# Summarize differences
#  
#  briefcomparemodsSUFFIX: briefly summarize differences for all modules
#  briefcomparemodSUFFIX NAME: briefly summarize differences for module NAME
#
#where SUFFIX may be one of:
#
#  -yap: Yap Prolog
#  -hprolog: hProlog
#  -sicstus: SICStus Prolog
#  -swiprolog: SWI-Prolog
#  -swiprolog_opt: SWI-Prolog (optimized)
#  2: Ciao (OptimComp bytecode)
#  3: Ciao (OptimComp native)
#  (nothing): default Ciao

for sys in $*; do
    do_tests ${sys}
done
