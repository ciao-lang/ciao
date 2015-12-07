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

oc_builder=$builder_src/oc_builder.bash

# ---------------------------------------------------------------------------
# Configuration

outdir="${cache_dir}/js-out"
savedir="${regression_dir}/js_backend"

# Path to the V8 shell
V8Shell="${HOME}/Documents/svn/v8-read-only/v8"
# Note: compilation of the V8 JavaScript shell command:
#   g++ -m32 -Iinclude samples/shell.cc -o v8 libv8.a -lpthread

# ---------------------------------------------------------------------------

# TODO: Not really nice
function ptojs__adddir() {
    if [ x"${testdir}" = x"" ]; then
	echo "${1}"
    else
	echo "${testdir}/${1}"
    fi
}
# TODO: Not really nice
function ptojs__get_outfile() {
    local mod
    mod=`basename "${1}" .pl`
    echo "${outdir}/${mod}"
}
# TODO: Not really nice
function ptojs__get_savefile() {
    local mod
    mod=`basename "${1}" .pl`
    echo "${savedir}/${mod}"
}

function ptojs__compile_mod() {
    local file
    echo "Compiling module ${1}"
    # TODO: missing option to compile just a module
    file=`ptojs__adddir "${1}"`
    "$oc_builder" comp-js ${ciao_opts} "${file}"
}

function ptojs__compile_exec() {
    local file
    echo "Compiling exec ${1}"
    file=`ptojs__adddir "${1}"`
    "$oc_builder" comp-js ${ciao_opts} "${file}"
}

function ptojs__check_mod() {
    ciao_opts="--target-platform=${platform}"
    ptojs__compile_mod ${1} && ptojs__compare_mod brief ${1}
}

function ptojs__check_exec() {
    ciao_opts="--target-platform=${platform}"
    ptojs__compile_exec ${1} && ptojs__compare_exec brief ${1}
}

function ptojs__run_exec() {
    ciao_opts="--target-platform=${platform}"
    custom_load ${1}
}

function ptojs__try_exec() {
    ciao_opts="--target-platform=${platform}"
    ptojs__compile_exec ${1} && custom_load ${1}
}

# regression test of a module output
function ptojs__regr_exec() { # file
    ciao_opts="--target-platform=${platform}"
    ptojs__compile_exec ${1} || { echo "Compilation failed, not executing file ${1}"; return 0; }
    ptojs__compare_exec brief ${1}
    custom_load_regr_exec ${1} && ptojs__compare_out brief ${1} 
}

# check-exec+try-exec
function ptojs__eval_exec() { # file
    ciao_opts="--target-platform=${platform}"
    ptojs__compile_exec ${1} && ptojs__compare_exec brief ${1} && custom_load ${1}
}

function custom_load() { # file
    file=`ptojs__get_outfile "${1}"`
    case ${platform} in
	safari) open -a "/Applications/Safari.app" "${file}.html" ;;
	chrome) open -a "/Applications/Google Chrome.app" "${file}.html" ;;
	firefox) open -a "/Applications/Firefox.app" "${file}.html" ;;
	opera) open -a "/Applications/Opera.app" "${file}.html" ;;
	v8) ${V8Shell} "${file}.out.js" ;;
	nodejs) node "${file}.out.js"
    esac
}

function custom_load_regr_exec() { # file
    file=`ptojs__get_outfile "${1}"`
    custom_load ${1} > ${file}.out.txt
}

function show_help() {
    cat <<EOF
Usage: test_js_backend.sh OPTIONS ACTION

Where OPTIONS are:

  --target-platform PLATFORM 

      Where PLATFORM is one of:
        nodejs: NodeJS (based on V8 javascript engine)
        v8: V8 javascript engine shell
        chrome: Chrome web browser
        firefox: Firefox web browser
        safari: Safari web browser
        opera: Opera web browser

and ACTION is one of:

  check-mod PRG    - compile and compare the module PRG
  check-exec PRG   - compile and compare the exec PRG
  run-exec PRG     - run the exec PRG
  try-exec PRG     - compile and run the exec PRG
  eval-exec PRG    - compile, check, and run the exec PRG
  regr-exec PRG    - execute the regression test (exec) PRG

  briefcompare-mod PRG - briefly compare the compilation results for PRG (mod)
  briefcompare-exec PRG - briefly compare the compilation results for PRG (exec)
  compare-mod PRG     - compare the compilation results for module PRG
  compare-exec PRG    - compare the compilation results for exec PRG
  compare-out PRG     - compare the execution output of exec PRG
  save-mod PRG        - save the compilation results and regression data (mod)
  save-exec PRG       - save the compilation results and regression data (exec)
  save-out PRG        - save the execution output of exec PRG

  (works for both mods, exec, and out)
  check-all           - do check-X on all the tests
  save-all            - do save-X on all the tests
  save-out-all        - do save-out on all the tests
  regr-all            - execute the regression tests (console only tests)
  bench-all           - execute the benchmarks (speed tests) (console only tests)
  briefcompare-all    - briefly compare the compilation results (all tests)
  briefcompare-out-all - briefly compare the execution output (all tests)
  compare-all         - compare the compilation results (all tests)
  compare-out-all     - only compare the execution output (all tests)
  
NOTE: This is alpha software -- see ${0} for more options
EOF
}

function ptojs__compare_any() { # mode file text suffix
    local left
    local right
    local ret
    local ofile
    local sfile
    ensure_regression_dir
    mkdir -p "${savedir}"

    ofile=`ptojs__get_outfile "${2}"`
    sfile=`ptojs__get_savefile "${2}"`
    left="${ofile}.${4}"
    right="${sfile}.${4}-saved"
    ret=0
    difffiles "${3} ${2}.${4}" ${left} ${right} ${1} || ret=1
    return ${ret}
}

function ptojs__compare_mod() { # mode file
    # TODO: out.js contains is static, but dyn.mi1 and dyn.mi2 files miss
    #       the libraries. I need to check also libraries.
    ptojs__compare_any "${1}" "${2}" "middle-level code (pass 1)" "dyn.mi1"
    ptojs__compare_any "${1}" "${2}" "middle-level code (pass 2)" "dyn.mi2"
    ptojs__compare_any "${1}" "${2}" "dynamic Javascript code" "dyn.js"
}

function ptojs__compare_exec() { # mode file
    # TODO: out.js contains is static, but dyn.mi1 and dyn.mi2 files miss
    #       the libraries. I need to check also libraries.
    ptojs__compare_any "${1}" "${2}" "middle-level code (pass 1)" "dyn.mi1"
    ptojs__compare_any "${1}" "${2}" "middle-level code (pass 2)" "dyn.mi2"
    ptojs__compare_any "${1}" "${2}__init" "middle-level code (pass 1)" "dyn.mi1"
    ptojs__compare_any "${1}" "${2}__init" "middle-level code (pass 2)" "dyn.mi2"
    ptojs__compare_any "${1}" "${2}" "linked Javascript code" "out.js"
}

function ptojs__compare_out() { # mode file
    local left
    local right
    local ret
    local ofile
    local sfile
    ensure_regression_dir
    mkdir -p "${savedir}"

    ofile=`ptojs__get_outfile "${2}"`
    sfile=`ptojs__get_savefile "${2}"`
    left="${ofile}.out.txt"
    right="${sfile}.out.txt-saved"
    ret=0
    difffiles "output of program ${2}.out.txt" ${left} ${right} ${1} || ret=1
    return ${ret}
}

#    diff ${left} ${right}
#    diff -q ${left} ${right} ||	opendiff ${left} ${right}
#    diff -q ${left} ${right} ||	meld ${left} ${right}
#    emacsclient --eval "(ediff \"${left}\" \"${right}\")"

# TODO: some of this information should not be here
# TODO: change default platform?
TESTS_DIR=$ciaoroot/tests/js_backend
TESTS_PLATFORM="nodejs"
BENCHS_DIR=$ciaoroot/examples/js_backend/general
BENCHS_PLATFORM="nodejs"
BENCHS="testsuite_1 testsuite_2 testsuite_3 testsuite_4"
DEMOS_DIR=$ciaoroot/examples/js_backend/general
DEMOS_PLATFORM="nodejs"
DEMOS="hello_world"
DEMOS_UI_DIR=$ciaoroot/examples/js_backend/general
DEMOS_UI_PLATFORM="chrome"
DEMOS_UI="demo queens_ui"

# TODO: Obtain them automatically (there is a similar list in optim_comp tests)
RUNTIME_MODS="arithmetic \
	      attr_rt \
	      basiccontrol \
	      freeze \
	      internals \
	      io_basic \
	      js_dom \
	      js_foreign \
	      mutables_rt \
	      streams_div \
	      streams_v8 \
	      streams_nodejs \
	      string_type_rt \
	      term_basic \
	      term_compare \
	      term_typing"

# Find the tests in the TESTS_DIR directory
function find_tests() {
    if [ x"${TESTS}" = x"" ]; then
	TESTS=`find_tests_`
    fi
}

function find_tests_() {
    OLDPWD=`pwd`
    cd ${TESTS_DIR}
    # TODO: filter PROPS (to integrate with the rest of the system tests)
    for i in `ls */PROPS`; do dirname $i; done
    cd ${OLDPWD}
}

# regression tests on program output
# note: no benchmarks, no user interfaces
function ptojs__regr_all() {
    platform="${TESTS_PLATFORM}"
    find_tests
    for i in ${TESTS}; do
	testdir="${TESTS_DIR}/${i}"
	ptojs__regr_exec ${i}
    done
}

# benchmark tests
#
# TODO: add options for benchmarking
#   may_use_functor_trait - disable functor traits
function ptojs__bench_all() {
    platform="${BENCHS_PLATFORM}"
    testdir="${BENCHS_DIR}"
    for i in ${BENCHS}; do
	ptojs__try_exec ${i}
    done
}

function ptojs__check_all() {
    platform="${TESTS_PLATFORM}"
    find_tests
    for i in ${TESTS}; do
	testdir="${TESTS_DIR}/${i}"
	ptojs__check_exec ${i}
    done
    platform="${BENCHS_PLATFORM}"
    testdir="${BENCHS_DIR}"
    for i in ${BENCHS}; do
	ptojs__check_exec ${i}
    done
    platform="${DEMOS_PLATFORM}"
    testdir="${DEMOS_DIR}"
    for i in ${DEMOS}; do
	ptojs__check_exec ${i}
    done
    platform="${DEMOS_UI_PLATFORM}"
    testdir="${DEMOS_UI_DIR}"
    for i in ${DEMOS_UI}; do
	ptojs__check_exec ${i}
    done
    # TODO: This should be 'check_mod', but 'compile_mod' is not working yet
    # Compare the runtime modules
    for i in ${RUNTIME_MODS}; do
	ptojs__compare_mod brief ${i}
    done
}

function ptojs__compare_all() { # mode
    # Compare the runtime modules
    for i in ${RUNTIME_MODS}; do
	ptojs__compare_mod ${1} ${i}
    done
    # Compare the tests
    find_tests
    for i in ${TESTS} ${BENCHS} ${DEMOS} ${DEMOS_UI}; do
	ptojs__compare_exec ${1} ${i}
    done
    #
    ptojs__compare_out_all    
}

function ptojs__compare_out_all() { # mode
    # Compare the output of the tests (for regression)
    find_tests
    for i in ${TESTS}; do
	ptojs__compare_out ${1} ${i}
    done
}

# TODO: do not use save-no-ask by default?
# TODO: add parameters to save different versions (e.g. for different
#       architectures, absmach options, etc.)
savecmd="save-no-ask"
#savecmd="save"

# ---------------------------------------------------------------------------

set_target_platform() { # platform
    case $1 in
	nodejs) true ;;
	v8) true ;;
	safari) true ;;
	chrome) true ;;
	firefox) true ;;
	opera) true ;;
	*) fail_message "Bad target platform $1."; exit -1 ;;
    esac
    platform=$1
}

# ---------------------------------------------------------------------------

function ptojs__eval_all() {
    echo "(The Javascript backend is evaluated by hand)"
}

# ---------------------------------------------------------------------------
# Begin

# Get options
while [ -t ]; do
    case $1 in
	--target-platform)   shift; set_target_platform $1 ;;
	-*)            fail_message "Bad option $1."; exit -1 ;;
	*)             break ;;
    esac
    shift
done

# TODO: decompose the command name to simplify the code

command=$1
shift
case $command in
    check-mod) testdir=""; ptojs__check_mod "$1" ;;
    check-exec) testdir=""; ptojs__check_exec "${1}" ;;
    run-exec) testdir=""; ptojs__run_exec "${1}" ;;
    try-exec) testdir=""; ptojs__try_exec "${1}" ;;
    eval-exec) testdir=""; ptojs__eval_exec "${1}" ;;
    #
    regr-exec) testdir=""; ptojs__regr_exec "${1}" ;;
    #
    briefcompare-mod) ptojs__compare_mod brief "${1}" ;;
    briefcompare-exec) ptojs__compare_exec brief "${1}" ;;
    compare-mod) ptojs__compare_mod full "${1}" ;;
    compare-exec) ptojs__compare_exec full "${1}" ;;
    compare-out) ptojs__compare_out full "${1}" ;;
    save-mod) ptojs__compare_mod $savecmd "${1}" ;;
    save-exec) ptojs__compare_exec $savecmd "${1}" ;;
    save-out) ptojs__compare_out $savecmd "${1}" ;;
    # --
    # (it does both 'exec', 'mod', and 'out')
    check-all) ptojs__check_all ;;
    save-all) ptojs__compare_all $savecmd ;;
    save-out-all) ptojs__compare_out_all $savecmd ;;
    #
    eval-all) ptojs__eval_all ;;
    regr-all) ptojs__regr_all ;;
    bench-all) ptojs__bench_all ;;
    #
    briefcompare-all) ptojs__compare_all brief ;;
    briefcompare-out-all) ptojs__compare_out_all brief ;;
    compare-all) ptojs__compare_all full ;;
    compare-out-all) ptojs__compare_out_all full ;;
    *) show_help ;;
esac


