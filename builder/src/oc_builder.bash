#!/bin/bash
# Builder for OptimComp
#
# TODO: use --cache-dir=DIR instead of --cache-dir DIR,
#       share code with other tools (see 'create')
# TODO: store tmpcomp_dir inside cache_dir? clean it?
# TODO: system-wide installation: add a user cache dir and system cache dir
# TODO: add an option to create binaries that have encoded the base dir and
#       environment variables
# TODO: do not use CIAOROOT, use something like CIAOLIB, and make it point to modules/core:module/extra:..., etc. right?

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
source "$builder_src"/car.bash

# ---------------------------------------------------------------------------
# Global info

# Default comp options
compopts=""

# Version 
version_dir=$ciaoroot/core/Manifest
VERSIONTAG="`cat ${version_dir}/GlobalVersion` [optim_comp]"
version_name="Ciao ${VERSIONTAG}"
title_string="${version_name} Tool"

# Show version
version() {
    title_message ${title_string}
}

# ---------------------------------------------------------------------------
# Functions to handle the compiler cache

# delete a directory
clean_dir() {
    [ -z ${1} ] && return 1
    [ ! -x ${1} ] && return 1
    rm -rf ${1}
    mkdir -p ${1}
}

make_cache_structure() {
    mkdir -p ${cache_dir}/mods-noarch
    mkdir -p ${cache_dir}/mods-noarch/engine
    mkdir -p ${cache_dir}/mods-arch
    mkdir -p ${cache_dir}/user-noarch
    mkdir -p ${cache_dir}/user-arch
    mkdir -p ${cache_dir}/js-out
    mkdir -p ${cache_dir}/tmp
}

clean_cache() {
    verbosemessage "Cleaning compiler cache"
    clean_dir ${cache_dir}/mods-noarch
    clean_dir ${cache_dir}/mods-noarch/engine
    clean_dir ${cache_dir}/mods-arch
    clean_dir ${cache_dir}/user-noarch 
    clean_dir ${cache_dir}/user-arch 
    clean_dir ${cache_dir}/js-out
    clean_dir ${cache_dir}/tmp
    make_cache_structure
}

clean_bin_cache() {
    verbosemessage "Cleaning compiler cache (action__compile)"
    make_cache_structure
    rm -f ${cache_dir}/mods-noarch/*.bin
    rm -f ${cache_dir}/user-noarch/*.bin
    rm -f ${cache_dir}/tmp/*.bin
}

clean_bootstrap() {
    verbosemessage "Cleaning the bootstrap compiler copy"
    rm -f ${ok_file}
    rm -f ${ok_step_file}
    rm -f ${bootstrap_modif}
    delete_exe ${compb[1]}
}

# ---------------------------------------------------------------------------
# Functions to compile

# get comp_1
# C1=bootstrap-compiler
update_bootstrap() {
    message "Updating bootstrap compiler"
    # TODO: could we skip this step if the .car is cleaned before?
    bootstrap_modif_orig=`modif_time ${bootstrap}`
    if [ -r ${bootstrap_modif} ]; then
        bootstrap_modif_current=`cat ${bootstrap_modif}`
    else
        bootstrap_modif_current=""
    fi
    if [ x"${bootstrap_modif_orig}" == x"${bootstrap_modif_current}" ]; then
	submessage "Preserving bootstrap compiler"
        return 0
    else
	submessage "Getting new bootstrap compiler"
	rm -f ${ok_file}
	rm -f ${ok_step_file}
	rm -f ${bootstrap_modif}
	delete_exe ${compb[1]}
	if unpack_exe ${bootstrap} ${compb[1]}; then
	    modif_time ${bootstrap} > ${bootstrap_modif}
	else
	    fail_message "Unpacking failed"
	    return 1
	fi
	delete_exe ${comp[1]}
	copy_exe ${compb[1]} ${comp[1]}
    fi
}

compmessage[1]="Step 1 (changes in the compiler, emulator or the bytecode format made visible)"
compmessage[2]="Step 2 (changes in the emulator or the bytecode format made visible)"
compmessage[3]="Step 3 (the compiler must reach a fixpoint now)"
okmessage[1]="Fixpoint found (no changes found, no more steps required)"
okmessage[2]="Fixpoint found (no changes found, no more steps required)"
okmessage[3]="Fixpoint found (the compiler seems to be correct)"

maxsteps=3

build_comp() {
    # note: do not enable build-info, it includes the build date, which
    #       makes comparing compilers binaries impossible
    CIAOOPTS="--build-info=no ${CIAOOPTS}" build_comp__2
}

build_comp__2() {
    local i
    local i2

    update_bootstrap || return 1

    message "Building the compiler"
    rm -f ${ok_file}
    rm -f ${ok_step_file}
    i=1
    while [ -t ]; do
	i2=`expr ${i} + 1`
	submessage ${compmessage[${i}]}
	clean_cache
	delete_exe ${compb[${i2}]}
	if run_exe ${comp[${i}]} ${compopts} --bootstrap ${compb[${i2}]} ${comp_module}; then
	    true
	else
	    fail_message "Compilation failed"
	    return 1
	fi

	if compare_exe ${compb[${i}]} ${compb[${i2}]}; then
	    echo > ${ok_file}
	    echo ${i} > ${ok_step_file}
	    delete_exe ${compcb}
	    copy_exe ${compb[${i}]} ${compcb}
	    delete_exe ${compc}
	    copy_exe ${comp[${i}]} ${compc}
	    ok_message ${okmessage[${i}]}
	    return 0
	else
	    if [ ${i} -lt ${maxsteps} ]; then
		i=${i2}
		delete_exe ${comp[${i}]}
		copy_exe ${compb[${i}]} ${comp[${i}]}
	    else
		fail_message "The compiler is incorrect! (no fixpoint found)"
		return 1
	    fi
	fi
    done
}

# TODO: see warning
fast_build_comp() {
    local i

    message "Fast compiler update (unique step)"
    submessage "Warning: use at your own risk, some changes in the abstract machine and compiler may not be updated correctly!"
    rm -f ${ok_file}
    if [ -r ${ok_step_file} ]; then
	i=`cat ${ok_step_file}`
#	clean_cache
#	delete_exe ${compc}
	if run_exe ${comp[${i}]} ${compopts} --bootstrap ${compc} ${comp_module} && ${compc}.car/compile_native; then
	    true
	else
	    fail_message "Compilation failed"
	    return 1
	fi
	ok_message "Compilation finished"
    else
	fail_message "No valid compiler found, run build-comp before"
    fi
}

status() {
    if [ -r ${ok_file} ]; then
	ok_message ${okmessage[3]}
    else
	fail_message "The compiler is incorrect! (no fixpoint found)"
    fi
}

# Compile the compiler using incremental compilation
stepinc() {
    message "Incremental compilation"
    clean_cache
    stepinc_aux

    submessage "Recompiling when nothing requires recompilation"
    stepinc_aux

    submessage "Recompiling when module_bin requires recompilation"
    clean_bin_cache
    stepinc_aux
}

stepinc_aux() {
    delete_exe ${compincb}
    if run_exe ${compc} ${compopts} --bootstrap ${compincb} ${comp_module}; then
	true
    else
	fail_message "Compilation failed"
	return 1
    fi

    if compare_exe ${compincb} ${compcb}; then
	ok_message "Fixpoint found (incremental compilation seems to work)"
	return 0
    else
	fail_message "The compiler is incorrect! Incremental compilation does not work!"
	return 1
    fi
}

# Compile the compiler using analysis and candidate compiler
# Cana=[[Cc]](comp) (using analysis)
stepana() {
    message "Compile the compiler using analysis"

    delete_exe ${compana}
    clean_cache
    if run_exe ${compc} ${compopts} --analyze-all --bootstrap ${compana} ${comp_module}; then
        true
    else
	fail_message "compilation failed"
	return 1
    fi
}

promote() {
    message "Promote"
    if [ -r ${ok_file} ]; then
	bootstrap_backup="${backup_dir}/comp-`date +%Y%m%d%H%M%S`.tar"
	submessage "Backing up to ${bootstrap_backup}"
	mkdir -p "${backup_dir}" \
	|| { fail_message "cannot create directory"; exit -1; } 
	mv ${bootstrap} ${bootstrap_backup} \
	|| { fail_message "cannot move"; exit -1; } 
	pack_exe ${compcb} ${bootstrap} \
	|| { fail_message "packing failed"; exit -1; } 
	delete_exe ${comp[1]}
	delete_exe ${compb[1]}
	delete_exe ${comp[2]}
	delete_exe ${compb[2]}
	delete_exe ${comp[3]}
	delete_exe ${compb[3]}
	delete_exe ${compc}
	delete_exe ${compcb}
	delete_exe ${compincb}
	ok_message "Promoted"
	rm -f ${ok_file}
	rm -f ${ok_step_file}
    else
	fail_message "No valid new compiler found."
        fail_message "Promoting is only allowed immediately after a successful \"build-comp\" command."
	return 1
    fi
}

# ---------------------------------------------------------------------------
# Execute candidate compiler

# Ensure that the candidate compiler (compc) exists
ensure_comp() {
    [ -x ${compc}.car ] || { \
      fail_message "No compiled compiler found, try `b \"build-comp\"`"; exit -1; \
    }
}

# Execute the candidate compiler (compc)
comp() {
    ensure_comp
    rm -f ${ok_file}
    ${compc}.car/run "$@" || { fail_message "failed"; exit -1; }
}

# Execute the candidate compiler (compc) in debug mode
# TODO: replace 'debug' by 'debug-mode', make it a special parameter 
debug_comp() {
    # TODO: recompile with debug info?
    ensure_comp
    rm -f ${ok_file}
    ${compc}.car/debug "$@" || { fail_message "failed"; exit -1; }
}

# Execute the compiler compiled with analysis
comp_ana() {
    ensure_comp
    rm -f ${ok_file}
    ${compana}.car/run "$@" || { fail_message "failed"; exit -1; }
}

# ---------------------------------------------------------------------------
# Execute candidate compiler (testing version)

# Ensure that the candidate compiler (compc) exists (testing version)
ensure_comp_testing() {
    set_vervars
    [ -x ${compc}${versuf}.car ] || { \
      fail_message "No compiled compiler for testing found, try `b \"build-comp-testing\"`"; exit -1; \
    }
}

# Execute the candidate compiler (compc) (testing version)
comp_testing() {
    set_vervars
    ensure_comp_testing
    rm -f ${ok_file}
    ${compc}${versuf}.car/run "$@" || { fail_message "failed"; exit -1; }
}

# ---------------------------------------------------------------------------
# Execute compiler with JS backend

# TODO: this is a dynamic executable, because it is cheaper, do it in other way?

# Ensure that the js_backend compiler exists
ensure_comp_js() {
    [ -x ${bin_dir}/comp_js ] || { \
      fail_message "No compiled compiler with JS-backend found, try `b \"build-cmds\"`"; exit -1; \
    }
}

# Execute the compiler compiled with analysis
comp_js() {
    setup_install
    ensure_comp_js
#    rm -f ${ok_file}
    CIAOLOADER=${loader_cmd} ${bin_dir}/comp_js "$@" || { fail_message "failed"; exit -1; }
}

# ---------------------------------------------------------------------------
# Run an executable (optionally showing execution statistics)

# For static executables
run_exe() {
    local prg
    prg=$1
    shift
    if [ ! -x ${prg}.car ]; then
	fail_message "Cannot find ${prg} executable"
	exit -1
    fi
    if [ x"${stats}" = x"yes" ]; then
	${prg}.car/run "$@" && ( test -r ${cache_dir}/tmp/ciao__trace.txt && cat ${cache_dir}/tmp/ciao__trace.txt || true )
    else
	${prg}.car/run "$@"
    fi
}

# For dynamic executables
run_testing() {
    set_vervars
    setup_install
    ${loader}${versuf}.car/run "$@"
}

# ---------------------------------------------------------------------------
# Setup 

setup() {
    # Safety tests
    [ -z "$ciaoroot" ] && exit -1 
    [ -z ${cache_dir} ] && exit -1 

    # Set the compiler module name
    comp_module=$ciaoroot/core_OC/cmds/comp

    # Set backup dir
    backup_dir=$ciaoroot/core_OC/bootstrap/backup

    # Ensure that the temporary compiler directory exists
    mkdir -p ${tmpcomp_dir}

    # A file that indicates that compcb is correct
    ok_file=${tmpcomp_dir}/ok
    # A file that indicates the fixpoint iterations to get a correct compcb
    ok_step_file=${tmpcomp_dir}/ok_step

    # Compiler steps
    bootstrap=$ciaoroot/core_OC/bootstrap/comp.tar
    bootstrap_modif=${tmpcomp_dir}/bootstrap_modif
    comp[1]=${tmpcomp_dir}/comp1
    compb[1]=${tmpcomp_dir}/compb1
    comp[2]=${tmpcomp_dir}/comp2
    compb[2]=${tmpcomp_dir}/comp2b
    comp[3]=${tmpcomp_dir}/comp3
    compb[3]=${tmpcomp_dir}/comp3b
    compb[4]=${tmpcomp_dir}/comp4b
    compana=${tmpcomp_dir}/compana
    # Candidate compiler
    compc=${tmpcomp_dir}/compc
    compcb=${tmpcomp_dir}/compcb
    # Output for the incremental compilation tests
    compincb=${tmpcomp_dir}/compincb

    export CIAOROOT=$ciaoroot
    export CIAOCACHE=${cache_dir}
}

# Other options

enable_stats() {
    stats=yes
    export CIAOOPTS="--profile-stats=yes ${CIAOOPTS}"
}

enable_rtchecks() {
    export CIAOOPTS="--rtchecks=yes $CIAOOPTS"
}

# TODO: add option to enable --profile-insfreq=yes
enable_profile() {
    export CIAOOPTS="--profile=yes $CIAOOPTS"
    export CIAORTOPTS="-prof -proft $CIAORTOPTS"
}

enable_verbose() {
    verbose=yes
    compopts="--verbose ${compopts}"
}

set_cache_dir() {
    cache_dir=$1
}

# ---------------------------------------------------------------------------
# Loader, setting up the environment, etc.
# TODO: move to other script???????

setup_install() {
    # Set loader module name
    loader_module=$ciaoroot/core_OC/cmds/loader
    # Set loader executable
    loader=${bin_dir}/ciaoloader
    # Set loader command
    loader_cmd=${bin_dir}/ciaoloader.car/run
}

setup_path() {
    # Insert bin directory in path (without duplications)
    # note: included ciaoroot to access this script
    new_path=${bin_dir}:$ciaoroot/optim_comp:${PATH/${bin_dir}:/}
}

# type 'eval `$0 bash-env`'
show_bash_env() {
    setup_install
    setup_path
    cat <<EOF
export CIAOROOT=${CIAOROOT};
export CIAOCACHE=${CIAOCACHE};
export CIAOLOADER=${loader_cmd};
export PATH=${new_path};
EOF
}

# type 'eval `$0 csh-env`'
show_csh_env() {
    setup_install
    setup_path
    cat <<EOF
setenv CIAOROOT ${CIAOROOT};
setenv CIAOCACHE ${CIAOCACHE};
setenv CIAOLOADER ${loader_cmd};
setenv PATH ${new_path};
EOF
}

set_vervars() {
    if [ x"${VERNAME}" == x"" ]; then
	vermsg=""
	versuf=""
    else
	vermsg=" (${VERNAME} version)"
	versuf="_${VERNAME}"
    fi
}

build_comp_testing() {
    # note: do only one compilation step in testing mode
    set_vervars
    message "Building the compiler""${vermsg}"
    delete_exe ${compc}${versuf}
    if comp ${compopts} --bootstrap ${compc}${versuf} ${comp_module}; then
	true
    else
	fail_message "Compilation failed"
	return 1
    fi
}

build_loader() {
    set_vervars
    message "Building the dynamic executable loader""${vermsg}"
    ensure_bin_dir
    setup_install
    delete_exe ${loader}${versuf}
    comp_testing --bootstrap ${loader}${versuf} ${loader_module}
    ${loader}${versuf}.car/compile_native
}

build_cmd() { # bundle cmd
    local bundle=$1; shift
    local cmd cmdmain cmdexec
    set_vervars
    ensure_bin_dir

    cmd=$1
    
    cmd_message "$bundle" "building ${cmd}""${vermsg}"" (command)"
    cmdmain=$ciaoroot/$bundle/cmds/${cmd}
    cmdexec=${bin_dir}/${cmd}
    # TODO: use a directory per cmd and a PROPS file (like it is done for tests)?
    if [ -r ${cmdmain}.pl ]; then
	case "$bundle" in
	    core_OCjs) CIAOALIASPATH="compiler=$ciaoroot/core_OCjs/compiler" comp_testing --use-alias-path --dynexec ${cmdexec}${versuf} ${cmdmain} ;;
	    *) comp_testing --dynexec ${cmdexec}${versuf} ${cmdmain}
	esac
    elif [ -r ${cmdmain}.c ]; then # TODO: port to Prolog
	gcc -O3 -o ${cmdexec}${versuf} ${cmdmain}.c
    else
	fail_message "Not found source for ${cmd}"
    fi
}

build_cmds() {
    local cmd cmds cmdmain cmdexec
    set_vervars
    message "Building commands""${vermsg}"

    ensure_bin_dir
    # TODO: identify available commands automatically
    cmds="ciaodump ciaosh ciao-shell funcsize"
    for cmd in ${cmds}; do
	build_cmd "core_OC" ${cmd}
    done

    # Post update cmds operations
    pushd ${bin_dir} > /dev/null
    rm -f ciao-toplevel${versuf}
    ln -s ciaosh${versuf} ciao-toplevel${versuf}
    popd > /dev/null
}

# ---------------------------------------------------------------------------
# Tests and benchmarking

bench() {
    ${builder_src}/bench.bash "$@"
}

# ---------------------------------------------------------------------------
# Other Javascript backend commands

# TODO: Split test actions from compiler/run actions
js_backend() {
    ${builder_src}/tests_js_backend.bash "$@"
}

# ---------------------------------------------------------------------------
# Begin

# Get options
while [ -t ]; do
    case $1 in
	--cache-dir)   shift; set_cache_dir $1 ;;
	--rtchecks)    enable_rtchecks ;;
	--stats)       enable_stats ;;
	--verbose)     enable_verbose ;;
	--profile)     enable_profile ;;
	-*)            fail_message "Unknown option '$1'"; exit -1 ;;
	*)             break ;;
    esac
    shift
done

setup

# Execute command
action=$1
action=`echo "$action" | sed -e s:-:_:g`
#
case $action in
    oc:*) action=${action#oc:} ;; # remove oc: prefix
    *) true
esac
#
case $action in
    # Compile the compiler
    build_comp)   build_comp ;;
    build_comp_rt) enable_rtchecks && build_comp ;;
    status)        status ;;
    promote)       promote ;;
    fast_build_comp) fast_build_comp ;;
    # Incremental compilation
    inc)           stepinc ;;
    # Calling the candidate compiler
    comp)          shift; comp "$@" ;;
    debug_comp)    shift; debug_comp "$@" ;;
    comp_ana)      shift; comp_ana "$@" ;;
    # Calling the compiler with js_backend
    build_comp_js) shift; build_cmd "core_OCjs" comp_js ;;
    comp_js)       shift; comp_js "$@" ;;
    # Setting up environment
    bash_env)      show_bash_env ;;
    csh_env)       show_csh_env ;;
    # Compiling the system
    build)         build_comp && build_loader && build_cmds ;;
    build_loader)  build_loader ;;
    build_cmds)    build_cmds ;;
    build_all)     build_loader && build_cmds ;;
    # Maintenance
    clean_cache)   clean_cache ;;
    clean_bootstrap) clean_bootstrap ;;
    # Testing version
    run_testing)   shift; VERNAME=testing run_testing "$@" ;;
    comp_testing)  shift; VERNAME=testing comp_testing "$@" ;;
    build_comp_testing) VERNAME=testing build_comp_testing ;;
    build_loader_testing) VERNAME=testing build_loader ;;
    build_cmds_testing) VERNAME=testing build_cmds ;;
    # Tests
    tests)         shift; ${builder_src}/tests.bash "$@" ;;
    bench)         shift; bench "$@" ;;
    # Experimental
    ana)           stepana ;;
    js_backend)    shift; js_backend "$@" ;;
    # Show version
    version)       version ;;
    # Nothing
    '')            fail_message "No command specified."; exit -1 ;;
    # Error
    *)             fail_message "Bad command '$action'."; exit -1 ;;
esac


