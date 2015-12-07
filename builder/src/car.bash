# ---------------------------------------------------------------------------
# Functions to handle .car self-contained executables

compare_exe() {
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ ! -x ${2}.car ] && return 1
    # Compare
    diff -r ${1}.car ${2}.car > /dev/null 2>&1
}

delete_exe() {
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    # Delete
    rm -rf ${1}.car
}

move_exe() { # src dst
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ -x ${2}.car ] && return 1
    # Move
    mv ${1}.car ${2}.car
}

copy_exe() { # src dst
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ -x ${2}.car ] && return 1
    # Copy
    cp -R ${1}.car ${2}.car
}

pack_exe() { # src dst
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ -z ${2} ] && return 1
    # Pack
    tar -cf ${2} -C ${1}.car .
}

unpack_exe() { # src dst
    # Validate arguments
    [ -z ${1} ] && return 1
    [ -x ${2}.car ] && return 1
    # Unpack
    mkdir ${2}.car
    pushd "${2}.car" > /dev/null
    tar -xf ${1}
    popd > /dev/null
}

