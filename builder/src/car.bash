# ---------------------------------------------------------------------------
# Functions to handle .car self-contained executables

car_compare() {
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ ! -x ${2}.car ] && return 1
    # Compare
    diff -r ${1}.car ${2}.car > /dev/null 2>&1
}

car_delete() {
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    # Delete
    rm -rf ${1}.car
}

car_move() { # src dst
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ -x ${2}.car ] && return 1
    # Move
    mv ${1}.car ${2}.car
}

car_copy() { # src dst
    # Validate arguments
    [ ! -x ${1}.car ] && return 1
    [ -x ${2}.car ] && return 1
    # Copy
    cp -R ${1}.car ${2}.car
}
