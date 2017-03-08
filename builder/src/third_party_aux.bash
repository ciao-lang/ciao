#!/bin/bash
#
#  third_party_aux.bash
#
#  Alternative implementation for installation of third-party code for
#  Ciao bindings
#
#  Copyright (C) 2017 Jose F. Morales
#

# TODO: merge with the Prolog version

# Exit immediately if a simple command exits with a non-zero status
set -e

# --------------------------------------------------------------------------

if [ "$THIRDPARTY" = "" ]; then
    cat <<EOF
ERROR: THIRDPARTY directory missing (use 'ciao build')
EOF
    exit 1
fi

cachedir="$THIRDPARTY/cache"
storedir="$THIRDPARTY/store"
srcdir="$THIRDPARTY/src"

# --------------------------------------------------------------------------

# Uncompress the archive, striping first path component
# handle both .zip and .tar.gz
function uncompress_strip1() { # source target
    case $1 in
	*.tgz|*.tar.gz) uncompress_tgz "$1" "$2" ;;
	*.zip) uncompress_zip "$1" "$2" ;;
    esac
}

function uncompress_zip() { # source target
    local temp=`mktemp -d`
    unzip -q -d "$temp" "$1"
    mkdir -p "$2"
    local f=("$temp"/*) # (array init)
    if (( ${#f[@]} == 1 )) && [[ -d "${f[0]}" ]] ; then	# one element
        mv "$temp"/*/* "$2"
    else # more than one element
        mv "$temp"/* "$2"
    fi
    rmdir "$temp"/* "$temp"
}

function uncompress_tgz() { # source target
    tar -xz --strip-components 1 -f "$1" -C "$2"
}

# --------------------------------------------------------------------------

function fetch_pkg() {
    # Ensure that cachedir is created
    mkdir -p "$cachedir"

    # Download pkg
    rm -f "$cachedir/$pkg_tarfile"
    curl -L "$pkg_url" -o "$cachedir/$pkg_tarfile"
}

function uncompress_pkg_bin() {
    # Cleanup storedir and uncompress
    rm -rf "$storedir/$pkg_name"
    mkdir -p "$storedir/$pkg_name"
    uncompress_strip1 "$cachedir/$pkg_tarfile" "$storedir/$pkg_name"
}

function uncompress_pkg_src() {
    # Cleanup srcdir and uncompress
    rm -rf "$srcdir/$pkg_name"
    mkdir -p "$srcdir/$pkg_name"
    uncompress_strip1 "$cachedir/$pkg_tarfile" "$srcdir/$pkg_name"
}

function fix_dylibs() {
    local pkg_libpath pkg_libpathAct
    # Fix install dir (it was /usr/local)
    case "$CIAO_OS" in
	LINUX)
	    pushd "$storedir/$pkg_name" > /dev/null 2>&1
	    /sbin/ldconfig -n "lib"
            # Link name without version
	    if [ x"$pkg_libfileAct" != x"" ]; then
		ln -sf "$pkg_libfile" "lib/$pkg_libfileAct"
	    fi
	    popd > /dev/null 2>&1
	    ;;
	DARWIN)
	    pkg_libpath="$storedir/$pkg_name/lib/$pkg_libfile"
	    pkg_libpathAct="$storedir/$pkg_name/lib/$pkg_libfileAct"
	    install_name_tool -id "$pkg_libpath" "$pkg_libpath"
            # Link name without version
	    if [ x"$pkg_libfileAct" != x"" ]; then
		ln -sf "$pkg_libpath" "$pkg_libpathAct"
	    fi
	    ;;
    esac
}

# ---------------------------------------------------------------------------

function gen_config_auto() { # output
    local RPATH_OPTS=
    case "$CIAO_OS" in
	LINUX)
	    RPATH_OPTS="'-Wl,-rpath,$storedir/$pkg_name/lib,-rpath,\\'\$ORIGIN\\'',"
	    ;;
    esac
    cat > "$1" <<EOF
:- extra_compiler_opts([
	'-I$storedir/$pkg_name/include'
	]).
:- extra_linker_opts(['-L.']).
:- extra_linker_opts([
	$RPATH_OPTS
        '-L$storedir/$pkg_name/lib'
	]).

:- use_foreign_library(['$pkg_lib']).
EOF
}

# ===========================================================================

function install_dist() { # Mode=bin|src
    if [ -x "$storedir/$pkg_name" ]; then
	# echo "$pkg_name already downloaded" 1>&2
	return 0
    fi

    if [ "$1" = bin ]; then
	pkg_bin_origin
    else # src
	pkg_src_origin
    fi
    fetch_pkg
    if [ "$1" = bin ]; then
	uncompress_pkg_bin
	pkg_fix_bin
    else # src
	# Build at srcdir
	uncompress_pkg_src
	pushd "$srcdir/$pkg_name" > /dev/null 2>&1
	pkg_build
	popd > /dev/null 2>&1
	pushd "$srcdir/$pkg_name" > /dev/null 2>&1
	# Cleanup storedir and install
	rm -rf "$storedir/$pkg_name"
	mkdir -p "$storedir/$pkg_name"
	pkg_install
	popd > /dev/null 2>&1
    fi
    fix_dylibs
}

# ===========================================================================

# Import 3rd-party package definition
source "$1"
shift

# Run action
case $1 in
    install_bin_dist) install_dist bin ;;
    install_src_dist) install_dist src ;;
    gen_conf) shift; gen_config_auto "$1" ;;
    *)
	echo "ERROR: Unknown action" 1>&2
	exit 1
esac
