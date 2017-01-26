#!/bin/sh
#
#  ciao-boot.sh
#
#  Boot the Ciao builder (with or without sources)
#
#  Copyright (C) Ciao Developer team
#

# Boot from sources
if [ "$0" != "sh" ]; then
    # Physical directory where the script is located
    _base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
         cd "$d";done;cd "$(dirname "$e")";pwd -P)
    if [ -x "$_base"/builder/sh_boot/builder_boot.sh ]; then
	exec "$_base"/builder/sh_boot/builder_boot.sh "$@"
    fi
fi
# Otherwise, lets continue with the on-line installer...

# ---------------------------------------------------------------------------
# Origins and default versions

default_vers_bin=1.16.0-alpha.2
default_vers_src=master
default_url_src=https://github.com/ciao-lang/ciao/archive
default_url_bin=https://dl.bintray.com/ciao-lang/builds

# ---------------------------------------------------------------------------

normal_message() {
    echo "   $*" 1>&2
}

# Get normalized $os and $arch
get_os_arch() { # (simpler version of ciao_sysconf)
    os=`uname -s`
    arch=`uname -m`
    case "$os" in
	Linux) os=LINUX ;;
	CYGWIN_*|MSYS_NT*|MINGW32_NT*|MINGW64_NT*) os=Win32 ;;
	Darwin) os=DARWIN ;;
    esac
    case "$arch" in
	i[3456]86|i86pc) arch=i686 ;;
	x86_64|amd64) arch=x86_64 ;;
    esac
}

select_src() {
    prebuilt=no
    vers=$default_vers_src
    url=$default_url_src/$vers.tar.gz
}
select_bin() {
    prebuilt=yes
    get_os_arch
    cfg=$os$arch
    vers=$default_vers_bin
    url=$default_url_bin/ciao-$vers-$cfg.tar.gz
}

fetch_url() {
    curl -SfL "$url" | tar -xz --strip-components 1 -C "$ciaoroot" -f -
}

# TODO: move somewhere else
fix_symlinks() {
    # normal_message "relocating symbolic links..."
    # Fix some symbolic links in build/ directory
    cd "$ciaoroot"
    # TODO: meta_sh, etc. are not patched
    . "$ciaoroot/build/eng/ciaoengine/cfg/$cfg/meta_sh" # (for eng_srcpath)
    local old=$eng_srcpath # old srcpath at build host
    local new="$ciaoroot/core/engine" # new srcpath at this host
    local i f g
    for i in $(find "$ciaoroot/build" -type l); do
        f=$(readlink $i)
        g=$(printf "%s" $f | sed -e "s;^$old;$new;")
        if [ "$f" != "$g" ]; then
            unlink $i
            ln -s $g $i
        fi
    done
}
fix_dot_shell() {
    # These would not be needed with proper relocation
    cat >> "$ciaoroot/core/etc/DOTprofile" <<EOF
export CIAOENGINE="$ciaoroot/build/eng/ciaoengine/objs/$cfg/ciaoengine"
export CIAOHDIR="$ciaoroot/build/eng/ciaoengine/include"
export CIAOLIB="$ciaoroot/core"
EOF
}

# TODO: move somewhere else (ciao.skel?)
show_env() {
    . "$ciaoroot/core/etc/DOTprofile"
    get_os_arch
    cfg=$os$arch
    cat <<EOF
export PATH="$PATH"
export MANPATH="$MANPATH"
export INFOPATH="$INFOPATH"
#
export CIAOENGINE="$CIAOENGINE"
export CIAOHDIR="$CIAOHDIR"
export CIAOLIB="$CIAOLIB"

# Run this command to setup your environment:
# 
# eval "\$($bootsh env)"
EOF
}

# How this command was called (for help messages)
if [ "$0" = "sh" ]; then
    booturl="https://raw.githubusercontent.com/ciao-lang/ciao/master"
    bootsh="curl $booturl/ciao-boot.sh -sSf | sh -s --"
else
    bootsh=$0
fi

if [ $# = 0 ]; then
    cat <<EOF
Welcome to the on-line installer for Ciao!

Examples:

  # Do a local-install under ~/.ciaoroot
  $bootsh local-install

  # Do a local-install using prebuilt code
  $bootsh --prebuilt local-install

  # Do not update your shell environment
  $bootsh --prebuilt local-install --core:update_bashrc=no --core:update_cshrc=no

  # Activate the existing installation (bash)
  source ~/.ciaoroot/$default_vers_bin/core/etc/DOTprofile

  # Activate the existing installation (csh)
  source ~/.ciaoroot/$default_vers_bin/core/etc/DOTcshrc

  # Activate some installation (bash) -- dynamic, it will consider CIAOPATH
  eval \$($bootsh env)

EOF
    exit 1
fi

case $1 in
    --prebuilt)
	shift
	select_bin
	;;
    *)
	select_src
	;;
esac
ciaoroot=$HOME/.ciaoroot/$vers

# Other commands
case $1 in
    env)
	shift
	show_env
	exit 1
	;;
esac

# Prepare for download
# TODO: split tar.gz into source, bin-$os$arch, etc. so that there is
#   no overlapping and we can do multi-architecture installs
if [ -x "$ciaoroot" ]; then
    cat <<EOF
ERROR: already fetched '$vers' version under:

  $ciaoroot

Please remove it to force a new installation.

EOF
    exit 1
fi
mkdir -p "$ciaoroot"
# Download
if [ $prebuilt = yes ]; then
    normal_message "fetching '$vers' version (prebuilt)"
else
    normal_message "fetching '$vers' version (source)"
fi
fetch_url
cd "$ciaoroot" # (needed for installation)
if [ $prebuilt = yes ]; then
    # TODO: improve relocation
    #  - patch binaries instead?
    #  - prebuild in a better default location?
    #  - use a single CIAOROOT env var (instead of CIAOENGINE,CIAOHDIR,CIAOLIB)
    # TODO: touch all po,itf so that no recompilation happens
    # TODO: mingw version?
    # TODO: create exec stub (turn into static + lib)
    # TODO: create small ciaoengine (dynlink to so)
    
    # TODO: allow more commands
    case $1 in
	local-install) shift; cmd=install ;;
	install) shift; cmd=install ;;
	*) cat <<EOF
ERROR: Command $1 not allowed with --prebuilt

EOF
	   exit 1
	   ;;
    esac
    #
    cd "$ciaoroot" # (needed for installation)
    "$ciaoroot"/builder/sh_boot/builder_boot.sh rescan-bundles # (fix paths)
    "$ciaoroot"/builder/sh_boot/builder_boot.sh configure "$@"
    "$ciaoroot"/builder/sh_boot/builder_boot.sh build core.dot_shell # (fix paths)
    fix_dot_shell
    fix_symlinks
    exec "$ciaoroot"/builder/sh_boot/builder_boot.sh "$cmd"
else
    exec "$ciaoroot"/builder/sh_boot/builder_boot.sh "$@"
fi

