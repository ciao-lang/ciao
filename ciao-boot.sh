#!/bin/sh
#
#  ciao-boot.sh
#
#  Boot the Ciao builder (with or without sources)
#
#  Copyright (C) 2015-2017 Ciao Developer Team
#

# Boot from existing sources
if [ "$0" != "sh" ]; then
    # Physical directory where the script is located
    _base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
         cd "$d";done;cd "$(dirname "$e")";pwd -P)
    boot="$_base"/builder/sh_boot/builder_boot.sh
    if [ -x "$boot" ]; then exec "$boot" "$@"; fi
fi
# otherwise try boot from network mode (below)

# ---------------------------------------------------------------------------
# Origins and default versions

set_defaults() {
    default_bundle=ciao
    default_vers_bin=1.16.0-alpha.2
    default_vers_src=master
    default_prebuilt=yes # TODO: make it depend on selected version?
    default_url_src=https://github.com/ciao-lang/ciao/archive
    default_url_bin=https://dl.bintray.com/ciao-lang/builds
}

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
    vers=$default_vers_src
    url=$default_url_src/$vers.tar.gz
}
select_bin() {
    get_os_arch
    cfg=$os$arch
    vers=$default_vers_bin
    url=$default_url_bin/ciao-$vers-$cfg.tar.gz
}

fetch_url() {
    curl -SfL "$url" | tar -xz --strip-components 1 -C "$ciaoroot" -f -
}

help() {
    get_os_arch
    cat <<EOF
   ▄▄▄
 ▄▀   ▀ ▀   ▄▄▄   ▄▄▄     Default version: $default_vers_bin
 █      █  █   █ █   █    Detected OS: $os
  ▀▄▄▄▀ ▀▄▄▀▄▄▀█▄▀▄▄▄▀    Detected architecture: $arch

Running ciao-boot.sh in network installation mode!

Please visit https://ciao-lang.org for more information.
Code will be installed under ~/.ciaoroot directory.

Examples:

  # local-install from sources
  $bootsh --no-prebuilt local-install

  # local-install using prebuilt code
  $bootsh --prebuilt local-install

  # Do not update your shell environment
  $bootsh --prebuilt local-install --core:update_bashrc=no --core:update_cshrc=no

  # Activate the existing installation (bash)
  source ~/.ciaoroot/$default_vers_bin/core/etc/DOTprofile

  # Activate the existing installation (csh)
  source ~/.ciaoroot/$default_vers_bin/core/etc/DOTcshrc

EOF
}

# TODO: use a local script instead (e.g., ~/.ciaoroot/env.sh)

## #  # Activate some installation (bash) -- dynamic, it will consider CIAOPATH
## #  eval \$($bootsh env)
## 
## # TODO: move somewhere else (ciao.skel?)
## # TODO: add paths under CIAOPATH too
## show_env() {
##     . "$ciaoroot/core/etc/DOTprofile"
##     cat <<EOF
## export PATH="$PATH"
## export MANPATH="$MANPATH"
## export INFOPATH="$INFOPATH"
## #
## export CIAOENGINE="$CIAOENGINE"
## export CIAOHDIR="$CIAOHDIR"
## export CIAOLIB="$CIAOLIB"
## 
## # Run this command to setup your environment:
## # 
## # eval "\$($bootsh env)"
## EOF
## }

fetch_and_boot() { # args
    set_defaults

    if [ $# = 0 ]; then
	help
	exit 1
    fi

    bundle=$default_bundle
    prebuilt=$default_prebuilt
    case $1 in
	--prebuilt) shift; prebuilt=yes ;;
	--no-prebuilt) shift; prebuilt=no ;;
    esac
    if [ $prebuilt = yes ]; then
	select_bin
    else
	select_src
    fi
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
    normal_message "fetching $bundle ($vers) from $url"
    fetch_url
    # Boot
    cd "$ciaoroot" # TODO: really needed now?
    if [ $prebuilt = yes ]; then
	CIAO_PREBUILT_CFG=$cfg exec "$ciaoroot"/builder/sh_boot/prebuilt_boot.sh "$@"
    else
	exec "$ciaoroot"/builder/sh_boot/builder_boot.sh "$@"
    fi
}

# How this command was called (for help messages)
if [ "$0" = "sh" ]; then
    # booturl="https://raw.githubusercontent.com/ciao-lang/ciao/master/ciao-boot.sh"
    # bootsh="curl $booturl -sSf | sh -s --"
    booturl="https://ciao-lang.org/boot"
    bootsh="curl $booturl -sSfL | sh -s --"
else
    bootsh=$0
fi

# The end of the script must be this function call.
# It ensures that we do not execute incomplete (and dangerous!) code
# if "curl" is interrupted.
fetch_and_boot "$@"

