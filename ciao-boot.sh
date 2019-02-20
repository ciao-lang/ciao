#!/bin/sh
#
#  ciao-boot.sh
#
#  Boot the Ciao builder (with or without sources)
#
#  Copyright (C) 2015-2019 Ciao Developer Team
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
    default_vers_bin=1.18.0
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
    curl --progress-bar -SfL "$url" | tar -xz --strip-components 1 -C "$ciaoroot" -f -
}

has() {
    command -v "$1" > /dev/null 2>&1
}
missing() {
    cat <<EOF    

ERROR: $1 not found

The command above is required for the selected installation. Please
consult the required dependencies at the installation instructions for
your operating system.

EOF
    exit 1
}

interactive() {
    # TODO: Experimental! This should be combined and simplified with
    # the '--interactive' flag in the builder.
    get_os_arch
    cat <<EOF
   ▄▄▄
 ▄▀   ▀ ▀   ▄▄▄   ▄▄▄     Default version: $default_vers_bin
 █      █  █   █ █   █    Detected OS: $os
  ▀▄▄▄▀ ▀▄▄▀▄▄▀█▄▀▄▄▄▀    Detected architecture: $arch

EOF
    if ! [ -t 1 ] ; then
	cat <<EOF
No tty was detected. Aborting.

Please visit https://ciao-lang.org for more information or try the
non-interactive installation.

EOF
	exit 1
    fi
    cat <<EOF
Welcome to the interactive network installation for Ciao!
(Press C-c to cancel)

EOF
    printf "Install the development environment? (yes/no) "
    read use_devenv < /dev/tty

    if [ x"$use_devenv" = x"no" ]; then
	printf "Install a prebuilt distribution? (yes/no) "
	read prebuilt < /dev/tty
    else
	# TODO: 'get' not working in prebuilt
	prebuilt=no
    fi

    if [ x"$use_devenv" = x"no" ]; then
	with_docs=yes # Just use default
    else
	printf "Install local documentation? (no/yes) "
	read with_docs < /dev/tty
    fi

    printf "Enable this installation by default? (yes/no) "
    read update_shell < /dev/tty

    # Quickly check dependencies before we continue
    has curl || missing "'curl' command"
    # ! has rlwrap && missing "Command rlwrap"
    if [ x"$prebuilt" = x"no" ]; then
	has gcc || has clang || missing "C compiler (gcc, clang, etc.)"
	has make || has gmake || missing "'make' command"
    fi
    if [ x"$use_devenv" = x"no" ]; then
	true
    else
	has emacs || missing "'emacs' command"
	if [ x"$with_docs" = x"yes" ]; then
	    has bibtex || missing "'bibtex' command"
	    has makeinfo || missing "'makeinfo' command"
	    has convert || missing "'convert' command"
	fi
    fi

    opts=
    if [ x"$prebuilt" = x"no" ]; then
	opts="$opts --no-prebuilt"
    else
	true # opts=" --prebuilt"
    fi
    if [ x"$use_devenv" = x"no" ]; then
	cmd=" local-install"
    else
	cmd=" get devenv"
    fi
    flags=
    if [ x"$update_shell" = x"no" ]; then
	flags="$flags --core:update_bashrc=no --core:update_cshrc=no"
    else
	true
    fi
    if [ x"$with_docs" = x"yes" ]; then
	true
    else
	flags="$flags --builder:with_docs=no"
    fi

    cat <<EOF

Code will be installed under ~/.ciaoroot directory.
The selected installation is reproducible with:

  $bootsh$opts$cmd$flags

(Press enter to continue or C-c to cancel)
EOF
    read dummy < /dev/tty

    if ! ( fetch_and_boot $opts$cmd$flags ); then
	exit 1
    fi

    cat <<EOF

Installation is completed!
EOF
    if [ x"$update_shell" = x"no" ]; then
	cat <<EOF

Now you can enable this installation manually with (bash):
  eval \$(~/.ciaoroot/$default_vers_bin/build/bin/ciao-env --sh)
or (csh):
  eval \`~/.ciaoroot/$default_vers_bin/build/bin/ciao-env --csh\`
EOF
    fi
    echo
}

fetch_and_boot() { # args
    set_defaults

    if [ $# = 0 ]; then
	interactive
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
ERROR: '$vers' seems to be already installed at:

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

