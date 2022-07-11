#!/bin/sh
#
#  ciao-boot.sh
#
#  Boot the Ciao builder (with or without sources)
#
#  Copyright (C) 2015-2022 Ciao Developer Team
#

set -u

if [ "$0" != "sh" ]; then # Boot from sources
    # Physical directory where the script is located
    _base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
            cd "$d";done;cd "$(dirname "$e")";pwd -P)
    # Builder boot
    _boot="$_base"/builder/sh_boot/builder_boot.sh
    if [ -x "$_boot" ]; then exec "$_boot" "$@"; fi
fi
# Otherwise continue with network-based installer... (below)

# ---------------------------------------------------------------------------
# The network-based installer automatically downloads and installs a
# Ciao release selected by the user. It extends builder installation
# 'target' with the notion of 'release' and 'tag', and allows using
# precompiled parts when available (binaries, documentation, etc.).
#
# It can run in a fully automatic mode (when the target and parameters
# are fully specified) or (partially) interactivelly (when the target
# is not specified).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Release info

# input: release os arch target
# output:
#  - `tag` that identifies this source version or binary release
#  - `a__*`: `v__*` is allowed

release_query_info() {
    case "$v__release" in
        stable) tag=v1.21.0-m3 ;;
        beta)   tag=v1.21.0-m3 ;;
        latest) tag=master ;;
    esac
    # Set other defaults based on release
    if [ "$v__devenv" = "yes" ]; then
        a__prebuilt_bin=no # TODO: no prebuilt bin for devenv yet
        case "$tag" in
            master) a__prebuilt_docs=no; a__with_docs=yes ;;
            *) a__prebuilt_docs=yes; a__with_docs=no ;;
        esac
    else
        case "$os$arch" in
            DARWINaarch64) a__prebuilt_bin=no ;; # TODO: not yet
            *) a__prebuilt_bin=yes ;;
        esac
        a__prebuilt_docs=no; a__with_docs=no
    fi
}

# Select URL for fetching sources and releases
select_url() { # requires: cfg, tag
    _url_archive="https://github.com/ciao-lang/ciao/archive"
    _url_releases="https://github.com/ciao-lang/ciao/releases/download"
    case "$tag" in # extract version from tag (when needed)
        master) _vers=$tag ;;
        v*) _vers=${tag%%-*}; _vers=${_vers#?} ;;
    esac
    if [ "$tag" = "master" ]; then # no release for head
        url="$_url_archive/refs/heads/$tag.tar.gz"
        unset urldocs # (error if used)
    else
        if [ "$cfg" = "source" ]; then
            url="$_url_archive/refs/tags/$tag.tar.gz"
        else
            url="$_url_releases/$tag/ciao-$_vers-$cfg.tar.gz"
        fi
        urldocs="$_url_releases/$tag/ciao-$_vers-docs.tar.gz"
    fi
}

# ---------------------------------------------------------------------------
# IO helper functions

errmsg() {
    printf "%s\n" "$*" 1>&2
}
errcat() {
    cat 1>&2
}
normal_message() {
    errmsg "   $*"
}

check_tty() {
    if ! [ -t 1 ] ; then
        errcat <<EOF

ERROR: No tty was detected, aborting

Please use '--help' for help.

EOF
        exit 1
    fi
}

ask() { # question [valid answers] (first is default)
    _q="$1"
    shift
    while true; do
        printf "%s" "$_q" 1>&2 # print question, wait answer
        read answer < /dev/tty
        if _valid_answer "$@"; then
            break
        else
            errmsg "Invalid answer. Please try again."
        fi
    done
}
_valid_answer() { # valid answers (first is default)
    if [ "$answer" = "" ]; then
        answer="$1"
        return 0 # OK (use default)
    fi
    for _v in "$@"; do
        if [ "$answer" = "$_v" ]; then
            return 0 # OK
        fi
    done
    return 1 # Wrong
}

# ---------------------------------------------------------------------------
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
        arm64) arch=aarch64 ;;
    esac
}

# ---------------------------------------------------------------------------

# (source or binary)
fetch_url() {
    normal_message "fetching core from $url"
    curl -sSfL "$url" | tar -xz --strip-components 1 -C "$ciaoroot" -f -
}

# (docs)
fetch_urldocs() {
    normal_message "fetching docs from $urldocs"
    mkdir -p "$ciaoroot"/build
    curl -sSfL "$urldocs" | tar -xz -C "$ciaoroot"/build -f -
    # Link to site dir docs (for ciao-serve) # TODO: move
    mkdir -p "$ciaoroot"/build/site/ciao/build
    ln -s ../../../doc "$ciaoroot"/build/site/ciao/build/doc
}

# ---------------------------------------------------------------------------

fetch_and_boot() { # [BOOTOPTS]
    # Prepare for download
    # TODO: split tar.gz into source, bin-$os$arch, etc. so that there is
    #   no overlapping and we can do multi-architecture installs
    if [ -x "$ciaoroot" ]; then
        _ciaoroot_bak="$ciaoroot"-`date +%Y%m%d%H%M%S`
        normal_message "backing up previous installation (as `basename $_ciaoroot_bak`)"
        # TODO: remove ONLY if the directory is empty or this is a Ciao installation (e.g., ciao-boot.sh exists)
        mv "$ciaoroot" "$_ciaoroot_bak"
    fi
    mkdir -p "$ciaoroot"
    # Download
    select_url
    fetch_url
    if ! [ -x "$ciaoroot"/builder/sh_boot/builder_boot.sh ]; then
        errcat <<EOF

ERROR: Could not download a valid $cfg image for $tag

Please check your network connection and the availability of the
selected release at: https://github.com/ciao-lang/ciao/releases

EOF
        exit 1
    fi
    if [ "$v__prebuilt_docs" = yes ]; then
        fetch_urldocs
    fi

    # Boot
    cd "$ciaoroot" # (just make sure we are not in a weird directory, it should not be needed)
    if [ "$v__prebuilt_bin" = yes ]; then
        CIAO_PREBUILT_CFG=$cfg "$ciaoroot"/builder/sh_boot/prebuilt_boot.sh "$@"
    else
        "$ciaoroot"/builder/sh_boot/builder_boot.sh "$@"
    fi
}

# ---------------------------------------------------------------------------
# Selection of installation options (generic)

opts_ask() {
    while true; do
        ask "Proceed [yes, no = customize] (yes)? " yes no
        if [ "$answer" = yes ]; then
            break # accept and continue
        fi
        errmsg ""; errmsg "All right! Let's customize your installation... 🔧"; errmsg ""
        opts_select
        opts_summary
    done
}

opt_summary() {
    o=$1
    if eval ! [ -z '"$v__'$o'"' ]; then eval errmsg '"   $m__'$o'""... $v__'$o'"'; fi
}

# Select option
#   interactive=no|maybe  Guess parameters (do not ask yet)
#   interactive=yes       Ask
opt_select() {
    o=$1
    if [ "$interactive" = yes ]; then
        if eval [ '"$a__'$o'"' = no ]; then # not allowed, do not ask
            eval v__$o=no
        else # ask
            eval ask '"$q__'$o'"' '$o__'$o
            eval v__$o='$answer'
        fi
    else
        if eval [ -z '"$v__'$o'"' ] && eval ! [ '"$a__'$o'"' = '""' ]; then 
            eval v__$o='$a__'$o # enable if possible
        elif eval [ '"$v__'$o'"' = yes ] && eval [ '"$a__'$o'"' = no ]; then # not possible
            errmsg "WARNING: '$o' cannot not be enabled, assuming 'no'"
            eval v__$o=no # force no
        fi
    fi
}

# ---------------------------------------------------------------------------
# Dependency checking (generic)

has_cmd() {
    command -v "$1" > /dev/null 2>&1
}
missing() {
    errcat <<EOF    

ERROR: Command '$1' is not found in the path

The command above is required for the selected installation. Please
consult the required dependencies at the installation instructions for
your operating system.

EOF
    exit 1
}

# ---------------------------------------------------------------------------
# Installation options

opts_usage() {
    errcat <<EOF
  --[no-]prebuilt-[bin|docs]   Use prebuilt components (when available)
  --release=[...]              Release to be installed
  get devenv                   Full installation (development environment)
  local-install                Minimal installation (compiler, libs, ...)
EOF
}

opts_def() {
    q__release="Which release would you like to install [stable,beta,latest] (stable)? "
    o__release="stable beta latest"
    a__release="stable" # (default)
    #
    m__devenv="Full development environment"
    q__devenv="Install the full development environment [yes,no] (yes)? "
    o__devenv="yes no"
    a__devenv="yes" # (default)
    #
    m__prebuilt_bin="Use prebuilt binaries"
    q__prebuilt_bin="Use prebuilt binaries [yes,no] (yes)? "
    o__prebuilt_bin="yes no"
    #
    m__prebuilt_docs="Use prebuilt docs"
    q__prebuilt_docs="Use prebuilt documentation [yes,no] (yes)? "
    o__prebuilt_docs="yes no"
    #
    m__with_docs="Documentation generation"
    q__with_docs="Enable documentation generation [no,yes] (no)? "
    o__with_docs="no yes"
    a__with_docs="" # (empty if not specified, use system default)
    #
    m__update_shell="Update shell profiles"
    q__update_shell="Update shell profiles (.bashrc, .cshrc, .zshrc) [yes,no] (yes)? "
    o__update_shell="yes no"
    a__update_shell="" # (empty if not specified, use system default)
}

opts_reset() {
    v__release=
    v__devenv=
    v__prebuilt_bin=
    v__prebuilt_docs=
    v__with_docs= # (use default)
    v__update_shell= # (use default)
}

# Get parameters from arguments
# output:
#  - v__*
#  - opts_n: number of options arguments (to be shifted)
opts_parse() { # [OPTS]
    opts_n=0
    while [ $# != 0 ]; do
        case "$1" in
            --help) usage; exit 0 ;;
            # TODO: turn into a bootarg?
            --prebuilt-bin) v__prebuilt_bin=yes ;;
            --no-prebuilt-bin) v__prebuilt_bin=no ;;
            --prebuilt-docs) v__prebuilt_docs=yes ;;
            --no-prebuilt-docs) v__prebuilt_docs=no ;;
            # Select release
            --release=*) v__release=${1#--release=} ;;
            # Unknown
            --*) errcat <<EOF
ERROR: Network-based installation does not recognize argument '$1'

Please use '--help' for help.

EOF
                 exit -1 ;;
            *) return 0 ;;
        esac
        shift; opts_n=$((opts_n + 1))
    done
}

# Select option values (based on 'interactive')
opts_select() {
    # (step 1)
    opt_select "release"
    opt_select "devenv"
    release_query_info
    # (step 2)
    opt_select "prebuilt_bin"
    opt_select "prebuilt_docs"
    opt_select "with_docs"
    opt_select "update_shell"
    # (step 3)
    # TODO: allow custom 'cfg' and 'ciaoroot'?
    if [ "$v__prebuilt_bin" = yes ]; then cfg="$os$arch"; else cfg="source"; fi
    ciaoroot=$HOME/.ciaoroot/$tag
    if [ "$interactive" = yes ]; then
        errmsg "Done!"; errmsg ""
    fi
}

# Summary
opts_summary() {
    errmsg "The options for this installation are:"; errmsg ""
    errmsg "   Selected release... $tag ($v__release)"
    opt_summary "devenv"
    opt_summary "prebuilt_bin"
    opt_summary "prebuilt_docs"
    opt_summary "with_docs"
    opt_summary "update_shell"
    errmsg "   Installation root... $ciaoroot"
    errmsg ""
}

# (once installation finishes)
opts_cheers() {
    if [ "$v__update_shell" = "no" ]; then
        errcat <<EOF
This installation is not enabled by default.
Please enable it manually with (bash, zsh):
  eval \$($ciaoroot/build/bin/ciao-env --sh)
or (csh):
  eval \`$ciaoroot/build/bin/ciao-env --csh\`

EOF
    fi
}

deps_check() {
    # Quickly check dependencies before we continue
    has_cmd curl || missing "'curl' command"
    # ! has_cmd rlwrap && missing "Command rlwrap"
    if [ "$v__devenv" = "yes" ]; then
        has_cmd emacs || missing "'emacs' command"
    fi
    if [ "$v__prebuilt_bin" = "no" ]; then
        has_cmd gcc || has_cmd clang || missing "C compiler (gcc, clang, etc.)"
        has_cmd make || has_cmd gmake || missing "'make' command"
    fi
    if [ "$v__with_docs" = "yes" ]; then
        has_cmd bibtex || missing "'bibtex' command"
        has_cmd makeinfo || missing "'makeinfo' command"
        has_cmd convert || missing "'convert' command"
    fi
}

# Peek parameters from boot options (basically for dependency checking)
bootargs_peek() {
    while [ $# != 0 ]; do
        case "$1" in
            # allowed targets
            get) if [ "$2" = devenv ]; then v__devenv=yes; fi; break ;;
            local-install) v__devenv=no; break ;;
            --builder:with_docs=*) v__with_docs=${1#--builder:with_docs=}; shift ;;
            --core:update_shell=*) v__update_shell=${1#--core:update_shell=}; shift ;;
            *) shift ;;
        esac
        shift
    done
    if [ -z "$v__devenv" ]; then interactive=maybe; fi # No target? go interactive
}
# Set boot options from parameters
bootargs_set() {
    bootargs=
    if [ "$v__devenv" = "yes" ]; then
        bootargs="$bootargs get devenv"
    else
        bootargs="$bootargs local-install"
    fi
    if [ "$v__update_shell" = "no" ]; then
        bootargs="$bootargs --core:update_shell=no"
    fi # (otherwise use default)
    if [ "$v__with_docs" = "no" ]; then
        bootargs="$bootargs --builder:with_docs=no"
    fi # (otherwise use default)
}

# ---------------------------------------------------------------------------

banner() {
    reset=$(printf '\033[0m')
    cC=$(printf '\033[34m');cI=$(printf '\033[91m')
    cA=$(printf '\033[93m');cO=$(printf '\033[92m')
    errcat <<EOF
  ${cC}   ▄▄▄ $reset
  ${cC} ▄▀   ▀${cI} ▀  ${cA} ▄▄▄  ${cO} ▄▄▄ $reset        Network-based installer
  ${cC} █     ${cI} █  ${cA}█   █ ${cO}█   █$reset        Detected OS: $os
  ${cC}  ▀▄▄▄▀${cI} ▀▄▄${cA}▀▄▄▀█▄${cO}▀▄▄▄▀$reset        Detected architecture: $arch
EOF
}

usage() {
    banner
    errcat <<EOF

Usage:

  curl https://ciao-lang.org/boot -sSfL | sh
  curl https://ciao-lang.org/boot -sSfL | sh -s -- [OPTS]

where OPTS are:

  --help                       This message

EOF
    opts_usage
    errmsg ""
}

main() { # [OPTS]
    opts_def
    get_os_arch

    # User input (command line) for this instalation
    interactive=no
    opts_reset
    opts_parse "$@"; shift "$opts_n"
    bootargs_peek "$@"

    # Option selection
    banner
    if [ $interactive = maybe ]; then
        # TODO: combine with the '--interactive' flag in the builder?
        check_tty
        errcat <<EOF
╭───────────────────────────────────────────────────────────────╮
│ Welcome to the interactive Ciao installer 🖖                  │
│ [Press Ctrl-C to cancel or 'Enter' to accept defaults]        │
╰───────────────────────────────────────────────────────────────╯
EOF
    else
        errmsg ""
    fi
    opts_select
    opts_summary
    if [ $interactive = maybe ]; then
        interactive=yes # Really ask now
        opts_ask
        bootargs_set; set -- $bootargs
    fi

    deps_check
    errmsg "Installing... 🤖"; errmsg ""
    if fetch_and_boot "$@"; then
        errmsg ""; errmsg "Installation is completed! 🥳"
        opts_cheers
        exit 0
    fi
    exit $?
}

# The end of the script must be this function call.
# It ensures that we do not execute incomplete (and dangerous!) code
# if "curl" is interrupted.
main "$@"

