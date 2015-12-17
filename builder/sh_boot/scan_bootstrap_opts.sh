# ---------------------------------------------------------------------------
# Extract bootstrap options (before a working Prolog engine is
# compiled). This includes parameters such as the C compiler, linker,
# compile and link options, and options for ciaoc.sta for compilation
# of boot ciao_builder.
#
# Author: Jose F. Morales (based on previous code)
# ---------------------------------------------------------------------------

# Input: arguments and options
# Output: configuration variables for autoboot.sh:builddir_configure_boot
#   boot__* options (see decomp_opt)
scan_bootstrap_opts() {
    local opt qopt nopt val arg

    cleanup_bootstrap_opts
    #
    if [ ! $# = 0 ] ; then
	# Read first 'core' options as 'boot', then 'boot'
	for q in core boot; do
	    for arg in "$@" ; do
		if decomp_boot_opt "$arg"; then
		    if [ x"$qopt" = x"$q" ]; then
			eval "boot__${Nopt}=\$val"
		    fi
		fi
	    done
	done
    fi
    # Set defaults
    if [ x"$boot__DEBUG_LEVEL" = x"" ]; then
	boot__DEBUG_LEVEL=nodebug
    fi
    if [ x"$boot__OS" = x"" ]; then
	boot__OS=`"$sh_src_dir"/config-sysdep/ciao_sysconf --os`
    fi
    if [ x"$boot__ARCH" = x"" ]; then
	boot__ARCH=`"$sh_src_dir"/config-sysdep/ciao_sysconf --arch`
	# Force 32-bit architecture
	if [ x"$boot__M32" = x"yes" ] ; then
	    case $boot__ARCH in
		Sparc64) boot__ARCH=Sparc ;;
		x86_64)  boot__ARCH=i686 ;;
		ppc64)   boot__ARCH=ppc ;;
		*) true ;; # assume 32-bit
	    esac
	fi
	# Force 64-bit architecture
	if [ x"$boot__M64" = x"yes" ] ; then
	    case $boot__ARCH in
		Sparc64) true ;;
		x86_64)  true ;;
		ppc64)   true ;;
		*) boot__ARCH=empty ;; # force error # TODO: emit error instead?
	    esac
	fi
    fi
    # Options for ciaoc
    if [ x"$boot__UNUSED_PRED_WARNINGS" = x"yes" ]; then
	boot__CIAOC_OPTS="$boot__CIAOC_OPTS --unused-pred-warnings"
    fi
}

cleanup_bootstrap_opts() {
    boot__CUSTOM_CC=""
    boot__CUSTOM_LD=""
    boot__EXTRA_CFLAGS=""
    boot__EXTRA_LDFLAGS=""
    boot__DEBUG_LEVEL=""
    boot__UNUSED_PRED_WARNINGS=""
    boot__M32=""
    boot__M64=""
    boot__OS=""
    boot__ARCH=""
    boot__CIAOC_OPTS=""
}

# Parse a 'boot' option, or 'core' option applicable to 'boot'
decomp_boot_opt() {
    local arg=$1
    if ! expr x"$arg" : x'\(--[^=][^=]*=..*\)' >/dev/null; then
	return 1
    fi
    opt=`expr x"$arg" : x'--\([^=][^=]*\)=..*'|sed -e s:-:_:g` # --OPT=_
    val=`expr x"$arg" : x'--[^=][^=]*=\(..*\)'` # --_=VAL
    qopt=`expr x"$opt" : x'\([^:]*\):..*'` # Q:_
    nopt=`expr x"$opt" : x'[^:]*:\(..*\)'` # _:N
    Nopt=`printf "%s" "$nopt" | tr '[:lower:]' '[:upper:]'` # N in uppercase
    # Filter options
    case $qopt in
	core|boot) true ;;
	*) return 1 ;;
    esac
    case $nopt in
        custom_cc|custom_ld|extra_cflags|extra_ldflags) true ;;
        debug_level) true ;;
        m32|m64|os|arch) true ;;
	*) return 1 ;;
    esac
    return 0
}

# TODO: 
#   - We need an option to enable system-wide rtchecks, etc.  The
#     current way to enable rtchecks is with this configuration
#     option:
#
#	--ciaosh-commands="\"set_prolog_flag(runtime_checks,yes)\""
#
#     However, it is just a hack that enables the flag in the toplevel.
# TODO: Synchronize with definitions at the Prolog side


