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
#   core__CUSTOM_CC
#   core__CUSTOM_LD
#   core__EXTRA_CFLAGS
#   core__EXTRA_LDFLAGS
#   CIAOC_OPTS
#   BOOTDBG
scan_bootstrap_opts() {
    local opt val arg
    core__CUSTOM_CC=""
    core__CUSTOM_LD=""
    core__EXTRA_CFLAGS=""
    core__EXTRA_LDFLAGS=""
    CIAOC_OPTS=""
    BOOTDBG=no
    #
    if [ ! $# = 0 ] ; then
	for arg in "$@" ; do
	    if expr x"$arg" : x'\(--[^=][^=]*=..*\)' >/dev/null  ; then
		opt=`expr x"$arg" : x'--\([^=][^=]*\)=..*'|sed -e s:_:-:g`
		val=`expr x"$arg" : x'--[^=][^=]*=\(..*\)'`
		case "$opt" in
                    "core:custom-cc")
			core__CUSTOM_CC="$val" ;;
                    "core:custom-ld")
			core__CUSTOM_LD="$val" ;;
                    "core:extra-cflags")
			core__EXTRA_CFLAGS="$val" ;;
		    "core:extra-ldflags")
			core__EXTRA_LDFLAGS="$val" ;;
		    "core:unused-pred-warnings")
			# (option for ciaoc)
			test x"$val" = x"yes" && add_ciaoc_opt "--unused-pred-warnings" ;;
		    "bootdbg")
			BOOTDBG="$val" ;;
		esac
	    fi
	done
    fi
}

add_ciaoc_opt() {
    CIAOC_OPTS="$CIAOC_OPTS $1"
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


