# ---------------------------------------------------------------------------
# Functions to print messages
# (use with '.' (source))

# TODO: merge with messages_aux.pl (at least format)

color__reset=`tput sgr0`
color__title_message=`tput bold; tput el`
color__mark=`tput setaf 6`
color__message=`tput bold; tput smul; tput el`
color__submessage=`tput el`
color__verbosemessage=`tput el`
color__ok_message=`tput bold; tput setaf 2; tput el`
color__fail_message=`tput bold; tput setaf 1; tput el`
color__b=`tput bold`

title_message() {
    echo "${color__title_message}--$*${color__reset}" 1>&2
}

message() {
    echo "${color__mark}::${color__reset}${color__message}$*${color__reset}" 1>&2
}

submessage() {
    echo "${color__mark}  ${color__reset}${color__submessage}$*${color__reset}" 1>&2
}

cmd_message() { # target, message
    local target=$1; shift
    echo "${color__mark}=>${color__reset} ${color__b}$target${color__reset}: ${color__submessage}$*${color__reset}" 1>&2
}

verbosemessage() {
    if [ x"${verbose}" = x"yes" ]; then
	echo "${color__mark}  ${color__reset}${color__verbosemessage}($*)${color__reset}" 1>&2
    else
	true
    fi
}

ok_message() {
    echo "${color__mark}::${color__reset}${color__ok_message}$*${color__reset}" 1>&2
}

fail_message() {
    echo "${color__mark}::${color__reset}${color__fail_message}$*${color__reset}" 1>&2
}

ok_item() {
    echo "  [${color__ok_message}OK${color__reset}] $*"
}

fail_item() {
    echo "  [${color__fail_message}??${color__reset}] $*"
}

b() {
    printf "%s" "${color__b}$*${color__reset}"
}
