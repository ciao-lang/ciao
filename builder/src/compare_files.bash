mode_is_save() {
    [ x"${1}" == x"save" ] || [ x"${1}" == x"save-no-ask" ]
}

difffiles() {
    local name
    local A
    local B
    local mode
    local ret
    local banner
    name=${1}
    A=${2}
    B=${3}
    mode=${4}
    ret=0
    banner="${name}"
    if [ ! -r ${A} ]; then
        # do nothing, source not found
        true
    elif [ ! -r ${B} ]; then
	if mode_is_save "${mode}"; then
	    # no saved file in save mode
	    cp ${A} ${B} && echo "Saved ${name}"
	else
	    # TODO: no saved file, assume it is correct?
	    fail_item "${banner} (no saved file)"
	fi
    elif diff ${A} ${B} > /dev/null 2>&1; then
	if mode_is_save "${mode}"; then
	    true
	else
	    ok_item "${banner}"
	fi
    else
	ret=1
	fail_item "${banner}"
	if [ x"${mode}" == x"brief" ]; then
	    wc ${A}
	    wc ${B}
	elif mode_is_save "${mode}"; then
	    if [ x"${mode}" == x"save-no-ask" ] || save_question "${name}"; then
	        cp ${A} ${B} && echo "Saved"
	    else
	        echo "Not saved"
	    fi
	else
	    wc ${A}
	    wc ${B}
	    if [ ! -z "${DIFFCMD:-}" ]; then
	        ${DIFFCMD} ${A} ${B}
	    elif which meld > /dev/null; then
# TODO: add option to choose
#	    if false; then
	        # Use meld if available
	        meld ${A} ${B} || { echo "(Press C-c again to abort or ENTER to continue)"; read; }
	    else
	        diff ${A} ${B}
	    fi
	fi
    fi
    return ${ret}
}

# ---------------------------------------------------------------------------

save_question() {
    local qst_what
    qst_what=$1
    while [ -t ]; do
	printf "%s" "Really save results for ${qst_what}? (y/n) "
	read qst_ok
	if [ x"$qst_ok" == x"y" ]; then
	    return 0
	elif [ x"$qst_ok" == x"n" ]; then
	    return 1
	else
	    echo "Unrecognized answer. Please type 'y' for 'yes' or 'n' for 'no'."
	fi
    done
}

