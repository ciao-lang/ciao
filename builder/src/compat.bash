# Functions to provide portability among Linux and BSD systems

unamestr=`uname`

# ---------------------------------------------------------------------------
# Modification time of a file

function modif_time() {
    if [ "$unamestr" == "Linux" ]; then
	stat -c %Y $1
    else
	# Assume we are in a BSD system (Darwin, FreeBSD, etc.)
	stat -f %m $1
    fi
}

# ---------------------------------------------------------------------------
# Size of a file

function size_of_file() { # file
    size_of_file0 `ls -la $1`
}
function size_of_file0() {
    echo "$5"
}

# ---------------------------------------------------------------------------
# Print some system info
# TODO: only works in linux!

system_info() {
    echo "DATE: `date`"
    if [ "$unamestr" == "Linux" ]; then
	cat <<EOF
 ---------------------------------------------------------------------------
EOF
	echo
	if [ -r /proc/cpuinfo ]; then
	    echo "CPUINFO:"
	    cat /proc/cpuinfo
	fi
	if [ -r /proc/version ]; then
	    echo "KERNEL VERSION:"
	    cat /proc/version
	    echo
	fi
	if which gcc > /dev/null; then
	    echo "gcc version:"
	    gcc --version
	fi
    else
	hwprefs -v \
	    os_class \
	    os_type \
	    machine_type \
	    memory_size \
	    cpu_type \
	    cpu_freq \
	    cpu_bus_freq \
	    memctl_type \
	    ioctl_type \
	    cpu_count \
	    cpu_ht	
    fi
}

