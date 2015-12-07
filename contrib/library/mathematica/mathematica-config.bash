#!/bin/bash

config_darwin() {

    # Try to find mathematica with which
    mathkernel="$(which MathKernel)"

    if [ x"${mathkernel}" == x"" ]; then
	# Try to find mathematica with spotlite
	mathkernel="$(mdfind "(kMDItemDisplayName == MathKernel)" | grep Executables | head  -n1)"
    fi

    if [ x"${mathkernel}" == x"" ]; then
	# Try to find mathematic in common places
	mathkernel="$(ls ~/Applications/Mathematica.app/Contents/MacOS/MathKernel   \
                         ~/Applications/*/Mathematica.app/Contents/MacOS/MathKernel \
	                 /Applications/Mathematica.app/Contents/MacOS/MathKernel    \
                         /Applications/*/Mathematica.app/Contents/MacOS/MathKernel  \
                         2> /dev/null | head -n1)"
    else
	while [ -L "${mathkernel}" ]; do
	    xcc="$(readlink  "${mathkernel}")"
	done
    fi
    
    local basedir="$(dirname "$(dirname "${mathkernel}")")"
    includedir="${basedir}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions"
    LIBS="${basedir}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions/libMli3.a"
}

config_linux() {

    # Try to find mathematica with which
    mathkernel="$(readlink -f "$(which MathKernel)")"

    if [ x"${mathkernel}" == x"" ]; then
	# Try to find mathematic in common places
	mathkernel="$(ls /usr/local/Wolfram/Mathematica/*/Executables/MathKernel \
			/opt/Wolfram/Mathematica/*/Executables/MathKernel \
                        2> /dev/null | head -n1)"
    fi

    local basedir="$(dirname "$(dirname "${mathkernel}")")"
    includedir="${basedir}/SystemFiles/Links/MathLink/DeveloperKit/Linux/CompilerAdditions/"
    libs="${basedir}/SystemFiles/Links/MathLink/DeveloperKit/Linux/CompilerAdditions/libML32i3.a"
}


help() {
    echo "Usage: mathematica-config [OPTION]
Options:
  -h, --help        prints this help text to stdout
  --kernel          prints the command to start kernel
  --cppflags        prints preprocessor flags
  --ldflags         prints linker flags
 "
}

if [ "$(uname)" == "Darwin" ]; then
    config_darwin
elif [ "$(uname)" == "Linux" ]; then
    config_linux
else
    echo mathematic-config not implemented on $(uname)
fi

if [ x"${mathkernel}" == x"" ]; then
   echo "Mathematica not detected" >&2
   exit 1
fi

case ${1} in
    "--kernel")
	printf "%s" "${mathkernel}"
	;;
    "--cppflags")
	printf "%q" "-I${includedir}"
	;;
    "--ldflags")
	printf "%q -lstdc++ -framework Foundation" "${libs}"
	;;
    "-h" | "--help")
	help
	;;
    *)
	help
	exit 1
	;;
esac
