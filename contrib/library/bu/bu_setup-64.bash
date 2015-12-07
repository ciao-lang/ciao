#!/bin/bash

# Setting up BU - for Ciao/OptimComp 64-bits
#
# Jose Morales - jfmcjf@gmail.com
# Wed Dec 16 15:12:42 CET 2009
# ---------------------------------------------------------------------------

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------
# Load settings

if [ -r "$_base"/settings ]; then
    source "$_base"/settings
else
    cat > "$_base"/settings <<EOF
# Where PPL sources are located
pplsrcdir=/usr/local/src/ppl-0.10.2
# The prefix where PPL is installed
pplprefix=/usr/local
# The prefix where GMP is installed
gmpprefix=/usr/local
# Where Yices has been uncompressed
yicesdir=/usr/local/yices-1.0.26
# The directory where 'bu' source is
busrcdir=/usr/local/src/bu
# The directory where Ciao source is
ciaosrcdir=/usr/local/src/ciao
EOF
    cat <<EOF
An example settings file has been written. Please, modify it an rerun
this script.
EOF
    exit -1
fi

# ---------------------------------------------------------------------------
# Messages and input

function section() {
    cat <<EOF
---------------------------------------------------------------------------
  $*
---------------------------------------------------------------------------
EOF
}

function pause() {
    read
}

# ---------------------------------------------------------------------------
# Begin!

cat <<EOF
This script will guide through the compilation of BU using Ciao/OptimComp in 64-bits.
(Jose Morales - jfmcjf@gmail.com)
EOF
pause

# ---------------------------------------------------------------------------
section "Verifying that all the software is available"

# Verify that dependencies are OK, just be checking that some files
# are in those directories

some_missing_dep=no
function missing_dep() {
    echo "Error: $1 was not found in $2"
    some_missing_dep=yes
}
function verify_no_missing_dep() {
    if [ "$some_missing_dep" == "yes" ]; then
	cat <<EOF
Please, provide the location of each dependency by modifying the first
lines of this script ("${_base}/bu_setup-64.bash").
EOF
	exit -1
    fi
}

uname=`uname`
[ -r ${pplsrcdir}/README ] || missing_dep "PPL" "${pplsrcdir}"
[ -r ${yicesdir}/LICENSE ] || missing_dep "Yices" "${yicesdir}"
[ -r ${gmpprefix}/lib/libgmp.a ] || missing_dep "GMP" "${gmpprefix}"
[ -r ${busrcdir}/bu.pl ] || missing_dep "BU" "${busrcdir}"
verify_no_missing_dep

cat <<EOF
The script verified that:
  - The operating system is "${uname}"
  - GMP libraries are installed under "${gmpprefix}" (lib/, include/)
  - PPL source code is in "${pplsrcdir}" (lib/, include/)
  - Yices binary package is uncompressed in "${yicesdir}"
EOF
pause

# ---------------------------------------------------------------------------
section "Patching PPL"
cat <<EOF
As a first step, PPL must be patched to match the current Ciao
version. 
EOF
pause

cat <<EOF
Replace in "${pplsrcdir}/interfaces/Prolog/Ciao/ciao_cfli.hh" the following
line:
  #include <ciao_prolog.h>
by:
  #include "${ciaosrcdir}/core_OC/engine/engine__ciao_prolog.h"
EOF
pause

cat <<EOF
Replace in "${pplsrcdir}/interfaces/Prolog/Ciao/ppl_ciao.pl" the lines:
  :- extra_linker_opts('-L.libs').
  :- use_foreign_library(ppl_ciao).

  :- impl_defined(
  [
	ppl_version_major_2/2,
	ppl_version_minor_2/2,
  ...
  ]).
by:
  :- '\$pragma'(foreign_library_path('${pplprefix}/lib/ppl')).
  :- '\$pragma'(use_foreign_library(ppl_ciao)).
EOF
pause

# ---------------------------------------------------------------------------
section "Configure and install PPL" 
cat <<EOF
Now it is time to configure PPL installation:
$ cd ${pplsrcdir}
$ ./configure --with-prefix=${pplprefix} --with-libgmpxx-prefix=${gmpprefix} --enable-interfaces=ciao_prolog

PPL will be installed in "${pplprefix}" (--with-prefix=/usr/local is the default option)
EOF
pause

cat <<EOF
Install it with the following command (it may take a while):
(YOU MUST HAVE ADMIN RIGHTS IN THIS STEP)
$ sudo make install
EOF
pause

if [ "${uname}" == "Darwin" ]; then
    cat <<EOF
In Mac OS X, a last compilation step is not properly
done. Technically, PPL creates a 'bundle' instead of a 'dylib'.  Until
PPL is fixed, you need to rebuild the dylib by hand:

$ cd ${pplsrcdir}/interfaces/Prolog/Ciao
$ g++ -Wl,-undefined -Wl,dynamic_lookup -o .libs/libppl_ciao.dylib -dynamiclib  .libs/ciao_efli.o .libs/ppl_prolog_common.o .libs/ppl_prolog_Polyhedron.o .libs/ppl_prolog_Grid.o .libs/ppl_prolog_Rational_Box.o .libs/ppl_prolog_BD_Shape_mpz_class.o .libs/ppl_prolog_BD_Shape_mpq_class.o .libs/ppl_prolog_Octagonal_Shape_mpz_class.o .libs/ppl_prolog_Octagonal_Shape_mpq_class.o .libs/ppl_prolog_Constraints_Product_C_Polyhedron_Grid.o .libs/ppl_prolog_Pointset_Powerset_C_Polyhedron.o .libs/ppl_prolog_Pointset_Powerset_NNC_Polyhedron.o .libs/ppl_prolog_Double_Box.o .libs/ppl_prolog_BD_Shape_double.o .libs/ppl_prolog_Octagonal_Shape_double.o ../../../src/.libs/libppl.dylib -L${gmpprefix}/lib ../../../Watchdog/src/.libs/libpwl.dylib -lm ${gmpprefix}/lib/libgmpxx.dylib ${gmpprefix}/lib/libgmp.dylib 
$ cp .libs/libppl_ciao.dylib ${pplprefix}/lib/ppl/
EOF
    pause
fi

# ---------------------------------------------------------------------------
section "Patch BU" 
cat <<EOF
Finally, 'bu' must be patched as follows.
EOF
pause

cat <<EOF
In "${busrcdir}/ConvexHullAnalyser/cha.pl", replace:
  :- use_module('/home/jpg/PPL/ppl_ciao.pl').
by:
  :- use_module('${pplsrcdir}/interfaces/Prolog/Ciao/ppl_ciao').
EOF
pause

cat <<EOF
In "${busrcdir}/CTL/ciao_yices.pl", replace:
  :- extra_linker_opts('-L/home/jpg/yices-1.0.21/lib').
  :- use_foreign_library(yices).
by:
  :- '\$pragma'(foreign_library_path('${yicesdir}/lib')).
  :- '\$pragma'(foreign_library_path('${gmpprefix}')).
  :- '\$pragma'(use_foreign_library(yices)).
  :- '\$pragma'(use_foreign_library(gmp)).
  :- '\$pragma'(use_foreign_library('stdc++')).
EOF
pause

cat <<EOF
In "${busrcdir}/CTL/amcnew.pl", replace:
	F1 = 'Tests/waterlevelCont.standard.spec',
	V1 = 'Tests/waterlevelCont.standard.versions',
	T1 = 'Tests/waterlevelCont.standard.predExistsTable',
by:
	F1 = '${busrcdir}/CTL/Tests/waterlevelCont.standard.spec',
	V1 = '${busrcdir}/CTL/Tests/waterlevelCont.standard.versions',
	T1 = '${busrcdir}/CTL/Tests/waterlevelCont.standard.predExistsTable',
EOF
pause

# ---------------------------------------------------------------------------
section "Setting up the enviroment" 

if [ "${uname}" == "Darwin" ]; then
    ldpathvar="DYLD_LIBRARY_PATH"
else
    ldpathvar="LD_LIBRARY_PATH"
fi
cat <<EOF
To make accessible all the installed software (dynamic libraries and
Ciao), you must add some lines in your '~/.bashrc' file:

  export ${ldpathvar}=${pplprefix}/lib:${pplprefix}/lib/ppl:${yicesdir}/lib:${gmpprefix}/lib
  export ABSMACH_OPTGRP=tags
  export ABSMACH_OPTS=tagscheme28
  eval \`ciao oc:bash-env\`

Restart the shell and make sure that the changes appear in the
environment (for example, verify 'ciao' is found in the path by
typing 'which ciao')
EOF
pause

# ---------------------------------------------------------------------------
section "Compiling Ciao"
cat <<EOF
Ciao installation is performed by the following commands:
$ ciao oc:build-comp && ciao oc:build-all
EOF
pause

# ---------------------------------------------------------------------------
section "BU is ready"
cat <<EOF
Installation finished! Restart 'bash' to make changes visible.

Now you can start and use 'bu' (the first run will take some time
until all files are compiled). For example:

$ cd ${busrcdir}
$ ciaosh

?- use_module(testforeign).
?- main.
EOF
