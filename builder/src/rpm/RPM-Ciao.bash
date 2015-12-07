#!/bin/bash
#
# Setup a non-root RPM building environment and builds a Ciao package

set -e

if [ $# -lt 3 ]
    then
    echo "Usage: $0 PackDir TarName SpecFile [RpmbuildExtraOpt...]"
    exit 1
fi

if ! grep '^%_topdir[ \t]' "$HOME"/.rpmmacros >/dev/null 2>&1
then
	echo "%_topdir $HOME/tmp/RpmBuild" >> "$HOME"/.rpmmacros
fi

# This will ensure the creation of RPM related directories after a cleaning:

SOURCEDIR=`rpm --eval %_sourcedir`
mkdir -p "`rpm --eval %_topdir`"
mkdir -p "${SOURCEDIR}"
mkdir -p "`rpm --eval %_specdir`"
RPMDIR=`rpm --eval %_rpmdir`
mkdir -p "$RPMDIR" "$RPMDIR"/"`rpm --eval %_arch`"
mkdir -p "`rpm --eval %_srcrpmdir`"
mkdir -p "`rpm --eval %_builddir`"

if ! grep "^%_rpmfilename[ \t]" "$HOME"/.rpmmacros >/dev/null 2>&1
then echo "%_rpmfilename %{ARCH}/%{NAME}-%{VERSION}-%{RELEASE}.%{ARCH}.rpm" >>\
	"$HOME"/.rpmmacros
fi

ln -s -f $1$2.tar.gz "$SOURCEDIR"/
SOURCETAR="$SOURCEDIR/$2.tar.gz"
SPECFILE="./$3"
shift 3
rpmbuild -bb -v --clean --rmspec "$@" "$SPECFILE"
rm -f "$SOURCETAR"
