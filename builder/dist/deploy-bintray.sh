#!/bin/sh

# A simple script to deploy to BinTray.
#
# Usage:
#   deploy-bintray.sh <args>
#
# where the arguments are passed to curl (e.g., for -uUSER:KEY
# authentication).
#
# This script requires a valid value for the CIAOROOT environment
# variable.

if [ x"$CIAOROOT" = x"" ]; then
    cat <<EOF
ERROR: CIAOROOT not defined!

EOF
    exit 1
fi
ciaoroot=$CIAOROOT

cd "$ciaoroot"
hostname -f > build/PREBUILT # Mark that this is a prebuilt version
./ciao-boot.sh gen_pbundle --kind=bin_tgz
cd "$ciaoroot"/build/pbundle
VERS=$(cat "$ciaoroot"/core/Manifest/GlobalVersion).$(cat "$ciaoroot"/core/Manifest/GlobalPatch)
PBUNDLE=$(echo Ciao-*.tar.gz | sed 's/Ciao-.*-bin/ciao-'$VERS'/')
mv Ciao-*.tar.gz $PBUNDLE
curl -T $PBUNDLE "$@" 'https://api.bintray.com/content/ciao-lang/builds/'$PBUNDLE';bt_package=ciao;bt_version='$VERS';publish=1;override=1'
