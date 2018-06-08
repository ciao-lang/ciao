#!/bin/sh
#
# A simple script to deploy (Ciao) to BinTray.
#
# Copyright (C) 2017 The Ciao Development Team
#

help() {
    cat <<EOF
Usage: deploy-bintray.sh <args>

where the arguments are passed to curl (e.g., for -uUSER:KEY
authentication).

This script requires a valid value for the CIAOROOT environment
variable.

IMPORTANT! USE KEY ENCRYPTION FOR CONTINUOUS INTEGRATION SERVICES
(TRAVIS-CI AND APPVEYOR)

  The authentication key should never appear clear-text in scripts,
  specially if these are in open repositories. You must encrypt it and
  pass as an environment variable:

     CIAOROOT=$CIAOROOT .../deploy-bintray.sh -ujfmc:$BINTRAY_KEY

  Keys for uploading to BinTray must be encrypted:

  Travis-CI:
     Install Travis CI CLI (https://github.com/travis-ci/travis.rb)
     and use:
       travis encrypt BINTRAY_KEY=<YOUR_BINTRAY_KEY>
     The output is added to `env.global` at the `.travis.yml` file.

  AppVeyor:
     Use https://ci.appveyor.com/tools/encrypt
     and pass <YOUR_BINTRAY_KEY>.

EOF
}

short_help() {
    cat <<EOF

Use -h to obtain a help message for this command.

EOF
}

deploy() {
    cd "$ciaoroot"
    hostname -f > build/PREBUILT # Mark that this is a prebuilt version
    ./ciao-boot.sh gen_pbundle --kind=bin_tgz
    cd "$ciaoroot"/build/pbundle
    VERS=$(cat "$ciaoroot"/core/Manifest/GlobalVersion).$(cat "$ciaoroot"/core/Manifest/GlobalPatch)
    PBUNDLE=$(echo Ciao-*.tar.gz | sed 's/Ciao-.*-bin/ciao-'$VERS'/')
    mv Ciao-*.tar.gz $PBUNDLE
    curl -T $PBUNDLE "$@" 'https://api.bintray.com/content/ciao-lang/builds/'$PBUNDLE';bt_package=ciao;bt_version='$VERS';publish=1;override=1'
}

case "$1" in
    -h|--help) help; exit 1 ;;
esac

if [ x"$CIAOROOT" = x"" ]; then
    cat <<EOF
ERROR: No CIAOROOT is defined (which is probably an error).
EOF
    short_help
    exit 1
fi
ciaoroot=$CIAOROOT

if [ "$#" = 0 ]; then
    cat <<EOF
ERROR: No arguments are provided (which is probably an error).
EOF
    short_help
    exit 1
fi

deploy "$@"
