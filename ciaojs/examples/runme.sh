#!/usr/bin/env bash

# Exit immediately if a simple command exits with a non-zero status
set -e

# ---------------------------------------------------------------------------

# Follow symbolic links
self_exec=`test -L "$0" && readlink "$0" || echo "$0"`
# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname ${self_exec}`; self=`pwd`; cd ${old_dir}; old_dir=

# ---------------------------------------------------------------------------

function prepare() {
    cd "$self"
    rm -f ${self}/ciao
    ln -s ../dist ${self}/ciao
    bower --config.interactive=false install codemirror#5.1.0 jsconsole
}

# ---------------------------------------------------------------------------
# Open a server serving files from this directory

function test_server() {
    echo "Starting test server at http://localhost:8000"
    cd "$self"
    python -m SimpleHTTPServer
}

# ---------------------------------------------------------------------------

case $1 in
    "prepare") prepare ;;
    "test-server") test_server ;;
    *) echo "Usage: runme.sh prepare|test-server"
esac    

