#!/bin/sh
# Clean compilation output (.car archive)

# Get .car path from first argument
_carbase=$1; shift; [ -x "${_carbase}" ] || exit 1

rm -f "${_carbase}"/arch "${_carbase}"/o/*.o "${_carbase}"/compile_native_aux.c "${_carbase}"/compile_native_aux
