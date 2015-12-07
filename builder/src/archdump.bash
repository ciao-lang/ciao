# Utilities for native code disassembling
# TODO: use ciaodump, which already defines it as archdump

function archdump() {
    if which objdump > /dev/null; then
	# Linux
	objdump -d "$1"
    elif which otool > /dev/null; then
	# Mac OS X
	otool -Vt "$1"
    else
	echo "No disassembler found, cannot dump file \`$1'" 2>&1
    fi
}
