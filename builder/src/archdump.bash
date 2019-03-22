# Utilities for native code disassembling
# TODO: use ciaodump or ciaodump-oc, which already defines it as archdump

function archdump() {
    if command -v objdump > /dev/null 2>&1; then
	# Linux
	objdump -d "$1"
    elif command -v otool > /dev/null 2>&1; then
	# macOS
	otool -Vt "$1"
    else
	echo "No disassembler found, cannot dump file \`$1'" 2>&1
    fi
}
