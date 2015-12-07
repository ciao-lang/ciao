#!/bin/bash
# Simple script to check the indexer package translation

function failmsg() {
    echo $*
    exit 1
}
function warnmsg() {
    echo $*
}

MOD=idx_test

# Expanded code
# WARNING: Misses some translations (like goal expansions)
MODEXP0=${MOD}_co.pl # original output; renamed later to avoid problems
MODEXP=${MOD}_co.pl-out
MODEXP_REF=${MOD}_co.pl-ref

MODWAM=${MOD}.wam
MODWAM_REF=${MOD}.wam-ref

rm -f ${MODEXP} ${MOD}.itf ${MOD}.po ${MOD}.wam
echo "Compiling ${MOD}.pl (and generating .wam output)"
ciaoc -acm ${MOD} expander -w ${MOD} || failmsg "[Compilation failed]"
mv ${MODEXP0} ${MODEXP}
echo "Comparing current expanded code ${MODEXP} with reference ${MODREF}"
diff ${MODEXP} ${MODEXP_REF} && echo "[OK]" || warnmsg "[Please, check the differences w.r.t. the reference translation]"
echo "Comparing current WAM code ${MODWAM} with reference ${MODWAM}"
diff ${MODWAM} ${MODWAM_REF} && echo "[OK]" || warnmsg "[Please, check the differences w.r.t. the reference translation]"
