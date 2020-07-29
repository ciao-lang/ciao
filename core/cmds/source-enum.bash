# (common file for grep-source.bash charset-source.bash)

rel_builddir=build
boot_rel_builddir=build-boot

function find_source() { # [paths]
    # one line per file
    FINDCMDS="-print" find_source_ "$@"
}

function find_source0() { # [paths]
    # one line per file, end in \0 (useful for xargs)
    FINDCMDS="-print0" find_source_ "$@"
}

function find_source_() { # [paths]
    local d
    for d in "$@"; do
        find_source__ "$d"
    done
}

function find_source__() { # PATH
    find "$1" \
         -name '.svn' -prune -o \
         -name '.git' -prune -o \
         -name 'NOSEARCH' -prune -o \
         -name '3rd-bower' -prune -o \
         -name '3rd-npm' -prune -o \
         -path "$1"'/'"$boot_rel_builddir" -prune -o \
         -path "$1"'/'"$rel_builddir" -prune -o \
         -path "$1"'/regr-db' -prune -o \
         -path "$1"'/bndls/testsuite/multisystem/out' -prune -o \
         -path "$1"'/third-party' -prune -o \
         \( '!' -type d \
         '!' -name 'NOCOMPILE' -a \
         '!' -name 'NODISTRIBUTE' -a \
         '!' -name 'noarch' -a \
         '!' -name '.DS_Store' -a \
         '!' -name '*~' -a \
         '!' -name '*.old' -a \
         '!' -name '*.bak' -a \
         '!' -name '*.html*' -a \
         '!' -name '*.ps' -a \
         '!' -name '*.po' -a \
         '!' -name '*.itf' -a \
         '!' -name '*.asr' -a \
         '!' -name '*.ast' -a \
         '!' -name '*.testout' -a \
         '!' -name '*.o' -a \
         '!' -name '*.a' -a \
         '!' -name '*.so' -a \
         '!' -name '*.gcc.code' -a \
         '!' -name '*.texi*' -a \
         '!' -name '*.ascii' -a \
         '!' -name '*.eps' -a \
         '!' -name '*.ps' -a \
         '!' -name '*.pdf' -a \
         '!' -name '*.gif' -a \
         '!' -name '*.ico' -a \
         '!' -name '*.xcf' -a \
         '!' -name '*.png' -a \
         '!' -name '*.jpg' -a \
         '!' -name '*.tar' -a \
         '!' -name '*.tgz' -a \
         '!' -name '*.tar.gz' -a \
         '!' -name '*.tbz2' -a \
         '!' -name '*.tar.bz2' -a \
         '!' -name '*.zip' -a \
         '!' -name '*.exe' -a \
         '!' -name '*.cpx' -a \
         '!' -name '*.doc' -a \
         '!' -name '*.xls' -a \
         '!' -name '*.opt' -a \
         '!' -name '*.info' \) \
         $FINDCMDS
}
