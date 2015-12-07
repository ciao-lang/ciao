# (common file for grep-source.bash uchardet-source.bash)

rel_builddir=build
boot_rel_builddir=build-boot

function find_source() {
    find . \
	 -name '.svn' -prune -o \
	 -name '.git' -prune -o \
	 -path './'"$rel_builddir" -prune -o \
	 -path './regr-db' -prune -o \
	 -path './third-party' -prune -o \
	 -path './ide/web/externals' -prune -o \
	 -path './'"$boot_rel_builddir" -prune -o \
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
	 "$@"
}

