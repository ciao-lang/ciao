#!/bin/sh
# Compile native code (.car archive)

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# Get .car path from first argument
_carbase=$1; shift; [ -x ""${_carbase}"" ] || exit 1

log=${_carbase}/compile_native.log
execname=${_carbase}/arch

c_files() {
    echo "${CONFIGURATION_DIR}/engine/engine__configuration.c"
    for m in $*; do
	echo "${C_DIR}/${m}.c"
    done
}
o_file() {
    echo "${TARGET_DIR}/`basename $1 .c`.o"
}
s_file() {
    echo "${TARGET_DIR}/`basename $1 .c`.s"
}
compile_script() {
    for f in $*; do
	printf "%s" "echo \"{Compiling $f}\" >> ${log}; "
	if [ x"${CIAOGENASM}" = x"true" ]; then
	    echo "${CC} -S ${CFLAGS} ${H_PATH} -o `s_file $f` $f >> ${log} 2>&1 && ${CC} -c ${CFLAGS} ${H_PATH} -o `o_file $f` $f >> ${log} 2>&1"
	else
	    echo "${CC} -c ${CFLAGS} ${H_PATH} -o `o_file $f` $f >> ${log} 2>&1"
	fi
    done
}
emit_compile_native_aux() {
    cat "${_base}"/compile_native_aux.c > "${_carbase}"/compile_native_aux.c
}
compile() {
    # delete previous log, if exists
    [ -r ${log} ] && rm ${log}
    # compile aux program
    emit_compile_native_aux
    ${CC} ${CFLAGS} ${H_PATH} "${_carbase}"/compile_native_aux.c ${LDFLAGS} ${LIBS} -o "${_carbase}"/compile_native_aux >> ${log} 2>&1 || return 1
    # compile all files in input
    if [ -t 1 ]; then
	show_progress=1
    else
	show_progress=0
    fi
    total=0
    for f in $*; do
	total=`expr ${total} + 1`
    done
    maxjobs=`cat ${CONFIGURATION_DIR}/maxjobs`
    compile_script $* | "${_carbase}"/compile_native_aux ${maxjobs} ${total} ${show_progress} || return 1
}
o_files() {
    for i in $*; do echo `o_file ${i}`; done
}
link() {
    echo "{Linking}" >> ${log}
    OBJFILES2=`o_files $*`
    ${CC} ${LDFLAGS} ${OBJFILES2} ${LIBS} -o ${execname} >> ${log} 2>&1 || return 1
    chmod -f ${EXECMODE} ${execname} >> ${log} 2>&1
}

CONFIGURATION_DIR=${_carbase}/configuration
H_DIR=${_carbase}/c
C_DIR=${_carbase}/c/engine
H_PATH="-I${H_DIR} -I${CONFIGURATION_DIR}"
TARGET_DIR=${_carbase}/o

cfiles__2() {
    c_files `cat "${_carbase}"/native_modules`
}
CFILES=`cfiles__2`

COMPILER_VERSION=`cat "${_carbase}"/compiler_version`
REQUIRE64=`cat "${_carbase}"/require64`

rm -f ${execname}
"${_base}"/configure/create \
    ${CIAOOPTS} \
    --compiler-version=${COMPILER_VERSION} \
    --require64=${REQUIRE64} \
    --hpath=${H_PATH} \
    --version="${_carbase}"/version \
    --output=${CONFIGURATION_DIR} "$@" >> ${log} 2>&1 && \
. ${CONFIGURATION_DIR}/arch-settings && \
mkdir -p ${TARGET_DIR} >> ${log} 2>&1 && \
compile ${CFILES} && \
link ${CFILES}

cat "${_base}"/car_header > "${_carbase}"/run
chmod a+x "${_carbase}"/run

[ -x ${execname} ] || { \
  cat "${log}" 1>&2; \
  echo "{Compilation of ${_carbase} failed}" 1>&2; \
  echo "{Log saved in ${log}}" 1>&2; \
  exit 1; \
}
