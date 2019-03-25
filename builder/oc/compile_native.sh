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
    cat > "${_carbase}"/compile_native_aux.c <<EOF
#include <stdio.h>
#if defined(Solaris) || defined(LINUX) || defined(Win32)
#include <sys/types.h>
#include <sys/wait.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>

pthread_mutex_t job_mutex = PTHREAD_MUTEX_INITIALIZER;
int total_jobs = 0;
pthread_mutex_t finished_job_mutex = PTHREAD_MUTEX_INITIALIZER;
int finished_jobs = 0;
int stop = 0;

int show_progress = 0;

#define MAXCMD (16*1024)

#define MAXBARWIDTH 1024
#define DOTBARWIDTH 46

char bar[MAXBARWIDTH+1];

void make_progress_bar(int bar_cur, int bar_total) {
  int bar_p;
  int bar_w;
  int i;
  strcpy(bar, "{Compiling native code [");
  i = strlen(bar);
  bar_p = i + DOTBARWIDTH * bar_cur / bar_total;
  bar_w = i + DOTBARWIDTH;
  while (i < bar_p) {
    bar[i] = '=';
    i++;
  }
  while (i <= bar_p) {
    bar[i] = '>';
    i++;
  }
  while (i <= bar_w) {
    bar[i] = ' ';
    i++;
  }
  sprintf(&bar[i], "] %d%%}", 100*bar_cur/bar_total);
}

void progress_bar(int bar_cur, int bar_total) {
  make_progress_bar(bar_cur, bar_total);
  printf("\r%s", bar);
  fflush(stdout);
}

void clean_progress_bar(int bar_total) {
  char blk[MAXBARWIDTH+1];
  int i;
  make_progress_bar(bar_total, bar_total);
  i = 0;
  while (bar[i] != 0) {
    blk[i] = ' ';
    i++;
  }
  blk[i] = 0;
  printf("\r %s \r \b", blk);
  fflush(stdout);
}

void int_handler(int rc) {
  clean_progress_bar(total_jobs);
  fprintf(stderr, "{Aborted during compilation of native code}\n");
  exit(-1);
}

void *run_thread(int number) {
  char str[MAXCMD+1];
  int local_stop = 0;

  while (local_stop == 0) {
    char *res;
    int status;
    /* Get a job from stdio */
    pthread_mutex_lock(&job_mutex);
    res = fgets(str, MAXCMD, stdin);
    pthread_mutex_unlock(&job_mutex);
    if (res == NULL) break;
    /* Execute it */
    pid_t pid;
    pid = fork();
    if (pid == 0) {
      /* child: ignore signals and execute command */
      signal(SIGINT, SIG_IGN);
      if (system(str) != -1) {
        /* TODO: fork failed, handle error */
        exit(1);
      } else {
        /* TODO: handle WEXITSTATUS(status) */
        exit(0);
      }
    } else {
      /* parent: set signal handler and wait for termination for child */
      signal(SIGINT, int_handler);
      if (waitpid(pid, &status, 0) != pid) return 0;
      status = WEXITSTATUS(status);
    }
    pthread_mutex_lock(&finished_job_mutex);
    if (stop) {
      local_stop = 1;
    } else if (status == -1 || WEXITSTATUS(status) != 0) {
      stop = 1;
      local_stop = 1;
    } else {
      finished_jobs++;
      if (show_progress) progress_bar(finished_jobs, total_jobs);
    }
    pthread_mutex_unlock(&finished_job_mutex);
  }
  return 0;
}

int main(int argc, char **argv) {
  pthread_t *thread;
  int threads;
  int i;

  /* Get arguments */
  if (argc == 4) {
    /* number of workers */
    threads = atoi(argv[1]);
    if (threads == 0) return 1;
    /* total number of jobs */
    total_jobs = atoi(argv[2]);
    if (total_jobs == 0) return 1;
    /* show progress? */
    show_progress = atoi(argv[3]);
  } else {
    return 1;
  }

  /* Launch the workers */
  thread = malloc(sizeof(pthread_t) * threads);
  for (i = 0; i < threads; i++) {
    pthread_create(&thread[i], NULL, (void*(*)(void*))run_thread, (void*)(long)i);
  }
  for (i = 0; i < threads; i++) {
    pthread_join(thread[i], NULL);
  }
  free(thread);
  if (show_progress) clean_progress_bar(total_jobs);

  return stop ? -1 : 0;
}
EOF
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
