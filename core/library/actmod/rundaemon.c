/* (C code for rundaemon.pl) */

#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>

#define DAEMON_RUNNING_DIR "/tmp"

char *lock_file; /* path for lock file */

/* Save process PID in a lock file */
/* (use locks instead of O_EXCL, to avoid stale lock files) */
int write_lock_pid(void) {
  char str[256];
  int f;
  f = open(lock_file, O_RDWR|O_CREAT, 0640);
  if (f<0) exit(1);
  if (lockf(f,F_TLOCK,0)<0) return 0;

  sprintf(str, "%d\n", getpid());
  write(f, str, strlen(str));
  return 1;
}

/* Forks the process and turn into a daemon */
void daemonize(void) {
  int i;
  int f;
  pid_t pid;

  /* already a daemon */
  if (getppid() == 1) return;

  pid = fork();
  if (pid < 0) { /* error */
    exit(1);
  }
  if (pid > 0) { /* parent, exit */
    exit(0);
  }

  /* (we are the child) */

  /* New SID for the child */
  setsid(); 

  /* Close all open file descriptors */
  for (i = getdtablesize(); i>=0; i--) {
    close(i);
  }

  /* Redirect 0,1,2 fd to /dev/null */
  f = open("/dev/null",O_RDWR); /* 0 */
  dup(f); /* 1 */
  dup(f); /* 2 */

  /* Change file permissions and running directory */
  umask(027);
  chdir(DAEMON_RUNNING_DIR);

  /* Try write the lock */
  if (!write_lock_pid()) {
    exit(0); /* exit, assume that lock was taken */
  }

  /* Ignore signals: child, tty */
  signal(SIGCHLD, SIG_IGN);
  signal(SIGTSTP, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
  signal(SIGTTIN, SIG_IGN);
}

/* Partially copied from internals:$exec/9 -- merge */
CBOOL__PROTO(run_daemon)
{
  ERR__FUNCTOR("rundaemon:run_daemon", 3);
  tagged_t head, list;
  char *lock_file0;
  char *command;
  int args_n;
  char **args;

  /* Lockfile */
  DEREF(X(0), X(0));
  if (!TaggedIsATM(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(0), 1);
  }
  lock_file0 = GetString(X(0));

  /* Get command */
  DEREF(X(1), X(1));
  if (!TaggedIsATM(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(1), 2);
  }
  command = GetString(X(1));

  /* Compute number of arguments and check types */
  args_n = 0;
  DEREF(list, X(2));
  while(!IsVar(list) && TagIsLST(list)) {
    args_n++;
    DEREF(head, *TagToCar(list));
    if (!TaggedIsATM(head)) { /* We only allow atoms */
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), head, 3);
    }
    list = *TagToCdr(list);
    DEREF(list, list);
  }
  /* Make sure we had a real list */
  if (!(!IsVar(list) && TaggedIsATM(list) && (list == atom_nil))) {
    BUILTIN_ERROR(TYPE_ERROR(LIST), X(2), 3);
  }

  /* Allocate and fill array of command+arguments */
  args_n += 2; /* extra space for command and NULL */
  args = checkalloc_ARRAY(char *, args_n);

  int args_i = 0;
  args[args_i++] = command;

  DEREF(list, X(2));
  while(!IsVar(list) && TagIsLST(list)) {
    DEREF(head, *TagToCar(list));
    // printf("[%d]=%s\n", args_i, GetString(head));
    args[args_i++] = GetString(head);
    list = *TagToCdr(list);
    DEREF(list, list);
  }
  args[args_i] = NULL;

  /* Create lock file, daemonize, and execv */
  lock_file = strdup(lock_file0);
  daemonize();
  execv(args[0], args);
  /* not reachable */
  /* free(lock_file); */
  return FALSE;
}
