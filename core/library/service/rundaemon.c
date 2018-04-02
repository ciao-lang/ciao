/*
   Usage:
     start the daemon
       rundaemon LOCKFILE EXECPATH
     stop the daemon: 
       kill `cat LOCKFILE`
*/

#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#define DAEMON_RUNNING_DIR "/tmp"

char *lock_file; /* path for lock file */

/* Save process PID in a lock file */
/* (use locks instead of O_EXCL, to avoid stale lock files) */
int write_lock_pid() {
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
void daemonize() {
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

void run_daemon(char *lock_file0, char *exec_path) {
  lock_file = strdup(lock_file0);

  daemonize();

  execl(exec_path, 0);
  /* not reachable */
  /* free(lock_file); */
}
