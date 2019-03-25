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
    signal(SIGINT, int_handler);
    /* Execute it */
    pid_t pid;
    pid = fork();
    if (pid == 0) {
      execl("/bin/sh", "sh", "-c", str, (char *)0);
      _exit(127);
    } else {
      /* parent: set signal handler and wait for termination for child */
      signal(SIGINT, int_handler);
      if (waitpid(pid, &status, 0) != pid) return 0;                                                                                                                                                                                         
      status = WEXITSTATUS(status);                                                                                                                                                                         
    }                                                                                                                                                                                                       
    pthread_mutex_lock(&finished_job_mutex);
    if (stop) {
      local_stop = 1;
    } else if (status == -1) {
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
