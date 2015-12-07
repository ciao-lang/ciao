/*
 *  threads.h
 *
 *  Thread abstraction layer.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_THREADS_H
#define _CIAO_THREADS_H

#if defined(THREADS)

/* 
   These macros allows the same macros to be use with the POSIX and Solaris
   threads packages.  Some code taken from Kish's DASWAM
*/

/* ------------------------------------------------------------------------- */

#if defined(DARWIN) || defined(LINUX) || defined(Solaris) || defined(Win32) || defined(BSD)
#define USE_POSIX_THREADS
#endif

/* Previous case: use home-made Windows wrapper to approximate what we need */

/*
  #if defined(Win32) 
  #define USE_WIN32_THREADS
  #endif
*/

/* ------------------------------------------------------------------------- */

#if defined(USE_POSIX_THREADS)
#include <pthread.h>

typedef pthread_t THREAD_T;	/* The type of a thread */
typedef pthread_t THREAD_ID;	/* The unique identifier of a thread */
typedef void *    THREAD_ARG;
typedef void *    THREAD_RES_T;
typedef void *(*THREAD_START)(void *);

/*
#define Thread_Create_NoGoalId(Process, Arg, Id, Handle) { \
    pthread_create(&(Id), &detached_thread, Process, Arg); \
    Handle = Id; \
}
*/

#define Thread_Create_GoalId(Process, Arg, Id, Handle) { \
    if (pthread_create(&(Id), &joinable_thread, Process, Arg)){ \
      ENG_perror("eng_call/{3,4}"); \
 } \
    Handle = Id; \
}

#define Thread_Join(Id)     pthread_join(Id, NULL)
#define Thread_Exit(Status) pthread_exit(Status)
#define Thread_Id           pthread_self()
#define Allow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
#define Disallow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
#define Thread_Cancel(Id) pthread_cancel(Id)
#define Thread_Equal(thr1, thr2) pthread_equal(thr1, thr2)
#define Thread_Dispose(ThrH) pthread_join(ThrH)
#endif  

/* ------------------------------------------------------------------------- */

#if defined(USE_WIN32_THREADS)
#include <ciao/windows.h>

typedef HANDLE THREAD_T;	/* The thread object */
typedef DWORD  THREAD_ID;	/* The unique thread identifier */
typedef LPVOID THREAD_ARG;	/* The argument to the initial thread call */
typedef DWORD  THREAD_RES_T;
typedef LPTHREAD_START_ROUTINE  THREAD_START; /* The type of the routine */

/* 
   Every Win32 thread remains in memory until it has finished and all
   references to it have been removed by using CloseHandle(); I am
   wrapping it inside the Thread_Dispose() macro.
*/

/*
#define Thread_Create_NoGoalId(Process, Arg, Id, Handle) \
        Handle = CreateThread(NULL, 0, Process, Arg, 0, Id)
*/

/* Check for old and new versions of cygwin */
#if defined(__CYGWIN32__) || defined(__CYGWIN__)
 /* Macro uses args so we can cast start_proc to LPTHREAD_START_ROUTINE
    in order to avoid warnings because of return type */
#define _beginthreadex(security, stack_size, start_proc, arg, flags, pid) \
CreateThread(security, stack_size, (LPTHREAD_START_ROUTINE) start_proc, \
	     arg, flags, pid)      
 #define _endthreadex ExitThread      
 #endif 

/*
#define Thread_Create_GoalId(Process, Arg, Id, Handle) \
        Handle = CreateThread(NULL, 0, Process, Arg, 0, &(Id))
*/

#define Thread_Create_GoalId(Process, Arg, Id, Handle) \
        Handle = _beginthreadex(NULL, 0, Process, Arg, 0, &(Id))

/* Thread_Join in Win32 needs a handle, which is not the same as a ThrId! */
#define Thread_Join(Handle) { \
        WaitForSingleObject(Handle, INFINITE);\
        CloseHandle(Handle); \
      }

#define Thread_Exit(Status)  ExitThread(Status)
#define Thread_Id            GetCurrentThreadId()
/* We are not giving security access to the threads, so everybody
   can cancel other threads. */
#define Allow_Thread_Cancel 
#define Disallow_Thread_Cancel  
#define Thread_Cancel(ThrH)  TerminateThread(ThrH, 0) /* Last arg exit code */
#define Thread_Dispose(ThrH) CloseHandle(ThrH) /* Not in other APIs */
#define Thread_Equal(thrid1, thrid2) (thrid1 == thrid2) /* needs Ids */

#endif /* USE_WIN32_THREADS */
#endif /* THREADS */

#if !defined(THREADS) || (!defined(USE_WIN32_THREADS) && !defined(USE_POSIX_THREADS))

#include <unistd.h>
#include <signal.h>

#if defined(Solaris) || defined(LINUX) || defined(Win32) || defined(BSD)
#include <sys/types.h>
#include <sys/wait.h>
#endif

typedef int THREAD_T;
typedef int THREAD_ID;	/* The unique identifier of a thread */
typedef void *THREAD_ARG;
typedef void *THREAD_RES_T;
typedef void *(*THREAD_START)(void *);


#define Thread_Id getpid()
#define Thread_Create_GoalId(Process, Arg, Id, Handle) \
      if ((void *)Id != NULL) Handle = Id = getpid(); Process (Arg)
#define Thread_Create_no_Id(Process, Arg) Process (Arg)
#define Thread_Join(i)
#define Thread_Exit(status)    exit(0)           /* Not correct, actually */
#define Allow_Thread_Cancel
#define Disallow_Thread_Cancel
#define Thread_Cancel(Id) kill(Thread_Id, SIGTERM)
#define Thread_Equal(thr1, thr2) (thr1 == thr2)

#endif /* defined(THREADS) */

#endif /* _CIAO_THREADS_H */
