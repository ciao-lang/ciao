/*
 *  os_signal.h
 *
 *  OS signal handling (compatibility)
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2015 Ciao Development Team
 */

#ifndef _CIAO_OS_SIGNAL_H
#define _CIAO_OS_SIGNAL_H

#include <signal.h>
#include <setjmp.h>

/* Non-local goto with saving of signal mask */
/* NOTE: Use them when longjmp from signal handlers! */

/* TODO: some SIGLONGJMP may be LONGJMP */

#if defined(SYMM) || defined(Solaris) || defined(_WIN32) || defined(_WIN64)
#define SIGJMP_BUF jmp_buf
#define SIGSETJMP(Env) setjmp(Env)
#define SIGLONGJMP(Env, Val) longjmp(Env, Val)
#else
#define SIGJMP_BUF sigjmp_buf
#define SIGSETJMP(Env) sigsetjmp(Env, 1)
#define SIGLONGJMP(Env, Val) siglongjmp(Env, Val)
#endif

/* NOTE: Use sigprocmask() for sigblock() */ 
/* NOTE: Use sigsuspend() for sigpause() */ 

#if defined(Solaris)
#define SIGNAL(SIG,HDL) {			\
  struct sigaction act;				\
  sigemptyset(&act.sa_mask);			\
  act.sa_flags = 0 | SA_NODEFER;		\
  act.sa_handler = HDL;				\
  sigaction(SIG,&act,(struct sigaction *)NULL);	\
}
#else
#define SIGNAL signal
#endif

#endif /* _CIAO_OS_SIGNAL_H */
