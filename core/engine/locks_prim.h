/*
 *  locks_prim.h
 *
 *  Primitives for management of locks.
 *
 *  Copyright (C) 1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

#ifndef _CIAO_LOCKS_PRIM_H
#define _CIAO_LOCKS_PRIM_H

/* If we undefine this, only binary semaphores will be available */
#define GENERAL_LOCKS

/* In order to define whether a lock is a SPIN-lock or a POSIX-lock */
#define SPIN     0
#define POSIX    1

#if !defined(NULL)
#define NULL (void *)0
#endif

#if !defined(THREADS)                                   /* Empty macros */
/* Spin locks: fast, but make a busy-wait */
#define Init_slock(p)          
#define Wait_Acquire_slock(p)  
#define Release_slock(p)       
#define Destroy_slock(p)       
#define SLock_is_unset(p)      1

/* non spin locks: should suspend the thread, but are possibly slower */
#define Init_lock(p)          
#define Wait_Acquire_lock(p)  
#define Release_lock(p)       
#define Destroy_lock(p)       
#define Lock_is_unset(p)      1

/* Conditional vars */
#define Init_Cond(cond)
#define Cond_Var_Init(cond)
#define Cond_Var_Wait(cond, lock)
#define Cond_Var_Broadcast(cond)
#define Wait_Acquire_Cond_lock(Cond)
#define Release_Cond_lock(Cond)
#define Cond_Begin(Cond)
#define Broadcast_Cond(Cond)
#define Wait_For_Cond_End(Cond)
#define Wait_For_Cond_Begin(Predicate, Cond)

#if defined(DEBUG)
#define Cond_Lock_is_unset(Cond) FALSE
#endif

typedef char condition_t;                          /* Avoid compiler errors */
typedef char SLOCK;
typedef char LOCK;

#else  /* We have THREADS!!!!! */

#define USE_LOCKS

/* TODO: x86_64 missing */

/* First, we look at the spin locks.  We know how to do them in i686 and in
   Sparc architectures.  I am not really sure about the Sequent: could not
   try it, actually.  This is heavily influenced by Kish Shen and Roland
   Karlsson (thanks to both). */

#if (defined(i686) || defined(x86_64m32) || \
     defined(Sparc) || defined(Sparc64m32) ||  \
     defined(Sequent)) && \
    defined(__GNUC__)
#define HAVE_NATIVE_SLOCKS

/*
#if defined(Sparc) || defined(Sequent) 
#define DESTRUCTIVE  
#endif
*/

typedef volatile unsigned long int SLOCK_ST;
typedef volatile struct {
  SLOCK_ST  lock_st;
  SLOCK_ST *lock_pt;
} SLOCK;


#if defined(DESTRUCTIVE)
#define lock_offset 1 /*lock is offset from start of structure by one word*/
#define LockOffSet(p) (p.lock_pt + lock_offset)
#else                                                     /* !DESTRUCTIVE */
#define LockOffSet(p) (p.lock_pt)
#endif                                                     /* DESTRUCTIVE */


/* ------------------------------------------------------------------------- */

# if defined(Sparc) || defined(Sparc64m32)     /* Macro definition for sparc */
#   define aswap(addr,reg)                                      \
({ int _ret;                                                    \
   asm volatile ("swap %1,%0"                                   \
        : "=r" (_ret), "=m" (*(addr))    /* Output %0,%1 */     \
        : "m"  (*(addr)), "0" (reg));    /* Input (%2),%0 */    \
   _ret;                                                        \
})
# endif                                                          /* Sparc */

/* ------------------------------------------------------------------------- */

/* Was      asm volatile ("xchgw %0,%1"                                */

# if defined(i686) || defined(x86_64m32) || defined(Sequent)         /* Now, Intel 80x86 stuff */
#   define aswap(adr,reg)                                       \
  ({ long int _ret;                                             \
     asm volatile ("xchgl %0,%1"                                \
        : "=q" (_ret), "=m" (*(adr))    /* Output %0,%1 */      \
        : "m"  (*(adr)), "0"  (reg));   /* Input (%2),%0 */     \
     _ret;                                                      \
  })
#  endif

/* ------------------------------------------------------------------------- */
/* Untested for MIPS! */
# if defined(mips)
#   define asm_ll(adr)						\
  ({ int _ret;							\
     asm volatile ("ll %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)));		/* Input (%2) */	\
     _ret;							\
  })

#   define asm_sc(adr,reg)					\
  ({ int _ret;							\
     asm volatile ("sc %0,%1"					\
	: "=r" (_ret), "=m" (*(adr))	/* Output %0,%1 */	\
	: "m"  (*(adr)), "0" (reg));	/* Input (%2),%0 */	\
     _ret;							\
  })

#  define aswap(p,v)						\
  ({ int _ret;							\
     do { _ret = asm_ll(p); } while(! asm_sc(p, v));		\
     _ret;							\
  })

#define mips_try_lock(p) (*(LockOffset(p))=1)

# define mips_lock(p)						\
do { if (mips_try_lock(p)) break;			        \
     {{ t_lock_wait(); }}					\
     {{ t_debug(__FILE__,__LINE__); }}				\
     while(*(LockOffset*p))==1) continue;		        \
     {{ t_lock_ready(); }}					\
   } while(1)
# endif

/* ------------------------------------------------------------------------- */

/* Initialize a spin lock */
#define Init_slock(p)  { \
        p.lock_pt = &p.lock_st; \
        *(LockOffSet(p))=0; \
       }

/*
  From alan@lxorguk.ukuu.org.uk: 
  Whenever the lock grab fails the CPU will spin sampling the cache
  line until another CPU writes it back which invalidates our MESI
  cache line for the data. This avoids the code spin-generating lock
  cycles on the bus.
*/

#if defined(mips)
#define Wait_Acquire_slock(p) mips_lock(p)
#else
#define Wait_Acquire_slock(p) \
     while (aswap((LockOffSet(p)),1)) { \
        while(*(LockOffSet(p)));\
    }
#endif

#define Release_slock(p)          *(LockOffSet(p))=0
#define Destroy_slock(p)          p.lock_pt = NULL /* Force error afterwards */
#define SLock_is_unset(p)         *(LockOffSet(p))==0

/*
#define Acquire_slock_or_fail(p)  aswap((LockOffSet(p)),1)==0
#define Try_Acquire_self_slock(p) (aswap(*(LOCK_ST **)p, *(LOCK_ST *)p))
#define Reset_lock(p, value)     (aswap(*(LOCK_ST **)p, value))
#define Release_self_lock(p)
*/

#endif

/* Now, the O.S. for which we know there are locks provided by a library.
   These presumably put the thread to sleep and do not consume any
   resources, but also are more expensive to call. */

/* ------------------------------------------------------------------------- */

#if defined(LINUX) || defined (Solaris) || defined (DARWIN) || defined(BSD)
#define HAVE_LIB_LOCKS

#include <pthread.h>
#include <unistd.h>
#include <errno.h>

/* Some Solaris machines define _POSIX_PRIORITY_SCHEDULING */

#if defined(_POSIX_PRIORITY_SCHEDULING) && defined(LINUX) || defined(DARWIN) || defined(BSD)
/* 
   Unlike other implementation of threads, pthreads for Linux implements
   threads as user-space processes.
*/
#include <sched.h>
#define RELINQUISH_PROCESSOR sched_yield();
#else
#define RELINQUISH_PROCESSOR 
#endif

typedef pthread_mutex_t LOCK;

#define Init_lock(p)         pthread_mutex_init(&p, NULL)
#define Wait_Acquire_lock(p) pthread_mutex_lock(&p)
#define Release_lock(p)      pthread_mutex_unlock(&p)
#define Destroy_lock(p)      pthread_mutex_destroy(&p)
#define Lock_is_unset(p)     lock_is_unset(&p)

#define CONDITIONAL_VARS

typedef pthread_cond_t COND_VAR;

#define Cond_Var_Init(cond)    pthread_cond_init(&cond, NULL)
#define Cond_Var_Wait(cond, lock) pthread_cond_wait(&cond, &lock)
/*#define Cond_Var_Signal(cond) pthread_cond_signal(&cond)*/
#define Cond_Var_Broadcast(cond) pthread_cond_broadcast(&cond)
#endif

/* ------------------------------------------------------------------------- */

#if defined(Win32)
#define HAVE_LIB_LOCKS

#include <ciao/windows.h>

 /* I am using Critical Section Objects instead of mutexes because,
 according t the MS documentation, CSO are faster, although they cannot be
 used to synchronize objects belonging to different processes.  */

typedef CRITICAL_SECTION LOCK;

#define Init_lock(p)         InitializeCriticalSection(&p)
#define Wait_Acquire_lock(p) EnterCriticalSection(&p)
#define Release_lock(p)      LeaveCriticalSection(&p)
#define Destroy_lock(p)      DeleteCriticalSection(&p)
#define Lock_is_unset(p)     lock_is_unset_win32(&p)

#define CONDITIONAL_VARS
#define RELINQUISH_PROCESSOR

typedef HANDLE COND_VAR;

#define Cond_Var_Init(cond) cond = CreateEvent(NULL, FALSE, FALSE, NULL)

/* put a timeout; we might miss a wakeup otherwise, since we do not */
/* have atomic Wait and Unlock in Win95/98 */

#define Cond_Var_Wait(cond, lock) \
        Release_lock(lock); \
        WaitForSingleObject(cond, 10); \
        Wait_Acquire_lock(lock)
/*#define Cond_Var_Signal(cond)     SetEvent(cond)*/
#define Cond_Var_Broadcast(cond)  PulseEvent(cond)
#endif

/* ------------------------------------------------------------------------- */

#if !defined(HAVE_LIB_LOCKS) && defined(HAVE_NATIVE_SLOCKS)
/* Define the non-spin ones in terms of the spin ones */

typedef SLOCK LOCK;

#define Init_lock(p)          Init_slock(p)
#define Wait_Acquire_lock(p)  Wait_Acquire_slock(p)  
#define Release_lock(p)       Release_slock(p)       
#define Destroy_lock(p)       Destroy_slock(p)       
#define Lock_is_unset(p)      SLock_is_unset(p)      
#endif

/* ------------------------------------------------------------------------- */

#if defined(HAVE_LIB_LOCKS) && !defined(HAVE_NATIVE_SLOCKS)
/* Do it the other way around */

typedef LOCK SLOCK;

#define Init_slock(p)          Init_lock(p)
#define Wait_Acquire_slock(p)  Wait_Acquire_lock(p)  
#define Release_slock(p)       Release_lock(p)       
#define Destroy_slock(p)       Destroy_lock(p)       
#define SLock_is_unset(p)      Lock_is_unset(p)      
#endif

/* ------------------------------------------------------------------------- */

#if !defined(HAVE_LIB_LOCKS) && !defined(HAVE_NATIVE_SLOCKS)

typedef SLOCK int;
typedef LOCK  int;

#define Init_slock(p)          
#define Wait_Acquire_slock(p)  
#define Release_slock(p)       
#define Destroy_slock(p)       
#define SLock_is_unset(p)      1

#define Init_lock(p)          
#define Wait_Acquire_lock(p)  
#define Release_lock(p)       
#define Destroy_lock(p)       
#define Lock_is_unset(p)      1
#endif

/* ------------------------------------------------------------------------- */

/* The two below should not be used in user code, only here. */
#define Wait_Acquire_Cond_lock(Cond) Wait_Acquire_lock(Cond.cond_lock)
#define Release_Cond_lock(Cond) Release_lock(Cond.cond_lock)



/* Use of the conditional vars: 

A conditional variable is inited using Init_Cond(cond_var)

The variables involved in a condition which is expressed by cond_var are
changed using:

Cond_Begin(cond_var)
... change the variables ...
Broadcast_Cond(cond_var)

which releases all the threads waiting on the condition.

When a thread has to check that / wait for a condition to hold, 

Wait_For_Cond_Begin(pred(...), cond_var)
...do something...
Wait_For_Cond_End(cond_var)

where pred() determines when the condition *does not hold*, i.e., the 
execution continues when pred() is false.

*/


#define Wait_For_Cond_End(Cond)  Release_Cond_lock(Cond)
#define Cond_Lock_is_unset(Cond) Lock_is_unset(Cond.cond_lock)
#define Cond_Begin(Cond)  Wait_Acquire_Cond_lock(Cond)

#if defined(CONDITIONAL_VARS)

#define Init_Cond(Cond) {                                            \
                          Init_lock(Cond.cond_lock);                 \
                          Cond_Var_Init(Cond.cond_var);   \
                        }
#define Wait_For_Cond_Begin(Predicate, Cond) {                             \
                     Wait_Acquire_Cond_lock(Cond);                \
                     RELINQUISH_PROCESSOR                                  \
                     while (Predicate) {                                   \
                       Cond_Var_Wait(Cond.cond_var, Cond.cond_lock); \
                   }                                                       \
                }
/*
#define Signal_Cond(Cond) {                             \
                     Cond_Var_Signal(Cond.cond_var);   \
                     Release_Cond_lock(Cond);      \
                 }
*/
#define Broadcast_Cond(Cond) {                             \
                     Cond_Var_Broadcast(Cond.cond_var);   \
                     Release_Cond_lock(Cond);      \
                 }
#else

#define Init_Cond(Cond) Init_lock(Cond.cond_lock)
#define Wait_For_Cond_Begin(Predicate, Cond) {                     \
                       bool_t pred_is_signaled;                      \
                       Wait_Acquire_Cond_lock(Cond);      \
                       pred_is_signaled = (Predicate);             \
                       while(pred_is_signaled) {                   \
                         Release_Cond_lock(Cond);         \
                         RELINQUISH_PROCESSOR                      \
                         Wait_Acquire_Cond_lock(Cond);    \
                         pred_is_signaled = (Predicate);           \
                       }                                           \
                     }
/*#define Signal_Cond(Cond) Release_Cond_lock(Cond)*/
#define Broadcast_Cond(Cond) Release_Cond_lock(Cond)

#endif

typedef struct conditionstruct {
   LOCK     cond_lock;
#if defined(CONDITIONAL_VARS)
   COND_VAR cond_var;
#endif
} condition_t;

/* ------------------------------------------------------------------------- */

#endif                                                       /* THREADS */

#if defined(DEBUG)
#define GET_INC_COUNTER get_inc_counter()
#define RESET_COUNTER reset_counter()
#else
#define GET_INC_COUNTER 0
#define RESET_COUNTER 
#endif

#endif /* _CIAO_LOCKS_PRIM_H */
