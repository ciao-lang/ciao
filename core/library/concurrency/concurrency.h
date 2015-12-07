#ifndef _CIAO_CONCURRENCY_H
#define _CIAO_CONCURRENCY_H

#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t detached_thread;
extern pthread_attr_t joinable_thread;
#endif

CBOOL__PROTO(prolog_eng_kill);
CBOOL__PROTO(prolog_eng_killothers);
CBOOL__PROTO(prolog_eng_wait);
CBOOL__PROTO(prolog_eng_self);
CBOOL__PROTO(prolog_eng_status);
CBOOL__PROTO(prolog_eng_status1);
CBOOL__PROTO(prolog_eng_backtrack);
CBOOL__PROTO(prolog_eng_release);
CBOOL__PROTO(prolog_eng_cut);
intmach_t goal_from_thread_id(THREAD_ID id);

#endif /* _CIAO_CONCURRENCY_H */
