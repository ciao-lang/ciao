#ifndef _CIAO_CONCURRENCY_H
#define _CIAO_CONCURRENCY_H

CBOOL__PROTO(prolog_eng_call);
CBOOL__PROTO(prolog_eng_backtrack);
CBOOL__PROTO(prolog_eng_cut);
CBOOL__PROTO(prolog_eng_release);
CBOOL__PROTO(prolog_eng_wait);
CBOOL__PROTO(prolog_eng_kill);
CBOOL__PROTO(prolog_eng_killothers);
CBOOL__PROTO(prolog_eng_status);
CBOOL__PROTO(prolog_eng_self);
CBOOL__PROTO(prolog_lock_atom);
CBOOL__PROTO(prolog_unlock_atom);
CBOOL__PROTO(prolog_lock_atom_state);

intmach_t goal_from_thread_id(THREAD_ID id);

#endif /* _CIAO_CONCURRENCY_H */
