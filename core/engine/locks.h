/*
 *  locks.h
 *
 *  Predicates for locks (concurrency).
 *
 *  Copyright (C) 1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

#ifndef _CIAO_LOCKS_H
#define _CIAO_LOCKS_H

/*
  static LOCK_BLOCK_P new_lock_block(LOCK_BLOCK_P old_block);
 */

/*
void init_dynamic_locks(void);
LOCK create_dynamic_lock(void);
*/

CBOOL__PROTO(prolog_lock_atom);
CBOOL__PROTO(prolog_unlock_atom);
CBOOL__PROTO(prolog_lock_atom_state);
CBOOL__PROTO(prolog_unlock_predicate);

#if defined(DEBUG)

#if defined(Win32)
bool_t lock_is_unset_win32(LOCK *p);
#else
bool_t lock_is_unset(LOCK *p);
#endif

uintmach_t get_inc_counter(void);
void reset_counter(void);

CFUN__PROTO(lock_to_term, tagged_t, LOCK *l);
CFUN__PROTO(slock_to_term, tagged_t, SLOCK *s);
void term_to_lock(tagged_t t, LOCK **l);
void term_to_slock(tagged_t t, SLOCK **s);
#endif

#endif /* _CIAO_LOCKS_H */
