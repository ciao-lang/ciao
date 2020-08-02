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

CBOOL__PROTO(prolog_lock_atom);
CBOOL__PROTO(prolog_unlock_atom);
CBOOL__PROTO(prolog_lock_atom_state);
CBOOL__PROTO(prolog_unlock_predicate);

#if defined(DEBUG)

bool_t lock_is_unset(LOCK *p);

uintmach_t get_inc_counter(void);
void reset_counter(void);

#endif

#endif /* _CIAO_LOCKS_H */
