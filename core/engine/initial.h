/*
 *  initial.h
 *
 *  System initialization and extern defs for global Prolog terms.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_INITIAL_H
#define _CIAO_INITIAL_H

extern bool_t trace_calls;
#if defined(DEBUG)
extern bool_t trace_instr;
#endif

extern instance_clock_t def_clock;
extern instance_clock_t use_clock;

extern sw_on_key_t *switch_on_function;

#if defined(USE_ATOM_LEN)
atom_t *new_atom_check(char *str, 
		       unsigned int str_len,
		       unsigned int index);
#else
atom_t *new_atom_check(char *str, 
		       unsigned int index);
#endif

void glb_init_each_time(void);
CVOID__PROTO(init_each_time);
void init_kanji(void);
void init_latin1(void);
void init_once(void);
void init_locks(void);
CVOID__PROTO(init_streams_each_time);
CVOID__PROTO(local_init_each_time);

#endif /* _CIAO_INITIAL_H */
