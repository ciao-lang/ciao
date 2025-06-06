/*
 *  dynamic_rt.h
 *
 *  Database management support code.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_DYNAMIC_RT_H
#define _CIAO_DYNAMIC_RT_H

typedef enum {BLOCK, NO_BLOCK} BlockingType;

// TODO: move to dynamic_rt.c
CFUN__PROTO(current_instance0, instance_t *);
CBOOL__PROTO(first_instance);
CBOOL__PROTO(close_predicate);
CBOOL__PROTO(open_predicate);
CBOOL__PROTO(next_instance, instance_t **ipp);
CBOOL__PROTO(next_instance_conc, instance_t **ipp);

CBOOL__PROTO(current_clauses);

/* static void relocate_table_clocks(hashtab_t *sw, instance_clock_t *clocks) */

CBOOL__PROTO(prolog_purge);
CBOOL__PROTO(prolog_erase);
CBOOL__PROTO(prolog_ptr_ref);
CBOOL__PROTO(inserta);
CBOOL__PROTO(insertz);
size_t compile_large(tagged_t t, bcp_t p);
#if BC_SCALE==2
size_t compile_large_bc32(tagged_t t, bcp_t p);
#endif
CBOOL__PROTO(make_bytecode_object);
CVOID__PROTO(clock_overflow);
void relocate_clocks(instance_t *inst,  instance_clock_t *clocks);
void expunge_instance(instance_t *i);

void remove_link_chains(choice_t **topdynamic, choice_t *chpttoclear);

#endif /* _CIAO_DYNAMIC_RT_H */
