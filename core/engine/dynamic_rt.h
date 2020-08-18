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

// TODO: move to dynamic_rt.c
CFUN__PROTO(current_instance, instance_t *);
CBOOL__PROTO(first_instance);
CBOOL__PROTO(close_predicate);
CBOOL__PROTO(open_predicate);
CBOOL__PROTO(next_instance, instance_t **ipp);
CBOOL__PROTO(next_instance_conc, instance_t **ipp);
void move_queue(instance_handle_t **srcq, 
                instance_handle_t **destq,
                instance_t *destinst);
void jump_to_next_instance(instance_t *x2_p_insp,
                           instance_t *x5_p_insp,
                           instance_t **ipp,
                           instance_t **x2,
                           instance_t **x5);

/* static void relocate_table_clocks(sw_on_key_t *sw, instance_clock_t *clocks) */

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
CFUN__PROTO(active_instance, instance_t *, instance_t *i, int itime, bool_t normal);
CVOID__PROTO(clock_overflow);
void relocate_clocks(instance_t *inst,  instance_clock_t *clocks);
void expunge_instance(instance_t *i);
CFUN__PROTO(active_instance_conc, instance_t *, instance_t *i, int_info_t *pred_root);

void remove_link_chains(node_t **topdynamic,
                        node_t  *chpttoclear);

#endif /* _CIAO_DYNAMIC_RT_H */
