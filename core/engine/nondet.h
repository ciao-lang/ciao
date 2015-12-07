/*
 *  nondet.h
 *
 *  Non-deterministic predicates.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_NONDET_H
#define _CIAO_NONDET_H

/*
  static unsigned int predicate_property_bits(definition_t *d);
 */

typedef enum {X5, X2} WhichChain;

#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept);
#endif
CVOID__PROTO(pop_frame);
CVOID__PROTO(push_frame, int arity);
CVOID__PROTO(pop_choicept);
CVOID__PROTO(push_choicept, try_node_t *alt);
CBOOL__PROTO(nd_atom_concat);
CBOOL__PROTO(current_atom);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(current_clauses);
CBOOL__PROTO(prolog_repeat);
CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(module_is_static);
CBOOL__PROTO(current_predicate);
CBOOL__PROTO(nd_current_predicate);
CBOOL__PROTO(predicate_property);
CBOOL__PROTO(nd_predicate_property);
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
void remove_link_chains(node_t **topdynamic,
                        node_t  *chpttoclear);

#endif /* _CIAO_NONDET_H */
