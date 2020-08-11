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

#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept);
#endif
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

#endif /* _CIAO_NONDET_H */
