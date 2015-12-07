/*
 *  term_support.h
 *
 *  Term compiler for assert/record.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_TERM_SUPPORT_H
#define _CIAO_TERM_SUPPORT_H

CBOOL__PROTO(compile_term, worker_t **new_worker);
CBOOL__PROTO(prolog_atom_codes);
CBOOL__PROTO(prolog_atom_length);
CBOOL__PROTO(prolog_sub_atom);
CBOOL__PROTO(prolog_atom_concat);
CBOOL__PROTO(prolog_copy_term);
CBOOL__PROTO(prolog_copy_term_nat);
CBOOL__PROTO(prolog_cyclic_term);
CBOOL__PROTO(c_cyclic_term, tagged_t);
CBOOL__PROTO(prolog_unifiable);
CBOOL__PROTO(prolog_unifyOC);
CFUN__PROTO(cross_copy_term, tagged_t, tagged_t remote_term);
bool_t prolog_init_radix(void);
CBOOL__PROTO(prolog_name);
CBOOL__PROTO(prolog_number_codes_2);
CBOOL__PROTO(prolog_number_codes_3);
CFUN__PROTO(compile_term_aux, instance_t *,
	    tagged_t head, 
	    tagged_t body, 
	    worker_t **new_worker);
CVOID__PROTO(number_to_string, tagged_t term, int base);
CBOOL__PROTO(string_to_number, char *AtBuf, int base, tagged_t *strnum, int arity);
CFUN__PROTO(c_list_length, int, tagged_t list);

#endif /* _CIAO_TERM_SUPPORT_H */
