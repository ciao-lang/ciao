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
CBOOL__PROTO(string_to_number, char *str, int base, tagged_t *strnum, int arity);
CFUN__PROTO(c_list_length, int, tagged_t list);

/* `Out` is the large or small int from `Str` in base `Base`.
 * `Arity` is the number of live X registers (in case of heap GC).
 * (assumes `Str` represents a number)
 */
#define StringToInt(Str, Base, Out, Arity) ({				\
  int req = bn_from_string((Str),					\
			   (bignum_t *)w->global_top,			\
			   (bignum_t *)(Heap_End-CONTPAD), (Base));	\
  if (req != 0) {							\
    explicit_heap_overflow(Arg, req+CONTPAD, (Arity));			\
    if (bn_from_string((Str),						\
		       (bignum_t *)w->global_top,			\
		       (bignum_t *)(Heap_End-CONTPAD), (Base))) {	\
      SERIOUS_FAULT("miscalculated size of bignum");			\
    }									\
  }									\
  FinishInt(w->global_top, Out);					\
})

/* Like StringToInt, assuming enough heap (no GC) */
#define StringToInt_nogc(Str, Base, Out) ({		\
  if (bn_from_string((Str),				\
		     (bignum_t *)w->global_top,		\
		     (bignum_t *)Heap_End, (Base))) {	\
    SERIOUS_FAULT("miscalculated heap usage");		\
  }							\
  FinishInt(w->global_top, Out);			\
})

#endif /* _CIAO_TERM_SUPPORT_H */
