/*
 *  misc.h
 *
 *  Miscellaneous predicates.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_MISC_H
#define _CIAO_MISC_H

/*
static CFUN__PROTO(compare_aux, int, tagged_t x1, tagged_t x2)
static CFUN__PROTO(compare_args_aux, int, int arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2);
 */

CFUN__PROTO(compare_help, int, tagged_t x1, tagged_t x2);
CBOOL__PROTO(prompt);
CBOOL__PROTO(unknown);
CBOOL__PROTO(metachoice);
CBOOL__PROTO(metacut);
CBOOL__PROTO(retry_cut);
CBOOL__PROTO(setarg);
CBOOL__PROTO(undo);
CBOOL__PROTO(frozen);
CBOOL__PROTO(defrost);
CBOOL__PROTO(prolog_new_atom);
CBOOL__PROTO(debugger_state);
CBOOL__PROTO(debugger_mode);
CBOOL__PROTO(leash_mode);
CBOOL__PROTO(maxdepth);
CBOOL__PROTO(printdepth);
CBOOL__PROTO(breaklevel);
CBOOL__PROTO(compiling);
CBOOL__PROTO(ferror_flag);
CBOOL__PROTO(single_var_flag);
CBOOL__PROTO(character_escapes_flag);
CBOOL__PROTO(redefine_flag);
CBOOL__PROTO(quiet_flag);
CBOOL__PROTO(spypoint);
CBOOL__PROTO(prolog_radix);
CBOOL__PROTO(constraint_list);
CFUN__PROTO(find_constraints, intmach_t, tagged_t *limit);
CBOOL__PROTO(prolog_eq);
CBOOL__PROTO(prolog_dif, definition_t *address_dif);
CBOOL__PROTO(large_data);
CBOOL__PROTO(prolog_interpreted_clause);
int_info_t *current_clauses_aux(tagged_t head);
bool_t insertz_aux(int_info_t *root, instance_t *n);
CFUN__PROTO(var_address, intmach_t, tagged_t term);

CBOOL__PROTO(prolog_global_vars_set_root);
CBOOL__PROTO(prolog_global_vars_get_root);
CBOOL__PROTO(prolog_erase_atom);
CBOOL__PROTO(prolog_current_executable);

#endif /* _CIAO_MISC_H */
