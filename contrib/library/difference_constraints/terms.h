/*
 *  terms.h
 */

/* TODO (JFMC): goal descriptior should be a worker */

#ifndef _CIAO_DIFFERENCE_CONSTRAINTS_TERMS_H
#define _CIAO_DIFFERENCE_CONSTRAINTS_TERMS_H

tagged_t sep_make_var(goal_descriptor_t *state);
tagged_t sep_make_integer(goal_descriptor_t *state, int i);
tagged_t sep_make_float(goal_descriptor_t *state, double f);
tagged_t sep_make_list(goal_descriptor_t *state, tagged_t head, tagged_t tail);
tagged_t sep_make_functor(goal_descriptor_t *state, tagged_t atom, 
		      int arity, tagged_t *args);

#endif /* _CIAO_DIFFERENCE_CONSTRAINTS_TERMS_H */
