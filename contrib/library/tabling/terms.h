#ifndef _CIAO_TABLING_TERMS_H
#define _CIAO_TABLING_TERMS_H

#if defined(TABLING)
tagged_t chat_make_var(goal_descriptor_t *state);
tagged_t chat_make_integer(goal_descriptor_t *state, int i);
tagged_t chat_make_float(goal_descriptor_t *state, double f);
tagged_t chat_make_list(goal_descriptor_t *state, tagged_t head, tagged_t tail);
tagged_t chat_make_functor(goal_descriptor_t *state, tagged_t atom, 
			   int arity, tagged_t *args);
#endif

#endif /* _CIAO_TABLING_TERMS_H */
