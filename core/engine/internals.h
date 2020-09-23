/*
 *  internals.h
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_INTERNALS_H
#define _CIAO_INTERNALS_H

extern intmach_t num_of_predicates;

module_t *insert_module(sw_on_key_t **swp, tagged_t mod_atm, bool_t insertp);
module_t *new_module(tagged_t mod_atm);
void add_module(sw_on_key_t **swp, sw_on_key_node_t *node, tagged_t key, module_t *mod);

definition_t *find_definition(sw_on_key_t **swp, tagged_t term, tagged_t **argl, bool_t insertp);
definition_t *insert_definition(sw_on_key_t **swp, tagged_t tagpname, int arity, bool_t insertp);
void add_definition(sw_on_key_t **swp, sw_on_key_node_t *node, tagged_t key, definition_t *def);
definition_t *parse_definition(tagged_t complex);

definition_t *new_functor(tagged_t tagpname, int arity);

sw_on_key_node_t *incore_gethash(sw_on_key_t *sw, tagged_t key);
sw_on_key_t *new_switch_on_key(intmach_t size, try_node_t *otherwise);
void expand_sw_on_key(sw_on_key_t **psw, try_node_t *otherwise, bool_t deletep);
void leave_to_gc(enter_instr_t type, char *info);
CBOOL__PROTO(empty_gcdef_bin);
void relocate_gcdef_clocks(instance_clock_t *clocks);
CBOOL__PROTO(prolog_abolish); /* JFMC */
CBOOL__PROTO(abolish, definition_t *f); /* JFMC */
CBOOL__PROTO(define_predicate);
CBOOL__PROTO(erase_clause);
CBOOL__PROTO(clause_number);
CBOOL__PROTO(compiled_clause);
sw_on_key_node_t *dyn_puthash(sw_on_key_t **swp, tagged_t k);
CBOOL__PROTO(set_property);

typedef bignum_size_t (*bn_fun2_t)(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
typedef bignum_size_t (*bn_fun1_t)(bignum_t *x, bignum_t *z, bignum_t *zmax);

CFUN__PROTO(bn_call2, tagged_t, bn_fun2_t f, tagged_t x, tagged_t y);
CFUN__PROTO(bn_call1, tagged_t, bn_fun1_t f, tagged_t x);
CFUN__PROTO(bn_from_float_GC, tagged_t, flt64_t f);

void reinit_list(goal_descriptor_t *goal);
void init_goal_desc_list(void);
/*int kill_thread(goal_descriptor_t *goal_to_kill);*/
void allow_thread_cancellation(void);
void disallow_thread_cancellation(void);
CFUN__PROTO(attach_me_to_goal_desc_list, goal_descriptor_t *);
CVOID__PROTO(print_task_status);
void make_goal_desc_free(goal_descriptor_t *goal);
goal_descriptor_t *init_first_gd_entry(void);
goal_descriptor_t *gimme_a_new_gd(void);
goal_descriptor_t *look_for_a_free_goal_desc(void);
worker_t *get_my_worker(void);
void enqueue_thread(THREAD_T thread);
void unlink_wam(goal_descriptor_t *goal);
uintmach_t num_tasks_created(void);

CFUN__PROTO(cross_copy_term, tagged_t, tagged_t remote_term);

/* Support code for starting goal execution. */
int call_firstgoal(goal_descriptor_t *firstworker, tagged_t goal_term);
THREAD_RES_T startgoal(THREAD_ARG wo);
THREAD_RES_T make_backtracking(THREAD_ARG wo);

#endif /* _CIAO_INTERNALS_H */
