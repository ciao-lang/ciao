/*
 *  internals.h
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_INTERNALS_H
#define _CIAO_INTERNALS_H

extern intmach_t num_of_predicates;

sw_on_key_node_t *incore_gethash(sw_on_key_t *sw, tagged_t key);
sw_on_key_t *new_switch_on_key(intmach_t size, try_node_t *otherwise);
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

/* Support code for starting goal execution. */
int call_firstgoal(goal_descriptor_t *firstworker, tagged_t goal_term);
THREAD_RES_T startgoal(THREAD_ARG wo);
THREAD_RES_T make_backtracking(THREAD_ARG wo);

#endif /* _CIAO_INTERNALS_H */
