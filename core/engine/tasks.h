/*
 *  tasks.h
 *
 *  Tasks.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_TASKS_H
#define _CIAO_TASKS_H

void reinit_list(goal_descriptor_t *goal);
void init_goal_desc_list(void);
/*int kill_thread(goal_descriptor_t *goal_to_kill);*/
void allow_thread_cancellation(void);
void disallow_thread_cancellation(void);
CFUN__PROTO(attach_me_to_goal_desc_list, goal_descriptor_t *);
CVOID__PROTO(associate_wam_goal, goal_descriptor_t *goal_desc);
CVOID__PROTO(print_task_status);
void make_goal_desc_free(goal_descriptor_t *goal);
goal_descriptor_t *init_first_gd_entry(void);
goal_descriptor_t *gimme_a_new_gd(void);
goal_descriptor_t *look_for_a_free_goal_desc(void);
worker_t *get_my_worker(void);
void enqueue_thread(THREAD_T thread);
void unlink_wam(goal_descriptor_t *goal);
uintmach_t num_tasks_created(void);

#endif /* _CIAO_TASKS_H */
