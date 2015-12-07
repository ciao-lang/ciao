/*
 *  task_areas.h
 *
 *  Data structures for tasks.
 *
 *  Copyright (C) 1997 The CLIP Group
 */

#ifndef _CIAO_TASK_AREAS_H
#define _CIAO_TASK_AREAS_H

/* States a worker can be in */
typedef enum {
  IDLE,      /* The memory areas are available for being used by a thread */
  WORKING,                 /* The memory areas are being used by a thread */
  PENDING_SOLS,               /* Frozen --  backtracking can be requested */
  FAILED	             /* Frozen -- but no more solutions available */
} Thread_State;

/* Save wam() local variables, to reenter after leaving it blocked (e.g., if
   waiting for backtracking). */

typedef struct wam_private_ wam_private_t;
struct wam_private_ {
  bcp_t p;		                               /* program counter */
  int i;
  tagged_t *pt1, *pt2, t0, t1, t2, t3;
  bcp_t ptemp;
  int wam_exit_code;
  instance_t *ins;	
};

/* Possible actions requested from the toplevel. */

#define NO_ACTION         0
#define SHARES_STRUCTURE  1
#define HAS_CONTINUATION  2
#define KEEP_STACKS       4
#define BACKTRACKING      8
#define CREATE_THREAD    16
#define CREATE_WAM       32
#define NEEDS_FREEING    64

/* The goal descriptors are held together in a doubly linked circular
   list; there is a pointer to the list, which points always to a free
   goal descriptor (if there is any in the list).  All the free goal
   descriptors can be found following the forward link of this initial
   pointer.  */

struct goal_descriptor_ {
  /* Pointer to the WAM registers.  
     If NULL, no WAM has been associated to the goal.  */
  worker_t *worker_registers;
  wam_private_t wam_private_state;
  /* This defines the state of the WAM (if any) associated to the goal */
  Thread_State state;      
  /* If any thread is working on the WAM, these are the points to interact
     with it */
  THREAD_ID thread_id;
  THREAD_T  thread_handle;	/* Different from thread_id in Win32 */
  int      action;		/* Defines the behavior of the goal */
  tagged_t goal;		/* The pointer to the goal to execute */
  /* NOTE: due to Term <-> int conversions, this cannot be unsigned */
  /* TODO: change type for global_goal_number? */
  intmach_t goal_number;        /* Snapshot of global counter */
  SLOCK goal_lock_l;
  goal_descriptor_t *forward, *backward;
};

#define TermToGoalDesc(term) (goal_descriptor_t *)TermToPointer(term)
#define GoalDescToTerm(goal) PointerToTerm(goal)

#endif /* _CIAO_TASK_AREAS_H */
