/*
 *  tasks.c
 *
 *  Tasks.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <stdio.h>

#include <ciao/configure.h>
#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/task_areas.h>
#include <ciao/tasks.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/nondet.h>
#include <ciao/start.h>
#include <ciao/initial.h>

/* If we are not using threads, this simply points to a single WRB state;
   i.e., the list is actually a singleton. */
/* wrb_state_p wrb_state_list;  */

SLOCK    goal_desc_list_l;
goal_descriptor_t *goal_desc_list = NULL;

SLOCK thread_to_free_l;
THREAD_T thread_to_free = (THREAD_T)NULL;

uintmach_t global_goal_number = 0;                 /* Last number taken */

/* The initial list has a single goal descriptor with no WAM and in
   IDLE state */

void init_goal_desc_list(void)
{
  Init_slock(goal_desc_list_l);
  Init_slock(thread_to_free_l);
  
  goal_desc_list = checkalloc_TYPE(goal_descriptor_t);
  goal_desc_list->state = IDLE;
  goal_desc_list->worker_registers = NULL;
  Init_slock(goal_desc_list->goal_lock_l);
  goal_desc_list->forward = goal_desc_list->backward = goal_desc_list;
}

uintmach_t num_tasks_created(void) {
  return global_goal_number;
}

/* "goal" is to be the only working thread in the system.  The rest of the
   goals have no associated thread */

void reinit_list(goal_descriptor_t *myself)
{
     goal_descriptor_t *goal_ref;
     
     Wait_Acquire_slock(goal_desc_list_l);
     goal_ref = goal_desc_list;
     do {
       if ((goal_ref != myself)) {
         unlink_wam(goal_ref);
         goal_ref->state = IDLE;
       }
       goal_ref = goal_ref->forward;
     } while (goal_ref != goal_desc_list);
     goal_desc_list = myself->forward;
     Release_slock(goal_desc_list_l);
}


 /* Try to kill a goal (and release the wam it was attached to).
 Returns 0 if no error, -1 on error (maybe no such thread).  Actually,
 killing a thread should be done otherwise: the killed thread might be
 in an unstable state in which it should not be killed. */

/*
int kill_thread(goal_descriptor_t *this_goal)
{
  return Thread_Cancel(this_goal->thread_handle);
}
*/


/* Cause kills to this thread to be immediately executed */

void allow_thread_cancellation(void)
{
  Allow_Thread_Cancel;
}


/* Cause kills to this thread to be ignored (for symmetry with the above) */

void disallow_thread_cancellation(void)
{
  Disallow_Thread_Cancel;
}


/* Should be called after the list is inited */
goal_descriptor_t *init_first_gd_entry(void)
{
  goal_descriptor_t *first_gd;

  first_gd = gimme_a_new_gd();

  first_gd->thread_id = Thread_Id;
  first_gd->thread_handle = (THREAD_T)NULL; /* Special case? */
  first_gd->action = NO_ACTION;
  first_gd->goal = (tagged_t)NULL;
  
  return first_gd;
}

/* Returns a free goal descriptor, with a WAM attached to it.  If
   needed, create memory areas, initialize registers, etc. The worker
   is marked as WORKING in order to avoid other threads stealing it.
   The WAM areas are already initialized. */

goal_descriptor_t *gimme_a_new_gd(void)
{
  goal_descriptor_t *gd_to_run;

  if ((gd_to_run = look_for_a_free_goal_desc())) { /* Make sure it has a WAM */
    if (!(gd_to_run->worker_registers))
      associate_wam_goal(free_wam(), gd_to_run);
  } else gd_to_run = attach_me_to_goal_desc_list(free_wam());
  return gd_to_run;
}


/* We already have a WAM.  Create a goal descriptor in WORKING state,
   add it to the goal descriptor list, and mark it as ours.  */

CFUN__PROTO(attach_me_to_goal_desc_list, goal_descriptor_t *)
{
  goal_descriptor_t *goal_desc_p;

  goal_desc_p = checkalloc_TYPE(goal_descriptor_t);
  goal_desc_p->state = WORKING;
  goal_desc_p->goal_number = ++global_goal_number;
  Init_slock(goal_desc_p->goal_lock_l);
  associate_wam_goal(Arg, goal_desc_p);

  /* Add it at the end of the list, i.e., add it to the "backward"
     side of the list, where the non-free goal descriptors are. */
  Wait_Acquire_slock(goal_desc_list_l);
  goal_desc_p->forward = goal_desc_list;
  goal_desc_p->backward = goal_desc_list->backward;
  goal_desc_list->backward->forward = goal_desc_p;
  goal_desc_list->backward = goal_desc_p;
  Release_slock(goal_desc_list_l);
  return goal_desc_p;
}


/* cross-link a WAM and a goal */

CVOID__PROTO(associate_wam_goal, goal_descriptor_t *goal_desc)
{
  goal_desc->worker_registers = Arg;
  Arg->misc->goal_desc_ptr = goal_desc;
}


/* I know this is a hack and not manageable; moreover, as for now, the
   ThreadId printed is not the same as the one returned by the Prolog
   side.  O.K., promise to improve it. */

CVOID__PROTO(print_task_status)
{
  FILE *u_o = Output_Stream_Ptr->streamfile;

  goal_descriptor_t *current_goal = goal_desc_list;

  do {
    switch(current_goal->state) {
    case IDLE:
      fprintf(u_o, "Available: Wam %p\n", current_goal->worker_registers);
      break;
    case WORKING:
      fprintf(u_o, "Active: GoalDesc 0x%p", current_goal);
      fprintf(u_o, "\tGoal Id %" PRIum "\n", current_goal->goal_number);
      fprintf(u_o, "\tWam 0x%p\n", current_goal->worker_registers);
      break;
    case PENDING_SOLS:
      fprintf(u_o, "Pending solutions: GoalDesc 0x%p", current_goal);
      fprintf(u_o, "\tGoal Id %" PRIum "\n", current_goal->goal_number);
      fprintf(u_o, "\tWam 0x%p\n", current_goal->worker_registers);
      break;
    case FAILED:
      fprintf(u_o, "Failed: GoalDesc 0x%p", current_goal);
      fprintf(u_o, "\tGoal Id %" PRIum "\n", current_goal->goal_number);
      break;
    default:
      fprintf(u_o, "Unknown status: GoalDesc 0x%p!\n", current_goal);
    }
    current_goal = current_goal->forward;
  } while(current_goal != goal_desc_list);
}

#if 0
/* TODO: this seems to be wrong? (where is the list?) (JFMC) */

CFUN__PROTO(list_of_goals, tagged_t)
{
  tagged_t *pt1 = w->global_top;
  goal_descriptor_t *current_goal = goal_desc_list;
  int arity;
  
  do {

    switch(current_goal->state) {
    case IDLE:
      HeapPush(pt1,functor_available);
      HeapPush(pt1,PointerToTerm(current_goal));
      arity = 1;
      break;
    case WORKING:
      HeapPush(pt1,functor_active);
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal->goal_number));
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      arity = 4;
      break;
    case PENDING_SOLS:
      HeapPush(pt1,functor_pending);
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      arity = 4;
      break;
    case FAILED:
      HeapPush(pt1,functor_failed);
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      HeapPush(pt1,PointerToTerm(current_goal));
      arity = 3;
      break;
    }
    current_goal = current_goal->forward;
  } while (current_goal != goal_desc_list);

  w->global_top=pt1;
  return Tag(STR,HeapOffset(pt1,-3));
}

CBOOL__PROTO(prolog_eng_status1)
{
  DEREF(X(0), X(0));
  return cunify(Arg, list_of_goals(Arg), X(0));
}
#endif

/* The WAM used by goal is not to be used any more.  Remove the
   choicepoints and the possible dynamic concurrent choicepoints,
   unlink it fro the goal descriptor and return it to the free wam
   list. */

void unlink_wam(goal_descriptor_t *goal)
{
  worker_t *w;

  if ((Arg = goal->worker_registers)) {
    /*    goal->worker_registers = NULL; */
#if defined(THREADS)		/* Clean the possible conc. chpt. */
    remove_link_chains(&TopConcChpt, InitialNode);
#endif
    release_wam(Arg);
  }
}

/* A goal descripter state is to be marked as free --- no thread is
   working on it.  It is not, however, deleted from the state list, or
   the WAM freed, for creating areas is a costly process.  However, we
   move it to the beginning of the list.  We should have exclusive
   access to it, therefore I do not protect the goal descriptor areas.
   The WAM is put back in the lis of available WAMs. */

void make_goal_desc_free(goal_descriptor_t *goal)
{

  unlink_wam(goal);		/* Clean WAM, put it back to free list */
  Wait_Acquire_slock(goal_desc_list_l);
  goal->state = IDLE;
				/* Unlink from current place */
  goal->backward->forward = goal->forward;
  goal->forward->backward = goal->backward;
				/* Link at the beginning */
  goal->forward = goal_desc_list;
  goal->backward = goal_desc_list->backward;
  goal_desc_list->backward->forward = goal;
  goal_desc_list->backward = goal;
  goal_desc_list = goal;

  Release_slock(goal_desc_list_l);
}

/* TODO: this can be implemented more easily by declaring thread-local
   variables, supported by GCC -- JFMC */
/* What goal descriptor am I working for, if I do not know which is my
   WAM?  Its use is only justified when we are recovering from an
   interruption */

worker_t *get_my_worker(void)
{
  THREAD_ID thr_id = Thread_Id;
  goal_descriptor_t *this_goal = goal_desc_list->backward;

  /* Freeze the status of the goal descriptor list */

  Wait_Acquire_slock(goal_desc_list_l);

  /* Go backwards: the goals at the beginning are free. */

  while( (this_goal != goal_desc_list) &&
         ((this_goal->state != WORKING ) ||
          !Thread_Equal(this_goal->thread_id, thr_id)))
    this_goal = this_goal->backward;
  Release_slock(goal_desc_list_l);
  
  if (Thread_Equal(this_goal->thread_id, thr_id) &&
      (this_goal->state == WORKING))
    return this_goal->worker_registers;
  else
    SERIOUS_FAULT("Could not find goal descriptor");    
}


/* Return a free goal descriptor from the ring.  Mark it as WORKING as
   soon as we find one free so that no other thread can steal it. If
   there is any free descriptor, it should appear at the beginning of
   the chain.
*/

goal_descriptor_t *look_for_a_free_goal_desc(void)
{
  goal_descriptor_t *free_goal_desc;

  Wait_Acquire_slock(goal_desc_list_l);
  if (goal_desc_list->state == IDLE) {
    free_goal_desc = goal_desc_list;
    goal_desc_list = goal_desc_list->forward; 
    free_goal_desc->state = WORKING;
    free_goal_desc->goal_number = ++global_goal_number;
    /* Init_slock(free_goal_desc->goal_lock_l); */
  } else free_goal_desc = NULL;
  Release_slock(goal_desc_list_l);
  return free_goal_desc;
}




void enqueue_thread(THREAD_T thread)
{
  Wait_Acquire_slock(thread_to_free_l);
  if (thread_to_free) 
    Thread_Join(thread_to_free);
  thread_to_free = thread;
  Release_slock(thread_to_free_l);
}
