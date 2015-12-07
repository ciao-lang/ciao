#include <unistd.h>

/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/support.h>
#include <ciao/term_support.h>
#include <ciao/threads.h>
#include <ciao/locks_prim.h>
#include <ciao/global_defs.h>
#include <ciao/task_areas.h>
#include <ciao/wam_macros.h>
#include <ciao/wam.h>
#include <ciao/tasks.h>
#include <ciao/startgoal.h>
#include <ciao/nondet.h>
#include <ciao/start.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/streams_basic.h>

#include <ciao_prolog.h>

#include "concurrency.h"

/* local declarations */

int killing_threads = FALSE;  /* Set to TRUE when killing other threads to
                                 disable fast spawning of new threads. */

 /* POSIX defines a maximum (_PTHREAD_THREADS_MAX) on the number of threads
    per process --- 64, I think .  Implementations can go beyond this
    number.  I will allow 1024 simultaneous threads.  After the death of a
    thread, more can (if the implementation supports it) be created*/

/* Kill a thread.  We need cooperation from the thread! */

CBOOL__PROTO(prolog_eng_kill)
{
  ERR__FUNCTOR("concurrency:$eng_kill", 1);
  goal_descriptor_t *goal_to_kill;

  DEREF(X(0), X(0));
  if (!IsNumber(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);
  } else {
    goal_to_kill = TermToGoalDesc(X(0));

    if (goal_to_kill->state == IDLE)
      USAGE_FAULT("Trying to kill an IDLE worker")

    if (goal_to_kill == Arg->misc->goal_desc_ptr)
      return TRUE;

    if (goal_to_kill->state == WORKING) {
      Arg = goal_to_kill->worker_registers;
      Stop_This_Goal(Arg) = TRUE;
      Heap_Warn_Soft = HeapCharOffset(Heap_Start, -1);
    }
    return TRUE;
  }
}

extern goal_descriptor_t *goal_desc_list;
extern SLOCK goal_desc_list_l;

/* AMADEO */
CBOOL__PROTO(eng_killothers_hook)
{
  return prolog_eng_killothers(Arg);
}

CBOOL__PROTO(prolog_eng_killothers)
{
  goal_descriptor_t *myself;
  goal_descriptor_t *goal_ref;
  bool_t thread_cancelled;

  killing_threads = TRUE;
  thread_cancelled = TRUE;
  myself = Arg->misc->goal_desc_ptr;
  goal_ref = goal_desc_list;

/* First, tell all the active threads to quit working; use the internal
   event system. */
  do {
    if ((goal_ref != myself) && (goal_ref->state == WORKING)){
      Arg = goal_ref->worker_registers;
      Stop_This_Goal(Arg) = TRUE;
      Heap_Warn_Soft = HeapCharOffset(Heap_Start, -1);
      thread_cancelled = TRUE;
    }
    goal_ref = goal_ref->forward;
  } while (goal_ref != goal_desc_list);


  /* Some of them may need a little time to reach the appropiate point.  I
     know this is really a kludge, but since we have no RTS here, I see no
     other means of doing that.  */
  if (thread_cancelled) sleep(1);

  /* If any thread has not finished yet, then it may mean it is stucked or
     blocked.  Cancel it explicitly. */

  thread_cancelled = FALSE;
  do {
    if ((goal_ref != myself) && (goal_ref->state == WORKING)){
#if defined(DEBUG)
      /* printf("Canceling thread %x\n", goal_ref); */
#endif
      Thread_Cancel(goal_ref->thread_handle);
      thread_cancelled = TRUE;
    }
    goal_ref = goal_ref->forward;
  } while (goal_ref != goal_desc_list);

  /* Adjust the list: free every non-IDLE descriptor but ourselves.  But if
     any thread was cancelled, we better wait for it to really stop
     working. */

  if (thread_cancelled) sleep(2);
  reinit_list(myself);

  killing_threads = FALSE;

  return TRUE;
}



/* Wait for a goal to finish */

CBOOL__PROTO(prolog_eng_wait)
{
  ERR__FUNCTOR("concurrency:$eng_wait", 1);
  goal_descriptor_t *this_goal;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    {BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);}
  else  {
#if defined(DEBUG)
    if (debug_threads) printf("About to join goal %ld\n", GetSmall(X(0)));
#endif
    this_goal = TermToGoalDesc(X(0));
  }

  /* Waiting for oneself is a deadlock */
  if (this_goal == Arg->misc->goal_desc_ptr){
    MAJOR_FAULT("Goal waiting for itself!");
  }
  Wait_Acquire_slock(this_goal->goal_lock_l);
  if (this_goal->state == WORKING) { /* It does not need to enqueue itself */
    this_goal->action &= ~NEEDS_FREEING;
    Release_slock(this_goal->goal_lock_l);
    /*enqueue_thread((THREAD_T)NULL); */ /* Help others... */
    Thread_Join(this_goal->thread_handle);
  } else if (this_goal->state == IDLE) {
    Release_slock(this_goal->goal_lock_l);
    MAJOR_FAULT("Waiting for an IDLE goal!");
  } else Release_slock(this_goal->goal_lock_l);

#if defined(DEBUG)
    if (debug_threads) printf("Join goal %ld joined\n", GetSmall(X(0)));
#endif
  return TRUE;
}


/* Unifies its argument with the worker number of this task. */

CBOOL__PROTO(prolog_eng_self)
{
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  return
    cunify(Arg, X(0), GoalDescToTerm(Arg->misc->goal_desc_ptr)) &&
    cunify(Arg,
           X(1),
           MakeInteger(Arg, Arg->misc->goal_desc_ptr->goal_number)
           );
}


 /* Prints info about the status of the launched tasks and memory areas used
    by them. */

CBOOL__PROTO(prolog_eng_status)
{
  print_task_status(Arg);
  return TRUE;
}


#define NOT_CALLABLE(What) IsVar(What) || TagIsSmall(What) || TagIsLarge(What)

#define ENSURE_CALLABLE(What, ArgNum)			\
  if (NOT_CALLABLE(What)) {				\
    BUILTIN_ERROR(TYPE_ERROR(CALLABLE), What, ArgNum);	\
  }


/* When we release a goal, we have to close the handle to the
   descriptor (when the goal is waiting, the thread should have
   finished) and we deallocate the goal descriptor. */

CBOOL__PROTO(prolog_eng_release)
{
  ERR__FUNCTOR("concurrency:$eng_release", 1);
  goal_descriptor_t *goal;

  DEREF(X(0), X(0));

  if (!IsNumber(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);

  goal = TermToGoalDesc(X(0));
  if ((goal->state != PENDING_SOLS) &&
      (goal->state != FAILED))
    MAJOR_FAULT("Trying to release a worker working or without assigned work")

  make_goal_desc_free(goal);
  return TRUE;
}


CBOOL__PROTO(prolog_eng_call)
{
  ERR__FUNCTOR("concurrency:$eng_call", 4);
  goal_descriptor_t *gd;
  int           create_thread = NO_ACTION;
  int           create_wam    = NO_ACTION;
  int           keep_stacks   = NO_ACTION;
  bool_t          exec_result;

  if (killing_threads) return FALSE;

  DEREF(X(0), X(0));         /* Make sure we are calling a callable term! */
  ENSURE_CALLABLE(X(0), 1);

  DEREF(X(1), X(1));              /* Create a wam or wait for a new one? */
  if ((X(1) == atom_wait) || X(1) == atom_create)
    create_wam = CREATE_WAM;                     /* By now, always create */
  else
    return FALSE;

  DEREF(X(2), X(2));           /* Create a thread, or wait for a new one? */
  if ((X(2) == atom_wait) || X(2) == atom_create)  /* distinguish later */
    create_thread = CREATE_THREAD;
  else
    if (X(2) != atom_self) return FALSE;

  DEREF(X(5), X(5));
  if (X(5) == atom_true) keep_stacks = KEEP_STACKS;

  gd = gimme_a_new_gd();	/* In a future we will wait for a free wam */

  gd->goal = X(0);		/* Got goal id + memory space, go on! */
  gd->action = create_wam | keep_stacks | create_thread;

  {  				            /* Copy goal to remote thread */
    /* Incredible hack: we set X(0) in the new worker to point to the goal
       structure copied in the memory space of that new worker. We can use
       the already existent macros just by locally renaming the Arg (c.f.,
       "w") worker structure pointer. */

    worker_t *w = gd->worker_registers;
    DEREF(X(0), cross_copy_term(Arg, gd->goal));
#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) 
    printf("Cross-copied starting goal from %x to %x\n", 
            (int)gd->goal, (int)X(0));
#endif
  }

  if (create_thread) {                            /* Always request ID! */
    gd->action |= NEEDS_FREEING;
    Thread_Create_GoalId(startgoal,
                         gd,
                         gd->thread_id,
                         gd->thread_handle);
    exec_result = TRUE;		/* Remote thread: always success */
  } else
    exec_result = (bool_t)startgoal((THREAD_ARG)(gd));

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("Goal %x created, continuing\n", (int)gd);
#endif

  return
    cunify(Arg, X(3), GoalDescToTerm(gd)) &&
    cunify(Arg, X(4), MakeInteger(Arg, gd->goal_number)) &&
    exec_result;
}


/* Backtrack over the worker ID passed as first argument.  The first
   thread which asks backtracking grabs a lock and changes the status
   of the goal being backtracked over to WORKING, so others signal an
   error. */

CBOOL__PROTO(prolog_eng_backtrack)
{
  ERR__FUNCTOR("concurrency:$eng_backtrack", 2);
  goal_descriptor_t *goal;
  int create_thread = NO_ACTION;
  bool_t exec_result;

  if (killing_threads) return FALSE;

  DEREF(X(0), X(0));                        /* Make sure we have a number */
  if (!IsNumber(X(0))){
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);
  }
  goal = TermToGoalDesc(X(0));

  DEREF(X(1), X(1)); /* Create a thread, or wait for a new one? */
  if ((X(1) == atom_wait) || X(1) == atom_create)    /* distinguish later */
    create_thread = CREATE_THREAD;
  else
    if (X(1) != atom_self)
      MAJOR_FAULT("eng_backtrack/2: bad thread creation specifier")

  /* Other threads might see this one and try to backtrack over it. */
  Wait_Acquire_slock(goal->goal_lock_l);

  /* Trying to backtrack over an already failed goal? */
  if (goal->state == FAILED) {
    Release_slock(goal->goal_lock_l);
    /* Local backtracking fails, remote threads always succeed. */
    return (create_thread == CREATE_THREAD);
  } else if (goal->state != PENDING_SOLS) {
      Release_slock(goal->goal_lock_l);
      MAJOR_FAULT("Trying to backtrack over a non-assigned goal.")
    }

  /* Then, we have a worker which is waiting. We ask for backtracking.
    If we are running locally and there are no more solutions, we
    fail. */
  goal->state = WORKING;

  Release_slock(goal->goal_lock_l);
  goal->action = BACKTRACKING | create_thread;

  if (create_thread) {
    goal->action |= NEEDS_FREEING;
    Thread_Create_GoalId(make_backtracking,
			 goal,
			 goal->thread_id,
			 goal->thread_handle);
    exec_result = TRUE;	   /* thread-delegated backtracking always suceeds */
  } else {
    goal->action &= ~NEEDS_FREEING;
    exec_result = (bool_t)make_backtracking((THREAD_ARG)goal);
  }
  return exec_result;
}


/* We should also have thread_delegated cut... */

CBOOL__PROTO(prolog_eng_cut)
{
  ERR__FUNCTOR("concurrency:$eng_cut", 1);
  goal_descriptor_t *goal_desc;

  /*
    set w->node  (that is what DOCUT does), call fail...
    look at metacut, remember to delete the conc. data structures...
  */


  DEREF(X(0), X(0));
  if (!IsNumber(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);
  goal_desc = TermToGoalDesc(X(0));

  Wait_Acquire_slock(goal_desc->goal_lock_l);

  if (goal_desc->state == FAILED){ /* Nothing to do , then */
    Release_slock(goal_desc->goal_lock_l);
    return TRUE;
  } else if (goal_desc->state != PENDING_SOLS){
    Release_slock(goal_desc->goal_lock_l);
    MAJOR_FAULT("Trying to cut a working or non assigned goal")
  }

  goal_desc->state = WORKING;	/* Nobody else should access it */
  Release_slock(goal_desc->goal_lock_l);

  goal_desc->action |= BACKTRACKING;

  {
    worker_t *w = goal_desc->worker_registers;
    w->node = InitialNode;            /* DOCUT to the initial choicepoint */
            /* For concurrent goals, erase the concurrent data structures */
    ConcChptCleanUp(TopConcChpt, w->next_node);
  }

  if (wam(goal_desc->worker_registers, goal_desc) == WAM_ABORT)
    MAJOR_FAULT("Cut in wam finished with abort");

  Wait_Acquire_slock(goal_desc->goal_lock_l);
  goal_desc->state = FAILED;
  Release_slock(goal_desc->goal_lock_l);

  return TRUE;
}


/* Find data from a thread id */

intmach_t goal_from_thread_id_hook(THREAD_ID id)
{
  return (intmach_t)goal_from_thread_id(id);
}

extern intmach_t (*eng_goal_from_thread_id)(THREAD_ID id);
extern cbool_pred_t eng_killothers_startgoal;

CBOOL__PROTO(prolog_concurrency_init)
{
  eng_goal_from_thread_id = goal_from_thread_id_hook;
  eng_killothers_startgoal = eng_killothers_hook;
  return TRUE;
}

intmach_t goal_from_thread_id(THREAD_ID id)
{
  goal_descriptor_t *initial_gdesc = goal_desc_list;
  goal_descriptor_t *running_gdesc = initial_gdesc->forward;
  // Make a search in existing structures
  while (
         (running_gdesc != initial_gdesc) &&
         (running_gdesc->state != IDLE)   && // Idle wams are not meaningful
         (running_gdesc->thread_id != id)
         )
    running_gdesc = running_gdesc->forward;

  // Find out in which case we are
   
    if ((running_gdesc->state != IDLE) && 
        (running_gdesc->thread_id == id))
      return running_gdesc->goal_number;
    else 
      return 0;
}
