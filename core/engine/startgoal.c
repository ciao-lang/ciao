/*
 *  startgoal.c
 *
 *  Support code for starting goal execution.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

/* TODO: Change the name of this file to rungoals.c */

#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/wam_macros.h>
#include <ciao/os_signal.h>
#include <ciao/task_areas.h>

#include <ciao/wam_alloc.h>
#include <ciao/io_basic.h>
#include <ciao/start.h>
#include <ciao/startgoal.h>
#include <ciao/tasks.h>
#include <ciao/term_support.h>
#include <ciao/wam.h>
#if defined(DEBUG)
#include <ciao/locks.h>
#endif
#include <ciao/wamsupport.h>
#include <ciao/interrupt.h>
#include <ciao/indexing.h> /* empty_gcdef_bin() */

/* Here with w->next_insn set up -- see local_init_each_time(). (MCL) */

SIGJMP_BUF abort_env; /* Shared */

CBOOL__PROTO(eng_killothers_hook_)
{
  return TRUE;
}

cbool_pred_t eng_killothers_startgoal = eng_killothers_hook_;

int firstgoal(goal_descriptor_t *goal_desc, tagged_t goal_term) {
  int i, exit_code;
  worker_t *w;

  Arg = goal_desc->worker_registers;
  Arg->node->term[0] = X(0) = goal_term;
  Arg->next_insn = bootcode;

  while(TRUE) {
    i = SIGSETJMP(abort_env);
    if (i == 0){                /* Just made longjmp */
      Arg->term[0] = Arg->node->term[0];
      wam_initialized = TRUE;
      exit_code = wam(Arg, goal_desc);
      Arg = goal_desc->worker_registers; /* segfault patch -- jf */
      flush_output(Arg);
      if (exit_code != WAM_ABORT) /* halting... */
        break;
    }
    else if (i == -1) {         /* SIGINT during I/O */
      tagged_t *pt1;
      /* No need to patch "p" here, since we are not exiting wam() */
      bcp_t p = (bcp_t)int_address;
      int_address = NULL;
      SETUP_PENDING_CALL(address_true);
      continue;
    }
#if defined(THREADS)
    eng_killothers_startgoal(Arg);
#endif

    {
      wam_initialized = FALSE;                    /* disable recursive aborts */
      reinitialize_wam_areas(Arg); /* aborting... */
      empty_gcdef_bin(Arg); /* TODO: here? */
      fflush(stdout); /* TODO: here? */
      fflush(stderr); /* TODO: here? */
      wam_initialized = TRUE;
    }

    init_each_time(Arg);                /* Sets X(0) to point to bootcode */
    {
      /* Patch predicate call at bootcode */
      bcp_t P = bootcode;
      P = BCoff(P, FTYPE_size(f_o)); /* CALLQ */
      P = BCoff(P, FTYPE_size(f_Q)); /* 0 */
      EMIT_E(address_restart);
    }
  }
  return exit_code;
}


/* Here with wam and goal */

THREAD_RES_T startgoal(THREAD_ARG wo)
{
  worker_t *w;
  goal_descriptor_t *goal_desc = (goal_descriptor_t *)wo;
  int result_state;
  int wam_result;

  Arg = goal_desc->worker_registers;
  Arg->next_insn = startgoalcode;
  Arg->next_alt = NULL;  /* Force backtracking after alts. exahusted */
  Arg->node->term[0] = X(0);    /* Will be the arg. of a call/1 */
 
#if defined(DEBUG) && defined(THREADS)
  if (debug_threads)
    printf("%d (%d) Goal %x (with starting point %x) entering wam()\n",
           (int)Thread_Id, (int)GET_INC_COUNTER, 
           (int)goal_desc, (int)X(0));
#endif

  wam_result = wam(Arg, goal_desc);    /* segfault patch -- jf */

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads)
    printf("%d (%d) Goal %x (with starting point %x) exiting wam()\n",
           (int)Thread_Id, (int)GET_INC_COUNTER, 
           (int)goal_desc, (int)X(0));
#endif

  if (wam_result == WAM_ABORT) {
    MAJOR_FAULT("Wam aborted!");
  }
  Arg = goal_desc->worker_registers;
  
#if defined(DEBUG) && defined(THREADS)
      if (debug_threads)
        printf("%d (%d) Goal %x exited wam()\n", 
               (int)Thread_Id, (int)GET_INC_COUNTER, (int)goal_desc);
#endif
  flush_output(Arg);

  /* eng_wait() may change NEEDS_FREEING and consults the state of the
     thread; therefore we lock until it is settled down */
  
  Wait_Acquire_slock(goal_desc->goal_lock_l);
  if (goal_desc->worker_registers->next_alt == termcode){
    unlink_wam(goal_desc);	/* We can make the WAM available right now */
    goal_desc->state = FAILED;
  } else goal_desc->state = PENDING_SOLS;

/* In some cases (i.e., Win32) the resources used up by the thread are
   not automatically freed upon thread termination.  If needed, the
   thread handle is enqued. In an (I hope) future implementation the
   thread will go to sleep instead of dying. */

  if (goal_desc->action & NEEDS_FREEING){ /* Implies thread created */
#if defined(DEBUG) && defined(THREADS)
    if (debug_threads) printf("Goal %x enqueuing itself\n", (int)goal_desc);
#endif
    enqueue_thread(goal_desc->thread_handle); /* Free, enqueue myself */
  } else   
    enqueue_thread((THREAD_T)NULL); /* Free whoever was there, enqueue no one*/
  
/* Save the state for the exit result (if we release the goal, its
   state may change before we return from the function). */
  result_state = goal_desc->state;

/* Goals failed when executed by the local thread, and goals for which
   no Id was requested, release the memory areas automatically */
  if ((wam_result == WAM_INTERRUPTED) ||
      !(goal_desc->action & KEEP_STACKS) ||
      ((goal_desc->state == FAILED) && !(goal_desc->action & CREATE_THREAD)))
    make_goal_desc_free(goal_desc);

  Release_slock(goal_desc->goal_lock_l);

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads || debug_conc)  printf("*** %d(%d) Goal %x is EXITING\n", 
           (int)Thread_Id, (int)GET_INC_COUNTER, (int)goal_desc);
#endif
  return (THREAD_RES_T)(uintptr_t)(result_state == PENDING_SOLS);
}


/* If we hit the initial ghost choicepoint, then it means that no
   solution was returned by this call.  If we call the
   make_backtracking() pirmitive, then KEEP_STACKS is true. */

THREAD_RES_T make_backtracking(THREAD_ARG wo)
{
  goal_descriptor_t *goal_desc = (goal_descriptor_t *)wo;
  int result_state;
  int wam_result;
  worker_t *w = goal_desc->worker_registers;

  /* segfault patch -- jf */
  wam_result = wam(Arg, goal_desc);
  if (wam_result == WAM_ABORT) {
    MAJOR_FAULT("Wam aborted while doing backtracking");
  }
  Arg = goal_desc->worker_registers;

  flush_output(Arg);

  Wait_Acquire_slock(goal_desc->goal_lock_l);
  if (Arg->next_alt == termcode) {
    unlink_wam(goal_desc);
    goal_desc->state = FAILED;
  } else goal_desc->state = PENDING_SOLS;

  if ((goal_desc->action & NEEDS_FREEING) ||
      (wam_result == WAM_INTERRUPTED)) /* Implies thread created */
    enqueue_thread(goal_desc->thread_handle); /* Free, enqueue myself */
  else   
    enqueue_thread((THREAD_T)NULL); /* Free whoever was there, enqueue no one*/

  result_state = goal_desc->state;

  /*
  if ((goal_desc->state == FAILED) && !(goal_desc->action & CREATE_THREAD))
    make_goal_desc_free(goal_desc);
  */

  Release_slock(goal_desc->goal_lock_l);

  return (THREAD_RES_T)(uintptr_t)(result_state == PENDING_SOLS);
}






