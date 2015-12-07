/*
 *  wam_alloc.c
 *
 *  Allocation of principal WAM areas.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(BSD)
#include <string.h>
#else
#include <memory.h>
#endif
#include <unistd.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/wam_alloc.h>
#include <ciao/support.h>
#include <ciao/wamsupport.h>
#include <ciao/timing.h>
#include <ciao/initial.h>
#include <ciao/io_basic.h>
#include <ciao/os_utils.h>

 /* # bytes used by the Prolog program & database code.  Probably not
    accurately measured (patched here and there) (MCL).  */
intmach_t mem_prog_count = 0;                                     /* Shared */

 /* Number of predicates asserted */
intmach_t num_of_predicates = 0;                                  /* Shared */

 /* Creates the wam structure, allocates its areas and initializes them.
    This returns an empty, fresh wam.  We do not add it here to the task
    state list; it needs its own thread, which we have after startwam() */

worker_t *create_and_init_wam()
{
  worker_t *w;
  /*intmach_t saved_program_count = mem_prog_count;  */

  Arg = create_wam_storage();                         /* Just create *Arg */
  create_wam_areas(Arg);                      /* Make room for the stacks */
  numstack_init(Arg);                                     /* bignum areas */
  local_init_each_time(Arg);                               /* Local areas */
  return Arg;
}

/* Available workers are queued here */

worker_t *wam_list = NULL;
SLOCK    wam_list_l;

worker_t *free_wam()
{
  worker_t *free_wam;

  Wait_Acquire_slock(wam_list_l);
  if (wam_list) {
    free_wam = wam_list;
    wam_list = Next_Worker(free_wam);
    Release_slock(wam_list_l);
    Next_Worker(free_wam) = NULL;
  } else {
    Release_slock(wam_list_l);
    free_wam = create_and_init_wam();
  }
  return free_wam;
}

CVOID__PROTO(release_wam)
{
  local_init_each_time(Arg);
  Wait_Acquire_slock(wam_list_l);
  Next_Worker(Arg) = wam_list;
  wam_list = Arg;
  Release_slock(wam_list_l);
}


#if defined(ANDPARALLEL)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;
bool_t unwinding_done = FALSE;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif

#if defined(PARBACK)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif

/* TODO: a global variable here is wrong if a clause is asserted in one worker and consulted in other */
int reg_bank_size = XREGBANKSIZE; /* Shared? Strange use in compile_term_aux */

worker_t *create_wam_storage() {
  worker_t *w;

  w = checkalloc_FLEXIBLE(worker_t, tagged_t, reg_bank_size);
  w->misc = checkalloc_TYPE(misc_info_t);
  w->streams = checkalloc_TYPE(io_streams_t);
  w->debugger_info = checkalloc_TYPE(debugger_state_t);

  return w;
}

CVOID__PROTO(create_wam_areas)
{
  int i, j;
  char *cp;

  Atom_Buffer_Length = STATICMAXATOM;
  Atom_Buffer = checkalloc_ARRAY(char, Atom_Buffer_Length);

#if defined(ANDPARALLEL)
  /* Initializing pointers and locks */
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Goal_Cache = NULL;
  Goal_Cache = NULL;
  Dep_Id = NULL;
  Dep_Size = 0;
  Event_Queue_Start = NULL;
  Event_Queue_Top = NULL;
  Last_Parallel_Exec = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Cancel_Goal_Exec = FALSE;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;
  Mode = FORWARD_EXEC;

  Init_slock(Goal_List_Lock);
  Init_slock(Event_Queue_Lock);
  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);

#if defined(VISANDOR)
  Pcall_Level(w) = 0;
  FirstEvent(w)  = checkalloc_ARRAY(visandor_event_t, maxevents);
  NextEvent(w)   = FirstEvent(w);
  LastEvent(w)   = &((FirstEvent(w))[maxevents]);
#endif

#endif

#if defined(PARBACK)
  /* Initializing pointers and locks */
  Act_PF = NULL;
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Cancel_Goal_List_Start = NULL;
  Cancel_Goal_List_Top = NULL;
  Back_Goal_List_Start = NULL;
  Back_Goal_List_Top = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Goal_To_Cancel = NULL;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;

  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);
#endif

  /* heap pointer is first free cell, grows ++ */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  Heap_Start = checkalloc_ARRAY(tagged_t, i);
  Heap_End =  HeapOffset(Heap_Start,i);
  Heap_Warn_Soft =  Heap_Warn =  HeapOffset(Heap_End,-DEFAULT_SOFT_HEAPPAD);

#if defined(USE_OVERFLOW_EXCEPTIONS)
  SOFT_HEAPPAD = DEFAULT_SOFT_HEAPPAD;
  Heap_Limit = 0;
#endif 

  /* stack pointer is first free cell, grows ++ */
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  Stack_Start  = checkalloc_ARRAY(tagged_t, i);
  Stack_End =  StackOffset(Stack_Start,i);
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);

  /* trail pointer is first free cell, grows ++ */
  /* choice pointer is last busy cell, grows -- */
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  Choice_End = Trail_Start = checkalloc_ARRAY(tagged_t, i);
  Choice_Start =  Trail_End = TrailOffset(Trail_Start, i);


 /*  Do not touch the (tagged_t) type casting! Or the emulator will break! */

#if defined(USE_TAGGED_CHOICE_START)
  Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
}

/* Cleanup after abort: shrink stacks to initial sizes. */
CVOID__PROTO(reinitialize_wam_areas)
{
  int i, j;
  char *cp;

  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  if ((j=HeapDifference(Heap_Start,Heap_End)) != i) {
    Heap_Start = checkrealloc_ARRAY(tagged_t, j, i, Heap_Start);
    Heap_End = HeapOffset(Heap_Start,i);
  }
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  if ((j=StackDifference(Stack_Start,Stack_End)) != i) {
    Stack_Start = checkrealloc_ARRAY(tagged_t, j, i, Stack_Start);
    Stack_End = StackOffset(Stack_Start,i);
  }
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  if ((j=TrailDifference(Trail_Start,Trail_End)) != i) {
    Choice_End = Trail_Start = checkrealloc_ARRAY(tagged_t, j, i, Trail_Start);
    Choice_Start = Trail_End = TrailOffset(Trail_Start,i);

#if defined(USE_TAGGED_CHOICE_START)
    Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
  }

  /* Create an expandable char array for loading po files */ 

  if (Atom_Buffer_Length != STATICMAXATOM) {
    Atom_Buffer = checkrealloc_ARRAY(char, 
				     Atom_Buffer_Length,
				     STATICMAXATOM,
				     Atom_Buffer);
    Atom_Buffer_Length = STATICMAXATOM;
  }

  Heap_Warn_Soft = Heap_Warn = HeapOffset(Heap_End,-DEFAULT_SOFT_HEAPPAD);
#if defined(USE_OVERFLOW_EXCEPTIONS)
  SOFT_HEAPPAD = DEFAULT_SOFT_HEAPPAD;
  Heap_Limit = 0;
#endif 

  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
}

/*static char *mem_start; */  /* beginning of our virtual memory -- Shared */

/*  mem_start wrongly calculated, and mem_prog_count set to zero only once */
/*
void mem_prog_reset()
{
    mem_start = (char *)(&end);
#if SMALLPTR_BASE
  if (mem_start < (char *)SMALLPTR_BASE)
    mem_start = (char *)SMALLPTR_BASE;
#endif

  mem_prog_count = 0;
}
*/

/* program_usage: [sizeof_used_space, 0] */
CBOOL__PROTO(program_usage)
{
  tagged_t x;

  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,mem_prog_count),x);
  return cunify(Arg,X(0),x);
}

/* internal_symbol_usage: [number_atoms_funcs_preds, number_pred_defs] */
CBOOL__PROTO(internal_symbol_usage)
{
  tagged_t x;

  MakeLST(x,MakeInteger(Arg,num_of_predicates),atom_nil);
  MakeLST(x,MakeInteger(Arg,ciao_atoms->count),x);
  return cunify(Arg,X(0),x);
}


/* total_usage: [total_space, 0].  Changed to use total_mem_count (MCL) */
CBOOL__PROTO(total_usage)
{
  tagged_t x;
  intmach_t n;

  n = total_mem_count;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,n),x);
  return cunify(Arg,X(0),x);
}


CBOOL__PROTO(statistics)
{
  stream_node_t *s = Output_Stream_Ptr;
  intmach_t used, free;
  inttime_t runtick0;
  inttime_t usertick0 = usertick();
  inttime_t systemtick0 = systemtick();
  inttime_t walltick0 = walltick();
  frame_t *newa;

  runtick0=usertick0;

  ENG_PRINTF(s,
	     "memory used (total)    %10" PRIdm " bytes\n",
	     total_mem_count);
  ENG_PRINTF(s, 
	     "   program space (including reserved for atoms): %" PRIdm " bytes\n", 
	     mem_prog_count);

  ENG_PRINTF(s,
	     "   number of atoms and functor/predicate names: %" PRIdm "\n", 
	     ciao_atoms->count);
  ENG_PRINTF(s,
	     "   number of predicate definitions: %" PRIdm "\n", 
	     num_of_predicates);

  used = HeapCharDifference(Heap_Start,w->global_top);
  free = HeapCharDifference(w->global_top,Heap_End);
  ENG_PRINTF(s, 
	     "   global stack   %10" PRIdm " bytes:%" PRIdm " in use,%10" PRIdm " free\n",
	     used+free, used, free);

  ComputeA(newa,w->node);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  ENG_PRINTF(s,
	     "   local stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
	     used+free, used, free);

  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->node)/2;
  ENG_PRINTF(s,
	     "   trail stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
	     used+free, used, free);

  used = ChoiceCharDifference(Choice_Start,w->node);
  free = ChoiceCharDifference(w->node,w->trail_top)/2;
  ENG_PRINTF(s,
	     "   control stack  %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n\n",
	     used+free, used, free);

  ENG_PRINTF(s,
	     " %10.6f sec. for %" PRIdm " global, %" PRIdm " local, and %" PRIdm " control space overflows\n",
	     ((flt64_t)ciao_statistics.ss_tick)/GET_CLOCKFREQ(ciao_statistics),
	     ciao_statistics.ss_global,
	     ciao_statistics.ss_local, ciao_statistics.ss_control);
  ENG_PRINTF(s,
	     " %10.6f sec. for %" PRIdm " garbage collections which collected %" PRIdm " bytes\n\n",
	     ((flt64_t)ciao_statistics.gc_tick)/GET_CLOCKFREQ(ciao_statistics),
	     ciao_statistics.gc_count,
	     (intmach_t)(ciao_statistics.gc_acc*sizeof(tagged_t)));

  ENG_PRINTF(s,
	     " runtime:    %10.6f sec. %12lld ticks at %12lld Hz\n",
	     (flt64_t)(runtick0-ciao_statistics.starttick)/GET_CLOCKFREQ(ciao_statistics),
	     runtick0-ciao_statistics.starttick,
	     GET_CLOCKFREQ(ciao_statistics));
  ENG_PRINTF(s,
	     " usertime:   %10.6f sec. %12lld ticks at %12lld Hz\n",
	     (flt64_t)(usertick0-ciao_statistics.startusertick)/ciao_statistics.userclockfreq,
	     usertick0-ciao_statistics.startusertick,
	     ciao_statistics.userclockfreq);
  ENG_PRINTF(s,
	     " systemtime: %10.6f sec. %12lld ticks at %12lld Hz\n",
	     (flt64_t)(systemtick0-ciao_statistics.startsystemtick)/ciao_statistics.systemclockfreq,
	     systemtick0-ciao_statistics.startsystemtick,
	     ciao_statistics.systemclockfreq);

  ENG_PRINTF(s,
	     " walltime:   %10.6f sec. %12lld ticks at %12lld Hz\n\n",
	     (flt64_t)(walltick0-ciao_statistics.startwalltick)/ciao_statistics.wallclockfreq,
	     walltick0-ciao_statistics.startwalltick,
	     ciao_statistics.wallclockfreq);

  return TRUE;
}
