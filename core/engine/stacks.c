/*
 *  stacks.c
 *
 *  Code for growing areas when full.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#if defined(DEBUG) || defined(THREADS)
#include <ciao/threads.h>
#endif

#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/alloc.h>
#include <ciao/wamsupport.h>

#include <ciao/stacks.h>
#include <ciao/heapgc.h>
#include <ciao/start.h>
#include <ciao/timing.h>
#if defined(DEBUG)
#include <ciao/locks.h>
#endif
#include <ciao/io_basic.h>

static CVOID__PROTO(calculate_segment_node);

/* stack_shift_usage: [global shifts,local+control/trail shifts,time spent] */

CBOOL__PROTO(stack_shift_usage)
{
  tagged_t x;
  inttime_t time = (ciao_statistics.ss_tick*1000)/
                   GET_CLOCKFREQ(ciao_statistics);
  
  MakeLST(x,MakeInteger(Arg,time),atom_nil);
  time = ciao_statistics.ss_local+ciao_statistics.ss_control;
  MakeLST(x,MakeInteger(Arg,time),x);
  time = ciao_statistics.ss_global;
  MakeLST(x,MakeInteger(Arg,time),x);
  return cunify(Arg,X(0),x);
}

/* termheap_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(termheap_usage)
{
  intmach_t used, free;
  tagged_t x;
  
  used = HeapCharDifference(Heap_Start,w->global_top);
  free = HeapCharDifference(w->global_top,Heap_End);
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* envstack_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(envstack_usage)
{
  intmach_t used, free;
  tagged_t x;
  frame_t *newa;

  ComputeA(newa,w->node);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* choice_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(choice_usage)
{
  intmach_t used, free;
  tagged_t x;
  
  used = ChoiceCharDifference(Choice_Start,w->node);
  free = ChoiceCharDifference(w->node,w->trail_top)/2;
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* trail_usage: [sizeof_used_space, sizeof_free_space] */
CBOOL__PROTO(trail_usage)
{
  intmach_t used, free;
  tagged_t x;
  
  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->node)/2;
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}


/* Service routine for HEAPMARGIN* instructions.
 * pad - required amount of heap space.
 * arity - number of live X regs at this point.
 */
CVOID__PROTO(explicit_heap_overflow, intmach_t pad, intmach_t arity)
{
  node_t *b = w->node;
  intmach_t i;
  frame_t *a;

#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %" PRIdm " calling explicit_heap_overflow\n", (intmach_t)Thread_Id);
#endif

  
  /* ensure that w->node is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* DO NOT clear w->next_alt -- we are still in "shallow mode" */
  if (!b->next_alt) {			/* try */
    b->next_alt = w->next_alt; /* 4 contiguous moves */
    b->frame = w->frame;
    b->next_insn = w->next_insn;
    SaveLtop(b);
    i=OffsetToArity(b->next_alt->node_offset);
    if (i>0){
      tagged_t *t = (tagged_t *)w->next_node;
      
      do
        ChoicePush(t,X(--i));
      while (i>0);
    }
    if (ChoiceYounger(ChoiceOffset(b,CHOICEPAD),w->trail_top))
      choice_overflow(Arg,CHOICEPAD),
	b = w->node;
  }
  
  /* ensure that X regs are seen by heap_overflow(): make a frame */
  ComputeA(a,b);
  a->term[0] = TaggedZero;
  for (i=0; i<arity; i++)
    a->term[i+1] = X(i);
  a->frame = w->frame;
  a->next_insn = w->next_insn;
  w->frame = a;
  w->next_insn = CONTCODE(i+1);
  w->local_top = (frame_t *)Offset(a,EToY0+i+1);
  heap_overflow(Arg,pad);
  for (i=0; i<arity; i++)
    X(i) = a->term[i+1];
  w->local_top = a;
  w->frame = a->frame;
  w->next_insn = a->next_insn;
}


/* Set w->segment_node to most recent choicept which is marked as pure. */
static CVOID__PROTO(calculate_segment_node)
{
  node_t *n;

  w->segment_node = NULL;
  for (n=w->node;
       w->segment_node==NULL;
       n=ChoiceCharOffset(n,-n->next_alt->node_offset))
    if (ChoiceptTestPure(n))
      w->segment_node = n;
}


#if defined(ANDPARALLEL)
bool_t is_rem_Hterm(tagged_t term,
		    worker_t *w,
		    worker_t *remote_w)
{
  if (remote_w == NULL)  // local case
    return !(((TagToPointer(term) >= w->heap_start) &&
             (TagToPointer(term) <= w->heap_end)));
  else  // remote case
    return ((TagToPointer(term) >= remote_w->heap_start) &&
            (TagToPointer(term) <= remote_w->heap_end));
}
#endif


/* Here when w->node and w->trail_top are within CHOICEPAD from each other. */
CVOID__PROTO(choice_overflow, intmach_t pad)
{
  inttime_t tick0;
  tagged_t *choice_top;
  try_node_t *next_alt;

#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %" PRIdm " calling choice overflow\n", (intmach_t)Thread_Id);
#endif

#if defined(ANDPARALLEL)
#if defined(DEBUG)
  if (debug_threads) {
    printf("\nWAM %x is in choice_overflow!\n",(unsigned int)w);
    printf("w->node and w->trail_top are within CHOICEPAD from each other.\n");
    fflush(stdout);
  }
#endif

  /* Suspend the rest of the agents and wait until that happens completely */
  worker_t *aux = NULL;
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == RELEASED)
      Suspend_Of(aux) = TOSUSPEND;
  }
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux))
    while (Suspend_Of(aux) == RELEASED) {
      if (Suspend_Of(aux) == RELEASED)
	Suspend_Of(aux) = TOSUSPEND;
    }
#endif

  tick0 = BASE_RUNTICK;

  if (!(next_alt = w->node->next_alt)) /* ensure A', P' exist */
    w->node->next_alt = w->next_alt,
    SaveLtop(w->node);

  if (pad<0)
    pad = -pad;			/* in compile_term: disable trail_gc */
  else {
    calculate_segment_node(Arg);
    trail_gc(Arg);
    compressTrail(Arg,FALSE);
  }
				/* ASSUMED: --CHOICE, TRAIL++ */

  choice_top = (tagged_t *)w->node+w->value_trail;
  if (ChoiceYounger(ChoiceOffset(choice_top,2*pad),w->trail_top)) {
    node_t *b;
    tagged_t *newtr;
    intmach_t mincount, newcount, oldcount, reloc_factor;
    
    {
      mincount = 2*pad - ChoiceDifference(choice_top,w->trail_top);
      oldcount = ChoiceDifference(Choice_Start,Choice_End);
      newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
      newtr = checkrealloc_ARRAY(tagged_t,
				 oldcount,
				 newcount,
				 Trail_Start);
#if defined(DEBUG)
      if (debug_gc)
        printf("Thread %" PRIdm " is reallocing TRAIL from %p to %p\n", 
               (intmach_t)Thread_Id, Trail_Start, newtr);
#endif
    }
    reloc_factor = (char *)newtr - (char *)Trail_Start;
    {
      tagged_t *tr;
      tagged_t *trb;
      
      tr = (tagged_t *)((char *)Choice_Start+reloc_factor);
      trb = (tagged_t *)((char *)choice_top+reloc_factor);
      Trail_Start = Choice_End = newtr;                /* new low bound */
      Choice_Start = Trail_End = newtr+newcount;      /* new high bound */
      /* Do not take out (tagged_t) casting, or the engine will break!! */

#if defined(USE_TAGGED_CHOICE_START)
      Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
      {
        tagged_t *x;
	/* We have to relocate the concurrent topmost choicepoint */
#if defined(THREADS)
        node_t *concchpt;
#endif
        
        x = Choice_Start;                  /* Copy the new choicepoint stack */
        while (OffChoicetop(trb,tr))
          ChoicePush(x,ChoiceNext(tr));
        w->node = b = (node_t *)(x-w->value_trail);

#if defined(THREADS)
        /* The chain of concurrent dynamic choicepoints has to be
           relocated as well.  The initial TopConcChpt was set to be
           the initial choice node.  MCL. */
        concchpt = TopConcChpt =
          (node_t *)((char *)TopConcChpt + reloc_factor +
                          (newcount-oldcount)*sizeof(tagged_t));

        while(concchpt != InitialNode) {
#if defined(DEBUG)
          if (debug_concchoicepoints || debug_gc)
            printf("*** %" PRIdm "(%" PRIdm ") Changing dynamic chpt@%x\n",
                   (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
                   (unsigned int)concchpt);
#endif
          concchpt->term[PrevDynChpt] =
            PointerToTermOrZero(
                (node_t *)((char *)TermToPointerOrNull(concchpt->term[PrevDynChpt])
                                + reloc_factor
                                + (newcount-oldcount)*sizeof(tagged_t))
                );
          concchpt = (node_t *)TermToPointerOrNull(concchpt->term[PrevDynChpt]);
        }
#endif
      }
    }
    w->next_node =
      (node_t *)((char *)w->next_node + reloc_factor +
                      (newcount-oldcount)*sizeof(tagged_t));
    w->trail_top = (tagged_t *)((char *)w->trail_top+reloc_factor);

#if defined(ANDPARALLEL)
    /* relocate pointer in handlers */
    parallel_exec_entry_t *lpe = Last_Parallel_Exec;
    while (lpe != NULL) {
      if (lpe->init != NULL)
	lpe->init = (node_t *)((char *)lpe->init+reloc_factor+
                    (newcount-oldcount)*sizeof(tagged_t));
      if (lpe->end != NULL)
	lpe->end = (node_t *)((char *)lpe->end+reloc_factor+
                   (newcount-oldcount)*sizeof(tagged_t));
      lpe = lpe->prev;
    }

#endif

    while (OffChoicetop(b,Choice_Start)){
      b->trail_top = (tagged_t *)((char *)b->trail_top+reloc_factor);
      b = ChoiceCharOffset(b,-b->next_alt->node_offset);
    }
  }

  w->node->next_alt = next_alt;

  ciao_statistics.ss_control++;
  tick0 = BASE_RUNTICK-tick0;
  ciao_statistics.starttick += tick0;
  ciao_statistics.lasttick += tick0;
  ciao_statistics.ss_tick += tick0;

#if defined(ANDPARALLEL)
  /* Wake up the rest of the agents! */
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == SUSPENDED) {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
      Release_lock(Waiting_For_Work_Lock_Of(aux));
    }
  }
  Release_slock(stackset_expansion_l);
#endif
}


/* Here when w->local_top and Stack_End are within STACKAD from each other. */
CVOID__PROTO(stack_overflow)
{
  intmach_t count, reloc_factor;
  tagged_t *newh;
  flt64_t tick0 = BASE_RUNTICK;
  
#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

#if defined(DEBUG)
  if (debug_gc) printf("Thread %" PRIdm " calling stack overflow\n", (intmach_t)Thread_Id);
#endif

#if defined(ANDPARALLEL)
#if defined(DEBUG)
  if (debug_threads) {
    printf("\nWAM %x is in stack_overflow!\n",(unsigned int)w);
    printf("w->local_top and Stack_End are within STACKAD from each other.\n");
    fflush(stdout);
  }
#endif

  /* Suspend the rest of the agents and wait until they really suspended */
  worker_t *aux;
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == RELEASED)
      Suspend_Of(aux) = TOSUSPEND;
  }
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux))
    while (Suspend_Of(aux) == RELEASED) {
      if (Suspend_Of(aux) == RELEASED)
	Suspend_Of(aux) = TOSUSPEND;
    }
#endif

  ComputeA(w->local_top,w->node);

  count = 2*StackDifference(Stack_Start,Stack_End);
  newh = checkrealloc_ARRAY(tagged_t,
			    count/2,
			    count,
			    Stack_Start);
#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %" PRIdm " is reallocing STACK from %p to %p\n", 
           (intmach_t)Thread_Id, Stack_Start, newh);
#endif

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */

  stack_overflow_adjust_wam(w,reloc_factor);

  /* Final adjustments */
  Stack_Start = newh;		/* new low bound */
  Stack_End = newh+count;	/* new high bound */
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
  ciao_statistics.ss_local++;
  tick0 = BASE_RUNTICK-tick0;
  ciao_statistics.starttick += tick0;
  ciao_statistics.lasttick += tick0;
  ciao_statistics.ss_tick += tick0;

#if defined(ANDPARALLEL)
  /* Wake up the rest of the agents! */
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == SUSPENDED) {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
      Release_lock(Waiting_For_Work_Lock_Of(aux));
    }
  }
  Release_slock(stackset_expansion_l);
#endif
}

CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor)
{
  node_t *n, *n2;
  tagged_t t1;
  tagged_t *pt1;

  if (reloc_factor!=0) {
    node_t *aux_node;
    frame_t *frame;
    intmach_t i;
    
    aux_node = ChoiceCharOffset(w->node,ArityToOffset(0));
    aux_node->next_alt = fail_alt;
    aux_node->frame = (frame_t *)((char *)w->frame+reloc_factor);
    aux_node->next_insn = w->next_insn;
    aux_node->local_top = (frame_t *)((char *)w->local_top+reloc_factor);

    /* relocate pointers in trail */
    pt1 = Trail_Start;
    while (TrailYounger(w->trail_top,pt1)) {
      t1 = TrailNext(pt1);
      if (TagIsSVA(t1))
	*(pt1-1) += reloc_factor;
    }

    /* relocate pointers in choice&env stks */
    for (n=aux_node; n!=InitialNode; n=n2){
      n2=ChoiceCharOffset(n,-n->next_alt->node_offset);
      //Tabling --> How to translate?
      *(tagged_t *)(&n2->local_top) += reloc_factor;
      *(tagged_t *)(&n2->frame) += reloc_factor;
      for (pt1=n->term; pt1!=(tagged_t *)n2;) {
        t1 = ChoicePrev(pt1);
        if (TagIsSVA(t1))
	  *(pt1-1) += reloc_factor;
      }
      
      i = FrameSize(n->next_insn);
      frame = n->frame;
      while (frame >= (frame_t*) NodeLocalTop(n2)) {
        pt1 = (tagged_t *)StackCharOffset(frame,i);
        while (pt1!=frame->term){
          t1 = *(--pt1);
          if (TagIsSVA(t1))
	    *pt1 += reloc_factor;
        }
        if (frame->frame)
	  *(tagged_t *)(&frame->frame) += reloc_factor,
	    i = FrameSize(frame->next_insn),
	    frame = frame->frame;
        else
          frame = NULL;
      }
    }

    w->frame = aux_node->frame;
    w->local_top = NodeLocalTop(aux_node);
    SetShadowregs(w->node);
  }
}


static bool_t gcexplicit = FALSE;       /* Shared, no locked --- global flag */


CBOOL__PROTO(gc_start)
{
    gcexplicit = TRUE;
    heap_overflow(Arg,SOFT_HEAPPAD);

    return TRUE;
}


/* Here when w->global_top and Heap_End are within SOFT_HEAPPAD from each other. */
CVOID__PROTO(heap_overflow, intmach_t pad)
{
  tagged_t *oldh = w->global_top;
  tagged_t *newh = w->global_top;
  tagged_t *lowboundh;
  bool_t gc = gcexplicit;

#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

#if defined(DEBUG)
  if (debug_gc) printf("Thread %" PRIdm " calling heap_overflow\n", (intmach_t)Thread_Id);
#endif

#if defined(ANDPARALLEL)
#if defined(DEBUG)
  if (debug_threads) {
    printf("\nWAM %x is in heap_overflow!\n",(unsigned int)w);
    printf("w->global_top and Heap_End are within SOFT_HEAPPAD from each other.\n");
    fflush(stdout);
  }
#endif

  /* Suspend the rest of the agents and wait until that happens completely */
  worker_t *aux;
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == RELEASED)
      Suspend_Of(aux) = TOSUSPEND;
  }
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux))
    while (Suspend_Of(aux) == RELEASED) {
      if (Suspend_Of(aux) == RELEASED)
	Suspend_Of(aux) = TOSUSPEND;
    }
#endif

  gcexplicit = FALSE;
  calculate_segment_node(Arg);
  if (gc ||
      (current_gcmode != atom_off &&
       HeapCharDifference(Heap_Start,oldh) >= GetSmall(current_gcmargin)*kB)) {
    GarbageCollect(Arg);
    newh = w->global_top;
    lowboundh = newh-Gc_Total_Grey;
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*kB ||
         HeapYounger(HeapOffset(newh,2*pad),Heap_End)) &&
        !(HeapCharDifference(lowboundh,oldh) < GetSmall(current_gcmargin)*kB ||
          HeapYounger(HeapOffset(lowboundh,2*pad),Heap_End))) {
      /* garbage collect the entire heap */
      w->segment_node = InitialNode;
      GarbageCollect(Arg);
      newh = w->global_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*kB) ||
      HeapYounger(HeapOffset(newh,2*pad),Heap_End)) {
    flt64_t tick0 = BASE_RUNTICK;
    /* increase heapsize */
    intmach_t mincount, newcount, oldcount, reloc_factor;
    tagged_t *newh;
    
    intmach_t wake_count = HeapCharDifference(Heap_Warn_Soft,Heap_Start);
    
    ComputeA(w->local_top,w->node);
    
    mincount = 2*pad - HeapDifference(w->global_top,Heap_End);
    oldcount = HeapDifference(Heap_Start,Heap_End);
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);


#if defined(USE_OVERFLOW_EXCEPTIONS)
    if ( Heap_Warn == HeapOffset(Heap_End,-HARD_HEAPPAD) ){
      /* Heap overflow exception already raised */
      SERIOUS_FAULT(tryalloc_errstring);
    } else if (SOFT_HEAPPAD == DEFAULT_SOFT_HEAPPAD) {
      /* Heap limit not reached */
      newh = tryrealloc(Heap_Start,
                        oldcount*sizeof(tagged_t),
                        newcount*sizeof(tagged_t));
    } else { 
      /* Heap limit reached */
      newh = NULL;
    }

    if (!newh) {
      /* Raise a heap overflow exception */
      Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn
		       ? HeapOffset(Heap_End,-HARD_HEAPPAD)
		       : Heap_Start);
      Heap_Warn = HeapOffset(Heap_End,-HARD_HEAPPAD);
      if ( wake_count < 0)
	Heap_Warn_Soft = Int_Heap_Warn;
      else 
	Heap_Warn_Soft = Heap_Start;

      TrailPush(w->trail_top,atom_undo_heap_overflow_excep);
      UNLOCATED_EXCEPTION(RESOURCE_ERROR(R_STACK));
    }
#else 
    newh = checkrealloc_ARRAY(tagged_t,
			      oldcount,
			      newcount,
			      Heap_Start);
#endif

#if defined(DEBUG)
    if (debug_gc)
      printf("Thread %" PRIdm " is reallocing HEAP from %p to %p\n", 
             (intmach_t)Thread_Id, Heap_Start, newh);
#endif

    reloc_factor = (char *)newh - (char *)Heap_Start;
    // fprintf(stderr, "reloc_factor=%x\n", reloc_factor);
    
    /* AA, HH and TR are free pointers;  BB is last used word. */

#if defined(ANDPARALLEL)
    /* Adjust remote pointers in other agents */
    heap_overflow_adjust_wam(w,reloc_factor,newh,LOCAL,NULL);
    for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux))
      heap_overflow_adjust_wam(aux,reloc_factor,newh,REMOTE,w);
#else
    heap_overflow_adjust_wam(w,reloc_factor,newh);
#endif

   /* Final adjustments */

#if defined(USE_OVERFLOW_EXCEPTIONS)
    if ((Heap_Limit != 0)  &&                             /* Heap limit is on */
	(Heap_Limit < newcount - DEFAULT_SOFT_HEAPPAD))   /* Heap bigger than Heap limit */
      SOFT_HEAPPAD = newcount - Heap_Limit;

#endif

    Heap_Start = newh; /* new low bound */
    Heap_End = newh+newcount; /* new high bound */
    Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn
                     ? HeapOffset(Heap_End,-SOFT_HEAPPAD)
                     : Heap_Start);
    Heap_Warn = HeapOffset(Heap_End,-SOFT_HEAPPAD);
    if (wake_count>=0)
      Heap_Warn_Soft = HeapCharOffset(Heap_Start,-wake_count);
    else
      Heap_Warn_Soft = Int_Heap_Warn;
    ciao_statistics.ss_global++;
    tick0 = BASE_RUNTICK-tick0;
    ciao_statistics.starttick += tick0;
    ciao_statistics.lasttick += tick0;
    ciao_statistics.ss_tick += tick0;
  }

#if defined(ANDPARALLEL)
  /* Wake up the rest of the agents! */
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == SUSPENDED) {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
      Release_lock(Waiting_For_Work_Lock_Of(aux));
    }
  }
  Release_slock(stackset_expansion_l);
#endif
}


#if defined(ANDPARALLEL)
CVOID__PROTO(heap_overflow_adjust_wam,
	     intmach_t reloc_factor,
	     tagged_t *newh,
	     bool_t rem_reloc,
	     worker_t *rem_w)
#else
CVOID__PROTO(heap_overflow_adjust_wam,
	     intmach_t reloc_factor,
	     tagged_t *newh)
#endif
{
  node_t *n, *n2 = NULL;
  tagged_t t1;
  tagged_t *pt1;

  if (reloc_factor!=0) {
    node_t *aux_node;
    frame_t *frame;
    intmach_t i;

    aux_node = ChoiceCharOffset(w->node,ArityToOffset(0));
    aux_node->next_alt = fail_alt;
    aux_node->frame = w->frame;
    aux_node->next_insn = w->next_insn;
    aux_node->global_top = w->global_top;
    aux_node->local_top = w->local_top; /* segfault patch -- jf */

    /* relocate pointers in global stk */
#if defined(ANDPARALLEL)
    if (rem_reloc == LOCAL) {
      pt1 = newh;
      w->global_top = (tagged_t *)((char *)w->global_top + reloc_factor);
    }
    else
      pt1 = Heap_Start;
#else
    pt1 = newh;
    w->global_top = (tagged_t *)((char *)w->global_top + reloc_factor);
#endif
    while (HeapYounger(w->global_top,pt1)) {
      t1 = HeapNext(pt1);
      if (t1&QMask) pt1 += LargeArity(t1);
      else if (IsHeapTerm(t1)) {
#if defined(ANDPARALLEL)
	if (((rem_reloc == LOCAL) && !is_rem_Hterm(t1,w,rem_w)) ||
            ((rem_reloc == REMOTE) && is_rem_Hterm(t1,w,rem_w)))
#endif
	  *(pt1-1) += reloc_factor;
      }
    }

#if defined(USE_GLOBAL_VARS)
    /* relocate pointers in global vars root */
    if (IsHeapTerm(GLOBAL_VARS_ROOT)) {
      GLOBAL_VARS_ROOT += reloc_factor;
    }
#endif

    /* relocate pointers in trail stk */
    pt1 = Trail_Start;
    TrailPush(w->trail_top,Current_Debugger_State);
    while (TrailYounger(w->trail_top,pt1)) {
      t1 = TrailNext(pt1);
      if (IsHeapTerm(t1)) {
#if defined(ANDPARALLEL)
	if (((rem_reloc == LOCAL) && !is_rem_Hterm(t1,w,rem_w)) ||
            ((rem_reloc == REMOTE) && is_rem_Hterm(t1,w,rem_w)))
#endif
	  *(pt1-1) += reloc_factor;
      }
    }
    Current_Debugger_State = TrailPop(w->trail_top);

#if defined(ANDPARALLEL)
    /* relocate pointers in goal list */
    handler_entry_t *gle = Goal_List_Start;
    tagged_t x1 = (tagged_t)NULL;
    if (gle != NULL) {
      if (((rem_reloc == LOCAL) && !is_rem_Hterm(gle->handler,w,rem_w)) ||
	  ((rem_reloc == REMOTE) && is_rem_Hterm(gle->handler,w,rem_w))) {
	DerefArg(x1,gle->handler,1);
	((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
	gle->handler += reloc_factor;
      }
      while (gle != Goal_List_Top) {
	gle = gle->next;
	if (((rem_reloc == LOCAL) && !is_rem_Hterm(gle->handler,w,rem_w)) ||
	    ((rem_reloc == REMOTE) && is_rem_Hterm(gle->handler,w,rem_w))) {
	  DerefArg(x1,gle->handler,1);
	  ((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
	  gle->handler += reloc_factor;
	}
      }
    }

    /* relocate pointers in event queue */
    event_entry_t *eqe = Event_Queue_Start;
    if (eqe != NULL) {
      if (((rem_reloc == LOCAL) && !is_rem_Hterm(eqe->handler,w,rem_w)) ||
	  ((rem_reloc == REMOTE) && is_rem_Hterm(eqe->handler,w,rem_w)))
	eqe->handler += reloc_factor;
      while (eqe != Event_Queue_Top) {
	eqe = eqe->next;
	if (((rem_reloc == LOCAL) && !is_rem_Hterm(eqe->handler,w,rem_w)) ||
	    ((rem_reloc == REMOTE) && is_rem_Hterm(eqe->handler,w,rem_w)))
	  eqe->handler += reloc_factor;
      }
    }
#endif

    /* relocate pointers in choice&env stks */
    for (n=aux_node; n!=InitialNode && n->next_alt!=NULL; n=n2)
      {
	if (n->next_alt != NULL) {
	  n2=ChoiceCharOffset(n,-n->next_alt->node_offset);
	  for (pt1=n->term; pt1!=(tagged_t *)n2;)
	    {
	      t1 = ChoicePrev(pt1);
	      if (IsHeapTerm(t1)) {
#if defined(ANDPARALLEL)
		if (((rem_reloc == LOCAL) && !is_rem_Hterm(t1,w,rem_w)) ||
		    ((rem_reloc == REMOTE) && is_rem_Hterm(t1,w,rem_w)))
#endif
		  *(pt1-1) += reloc_factor;
	      }
	    }
	  i = FrameSize(n->next_insn);
	  frame = n->frame;
	  while ((frame >= (frame_t*) NodeLocalTop(n2)) && frame->next_insn != NULL)
	    {
	      pt1 = (tagged_t *)StackCharOffset(frame,i);
	      while (pt1!=frame->term)
		{
		  t1 = *(--pt1);
		  if (IsHeapTerm(t1)) {
#if defined(ANDPARALLEL)
		    if (((rem_reloc == LOCAL) && !is_rem_Hterm(t1,w,rem_w)) ||
			((rem_reloc == REMOTE) && is_rem_Hterm(t1,w,rem_w)))
#endif
		      *pt1 += reloc_factor;
		  }
		}
	      i = FrameSize(frame->next_insn);
	      frame = frame->frame;
	    } 

	  //TABLING ->> How to translate???
#if defined(ANDPARALLEL)
	  if (rem_reloc == LOCAL)
	    *(tagged_t *)(&n->global_top) += reloc_factor;
#else
	  *(tagged_t *)(&n->global_top) += reloc_factor;
#endif
	}
      }

	  //TABLING ->> How to translate???
#if defined(ANDPARALLEL)
    if (rem_reloc == LOCAL)
      *(tagged_t *)(&n->global_top) += reloc_factor;
#else
    *(tagged_t *)(&n->global_top) += reloc_factor;
#endif    
    SetShadowregs(w->node);
  }
}


/* Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment. */
CVOID__PROTO(collect_goals_from_trail, intmach_t wake_count)
{
  intmach_t sofar=0;
  tagged_t *tr = w->trail_top;
  tagged_t *h = w->global_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = TagToPointer(w->node->trail_top);
  
  while (sofar<wake_count && TrailYounger(tr,limit))
    {
      tagged_t ref, value;

      ref = TrailPop(tr);
      if (!TagIsCVA(ref))
	continue;
      RefCVA(value,ref);
      if (value==ref)
	SERIOUS_FAULT("wake - unable to find all goals");
      if (sofar++ > 1)
	{
	  HeapPush(h,X(0));
	  HeapPush(h,X(1));
	  X(1) = Tag(LST,HeapOffset(h,-2));
	}
      else if (sofar > 1)
	X(1) = X(0);

      X(0) = Tag(LST,TagToGoal(ref));
      if (!CondCVA(ref))
	{
	  tr0=tr, *tr=0;
	}
    }
  w->global_top = h;
  Heap_Warn_Soft = Heap_Start;	/* make WakeCount==0 */

  if (sofar<wake_count)
    SERIOUS_FAULT("wake - unable to find all goals")
  else if (sofar==1)
    X(1) = *TagToCdr(X(0)),
    X(0) = *TagToCar(X(0));

  /* now compress the trail */

  if (tr0)
    {
      h = tr = tr0;
      while (TrailYounger(w->trail_top,tr))
	{
	  tagged_t ref;
	  
	  if ((ref = TrailNext(tr)))
	    TrailPush(h,ref);
	}
      w->trail_top = h;
    }
}


/* Tidy new half of trail exhaustively. */
CVOID__PROTO(trail_gc)
{
  tagged_t *tr = w->trail_top;
  node_t *b = w->node;
  intmach_t wake_count = WakeCount;
  tagged_t heap_last = TagHVA(HeapOffset(Heap_End,-1));
  /*extern node_t *gc_aux_node;*/ /* Now in a register */
  /*extern node_t *gc_choice_start;*/ /* No in a register */
  /*extern tagged_t *gc_trail_start;*/ /* now in a register */

  Gc_Aux_Node = ChoiceCharOffset(b,ArityToOffset(0));
  Gc_Aux_Node->next_alt = fail_alt;
  Gc_Aux_Node->trail_top = tr;
  Gc_Choice_Start = w->segment_node;
  Gc_Trail_Start = TagToPointer(w->segment_node->trail_top);
  
  if (current_gctrace == atom_verbose) {
    ENG_TTYPRINTF("{GC}  Trail GC started\n");
  }

  while (!OffChoicetop(Gc_Choice_Start,b)) {
    /* sweep trail segment to get rid of multiple 'undo setarg'
       trailings of the same location.  Go from old to new. */
    tagged_t *x;
    tagged_t t1;
      
    for (x=TagToPointer(b->trail_top); !OffTrailtop(x,tr); (void)TrailNext(x))
      if (TagIsHVA(t1 = *x)) {
        if (*TagToHVA(t1) & 1)
          *TrailOffset(x,-1) = *x = heap_last;
        else
          *TagToHVA(t1) ^= 1; /* turn mark bit on */
      }

    /* sweep trail segment to get rid of unconditional entries.
       Keep count of relevant entries.  Turn mark bits off.
       Go from new to old. */
    SetShadowregs(b);
    x=TagToPointer(b->trail_top);
    while (!OffTrailtop(x,tr)){
      tagged_t t1 /*, *pt */ ; /* unused */

      t1 = TrailPop(tr);
      if (!IsVar(t1)) {
        /* kill unconditional 'undo setarg' */
        if (TagIsSTR(t1) &&
            TagToHeadfunctor(t1)==functor_Dsetarg &&
            !CondHVA(TagHVA(TagToPointer(*TagToArg(t1,2)))))
          *tr = 0;
      } else
        if (t1 & TagBitSVA) {
          if (!CondSVA(t1))
            *tr = 0;
        }
        else if (!(t1 & TagBitCVA)) {
          *TagToHVA(t1) ^= 1; /* turn mark bit off */
          if (!CondHVA(t1))
            *tr = 0;
        } else if (wake_count>0) --wake_count;
	  else if (!CondCVA(t1)) *tr = 0;
    }
    b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  }
  
  /* restore misc. registers used above */

  b = w->node;
  SetShadowregs(b);
}


#if defined(USE_OVERFLOW_EXCEPTIONS)
CBOOL__PROTO(undo_heap_overflow_excep)
{
  intmach_t wake_count = WakeCount;

  Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn
		   ? HeapOffset(Heap_End,-SOFT_HEAPPAD)
		   : Heap_Start);
  Heap_Warn = HeapOffset(Heap_End,-SOFT_HEAPPAD);
  if (wake_count<0){
    Heap_Warn_Soft = Int_Heap_Warn;
  }

  return TRUE;
}


// heap_limit assumes X(0) is either variable or small integer
CBOOL__PROTO(heap_limit)
{
  tagged_t x;
  intmach_t wake_count;

  DEREF(x,X(0)); 
  if (IsVar(x)) return cunify(Arg, x, MakeSmall(Heap_Limit));

  Heap_Limit = GetSmall(x);
  wake_count = WakeCount;
   
  if ((Heap_Limit == 0)  ||                                             /* Heap limit is off */
      (Heap_Limit >= HeapDifference(Heap_Start,Heap_End) - DEFAULT_SOFT_HEAPPAD))   /* Heap smaller than Heap limit */
    SOFT_HEAPPAD = DEFAULT_SOFT_HEAPPAD;
  else 
    SOFT_HEAPPAD = HeapDifference(Heap_Start,Heap_End) - Heap_Limit;
  
  Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn
		   ? HeapOffset(Heap_End,-SOFT_HEAPPAD)
		   : Heap_Start);
  Heap_Warn = HeapOffset(Heap_End,-SOFT_HEAPPAD);
  
  if (wake_count>=0)
    Heap_Warn_Soft = HeapCharOffset(Heap_Start,-wake_count);
  else
    Heap_Warn_Soft = Int_Heap_Warn;
  
  return TRUE;

}
#else 
CBOOL__PROTO(heap_limit)
{
  return cunify(Arg, TaggedZero, X(0));
}
#endif
