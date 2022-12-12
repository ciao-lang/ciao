/*
 *  eng_gc.c
 *
 *  Garbage collector and code for growing areas when full.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/basiccontrol.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_start.h>
#include <ciao/timing.h>
#endif
#include <ciao/io_basic.h>

/* TODO: move USE_* options to eng_definitions header */

#if defined(OPTIM_COMP)
/* TODO: seems to work very well, but see if it can be handled without that tricky */
#define USE_GC_SETARG 1 /* fix setarg when segmented GC is enabled. */ /* TODO: make setarg optional */
#endif

#if !defined(OPTIM_COMP)
#define RTCHECK(X)
#endif

#define USE_SEGMENTED_GC 1
#define USE_EARLY_RESET 1

// TODO:[oc-merge] tabling replaced some ->local_top and ->heap_top by these macros; does it make sense doing it here? (some pointers may not be relocated) (JF)
#if !defined(OPTIM_COMP)
#define GCNodeLocalTop(X) NodeLocalTop(X)
#define GCNodeGlobalTop(X) NodeGlobalTop(X)
#else
#define GCNodeLocalTop(X) (X->local_top)
#define GCNodeGlobalTop(X) (X->heap_top)
#endif

/* --------------------------------------------------------------------------- */
/* Switch on tag (special for GC) */

#if defined(OPTIM_COMP)

#if defined(USE_GC_SETARG)
#define SwSetArg(X, CODE_SetArg, CODE_UndoGoal) \
  if ((X)==functor_Dsetarg) { \
    CODE_SetArg; \
  } else { \
    CODE_UndoGoal; \
  }
#else
#define SwSetArg(Reg, CODE_SetArg, CODE_UndoGoal) CODE_UndoGoal
#endif

#define SwOnTrTag(Reg, CODE_Var, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVAorSVA_STR(Reg, { \
    CODE_Var; \
  }, { \
    SwSetArg(TaggedToHeadfunctor(Reg), CODE_SetArg, CODE_UndoGoal); \
  }); \
}

#define SwOnTrTagT(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    SwSetArg(TaggedToHeadfunctor(Reg), CODE_SetArg, CODE_UndoGoal); \
  }); \
}

#define SwOnTrTagTU(Reg, CODE_HVAorCVA, CODE_SVA, CODE_SetArg, CODE_UndoGoal) { \
  Sw_HVAorCVA_SVA_STR(Reg, { \
    CODE_HVAorCVA; \
  }, { \
    CODE_SVA; \
  }, { \
    SwSetArg(GC_UNMARKED(TaggedToHeadfunctor(Reg)), CODE_SetArg, CODE_UndoGoal); \
  }); \
}

#endif /* OPTIM_COMP */

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP)
#if defined(USE_GC_SETARG)
#define MutatedPtr(Reg, Ptr) { \
  tagged_t m__mutated; \
  intval_t m__i; \
  m__mutated = *TaggedToArg((Reg),2); \
  m__i = GetSmall(*TaggedToArg((Reg),1)); \
  ComplexToArg(Ptr, m__mutated, m__i); \
}
#define MutatedPtrU(Reg, Ptr) { \
  tagged_t m__mutated; \
  intval_t m__i; \
  m__mutated = GC_UNMARKED(*TaggedToArg((Reg),2)); \
  m__i = GetSmall(GC_UNMARKED(*TaggedToArg((Reg),1))); \
  ComplexToArg(Ptr, m__mutated, m__i); \
}
#define OldvarPtr(Reg, Ptr) { Ptr = TaggedToArg((Reg),3); }
#endif
#endif /* OPTIM_COMP */

/* --------------------------------------------------------------------------- */

#if !defined(OPTIM_COMP)
#define TG_Let(X, Ptr) tagged_t *X=(Ptr); tagged_t X##val
#define TG_Val(X) X##val
#define TG_Fetch(X) ({ TG_Val(X) = *(X); })
//
#define TG_Put(V,X) ({ *(X) = (V); })
#define TG_PutPtr(p,dest) ({ \
  TG_Put(gc_PutValue((tagged_t)p,*dest), dest); \
})
#endif

/* --------------------------------------------------------------------------- */

#define PtrCharInc(X,I) (X) = CharOffset((X), (I))

#define RelocateIfSVA(PTR, RelocFactor) \
  if (TaggedIsSVA(TG_Val(PTR))) { \
    RelocateTagged(PTR, RelocFactor); \
  }
#define RelocateIfHeapPtr(PTR, RelocFactor) \
  if (IsHeapPtrAndNeedsReloc(TG_Val(PTR))) { \
    RelocateTagged(PTR, RelocFactor); \
  }

/* --------------------------------------------------------------------------- */

#if defined(USE_SEGMENTED_GC)
#define GC_HEAP_IN_SEGMENT(P) OffHeaptop((P),Gc_Heap_Start)
#define GC_STACK_IN_SEGMENT(P) OffStacktop((P),Gc_Stack_Start)
#else
#define GC_HEAP_IN_SEGMENT(P) TRUE
#define GC_STACK_IN_SEGMENT(P) TRUE
#endif

/* Support for ANDPARALLEL (multiple heaps) */
/* TODO: only for stack expansions, not garbage collection */

/* Relocate heap pointers in tagged words */
#if defined(ANDPARALLEL)
#define IsHeapPtrAndNeedsReloc(T) (IsHeapPtr((T)) && HeapTermNeedsReloc((T)))
/* Needs reloc if `remote_reloc` coincides with `is_remote_HeapTerm` for `T` */
#define HeapTermNeedsReloc(T) (remote_reloc == is_remote_HeapTerm((T),w,rem_w))
/* If remote_w==NULL, check if term is outside w heap; otherwise check if term is in remote_w heap */
// TODO: two cases really needed?
bool_t is_remote_HeapTerm(tagged_t term, worker_t *w, worker_t *remote_w) {
  if (remote_w == NULL) { // local case
    return !(((TaggedToPointer(term) >= w->heap_start) &&
              (TaggedToPointer(term) <= w->heap_end)));
  } else { // remote case
    return ((TaggedToPointer(term) >= remote_w->heap_start) &&
            (TaggedToPointer(term) <= remote_w->heap_end));
  }
}
#else
#define IsHeapPtrAndNeedsReloc(T) IsHeapPtr((T))
#endif

#if defined(ANDPARALLEL)
#define AssignRelocPtrNotRemote(P,Offset) if (!remote_reloc) AssignRelocPtr((P),(Offset)) 
#else
#define AssignRelocPtrNotRemote(P,Offset) AssignRelocPtr((P),(Offset)) 
#endif

/* --------------------------------------------------------------------------- */
/* Do a backward and forward pass over the choice stack */

/* TODO: document: Gc_Aux_Choice is the newest choice and Gc_Choice_Start the oldest */
#define CHOICE_PASS(CP, PREVCP, ARITY, BACKWARD, FORWARD) { \
  try_node_t *ALT; \
  (CP) = Gc_Aux_Choice; \
  (PREVCP) = w->choice; \
  (ALT) = fail_alt; \
  (ARITY) = (ALT)->arity; \
  while (ChoiceYounger((CP),Gc_Choice_Start)) { \
    BACKWARD; \
    CHOICE_PASS__ReverseChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = (ALT)->arity; \
  } \
  while (ChoiceYounger(Gc_Aux_Choice,(CP))) { \
    CHOICE_PASS__UndoChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = (ALT)->arity; \
    FORWARD; \
  } \
}

#define CHOICE_PASS__ReverseChoice(CP,PREVCP,ALT) { \
  try_node_t *m_alt = (ALT); \
  (CP) = (PREVCP); \
  (ALT) = (CP)->next_alt; \
  (PREVCP) = ChoiceCont((CP)); \
  (CP)->next_alt = m_alt; \
}

#define CHOICE_PASS__UndoChoice(CP,PREVCP,ALT) { \
  try_node_t *m_alt = (ALT); \
  (PREVCP) = (CP); \
  (ALT) = (PREVCP)->next_alt; \
  (CP) = ChoiceNext0((PREVCP), (ALT)->arity); \
  (PREVCP)->next_alt = m_alt; \
}

/* ------------------------------------------------------------------------- */

/* Do DO for each frame variable (*PTR) of each frame between the
   initial frame in CP and END_FRAME. */
#define ForEachChoiceFrameX(CP, END_FRAME, PTR, DO) do { \
  frame_t *FRAME = (CP)->frame; \
  intmach_t FRAME_SIZE = FrameSize((CP)->next_insn); \
  while (OffStacktop((FRAME), (END_FRAME))) { \
    ForEachFrameX((FRAME), (FRAME_SIZE), PTR, DO); \
    (FRAME_SIZE) = FrameSize((FRAME)->next_insn); \
    (FRAME) = (FRAME)->frame; \
  } \
} while(0)

/* (same with optional code after each frame) */
#define ForEachChoiceFrameXf(CP, END_FRAME, PTR, DO, FRAME, DOFrame) do { \
  frame_t *FRAME = (CP)->frame; \
  intmach_t FRAME_SIZE = FrameSize((CP)->next_insn); \
  while ((FRAME) >= (END_FRAME)) { \
    ForEachFrameX((FRAME), (FRAME_SIZE), PTR, DO); \
    DOFrame; \
    (FRAME_SIZE) = FrameSize((FRAME)->next_insn); \
    (FRAME) = (FRAME)->frame; \
  } \
} while(0)

/* Do DO for each frame variable (*PTR) */
#define ForEachFrameX(FRAME, FRAME_SIZE, PTR, DO) do { \
  TG_Let(PTR, (tagged_t *)StackCharOffset((FRAME), (FRAME_SIZE))); \
  while (PTR != (FRAME)->x) { \
    PTR += -StackDir; \
    DO; \
  } \
} while(0)

/* Do DO for each variable CHOICE->x[I] in the choice point */
#define ForEachChoiceX(CHOICE, PTR, DO) do { \
  intmach_t i; \
  i = ChoiceArity((CHOICE)); \
  TG_Let(PTR, &(CHOICE)->x[i]); \
  for (;;) { \
    if (i <= 0) break; \
    i--; \
    PTR--; \
    DO; \
  } \
} while(0)
// TODO: use this one?
// #define ForEachChoiceX(CHOICE, PTR, DO) ForEachChoiceX2(CHOICE, ChoiceArity((CHOICE)), PTR, DO) 
/* Do DO for each variable CHOICE->x[I] in the choice point */
#define ForEachChoiceX2(CHOICE, ARITY, PTR, DO) do { \
  TG_Let(PTR, (CHOICE)->x + (ARITY)); \
  while (PTR != (CHOICE->x)) { \
    PTR--; \
    DO; \
  } \
} while(0)

/* ------------------------------------------------------------------------- */

/* TODO: is this test correct? */
#if defined(USE_LOWRTCHECKS)
static inline bool_t rtcheck__is_M(tagged_t *t0) {
  TG_Let(t, t0);
  TG_Fetch(t);
  return TG_IsM(t);
}
/* must be inside the heap */
#define ASSERT__INTORC0(X, EV) { \
  if (!OnHeap(X)) { \
    TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: %p out of heap cannot be relocated into %p}\n", (long)debug_inscount, (long)__LINE__, (X), (EV)); \
  } \
}
/* must be inside the heap and not marked */
#define ASSERT__INTORC(X, EV) ({ \
  ASSERT__INTORC0((X), (EV)); \
  if (!rtcheck__is_M((X))) { \
    TG_Fetch(X); \
    TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: should be marked 0x%lx (at %p)}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_Val(X)), (X)); \
  } \
  if (rtcheck__is_M((EV))) { \
    TG_Fetch(EV); \
    TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: cannot relocate into marked 0x%lx (at %p)}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_Val(EV)), (EV)); \
  } \
})
#define ASSERT__VALID_TAGGED(X) ({ \
  if (IsHeapPtr((X)) && !OnHeap(TaggedToPointer((X)))) { \
    TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: out of heap cell 0x%lx wrote}\n", (long)debug_inscount, (long)__LINE__, (long)(X)); \
  } \
})
#define ASSERT__NO_MARK(X) ({ \
  if (rtcheck__is_M((X))) { \
    TG_Fetch(X); \
    TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: cell 0x%lx at %p is marked}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_Val(X)), (X)); \
  } \
})
#else
#define ASSERT__INTORC0(X, EV)
#define ASSERT__INTORC(X, EV)
#define ASSERT__VALID_TAGGED(X)
#define ASSERT__NO_MARK(X)
#endif

/* ------------------------------------------------------------------------- */
/* Service routine for HEAPMARGIN* instructions.
 * pad - required amount of heap space.
 * arity - number of live X regs at this point.
 */
CVOID__PROTO(explicit_heap_overflow, intmach_t pad, intmach_t arity) {
  intmach_t i;
  frame_t *a;

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling explicit_heap_overflow\n", (intmach_t)Thread_Id);

  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* We are still in "shallow mode" */
  /* Pre: !IsDeep() */
#if !defined(OPTIM_COMP) && !defined(USE_DEEP_FLAGS)
  choice_t *B = w->choice;
#endif
#if !defined(OPTIM_COMP)
  bool_t was_shallow = IsShallowTry();
#endif
  CODE_MAYBE_NECK_TRY();
#if !defined(OPTIM_COMP)
  /* TODO: OPTIM_COMP version does not test choice overflow here! Does
     it make sense? Choice stack space should be already reserved
     before neck. */
  if (was_shallow) {
    if (ChoiceYounger(ChoiceCharOffset(w->choice,CHOICEPAD*sizeof(tagged_t)),w->trail_top)) {
      CVOID__CALL(choice_overflow,2*CHOICEPAD*sizeof(tagged_t),TRUE);
    }
  }
#endif
  
  /* ensure that X regs are seen by heap_overflow(): make a frame */
  CODE_ALLOC(a);
  a->x[0] = TaggedZero;
  for (i=0; i<arity; i++) {
    a->x[i+1] = X(i);
  }
  CODE_CFRAME(a, CONTCODE(arity+1));

  CVOID__CALL(heap_overflow,pad);
  for (i=0; i<arity; i++) {
    X(i) = a->x[i+1];
  }
  SetLocalTop(a);
  DEALLOCATE(a);
}

/* ------------------------------------------------------------------------- */

/* Set w->segment_choice to most recent choicept which is marked as pure. */
static CVOID__PROTO(calculate_segment_choice) {
#if defined(USE_SEGMENTED_GC)
  choice_t *n;
  w->segment_choice = NULL;
  for (n=w->choice;
       w->segment_choice==NULL;
       n=ChoiceCont(n)) {
    if (ChoiceptTestPure(n)) {
      w->segment_choice = n;
    }
  }
#else
  w->segment_choice=InitialChoice;
#endif
}

/* ------------------------------------------------------------------------- */
/* Support for ANDPARALLEL (suspend/resume all WAMs) */

#if defined(ANDPARALLEL)
/* Suspend the rest of the agents and wait until that happens completely */
CVOID__PROTO(suspend_all) {
  worker_t *aux;
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == RELEASED) {
      Suspend_Of(aux) = TOSUSPEND;
    }
  }
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    while (Suspend_Of(aux) == RELEASED) {
      if (Suspend_Of(aux) == RELEASED) {
        Suspend_Of(aux) = TOSUSPEND;
      }
    }
  }
}
#endif

#if defined(ANDPARALLEL)
/* Wake up the rest of the agents! */
CVOID__PROTO(resume_all) {
  worker_t *aux;
  for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
    if (Suspend_Of(aux) == SUSPENDED) {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
      Release_lock(Waiting_For_Work_Lock_Of(aux));
    }
  }
}
#endif

/* =========================================================================== */
/* Trail GC (both for gc__heap_collect and choice_overflow) */

/* delete 0's from the trail of several choice points */
CVOID__PROTO(trail__compress, bool_t from_gc) {
  tagged_t *curr;
  tagged_t *dest;
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity MAYBE_UNUSED;

  dest = TrailTopUnmark(Gc_Choice_Start->trail_top);
  curr = dest;
  CHOICE_PASS(cp, prevcp, arity, {
  }, {
    COMPRESS_TRAIL(cp, curr, dest);
    CHPTFLG(cp->flags = 0);
    if (from_gc) ChoiceptMarkPure(cp);
  });

  G->trail_top = dest;
}

/* =========================================================================== */
/* Growing heap/choice/stack memory areas when full */
/* (heap_overflow attempts gc__heap_collect first) */

#if defined(ANDPARALLEL)
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor, tagged_t *newh, bool_t remote_reloc, worker_t *remote_worker);
#else
CVOID__PROTO(heap_overflow_adjust_wam, intmach_t reloc_factor, tagged_t *newh);
#endif

CVOID__PROTO(trail__remove_uncond);
CVOID__PROTO(gc__heap_collect);
#if defined(OPTIM_COMP) && defined(USE_GC_STATS)
inttime_t usertick(void);
#endif

/* --------------------------------------------------------------------------- */

/* Here when w->choice and G->trail_top are within CHOICEPAD from each other. */
CVOID__PROTO(choice_overflow, intmach_t pad, bool_t remove_trail_uncond) {
  tagged_t *choice_top;

#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

  RTCHECK(CBOOL__SUCCEED(proofread, "Before choice_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling choice overflow\n", (intmach_t)Thread_Id);

#if defined(ANDPARALLEL)
  DEBUG__TRACE(debug_threads, "WAM %p is in choice_overflow!\n", w);
  CVOID__CALL(suspend_all);
#endif

#if defined(USE_GC_STATS)          
  inttime_t tick0 = RunTickFunc();
#endif

#if !defined(OPTIM_COMP) && !defined(USE_DEEP_FLAGS)
  choice_t *B = w->choice;
#endif
  bool_t shallow_try = IsShallowTry();
  if (shallow_try) { /* ensure A', P' exist */
    w->choice->next_alt = G->next_alt;
    w->choice->local_top = G->local_top;
  }

  if (remove_trail_uncond) {
    /* note: trail__remove_uncond not executed in compile_term */
    CVOID__CALL(calculate_segment_choice);
    CVOID__CALL(trail__remove_uncond);
    CVOID__CALL(trail__compress,FALSE);
  }

  /* ASSUMED: --CHOICE, TRAIL++ */

  choice_top = ChoiceTopFromChoice(w->choice);
  if (ChoiceCharAvailable(choice_top) < pad) {
    choice_t *b;
    tagged_t *newtr;
    intmach_t mincount, newcount, oldcount, trail_reloc_factor, choice_reloc_factor;
    
    {
      mincount = pad - ChoiceCharDifference(choice_top,G->trail_top);
      oldcount = ChoiceCharDifference(Choice_Start,Choice_End);
      newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
      newtr = REALLOC_AREA(Trail_Start, oldcount, newcount);
      DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing TRAIL from %p to %p\n", (intmach_t)Thread_Id, Trail_Start, newtr);
    }
    trail_reloc_factor = (char *)newtr - (char *)Trail_Start;
    choice_reloc_factor = trail_reloc_factor + (newcount-oldcount);
    {
      tagged_t *tr;
      tagged_t *trb;
      
      tr = RelocPtr(Choice_Start, trail_reloc_factor);
      trb = RelocPtr(choice_top, trail_reloc_factor);
      Trail_Start = Choice_End = newtr;                /* new low bound */
      Choice_Start = Trail_End = (tagged_t *)TrailCharOffset(newtr, newcount);      /* new high bound */

#if defined(USE_TAGGED_CHOICE_START)
      /* Do not take out (tagged_t) casting, or the engine will break!! */
      Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif

      {
        /* Copy the new choicepoint stack */
        /* TODO: move this copy loop to basiccontrol.h */
        tagged_t *x = (tagged_t *)Choice_Start;
        tagged_t *y = (tagged_t *)tr;
        while ((tagged_t *)trb < y) {
          y--;
          x--;
          *x = *y;
        }
        w->choice = b = ChoiceFromChoiceTop(x);
      }

#if defined(USE_THREADS)
      /* We have to relocate the concurrent topmost choicepoint */
      RelocateConcChptChain(choice_reloc_factor);
#endif
    }
    AssignRelocPtr(w->previous_choice, choice_reloc_factor);
    AssignRelocPtr(G->trail_top, trail_reloc_factor);

#if defined(ANDPARALLEL)
    /* relocate pointer in handlers */
    parallel_exec_entry_t *lpe = Last_Parallel_Exec;
    while (lpe != NULL) {
      if (lpe->init != NULL) {
        AssignRelocPtr(lpe->init, choice_reloc_factor);
      }
      if (lpe->end != NULL) {
        AssignRelocPtr(lpe->end, choice_reloc_factor);
      }
      lpe = lpe->prev;
    }
#endif

    /* Relocate trail_top of each choice */
    while (OffChoicetop(b,Choice_Start)) {
      AssignRelocPtr(b->trail_top, trail_reloc_factor);
      b = ChoiceCont(b);
    }
  }

  if (shallow_try) { /* ShallowTry was on */
    SetShallowTry();
  }

#if defined(USE_GC_STATS)          
  ciao_stats.ss_control++;
  tick0 = RunTickFunc()-tick0;
  ciao_stats.starttick += tick0;
  ciao_stats.lasttick += tick0;
  ciao_stats.ss_tick += tick0;
#endif

#if defined(ANDPARALLEL)
  CVOID__CALL(resume_all);
  Release_slock(stackset_expansion_l);
#endif
  RTCHECK(CBOOL__SUCCEED(proofread, "After choice_overflow", 0, TRUE));
}

/* ------------------------------------------------------------------------- */

/* pre: stack top and end are within STACKPAD bytes from each other */
CVOID__PROTO(stack_overflow) {
  intmach_t count;
  intmach_t reloc_factor;
  tagged_t *newh;

#if defined(USE_GC_STATS)          
  flt64_t tick0 = RunTickFunc();
#endif
  
#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

  RTCHECK(CBOOL__SUCCEED(proofread, "Before stack_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling stack overflow\n", (intmach_t)Thread_Id);

#if defined(ANDPARALLEL)
  DEBUG__TRACE(debug_threads, "WAM %p is in stack_overflow!\n", w);
  CVOID__CALL(suspend_all);
#endif

  UpdateLocalTop(w->choice,G->frame);

  count = StackCharSize();
  newh = REALLOC_AREA(Stack_Start, count, 2*count);
  count = 2*StackCharSize();
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing STACK from %p to %p\n", (intmach_t)Thread_Id, Stack_Start, newh);

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */
  CVOID__CALL(stack_overflow_adjust_wam,reloc_factor);

  /* Final adjustments */
  Stack_Start = newh; /* new low bound */
  Stack_End = (tagged_t *)StackCharOffset(newh, count); /* new high bound */
#if defined(USE_GC_STATS)          
  ciao_stats.ss_local++;
  tick0 = RunTickFunc()-tick0;
  ciao_stats.starttick += tick0;
  ciao_stats.lasttick += tick0;
  ciao_stats.ss_tick += tick0;
#endif

#if defined(ANDPARALLEL)
  CVOID__CALL(resume_all);
  Release_slock(stackset_expansion_l);
#endif
  RTCHECK(CBOOL__SUCCEED(proofread, "After stack_overflow", 0, TRUE));
}

CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor) {
  if (reloc_factor==0) return;

  choice_t *n, *n2;
  choice_t *aux_choice;

  aux_choice = ChoiceNext0(w->choice,0);
  aux_choice->next_alt = fail_alt;
  aux_choice->frame = RelocPtr(G->frame, reloc_factor);
  aux_choice->next_insn = G->next_insn;
  aux_choice->local_top = RelocPtr(G->local_top, reloc_factor);

  /* relocate pointers in trail */
  {
    TG_Let(pt1, Trail_Start);
    while (TrailYounger(G->trail_top,pt1)) {
      TG_Fetch(pt1);
      RelocateIfSVA(pt1, reloc_factor);
      pt1 += TrailDir;
    }
  }

  /* relocate pointers in choice&env stks */
  for (n=aux_choice; n!=InitialChoice; n=n2) {
    ForEachChoiceX(n, ptr, {
      TG_Fetch(ptr);
      RelocateIfSVA(ptr, reloc_factor);
    });
      
    n2 = ChoiceCont(n);
    //Tabling --> How to translate?
    AssignRelocPtr(n2->local_top, reloc_factor);
    AssignRelocPtr(n2->frame, reloc_factor);
    ForEachChoiceFrameXf(n, GCNodeLocalTop(n2), pt1, {
      TG_Fetch(pt1);
      RelocateIfSVA(pt1, reloc_factor);
    }, frame, {
      if (!frame->frame) {
        /* TODO:[oc-merge] this is never reached in practice since the
           youngest GCNodeLocalTop(n2) is Offset(Stack_Start,EToY0)
           and not Stack_Start (see local_init_each_time at
           eng_registry.c) */
        break;
      }
      AssignRelocPtr(frame->frame, reloc_factor);
    });
  }

  G->frame = aux_choice->frame;
  G->local_top = GCNodeLocalTop(aux_choice);
  SetChoice(w->choice);
}

/* ------------------------------------------------------------------------- */

/* TODO: per worker? */
bool_t current_gcmode;
intmach_t current_gctrace;
intmach_t current_gcmargin;

/* TODO: per worker? */
static bool_t gcexplicit = FALSE;       /* Shared, no locked --- global flag */

/* Explicit GC */
CBOOL__PROTO(gc_start) {
  gcexplicit = TRUE;
  CVOID__CALL(heap_overflow,CALLPAD*2);
  CBOOL__PROCEED;
}

/* PRE: G->heap_top and Heap_End are within CALLPAD from each other. */
CVOID__PROTO(heap_overflow, intmach_t pad) {
  tagged_t *oldh = G->heap_top;
  tagged_t *newh = G->heap_top;
  tagged_t *lowboundh;
  bool_t cint_event;
  bool_t event;
  bool_t gc = gcexplicit;

  event = TestEvent();
  cint_event = TestCIntEvent();

#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

  RTCHECK(CBOOL__SUCCEED(proofread, "Before heap_overflow", 0, TRUE));
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling heap_overflow\n", (intmach_t)Thread_Id);

#if defined(ANDPARALLEL)
  DEBUG__TRACE(debug_threads, "WAM %x is in heap_overflow!\n", w);
  CVOID__CALL(suspend_all);
#endif

  gcexplicit = FALSE;
  CVOID__CALL(calculate_segment_choice);
  if (gc ||
      (current_gcmode == TRUE &&
       HeapCharDifference(Heap_Start,oldh) >= GCMARGIN_CHARS)) {
    CVOID__CALL(gc__heap_collect);
    newh = G->heap_top;
    lowboundh = HeapCharOffset(newh, -Gc_Total_Grey);
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GCMARGIN_CHARS ||
         HeapCharAvailable(newh) < pad) &&
        !(HeapCharDifference(lowboundh,oldh) < GCMARGIN_CHARS ||
          HeapCharAvailable(lowboundh) < pad)) {
      /* garbage collect the entire heap */
      w->segment_choice = InitialChoice;
      CVOID__CALL(gc__heap_collect);
      newh = G->heap_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GCMARGIN_CHARS) ||
      HeapCharAvailable(newh) < pad) {
#if defined(USE_GC_STATS)          
    flt64_t tick0 = RunTickFunc();
#endif
    /* increase heapsize */
    intmach_t mincount, newcount, oldcount, reloc_factor;
    tagged_t *newh;

    intmach_t wake_count = WakeCount();
    
    UpdateLocalTop(w->choice,G->frame);
    
    mincount = pad - HeapCharAvailable(G->heap_top);
    oldcount = HeapCharSize();
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);

    newh = REALLOC_AREA(Heap_Start, oldcount, newcount);
    DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing HEAP from %p to %p\n", (intmach_t)Thread_Id, Heap_Start, newh);

    reloc_factor = (char *)newh - (char *)Heap_Start;

    /* AA, HH and TR are free pointers;  BB is last used word. */
#if defined(ANDPARALLEL)
    /* Adjust remote pointers in other agents */
    CVOID__CALL(heap_overflow_adjust_wam,reloc_factor,newh,FALSE,NULL);
    for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
      heap_overflow_adjust_wam(aux,reloc_factor,newh,TRUE,w);
    }
#else
    CVOID__CALL(heap_overflow_adjust_wam,reloc_factor,newh);
#endif

    Heap_Start = newh; /* new low bound */
    Heap_End = HeapCharOffset(newh, newcount); /* new high bound */

    UnsetEvent();
    UnsetCIntEvent();
    if (event) {
      SetWakeCount(wake_count);
    }
    if (cint_event) {
      SetCIntEvent();
    }

#if defined(USE_GC_STATS)          
    ciao_stats.ss_global++;
    tick0 = RunTickFunc()-tick0;
    ciao_stats.starttick += tick0;
    ciao_stats.lasttick += tick0;
    ciao_stats.ss_tick += tick0;
#endif
  }
  RTCHECK(CBOOL__SUCCEED(proofread, "After heap_overflow", 0, TRUE));

#if defined(ANDPARALLEL)
  CVOID__CALL(resume_all);
  Release_slock(stackset_expansion_l);
#endif
}

#if defined(ANDPARALLEL)
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor,
             tagged_t *newh,
             bool_t remote_reloc,
             worker_t *rem_w)
#else
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor,
             tagged_t *newh)
#endif
{
  if (reloc_factor==0) return;

  choice_t *n, *n2;
  choice_t *aux_choice;

  aux_choice = ChoiceNext0(w->choice,0);
  aux_choice->next_alt = fail_alt;
  aux_choice->frame = G->frame;
  aux_choice->next_insn = G->next_insn;
  aux_choice->heap_top = G->heap_top;
  aux_choice->local_top = G->local_top; /* segfault patch -- jf */

  /* relocate pointers in global stk */
  {
#if defined(ANDPARALLEL)
    TG_Let(pt1, remote_reloc ? Heap_Start : newh);
#else
    TG_Let(pt1, newh);
#endif
    AssignRelocPtrNotRemote(G->heap_top, reloc_factor);
    while (HeapYounger(G->heap_top,pt1)) {
      TG_Fetch(pt1);
      if (BlobHF(TG_Val(pt1))) {
        PtrCharInc(pt1, BlobFunctorSizeAligned(TG_Val(pt1))+2*sizeof(functor_t));
      } else {
        RelocateIfHeapPtr(pt1, reloc_factor);
        /* TODO: check that the written tagged is ok? */
        pt1++;
      }
    }
  }

#if defined(USE_GLOBAL_VARS)
  /* relocate pointers in global vars root */
  {
    TG_Let(pt1, &GLOBAL_VARS_ROOT);
    TG_Fetch(pt1);
    RelocateIfHeapPtr(pt1, reloc_factor);
  }
#endif

  /* push special registers on the trail stack */
#if !defined(OPTIM_COMP)
  TrailPush(G->trail_top,Current_Debugger_State);
#endif

  /* relocate pointers in trail stk */
  {
    TG_Let(pt1, Trail_Start);
    while (TrailYounger(G->trail_top,pt1)) {
      TG_Fetch(pt1);
      RelocateIfHeapPtr(pt1, reloc_factor);
      pt1 += TrailDir;
    }
  }

  /* pop special registers from the trail stack */
#if !defined(OPTIM_COMP)
  TrailDec(G->trail_top);
  Current_Debugger_State = *(G->trail_top); // (w->trail_top points to the popped element)
#endif

#if defined(ANDPARALLEL)
  /* relocate pointers in goal list */
  handler_entry_t *gle = Goal_List_Start;
  tagged_t x1 = (tagged_t)NULL;
  if (gle != NULL) {
    if (IsHeapPtrAndNeedsReloc(gle->handler)) {
      DerefArg(x1,gle->handler,1);
      ((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
      gle->handler += reloc_factor;
    }
    while (gle != Goal_List_Top) {
      gle = gle->next;
      if (IsHeapPtrAndNeedsReloc(gle->handler)) {
        DerefArg(x1,gle->handler,1);
        ((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
        gle->handler += reloc_factor;
      }
    }
  }

  /* relocate pointers in event queue */
  event_entry_t *eqe = Event_Queue_Start;
  if (eqe != NULL) {
    if (IsHeapPtrAndNeedsReloc(eqe->handler)) {
      eqe->handler += reloc_factor;
    }
    while (eqe != Event_Queue_Top) {
      eqe = eqe->next;
      if (IsHeapPtrAndNeedsReloc(eqe->handler)) {
        eqe->handler += reloc_factor;
      }
    }
  }
#endif

#if defined(ANDPARALLEL)
#warning "Superfluous checks introduced in 821b021450194cd45897964b27416b524204b0f2 and e295deb5cf06f7cf4a55ef9d7c17ec291c93b1b1 were removed (please check)"
#endif

  /* relocate pointers in choice&env stks */
  for (n=aux_choice; n!=InitialChoice; n=n2) {
    ForEachChoiceX(n, ptr, {
      TG_Fetch(ptr);
      RelocateIfHeapPtr(ptr, reloc_factor);
    });

    n2 = ChoiceCont(n);
//    n2 = ChoiceCont(n);
//    {
//      TG_Let(pt1, n->x);
//      for (; pt1!=(tagged_t *)n2;) {
//        TG_Fetch(pt1);
//        RelocateIfHeapPtr(pt1, reloc_factor);
//        pt1++;
//      }
//    }
    {
      frame_t *frame = n->frame;
      intmach_t i = FrameSize(n->next_insn);
      while ((frame >= (frame_t*)GCNodeLocalTop(n2))) {
        ForEachFrameX(frame, i, pt1, {
            TG_Fetch(pt1);
            RelocateIfHeapPtr(pt1, reloc_factor);
          });
        i = FrameSize(frame->next_insn);
        frame = frame->frame;
      } 
    }

    // TODO: TABLING ->> How to translate???
    AssignRelocPtrNotRemote(n->heap_top, reloc_factor);
  }

  // TODO: TABLING ->> How to translate???
  AssignRelocPtrNotRemote(n->heap_top, reloc_factor);
  SetChoice(w->choice);
}

/* Tidy new half of trail exhaustively. */
CVOID__PROTO(trail__remove_uncond) { // TODO:[oc-merge] was trail_gc
  tagged_t *tr;
  choice_t *orig_b;
  choice_t *b;
  intmach_t wake_count;
  tagged_t heap_last = Tagp(HVA,HeapOffset(Heap_End,-1));
  /*extern choice_t *gc_aux_choice;*/ /* Now in a register */
  /*extern choice_t *gc_choice_start;*/ /* No in a register */

  orig_b = w->choice;

  b = w->choice;

  /* TODO: move to a macro and call before this function */
  Gc_Aux_Choice = ChoiceNext0(b,0);
  CHPTFLG(Gc_Aux_Choice->flags = 0);
  Gc_Aux_Choice->next_alt = fail_alt;
  Gc_Aux_Choice->trail_top = G->trail_top;
  Gc_Choice_Start = w->segment_choice;

  wake_count = WakeCount();

  if (current_gctrace == GCTRACE__VERBOSE) {
    TRACE_PRINTF("{GC}  Trail GC started\n");
  }

  tr = w->trail_top;
  while (!OffChoicetop(Gc_Choice_Start,b)) {
    tagged_t *x;

    /* sweep trail segment to get rid of multiple 'undo setarg'
       trailings of the same location.  Go from old to new. */
    // TODO:[oc-merge] this was removed from OC, why?
    for (x=TrailTopUnmark(b->trail_top); TrailYounger(tr,x); x++) {
      tagged_t t1;
      t1 = *x;
      if (TaggedIsHVA(t1)) {
        if (*TagpPtr(HVA,t1) & 1) {
          *x = heap_last;
          *TrailOffset(x,-1) = heap_last;
        } else {
          *TagpPtr(HVA,t1) ^= 1; /* turn mark bit on */
        }
      }
    }

    /* sweep trail segment to get rid of unconditional entries.
       Keep count of relevant entries.  Turn mark bits off.
       Go from new to old. */
    SetChoice(b);
    x=TrailTopUnmark(b->trail_top);
    while (TrailYounger(tr,x)){
      tagged_t t1;

      TrailDec(tr);
      t1 = *tr; // (tr points to the popped element)
      if (!IsVar(t1)) {
        /* kill unconditional 'undo setarg' */
        if (TaggedIsSTR(t1) &&
            TaggedToHeadfunctor(t1)==functor_Dsetarg &&
            !CondHVA(Tagp(HVA,TaggedToPointer(*TaggedToArg(t1,2))))) {
          *tr = 0;
        }
      } else {
        if (t1 & TagBitSVA) {
          if (!CondSVA(t1)) {
            *tr = 0;
          }
        } else if (!(t1 & TagBitCVA)) {
          *TagpPtr(HVA,t1) ^= 1; /* turn mark bit off */
          if (!CondHVA(t1)) {
            *tr = 0;
          }
        } else if (wake_count>0) {
          --wake_count;
        } else if (!CondCVA(t1)) {
          *tr = 0;
        }
      }
    }
    b = ChoiceCont(b);
  }
  
  /* restore misc. registers used above */
  SetChoice(orig_b);
}

/* =========================================================================== */
/* GARBAGE COLLECTION ROUTINES */

/* Based on the algorithms described in:

   "Garbage Collection for Prolog Based on WAM",
   by K. Appleby, M. Carlsson, S. Haridi, and D. Sahlin,
   Communications of the ACM 31:6, pp. 719-741,

   somewhat complicated by support for freeze & wait decls
   (constrained variables), and undo/1 (goals on the trail).
*/

static CVOID__PROTO(shunt_variables);
static CVOID__PROTO(mark_trail_cva);
static CVOID__PROTO(mark_frames, choice_t *cp);
static CVOID__PROTO(mark_choicepoints);
static CVOID__PROTO(mark_root, tagged_t *start);
static CVOID__PROTO(sweep_frames, choice_t *cp);
static CVOID__PROTO(sweep_choicepoints);
static CVOID__PROTO(compress_heap);

#define gc_TrailStart TrailTopUnmark(w->segment_choice->trail_top)
#define gc_HeapStart (GCNodeGlobalTop(w->segment_choice))
#define gc_StackStart (GCNodeLocalTop(w->segment_choice))
#define gc_ChoiceStart (w->segment_choice)

/* --------------------------------------------------------------------------- */
/* The Shunting Phase */

/* invariant: in shunting phase only variables are marked */

#define gc_shuntVariable(shunt_dest) { \
  tagged_t shunt_src; \
  while (1) { \
    if (!IsVar(shunt_dest)) break; \
    shunt_src = *TaggedToPointer(shunt_dest); \
    if (gc_IsMarked(shunt_src)) break; \
    if (shunt_src==shunt_dest) break; \
    shunt_dest = shunt_src; \
  } \
}

static CVOID__PROTO(shunt_variables) {
  tagged_t *pt = w->trail_top;
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity MAYBE_UNUSED;
  tagged_t *limit;

  CHOICE_PASS(cp, prevcp, arity, {
    /* backward pass */
    /* all variables in the trail has a value given in the future */
    /* (leave unmarked only the more recent trail entry for each variable) */
    limit = TrailTopUnmark(prevcp->trail_top);
    while (TrailYounger(pt,limit)) {
      TrailDec(pt);
      tagged_t v = *pt; // (pt points to the popped element)

      if (v!=0 && IsVar(v) && !gc_IsMarked(*TaggedToPointer(v))) {
        gc_MarkM(*TaggedToPointer(v));
      } else {
        gc_MarkM(pt[0]);
      }
    }
  }, {
    /* forward pass */
    /* variables trailed in this choicepoint do not have a value given
       in the future */
    limit = TrailTopUnmark(cp->trail_top);
    pt = TrailTopUnmark(prevcp->trail_top);
    while (TrailYounger(limit,pt)) {
      tagged_t v = *pt++;

      if (!gc_IsMarked(v)) {
        gc_UnmarkM(*TaggedToPointer(v));
      }
    }
    /* shunt variables trailed in this choicepoint (may point out of
       the choice heap segment) */
    pt = TrailTopUnmark(prevcp->trail_top);
    while (TrailYounger(limit,pt)) {
      tagged_t v = *pt++;
      if (gc_IsMarked(v)) {
        gc_UnmarkM(pt[-1]);
      } else {
        gc_shuntVariable(*TaggedToPointer(v));
      }
    }
    pt = GCNodeGlobalTop(prevcp);
    while (HeapYounger(GCNodeGlobalTop(cp),pt)) {
      tagged_t v = *pt++;
      if (v&QMask) {
        pt += LargeArity(v);
      } else if (!gc_IsMarked(v)) {
        if (v==Tagp(CVA,pt-1)) {
          gc_MarkM(Cvas_Found);
          pt[-1] = Cvas_Found;
          Cvas_Found = v;
          pt += 2;
        } else {
          gc_shuntVariable(pt[-1]);
        }
      }
    }
    /* shunt frame vars */
    ForEachChoiceFrameX(cp, GCNodeLocalTop(prevcp), pt, {
      ptval = *pt;
      if (!gc_IsMarked(ptval)) {
        gc_shuntVariable(*pt);
      }
    });
        
    /* shunt choice vars */
    pt = cp->x+arity;
    while (pt!=cp->x) {
      --pt;
      gc_shuntVariable(*pt);
    }
  });
}

/* ------------------------------------------------------------------------- */
/* The Marking Phase */

#define TG_Reverse(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(*next); \
  TG_PutPtr(curr,next); \
  curr = next; \
  next = temp; \
}

#define TG_Undo(curr,next) TG_Reverse(next,curr)

#define TG_Advance(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(*curr); \
  TG_PutPtr(next,curr); \
  curr--; \
  next = TaggedToPointer(*curr); \
  TG_PutPtr(temp,curr); \
}

#define MarkRoot(S) CVOID__CALL(mark_root, (S))

/* Mark a root (was markVariable)
   Cyclic structs require this! (TODO: what?)
   pre: start always points outside GC segment
   pre: !TG_IsM(start)
   pre: IsHeapPtr(TG_Val(start))
*/
static CVOID__PROTO(mark_root, tagged_t *start) {
  RTCHECK({
    TG_Let(pt, start);
    if (OnHeap(pt) && GC_HEAP_IN_SEGMENT(pt)) {
      TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: marking a tagged inside the heap segment at %p}\n", (long)debug_inscount, (long)__LINE__, pt);
    }
  });
  RTCHECK({
    TG_Let(pt, start);
    if (rtcheck__is_M(pt)) {
      TG_Fetch(pt);
      TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: marking already marked var 0x%lx at %p}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_Val(pt)), pt);
    }
  });
  RTCHECK({
    TG_Let(pt, start);
    TG_Fetch(pt);
    if (!IsHeapPtr(TG_Val(pt))) {
      TRACE_PRINTF("[time = %ld] {assert[eng_gc:%ld]: marking a non heap term 0x%lx at %p}\n", (long)debug_inscount, (long)__LINE__, (long)(TG_Val(pt)), pt);
    }
  });

  intmach_t found = 0;
  tagged_t *current = start;
  tagged_t *next = TaggedToPointer(*current);
  gc_MarkF(*current);
  goto first_forward;
 forward:
  if (gc_IsMarked(*current)) goto backward;
  found += sizeof(tagged_t);
 first_forward:
  gc_MarkM(*current);
  if (GC_HEAP_IN_SEGMENT(next)) {
    switch(TagOf(*current)) {
    case SVA: /* No pointers from heap to stack */
      SERIOUS_FAULT("GC: stack variable in heap");
    case CVA: /* new 3-field CVA */
      /* N.B. there can be LST pointers to the second cell as well */
      if (!gc_IsForM(*(next+2))) { 
        /* no marking in progress as CVA nor as LST */
        /* treat as 3-cell tuple */
        next++;
        gc_MarkF(*next);
        next++;
        gc_MarkF(*next);
        TG_Reverse(current,next);
        goto forward;
      } else {
        /* otherwise, just treat the value cell */
        goto treat_value_cell;
      }
    case HVA:
    treat_value_cell:
      if (gc_IsForM(*next)) goto backward;
      TG_Reverse(current,next);
      /* note: next may be something that is not a valid pointer
         (i.e. part of a non-pointer tagged) */
      goto forward;
    case LST:
      if (gc_IsFirst(*(next+1)) ||
          (gc_IsMarked(*next) &&
           gc_IsMarked(*(next+1)))) {
        goto backward;
      }
      next++;
      gc_MarkF(*next);
      TG_Reverse(current,next);
      goto forward;
    case STR:
      if (gc_IsMarked(*next)) goto backward;
      if (*next&QMask) { /* box */
        intmach_t ar = LargeArity(*next);
        gc_MarkM(*next);
        found += (ar+1)*sizeof(tagged_t);
        goto backward;
      } else if (!gc_IsFirst(*(next+1))) {
        intmach_t n;
        for (n = Arity(*next); n>0; --n) {
          next++;
          gc_MarkF(*next);
        }
        TG_Reverse(current,next);
        goto forward;
      } else {
        goto backward;
      }
    default: /* all other treated as constants */
      goto backward;
    }
  } else {
    goto backward;
  }
 backward:
  for (;;) {
    if (gc_IsFirst(*current)) break;
    /* internal cell */
    /* note: next may be something that is not a valid pointer
       (i.e. part of a non-pointer tagged) */
    TG_Undo(current,next);
  }
  /* head of chain */
  gc_UnmarkF(*current);
  if (current != start) {
    /* note: next may be something that is not a valid pointer
       (i.e. part of a non-pointer tagged) */
    TG_Advance(current,next);
    goto forward;
  }
  Total_Found += found;
}

/* mark all unbound/newly bound constraint variables */
static CVOID__PROTO(mark_trail_cva) {
  tagged_t *tr = w->trail_top;
  tagged_t v;
  intmach_t wake_count = WakeCount();

  while (Cvas_Found!=atom_nil) { /* mark unbound CVAs */
    *tr = v = Cvas_Found;
    Cvas_Found = *TagpPtr(CVA,v);
    gc_UnmarkM(Cvas_Found);
    *TagpPtr(CVA,v) = v;
    MarkRoot(tr);
  }

  /* find_constraints(Arg,Gc_Choice_Start);
     mark_root(tr);
     {
       tagged_t l = *tr;
       tagged_t v;

       while (TaggedIsCVA(l)) {
         v = l;
         l = *TaggedToPointer(v);
         *TaggedToPointer(v) = v;
       }
     }
     */

  /* mark newly bound CVAs */
  while (wake_count>0) {
    TrailDec(tr);
    tagged_t v = *tr; // (tr points to the popped element)

    if (TaggedIsCVA(v)) {
      --wake_count;
      MarkRoot(tr);
    }
  }
}

/* A frame slot is marked iff it is in the chain of environments for
   one of the frozen execution states, regardless of contents. */
static CVOID__PROTO(mark_frames, choice_t *cp) {
  /* Mark frame chain */
  ForEachChoiceFrameX(cp, Gc_Stack_Start, ev, {
    evval = *ev;
    if (gc_IsMarked(evval)) return;
    if (IsHeapPtr(evval)) {
      MarkRoot(ev);
    } else {
      gc_MarkM(*ev);
    }
  });
}

/* A choicepoint slot is marked iff it contains a heap reference. */
/* A trail slot is marked iff it contains
   an unbound constrained variable reference or a goal.
*/
static CVOID__PROTO(mark_choicepoints) {
#if defined(USE_SEGMENTED_GC)
  {
    /* First mark all trailed old variables */
    tagged_t *tr = w->trail_top;
    tagged_t *limit = TrailTopUnmark(Gc_Choice_Start->trail_top);
    while (TrailYounger(tr,limit)) {
      TrailDec(tr);
      tagged_t v = *tr; // (tr points to the popped element)
      tagged_t *p = TaggedToPointer(v);

      /* TODO: why?? Must be done before any early reset is done.  why?? */
      if (v!=0 && !gc_IsMarked(v) &&
          ((IsHeapVar(v) && !GC_HEAP_IN_SEGMENT(p)) ||
           (IsStackVar(v) && !GC_STACK_IN_SEGMENT(p)))) {
        gc_MarkM(*tr);                       /* so won't look at it again */
        if (IsHeapPtr(*p) && !gc_IsMarked(*p) &&
            GC_HEAP_IN_SEGMENT(TaggedToPointer(*p))) {
          Gcgrey-= Total_Found;
          MarkRoot(p);
          Gcgrey+= Total_Found;
        }
      }
    }
  }
#endif

  /* Mark choicepoints and corresponding chains of frames */
  /* and mark remaining trail entries */
  choice_t *cp = Gc_Aux_Choice;
  tagged_t *tr = w->trail_top;
  tagged_t *limit;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    intmach_t n = ChoiceArity(cp);
    intmach_t i = n;

    CVOID__CALL(mark_frames, cp);
    while ((i--)>0) {
      if (IsHeapPtr(cp->x[i])) {
        MarkRoot(&cp->x[i]);
      }
    }
    cp = ChoiceCont0(cp, n);

    /* mark goals and unbound constrained variables;
       reset unmarked bound variables
       between cp->trail_top and tr */

    limit = TrailTopUnmark(cp->trail_top);
    while (TrailYounger(tr,limit)) {
      TrailDec(tr);
      tagged_t v = *tr; // (tr points to the popped element)
        
      if (v==(tagged_t)NULL || gc_IsMarked(v)) {
      } else if (!IsVar(v)) {
        MarkRoot(tr);
      }
#if defined(USE_EARLY_RESET)
      else if (TaggedIsCVA(v)) {
        if (!gc_IsMarked(*TagpPtr(CVA,v))) {
          *TagpPtr(CVA,v) = v;
          MarkRoot(tr);
          *tr = 0;
        }
      } else {
        if (!gc_IsMarked(*TaggedToPointer(v))) {
          *TaggedToPointer(v) = v;
          *tr = 0;
        }
      }
#else
      else if (TaggedIsCVA(v)) {
          MarkRoot(tr);
      }
#endif
    }
  }
}

/* --------------------------------------------------------------------------- */
/* The Compaction Phase */

#define intoRelocationChain(j,curr) { \
  *(curr) = gc_PutValueFirst(*(j),*(curr)); \
  *(j) = gc_PutValueFirst((tagged_t)curr|GC_FIRSTMASK,*(j)); \
}

#if defined(DEBUG)
/*static int upcount = 0;*/
#endif

static void updateRelocationChain(tagged_t *curr, tagged_t *dest) {
  tagged_t *j;
  tagged_t j1,c1;

#if defined(DEBUG)
    /* Make it go slower to trace it with TOP tool */
    /* struct timespec delay = {0, 0} ;*/
    /* upcount++; */
    /* printf("%d\n", upcount); */
    /* nanosleep(&delay, NULL); */
#endif

  /* F-bit is set in *curr */
  c1 = *curr;
  do {
    j = TaggedToPointer(c1);
    j1 = *j;
    c1 = gc_PutValueFirst(j1,c1);
    *(j) = gc_PutValueFirst((tagged_t)dest,j1);
    /* dest is a pointer, i.e its F-bit is FALSE */
  } while (gc_IsFirst(c1));
  *curr = c1;
}

static CVOID__PROTO(sweep_frames, choice_t *cp) {
  /* sweep frame chain */
  ForEachChoiceFrameX(cp, Gc_Stack_Start, ev, {
    tagged_t *p;
    evval = *ev;
    if (!gc_IsMarked(evval)) return;
    gc_UnmarkM(*ev);
    p = TaggedToPointer(evval);
    if (IsHeapPtr(evval) && GC_HEAP_IN_SEGMENT(p)) {
      intoRelocationChain(p,ev);
    }
  });
}

/* Sweep choicepoints, corresponding chains of frames and trail */
static CVOID__PROTO(sweep_choicepoints) {
  tagged_t *tr;
  tagged_t v, *p;

  tr = w->trail_top;
  tagged_t *limit = TrailTopUnmark(Gc_Choice_Start->trail_top);
  while (TrailYounger(tr,limit)) {
    TrailDec(tr);
    v = *tr; // (tr points to the popped element)
    if (v==0) continue;
    gc_UnmarkM(*tr);
    p = TaggedToPointer(v);
#if defined(USE_SEGMENTED_GC)
    if ((IsHeapVar(v) && !GC_HEAP_IN_SEGMENT(p)) ||
        (IsStackVar(v) && !GC_STACK_IN_SEGMENT(p))) {
      tagged_t *p1 = TaggedToPointer(*p);
      if (IsHeapPtr(*p) &&
          gc_IsMarked(*p) &&
          GC_HEAP_IN_SEGMENT(p1)) {
        gc_UnmarkM(*p);
        intoRelocationChain(p1,p);
      }
    } else if (IsHeapPtr(v) && GC_HEAP_IN_SEGMENT(p)) {
      intoRelocationChain(p,tr);
    }
#else
    if (IsHeapPtr(v)) {
      intoRelocationChain(p,tr);
    }
#endif
  }

  choice_t *cp = Gc_Aux_Choice;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    intmach_t n = ChoiceArity(cp);
    intmach_t i = n;

    sweep_frames(Arg, cp);
    while ((i--)>0) {
      tagged_t v = cp->x[i];
      tagged_t *p = TaggedToPointer(v);
        
      gc_UnmarkM(cp->x[i]);
      if (IsHeapPtr(v) && GC_HEAP_IN_SEGMENT(p)) {
        intoRelocationChain(p, &cp->x[i]);
      }
    }
    cp = ChoiceCont0(cp, n);
  }
}

static CVOID__PROTO(compress_heap) {
  tagged_t cv;
  choice_t *cp = Gc_Aux_Choice;
  tagged_t *curr = G->heap_top;
  tagged_t *dest = HeapCharOffset(Gc_Heap_Start,Total_Found);
  intmach_t garbage_words = 0;
  intmach_t extra;

  /* the upward phase */
  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    cp->heap_top = dest;
    cp=ChoiceCont(cp);
        
    while (HeapYounger(curr,GCNodeGlobalTop(cp))) {
      curr--;
      cv = *curr;
      if (cv&QMask) {     /* skip to box header */
        extra = LargeArity(cv);
                
        curr -= extra;
        cv = *curr;
        if (gc_IsMarked(cv)) {
          dest -= extra;
        } else {
          garbage_words += extra;
        }
      } else {
        extra = 0;
      }
      if (gc_IsMarked(cv)) {
        if (garbage_words) {
          curr[extra+1] = BlobFunctorBignum(garbage_words - 2);
          garbage_words = 0;
        }
        dest--;
        if (gc_IsFirst(cv)) {
          updateRelocationChain(curr,dest);
          cv = *curr;
        }
        if (IsHeapPtr(cv)) {
          tagged_t *p = TaggedToPointer(cv);
                
          if (HeapYounger(curr,p) && GC_HEAP_IN_SEGMENT(p)) {
            intoRelocationChain(p,curr);
          } else if (p==curr) { /* a cell pointing to itself */
            *curr = gc_PutValue((tagged_t)dest,cv);
          }
        }
      } else {
        garbage_words++;
      }
    }
  }

  /* The downward phase */
  /* curr and dest both point to the beginning of the heap */
  curr += garbage_words;
  while (HeapYounger(G->heap_top,curr)) {
    cv = *curr;
    if (gc_IsMarked(cv)) {
      if (gc_IsFirst(cv)) {
        updateRelocationChain(curr,dest);
        cv = *curr;
      }
      gc_UnmarkM(cv);  /* M and F-flags off */
      {
        tagged_t *p = TaggedToPointer(cv);
        
        if (IsHeapPtr(cv) && HeapYounger(p,curr)) {              
          /* move the current cell and insert into the reloc.chain */
          *dest = cv;
          intoRelocationChain(p,dest);
        } else if (cv&QMask) { /* move a box */
          *curr = cv;
          for (extra = LargeArity(cv); extra>0; extra--) {
            *dest++ = *curr++;
          }
          *dest = cv;
        } else { /* just move the current cell */
          *dest = cv;
        }
      }
      dest++;
    } else { /* skip a box---all garbage is boxed */
      curr += LargeArity(cv);
    }
    curr++;
  }
  G->heap_top = dest;
}

/* ------------------------------------------------------------------------- */
/* The main garbage collection routine */

/* The X REGISTERS have been saved already in an frame */
/* note: calculate_segment_choice has to be called before */
CVOID__PROTO(gc__heap_collect) {
  frame_t *newa;

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " enters gc__heap_collect\n", (intmach_t)Thread_Id);

  GetFrameTop(newa, w->choice,G->frame);
#if defined(USE_GC_STATS)
  intmach_t hz = HeapDifference(Heap_Start,G->heap_top); /* current heap size */
  switch (current_gctrace) {
  case GCTRACE__OFF:
    break;
  case GCTRACE__TERSE:
    TRACE_PRINTF("{GC}\n");
    break;
  case GCTRACE__VERBOSE:
    TRACE_PRINTF("\n{GC}  Heap GC started\n");
    TRACE_PRINTF("Heap:   from %p to %p (total size = %" PRIdm ")\n",
                 Heap_Start, 
                 Heap_End,
                 (intmach_t)HeapDifference(Heap_Start, Heap_End));
    TRACE_PRINTF("        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                 G->heap_top,  
                 (intmach_t)HeapDifference(Heap_Start, G->heap_top),
                 (intmach_t)HeapDifference(G->heap_top, Heap_End));
    TRACE_PRINTF("        GC start at %p\n", 
                 gc_HeapStart);

    TRACE_PRINTF("Stack:  from %p to %p (total size = %" PRIdm ")\n",
                 Stack_Start, 
                 Stack_End,
                 (intmach_t)StackCharSize());
    TRACE_PRINTF("        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                 w->local_top, 
                 (intmach_t)StackCharUsed(w->local_top),
                 (intmach_t)StackCharAvailable(w->local_top));
    TRACE_PRINTF("        GC start at %p\n", 
                 gc_StackStart);

    TRACE_PRINTF("Choice/Trail: from %p to %p (total size = %" PRIdm ")\n",
                 Choice_Start, 
                 Choice_End,
                 (intmach_t)ChoiceDifference(Choice_Start,Choice_End));
    TRACE_PRINTF("        Ch. top at %p (used = %" PRIdm ")\n", 
                 w->choice, 
                 (intmach_t)ChoiceDifference(Choice_Start, w->choice));
    TRACE_PRINTF("        Tr. top at %p (used = %" PRIdm ")\n", 
                 w->trail_top, 
                 (intmach_t)TrailDifference(Trail_Start,w->trail_top));
    TRACE_PRINTF("        Ch./Tr. free %" PRIdm "\n",
                 (intmach_t)ChoiceDifference(w->choice, w->trail_top));
    break;
  }
  flt64_t t1 = RunTickFunc();
#endif

  /* push special registers on the trail stack */
#if defined(USE_GLOBAL_VARS)
  TrailPush(w->trail_top,GLOBAL_VARS_ROOT);
#endif
  TrailPush(w->trail_top,Current_Debugger_State);

  Total_Found = 0;
  Gcgrey = 0;
  if (w->segment_choice == InitialChoice) {
    Gc_Total_Grey = 0;
  }
  CVOID__CALL(trail__remove_uncond); /* sets Gc_Aux_Choice, gc_Choice_Start */
  Gc_Aux_Choice->local_top = newa;
  Gc_Aux_Choice->heap_top = G->heap_top;
  Gc_Aux_Choice->frame = w->frame;
  Gc_Aux_Choice->next_insn = w->next_insn;
  Gc_Heap_Start = gc_HeapStart;
  Gc_Stack_Start = gc_StackStart;

  Cvas_Found = atom_nil;

  if (WakeCount()) {
    /* TODO: why? */
    if (current_gctrace == GCTRACE__VERBOSE) {
      TRACE_PRINTF("{GC}  Shunting disabled due to pending unifications\n");
    }
  } else {
    CVOID__CALL(shunt_variables);
  }

  CVOID__CALL(mark_trail_cva);
  CVOID__CALL(mark_choicepoints);
  CVOID__CALL(trail__compress, TRUE);

  Gc_Total_Grey += Gcgrey;
#if defined(USE_GC_STATS)          
  flt64_t t2 = RunTickFunc();
  flt64_t mark_time = t2 - t1;
  if (current_gctrace == GCTRACE__VERBOSE) {
    TRACE_PRINTF("        mark: %" PRIdm " bytes marked in %.3f sec\n",
                 Total_Found,((flt64_t)mark_time)/RunClockFreq(ciao_stats));
#if defined(USE_SEGMENTED_GC)
    TRACE_PRINTF("{GC}  no more than %" PRIdm " garbage bytes left\n", Gcgrey);
#endif
  }
#endif

  CVOID__CALL(sweep_choicepoints);
  CVOID__CALL(compress_heap);

  /* pop special registers from the trail stack */
  TrailDec(w->trail_top);
  Current_Debugger_State = *(w->trail_top); // (w->trail_top points to the popped element)
#if defined(USE_GLOBAL_VARS)
  TrailDec(w->trail_top);
  GLOBAL_VARS_ROOT = *(w->trail_top); // (w->trail_top points to the popped element)
#endif
    
  SetChoice(w->choice);     /* shadow regs may have changed */
#if defined(USE_GC_STATS)
  /* statistics */
  flt64_t compress_time = RunTickFunc()-t2;
  flt64_t gc_time = mark_time + compress_time;
  ciao_stats.gc_tick  += gc_time;
  ciao_stats.starttick += gc_time;
  ciao_stats.lasttick += gc_time;
#if defined(OPTIM_COMP)
  if (ciao_stats.gc_longest_tick < gc_time) {
    ciao_stats.gc_longest_tick = gc_time;
  }
#endif
  ciao_stats.gc_count++;
  intmach_t gc_reclaimed = hz-HeapDifference(Heap_Start,G->heap_top);
  ciao_stats.gc_acc += gc_reclaimed;
  if (current_gctrace == GCTRACE__VERBOSE) {
    TRACE_PRINTF("        Heap: %" PRIdm " cells reclaimed in %.3f sec\n",
                 (intmach_t)gc_reclaimed,
                 ((flt64_t)compress_time)/RunClockFreq(ciao_stats));
    TRACE_PRINTF("Heap:   from %p to %p (total size = %" PRIdm ")\n",
                 Heap_Start, 
                 Heap_End,
                 (intmach_t)HeapDifference(Heap_Start, Heap_End));
    TRACE_PRINTF("        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                 G->heap_top,  
                 (intmach_t)HeapDifference(Heap_Start, G->heap_top),
                 (intmach_t)HeapDifference(G->heap_top, Heap_End));
    TRACE_PRINTF("        GC start at %p\n", 
                 gc_HeapStart);

    TRACE_PRINTF("        Total: %" PRIdm " cells reclaimed in %" PRIdm " gc's\n",
                 ciao_stats.gc_acc,ciao_stats.gc_count);
    TRACE_PRINTF("        GC time = %.6f  Total = %.6f\n\n",
                 ((flt64_t)gc_time)/RunClockFreq(ciao_stats),
                 ((flt64_t)ciao_stats.gc_tick)/RunClockFreq(ciao_stats));
  }
#endif
}

/* --------------------------------------------------------------------------- */

void init_gc(void) {
  current_gcmode = TRUE;
  current_gctrace = GCTRACE__OFF;
  current_gcmargin = 500; /* Quintus has 1024 */
}
