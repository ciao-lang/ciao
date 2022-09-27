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

#define USE_SEGMENTED_GC 1
#define USE_EARLY_RESET 1

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

#if defined(OPTIM_COMP)
#define TryNodeArity(ALT) ((ALT)->arity)
#endif

/* TODO: document: Gc_Aux_Choice is the newest choice and Gc_Choice_Start the oldest */
#define CHOICE_PASS(CP, PREVCP, ARITY, BACKWARD, FORWARD) { \
  try_node_t *ALT; \
  (CP) = Gc_Aux_Choice; \
  (PREVCP) = w->choice; \
  (ALT) = fail_alt; \
  (ARITY) = TryNodeArity((ALT)); \
  while (ChoiceYounger((CP),Gc_Choice_Start)) { \
    BACKWARD; \
    CHOICE_PASS__ReverseChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = TryNodeArity((ALT)); \
  } \
  while (ChoiceYounger(Gc_Aux_Choice,(CP))) { \
    CHOICE_PASS__UndoChoice((CP),(PREVCP),(ALT)); \
    (ARITY) = TryNodeArity((ALT)); \
    FORWARD; \
  } \
}

#define CHOICE_PASS__ReverseChoice(cp,prevcp,alt) { \
  try_node_t *m_alt = alt; \
  cp = prevcp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  prevcp = GEN_ChoiceCont00(cp,GEN_TryNodeOffset(alt)); \
}

#define CHOICE_PASS__UndoChoice(cp,prevcp,alt) { \
  try_node_t *m_alt = alt; \
  prevcp = cp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  cp = GEN_ChoiceNext00(cp,GEN_TryNodeOffset(alt)); \
}

/* =========================================================================== */

static CVOID__PROTO(shunt_variables);
static CVOID__PROTO(mark_trail_cva);
static CVOID__PROTO(mark_frames, choice_t *cp);
static CVOID__PROTO(mark_choicepoints);
static CVOID__PROTO(mark_root, tagged_t *start);
static void updateRelocationChain(tagged_t *curr, tagged_t *dest);
static CVOID__PROTO(sweep_frames, choice_t *cp);
static CVOID__PROTO(sweep_choicepoints);
static CVOID__PROTO(compress_heap);

#define gc_TrailStart TaggedToPointer(w->segment_choice->trail_top)
#define gc_HeapStart (NodeGlobalTop(w->segment_choice))
#define gc_StackStart (NodeLocalTop(w->segment_choice))
#define gc_ChoiceStart (w->segment_choice)

/* ------------------------------------------------------------------------- */
/* GARBAGE COLLECTION ROUTINES */

/* Based on the algorithms described in:

   "Garbage Collection for Prolog Based on WAM",
   by K. Appleby, M. Carlsson, S. Haridi, and D. Sahlin,
   Communications of the ACM 31:6, pp. 719-741,

   somewhat complicated by support for freeze & wait decls
   (constrained variables), and undo/1 (goals on the trail).
*/

/* gc global variables */

/*  These were shared, and a lot of havoc when concurrent GC was taking place.

ptrdiff_t gc_total_grey=0;
choice_t *gc_aux_node;
choice_t *gc_choice_start;

static tagged_t *gc_heap_start;
static frame_t *gc_stack_start;
static ptrdiff_t gcgrey;
static ptrdiff_t total_found;
static tagged_t cvas_found;

*/

/* --------------------------------------------------------------------------- */
/* The Shunting Phase */

#define gc_shuntVariable(shunt_dest) { \
  tagged_t shunt_src; \
  while (IsVar(shunt_dest) && \
         !gc_IsMarked(shunt_src = *TaggedToPointer(shunt_dest)) && \
         shunt_src!=shunt_dest) { \
    shunt_dest = shunt_src; \
  } \
}

static CVOID__PROTO(shunt_variables) {
  tagged_t *pt = w->trail_top;
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity MAYBE_UNUSED;
  intmach_t i;
  tagged_t *limit;
  frame_t *frame;

  CHOICE_PASS(cp, prevcp, arity, {
    /* backward pass */
    /* all variables in the trail has a value given in the future */
    /* (leave unmarked only the more recent trail entry for each variable) */
    limit = TaggedToPointer(prevcp->trail_top);
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
    limit = TaggedToPointer(cp->trail_top);
    pt = TaggedToPointer(prevcp->trail_top);
    while (TrailYounger(limit,pt)) {
      tagged_t v = *pt++;

      if (!gc_IsMarked(v)) {
        gc_UnmarkM(*TaggedToPointer(v));
      }
    }
    /* shunt variables trailed in this choicepoint (may point out of
       the choice heap segment) */
    pt = TaggedToPointer(prevcp->trail_top);
    while (TrailYounger(limit,pt)) {
      tagged_t v = *pt++;
      if (gc_IsMarked(v)) {
        gc_UnmarkM(pt[-1]);
      } else {
        gc_shuntVariable(*TaggedToPointer(v));
      }
    }
    pt = NodeGlobalTop(prevcp);
    while (HeapYounger(NodeGlobalTop(cp),pt)) {
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
    i = FrameSize(cp->next_insn);
    frame = cp->frame;
    while (OffStacktop(frame,NodeLocalTop(prevcp))) {
      pt = (tagged_t *)StackCharOffset(frame,i);
      while (pt!=frame->x) {
        if (!gc_IsMarked(*(--pt))) {
          gc_shuntVariable(*pt);
        }
      }
      i = FrameSize(frame->next_insn);
      frame = frame->frame;
    }
        
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
#if defined(OPTIM_COMP)
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
#endif

  intmach_t found = 0;
  tagged_t *current = start;
  tagged_t *next = TaggedToPointer(*current);
  gc_MarkF(*current);
  goto first_forward;
 forward:
  if (gc_IsMarked(*current)) goto backward;
  found++;
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
      if (gc_IsMarked(*next)) {
      } else if (*next&QMask) { /* box */
        intmach_t ar = LargeArity(*next);
        gc_MarkM(*next);
        found += ar+1;
      } else if (!gc_IsFirst(*(next+1))) {
        intmach_t n;
        for (n = Arity(*next); n>0; --n) {
          next++;
          gc_MarkF(*next);
        }
        TG_Reverse(current,next);
        goto forward;
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
  frame_t *frame = cp->frame;
  bcp_t l = cp->next_insn;
  /* Mark frame chain */
  tagged_t *ev;

  while (OffStacktop(frame,Gc_Stack_Start)) {
    ev = (tagged_t *)StackCharOffset(frame,FrameSize(l));
    while (ev!=frame->x) {
      tagged_t v;
        
      StackDecr(ev);
      if (gc_IsMarked(v = *ev)) return;
      if (IsHeapPtr(v)) {
        MarkRoot(ev);
      } else {
        gc_MarkM(*ev);
      }
    }
    l = frame->next_insn;
    frame = frame->frame;
  }
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
    tagged_t *limit = TaggedToPointer(Gc_Choice_Start->trail_top);
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
    intmach_t n = GEN_ChoiceSize0(cp);
    intmach_t i = GEN_OffsetToArity(n);

    CVOID__CALL(mark_frames, cp);
    while ((i--)>0) {
      if (IsHeapPtr(cp->x[i])) {
        MarkRoot(&cp->x[i]);
      }
    }
    cp = GEN_ChoiceCont00(cp, n);

    /* mark goals and unbound constrained variables;
       reset unmarked bound variables
       between cp->trail_top and tr */

    limit = TaggedToPointer(cp->trail_top);
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

/* delete 0's from the trail */
CVOID__PROTO(compress_trail, bool_t from_gc) {
  tagged_t *curr;
  tagged_t *dest;
  choice_t *cp;
  choice_t *prevcp;
  intmach_t arity MAYBE_UNUSED;

  dest = TaggedToPointer(Gc_Choice_Start->trail_top);
  curr = dest;
  CHOICE_PASS(cp, prevcp, arity, {
  }, {
    tagged_t *limit;
    limit = TaggedToPointer(cp->trail_top);
    while (TrailYounger(limit,curr)) {
      tagged_t cv;
      cv = *curr;
      curr++;
      if (cv != (tagged_t)0) {
        TrailPush(dest,cv);
      }
    }
    cp->trail_top -= limit-dest;
    if (from_gc) ChoiceptMarkPure(cp);
  });

  w->trail_top = dest;
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
  frame_t *frame = cp->frame;
  bcp_t l = cp->next_insn;
  /* sweep frame chain */
  tagged_t *ev;

  while (OffStacktop(frame,Gc_Stack_Start)) {
    ev = (tagged_t *)StackCharOffset(frame,FrameSize(l));
    while (ev!=frame->x) {
      tagged_t v, *p;
        
      StackDecr(ev);
      if (!gc_IsMarked(v = *ev)) return;
      gc_UnmarkM(*ev);
      p = TaggedToPointer(v);
      if (IsHeapPtr(v) && GC_HEAP_IN_SEGMENT(p)) {
        intoRelocationChain(p,ev);
      }
    }
    l = frame->next_insn;
    frame = frame->frame;
  }
}

/* Sweep choicepoints, corresponding chains of frames and trail */
static CVOID__PROTO(sweep_choicepoints) {
  tagged_t *tr;
  tagged_t v, *p;

  tr = w->trail_top;
  tagged_t *limit = TaggedToPointer(Gc_Choice_Start->trail_top);
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
    intmach_t n = GEN_ChoiceSize0(cp);
    intmach_t i = GEN_OffsetToArity(n);

    sweep_frames(Arg, cp);
    while ((i--)>0) {
      tagged_t v = cp->x[i];
      tagged_t *p = TaggedToPointer(v);
        
      gc_UnmarkM(cp->x[i]);
      if (IsHeapPtr(v) && GC_HEAP_IN_SEGMENT(p)) {
        intoRelocationChain(p, &cp->x[i]);
      }
    }
    cp = GEN_ChoiceCont00(cp, n);
  }
}

static CVOID__PROTO(compress_heap) {
  tagged_t cv;
  choice_t *cp = Gc_Aux_Choice;
  tagged_t *curr = G->heap_top;
  tagged_t *dest = HeapOffset(Gc_Heap_Start,Total_Found);
  intmach_t garbage_words = 0;
  intmach_t extra;

  /* the upward phase */
  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    cp->heap_top = dest;
    cp=ChoiceCont(cp);
        
    while (HeapYounger(curr,NodeGlobalTop(cp))) {
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

  ComputeA(newa, w->choice);
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
                 (intmach_t)StackDifference(Stack_Start, Stack_End));
    TRACE_PRINTF("        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                 w->local_top, 
                 (intmach_t)StackDifference(Stack_Start,w->local_top),
                 (intmach_t)StackDifference(w->local_top, Stack_End));
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
  if (w->segment_choice == InitialNode) {
    Gc_Total_Grey = 0;
  }
  CVOID__CALL(trail_gc); /* sets Gc_Aux_Choice, gc_Choice_Start */
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
  CVOID__CALL(compress_trail, TRUE);

  Gc_Total_Grey += Gcgrey;
#if defined(USE_GC_STATS)          
  flt64_t t2 = RunTickFunc();
  flt64_t mark_time = t2 - t1;
  if (current_gctrace == GCTRACE__VERBOSE) {
    TRACE_PRINTF("        mark: %" PRIdm " cells marked in %.3f sec\n",
                 Total_Found,((flt64_t)mark_time)/RunClockFreq(ciao_stats));
#if defined(USE_SEGMENTED_GC)
    TRACE_PRINTF("        no more than %" PRIdm " garbage cells left\n",
                 Gcgrey);
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
    
  SetShadowregs(w->choice);     /* shadow regs may have changed */
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

/* =========================================================================== */
/* Code for growing areas when full. */

static CVOID__PROTO(calculate_segment_choice);

/* Service routine for HEAPMARGIN* instructions.
 * pad - required amount of heap space.
 * arity - number of live X regs at this point.
 */
CVOID__PROTO(explicit_heap_overflow, intmach_t pad, intmach_t arity) {
  choice_t *b = w->choice;
  intmach_t i;
  frame_t *a;

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling explicit_heap_overflow\n", (intmach_t)Thread_Id);
  
  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* DO NOT clear w->next_alt -- we are still in "shallow mode" */
  if (!b->next_alt) {                   /* try */
    b->next_alt = w->next_alt; /* 4 contiguous moves */
    b->frame = w->frame;
    b->next_insn = w->next_insn;
    b->local_top = w->local_top;
    i=ChoiceArity(b);
    if (i>0) {
      tagged_t *t = (tagged_t *)w->previous_choice;
      do {
        *--(t) = X(--i);
      } while (i>0);
    }
    if (ChoiceYounger(ChoiceOffset(b,CHOICEPAD),w->trail_top)) {
      choice_overflow(Arg,CHOICEPAD,TRUE);
      b = w->choice;
    }
  }
  
  /* ensure that X regs are seen by heap_overflow(): make a frame */
  ComputeA(a,b);
  a->x[0] = TaggedZero;
  for (i=0; i<arity; i++) {
    a->x[i+1] = X(i);
  }
  a->frame = w->frame;
  a->next_insn = w->next_insn;
  w->frame = a;
  w->next_insn = CONTCODE(i+1);
  w->local_top = (frame_t *)Offset(a,EToY0+i+1);

  heap_overflow(Arg,pad);

  for (i=0; i<arity; i++) {
    X(i) = a->x[i+1];
  }
  w->local_top = a;
  w->frame = a->frame;
  w->next_insn = a->next_insn;
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

/* ------------------------------------------------------------------------- */

#if defined(ANDPARALLEL)
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor, tagged_t *newh, bool_t remote_reloc, worker_t *remote_worker);
#else
CVOID__PROTO(heap_overflow_adjust_wam, intmach_t reloc_factor, tagged_t *newh);
#endif

/* --------------------------------------------------------------------------- */

/* Here when w->choice and w->trail_top are within CHOICEPAD from each other. */
CVOID__PROTO(choice_overflow, intmach_t pad, bool_t remove_trail_uncond) {
  tagged_t *choice_top;

#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling choice overflow\n", (intmach_t)Thread_Id);

#if defined(ANDPARALLEL)
  DEBUG__TRACE(debug_threads, "WAM %p is in choice_overflow!\n", w);
  CVOID__CALL(suspend_all);
#endif

#if defined(USE_GC_STATS)          
  inttime_t tick0 = RunTickFunc();
#endif

  try_node_t *next_alt;
  if (!(next_alt = w->choice->next_alt)) { /* ensure A', P' exist */
    w->choice->next_alt = w->next_alt;
    w->choice->local_top = w->local_top;
  }

  if (remove_trail_uncond) {
    /* note: trail__remove_uncond not executed in compile_term */
    CVOID__CALL(calculate_segment_choice);
    CVOID__CALL(trail_gc);
    CVOID__CALL(compress_trail,FALSE);
  }

  /* ASSUMED: --CHOICE, TRAIL++ */

  choice_top = (tagged_t *)w->choice+w->value_trail;
  if (ChoiceYounger(ChoiceOffset(choice_top,2*pad),w->trail_top)) {
    choice_t *b;
    tagged_t *newtr;
    intmach_t mincount, newcount, oldcount, reloc_factor, chpt_reloc_factor;
    
    {
      mincount = 2*pad - ChoiceDifference(choice_top,w->trail_top);
      oldcount = ChoiceDifference(Choice_Start,Choice_End);
      newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
      newtr = checkrealloc_ARRAY(tagged_t,
                                 oldcount,
                                 newcount,
                                 Trail_Start);
      DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing TRAIL from %p to %p\n", (intmach_t)Thread_Id, Trail_Start, newtr);
    }
    reloc_factor = (char *)newtr - (char *)Trail_Start;
    chpt_reloc_factor = reloc_factor + (newcount-oldcount)*sizeof(tagged_t);
    {
      tagged_t *tr;
      tagged_t *trb;
      
      tr = RelocPtr(Choice_Start, reloc_factor);
      trb = RelocPtr(choice_top, reloc_factor);
      Trail_Start = Choice_End = newtr;                /* new low bound */
      Choice_Start = Trail_End = newtr+newcount;      /* new high bound */

#if defined(USE_TAGGED_CHOICE_START)
      /* Do not take out (tagged_t) casting, or the engine will break!! */
      Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif

      {
        tagged_t *x;
        /* We have to relocate the concurrent topmost choicepoint */
#if defined(USE_THREADS)
        choice_t *concchpt;
#endif
        
        x = Choice_Start;                  /* Copy the new choicepoint stack */
        while (OffChoicetop(trb,tr)) {
          *(--x) = *--(tr);
        }
        w->choice = b = (choice_t *)(x-w->value_trail);

#if defined(USE_THREADS)
        /* The chain of concurrent dynamic choicepoints has to be
           relocated as well.  The initial TopConcChpt was set to be
           the initial choice node.  MCL. */
        concchpt = TopConcChpt = RelocPtr(TopConcChpt, chpt_reloc_factor);

        while(concchpt != InitialNode) {
          DEBUG__TRACE(debug_concchoicepoints || debug_gc,
                       "*** %" PRIdm "(%" PRIdm ") Changing dynamic chpt@%x\n",
                       (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
                       (unsigned int)concchpt);
          // TODO: wrong if it is Zero (null)? (JFMC)
          choice_t *prev = (choice_t *)TermToPointerOrNull(concchpt->x[PrevDynChpt]);
          AssignRelocPtr(prev, chpt_reloc_factor);
          concchpt->x[PrevDynChpt] = PointerToTermOrZero(prev);
          concchpt = prev;
        }
#endif
      }
    }
    AssignRelocPtr(w->previous_choice, chpt_reloc_factor);
    AssignRelocPtr(w->trail_top, reloc_factor);

#if defined(ANDPARALLEL)
    /* relocate pointer in handlers */
    parallel_exec_entry_t *lpe = Last_Parallel_Exec;
    while (lpe != NULL) {
      if (lpe->init != NULL) {
        AssignRelocPtr(lpe->init, chpt_reloc_factor);
      }
      if (lpe->end != NULL) {
        AssignRelocPtr(lpe->end, chpt_reloc_factor);
      }
      lpe = lpe->prev;
    }

#endif

    while (OffChoicetop(b,Choice_Start)) {
      AssignRelocPtr(b->trail_top, reloc_factor);
      b = ChoiceCont(b);
    }
  }

  w->choice->next_alt = next_alt;

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
}

/* Here when w->local_top and Stack_End are within STACKAD from each other. */
CVOID__PROTO(stack_overflow) {
  intmach_t count, reloc_factor;
  tagged_t *newh;

#if defined(USE_GC_STATS)          
  flt64_t tick0 = RunTickFunc();
#endif
  
#if defined(ANDPARALLEL)
  Suspend = WAITING;
  Wait_Acquire_slock(stackset_expansion_l);
  Suspend = RELEASED;
#endif

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " calling stack overflow\n", (intmach_t)Thread_Id);

#if defined(ANDPARALLEL)
  DEBUG__TRACE(debug_threads, "WAM %p is in stack_overflow!\n", w);
  CVOID__CALL(suspend_all);
#endif

  ComputeA(w->local_top,w->choice);

  count = StackDifference(Stack_Start,Stack_End);
  newh = checkrealloc_ARRAY(tagged_t,
                            count/2,
                            2*count,
                            Stack_Start);
  count = 2*StackDifference(Stack_Start,Stack_End);
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing STACK from %p to %p\n", (intmach_t)Thread_Id, Stack_Start, newh);

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */
  CVOID__CALL(stack_overflow_adjust_wam,reloc_factor);

  /* Final adjustments */
  Stack_Start = newh;           /* new low bound */
  Stack_End = newh+count;       /* new high bound */
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
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
}

CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor) {
  if (reloc_factor==0) return;

  choice_t *n, *n2;
  choice_t *aux_node;
  frame_t *frame;
  intmach_t i;

  aux_node = ChoiceNext0(w->choice,0);
  aux_node->next_alt = fail_alt;
  aux_node->frame = RelocPtr(w->frame, reloc_factor);
  aux_node->next_insn = w->next_insn;
  aux_node->local_top = RelocPtr(w->local_top, reloc_factor);

  /* relocate pointers in trail */
  {
    TG_Let(pt1, Trail_Start);
    while (TrailYounger(w->trail_top,pt1)) {
      TG_Fetch(pt1);
      RelocateIfSVA(pt1, reloc_factor);
      pt1++;
    }
  }

  /* relocate pointers in choice&env stks */
  for (n=aux_node; n!=InitialNode; n=n2){
    n2 = ChoiceCont(n);
    //Tabling --> How to translate?
    AssignRelocPtr(n2->local_top, reloc_factor);
    AssignRelocPtr(n2->frame, reloc_factor);

    {
      TG_Let(pt1, n->x);
      for (; pt1!=(tagged_t *)n2;) {
        TG_Fetch(pt1);
        RelocateIfSVA(pt1, reloc_factor);
        pt1++;
      }
    }
      
    i = FrameSize(n->next_insn);
    frame = n->frame;
    while (frame >= (frame_t*) NodeLocalTop(n2)) {
      {
        TG_Let(pt1, (tagged_t *)StackCharOffset(frame,i));
        while (pt1!=frame->x){
          pt1--;
          TG_Fetch(pt1);
          RelocateIfSVA(pt1, reloc_factor);
        }
      }
      if (frame->frame) {
        AssignRelocPtr(frame->frame, reloc_factor);
        i = FrameSize(frame->next_insn);
        frame = frame->frame;
      } else {
        frame = NULL;
      }
    }
  }

  w->frame = aux_node->frame;
  w->local_top = NodeLocalTop(aux_node);
  SetShadowregs(w->choice);
}

/* ------------------------------------------------------------------------- */

/* TODO: per worker? */
bool_t current_gcmode;
intmach_t current_gctrace;
intmach_t current_gcmargin;

/* TODO: per worker? */
static bool_t gcexplicit = FALSE;       /* Shared, no locked --- global flag */

CBOOL__PROTO(gc_start) {
  gcexplicit = TRUE;
  heap_overflow(Arg,CALLPAD);
  return TRUE;
}

/* Here when G->heap_top and Heap_End are within CALLPAD from each other. */
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
    lowboundh = newh-Gc_Total_Grey;
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GCMARGIN_CHARS ||
         HeapYounger(HeapCharOffset(newh,2*pad),Heap_End)) &&
        !(HeapCharDifference(lowboundh,oldh) < GCMARGIN_CHARS ||
          HeapYounger(HeapCharOffset(lowboundh,2*pad),Heap_End))) {
      /* garbage collect the entire heap */
      w->segment_choice = InitialNode;
      CVOID__CALL(gc__heap_collect);
      newh = G->heap_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GCMARGIN_CHARS) ||
      HeapYounger(HeapCharOffset(newh,2*pad),Heap_End)) {
#if defined(USE_GC_STATS)          
    flt64_t tick0 = RunTickFunc();
#endif
    /* increase heapsize */
    intmach_t mincount, newcount, oldcount, reloc_factor;
    tagged_t *newh;
    
    intmach_t wake_count = WakeCount();
    
    ComputeA(w->local_top,w->choice);
    
    mincount = 2*pad - HeapCharDifference(G->heap_top,Heap_End);
    oldcount = HeapCharDifference(Heap_Start,Heap_End);
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);

    newh = (tagged_t *)checkrealloc_ARRAY(char, oldcount, newcount, Heap_Start);

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
  choice_t *aux_node;
  frame_t *frame;
  intmach_t i;

  aux_node = ChoiceNext0(w->choice,0);
  aux_node->next_alt = fail_alt;
  aux_node->frame = w->frame;
  aux_node->next_insn = w->next_insn;
  aux_node->heap_top = G->heap_top;
  aux_node->local_top = w->local_top; /* segfault patch -- jf */

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
      if (TG_Val(pt1)&QMask) {
        pt1 += LargeArity(TG_Val(pt1)) + 1;
      } else {
        RelocateIfHeapPtr(pt1, reloc_factor);
        pt1++;
      }
    }
  }

#if defined(USE_GLOBAL_VARS)
  /* relocate pointers in global vars root */
  if (IsHeapPtr(GLOBAL_VARS_ROOT)) {
    GLOBAL_VARS_ROOT += reloc_factor;
  }
#endif

  /* relocate pointers in trail stk */
  {
    TG_Let(pt1, Trail_Start);
    TrailPush(w->trail_top,Current_Debugger_State);
    while (TrailYounger(w->trail_top,pt1)) {
      TG_Fetch(pt1);
      RelocateIfHeapPtr(pt1, reloc_factor);
      pt1++;
    }
  }
  TrailDec(w->trail_top);
  Current_Debugger_State = *(w->trail_top); // (w->trail_top points to the popped element)

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

  /* relocate pointers in choice&env stks */
  n2 = NULL;
  for (n=aux_node; n!=InitialNode && n->next_alt!=NULL; n=n2) {
    if (n->next_alt != NULL) { // TODO: can be null?
      n2 = ChoiceCont(n);
      {
        TG_Let(pt1, n->x);
        for (; pt1!=(tagged_t *)n2;) {
          TG_Fetch(pt1);
          RelocateIfHeapPtr(pt1, reloc_factor);
          pt1++;
        }
      }
      i = FrameSize(n->next_insn);
      frame = n->frame;
      while ((frame >= (frame_t*) NodeLocalTop(n2)) && frame->next_insn != NULL) {
        TG_Let(pt1, (tagged_t *)StackCharOffset(frame,i));
        while (pt1!=frame->x) {
          --pt1;
          TG_Fetch(pt1);
          RelocateIfHeapPtr(pt1, reloc_factor);
        }
        i = FrameSize(frame->next_insn);
        frame = frame->frame;
      } 

      // TODO: TABLING ->> How to translate???
      AssignRelocPtrNotRemote(n->heap_top, reloc_factor);
    }
  }

  // TODO: TABLING ->> How to translate???
  AssignRelocPtrNotRemote(n->heap_top, reloc_factor);
  SetShadowregs(w->choice);
}

/* Tidy new half of trail exhaustively. */
CVOID__PROTO(trail_gc) {
  tagged_t *tr = w->trail_top;
  choice_t *b = w->choice;
  intmach_t wake_count = WakeCount();
  tagged_t heap_last = Tagp(HVA,HeapOffset(Heap_End,-1));
  /*extern choice_t *gc_aux_node;*/ /* Now in a register */
  /*extern choice_t *gc_choice_start;*/ /* No in a register */

  Gc_Aux_Choice = ChoiceNext0(b,0);
  Gc_Aux_Choice->next_alt = fail_alt;
  Gc_Aux_Choice->trail_top = tr;
  Gc_Choice_Start = w->segment_choice;
  
  if (current_gctrace == GCTRACE__VERBOSE) {
    TRACE_PRINTF("{GC}  Trail GC started\n");
  }

  while (!OffChoicetop(Gc_Choice_Start,b)) {
    /* sweep trail segment to get rid of multiple 'undo setarg'
       trailings of the same location.  Go from old to new. */
    tagged_t *x;
    tagged_t t1;
      
    for (x=TaggedToPointer(b->trail_top); TrailYounger(tr,x); x++) {
      if (TaggedIsHVA(t1 = *x)) {
        if (*TagpPtr(HVA,t1) & 1) {
          *TrailOffset(x,-1) = *x = heap_last;
        } else {
          *TagpPtr(HVA,t1) ^= 1; /* turn mark bit on */
        }
      }
    }

    /* sweep trail segment to get rid of unconditional entries.
       Keep count of relevant entries.  Turn mark bits off.
       Go from new to old. */
    SetShadowregs(b);
    x=TaggedToPointer(b->trail_top);
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
  b = w->choice;
  SetShadowregs(b);
}

void init_gc(void) {
  current_gcmode = TRUE;
  current_gctrace = GCTRACE__OFF;
  current_gcmargin = 500; /* Quintus has 1024 */
}
