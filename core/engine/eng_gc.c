/*
 *  eng_gc.c
 *
 *  Garbage collector and code for growing areas when full.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <stddef.h> /* ptrdiff_t */

/* #defines MUST precede #includes here. */
#define SEGMENTED_GC 1
#define EARLY_RESET 1

#include <ciao/eng.h>
#include <ciao/basiccontrol.h>
#include <ciao/io_basic.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_start.h>
#include <ciao/timing.h>

// TODO: in eng_debug.h
#if !defined(OPTIM_COMP)
#if defined(DEBUG)
#define DEBUG__TRACE(COND, ...) ({ \
  if ((COND)) { TRACE_PRINTF(__VA_ARGS__); } \
})
#else
#define DEBUG__TRACE(COND, ...)
#endif
#endif

#define PreHeapRead(X) (*++(X))
#define HeapPop(X) (*--(X))

/* =========================================================================== */
/* Utility macros for heap GC. */

#define gc_Reverse(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(*next); \
  *next = gc_PutValue((tagged_t)curr,*next); \
  curr = next; \
  next = temp; \
}

#define gc_Undo(curr,next) gc_Reverse(next,curr)

#define gc_Advance(curr,next) { \
  tagged_t *temp; \
  temp = TaggedToPointer(*curr); \
  *curr = gc_PutValue((tagged_t)next,*curr); \
  HeapDecr(curr); \
  next = TaggedToPointer(*curr); \
  *curr = gc_PutValue((tagged_t)temp,*curr); \
}

#define gc_TrailStart TaggedToPointer(w->segment_choice->trail_top)
#define gc_HeapStart (NodeGlobalTop(w->segment_choice))
#define gc_StackStart (NodeLocalTop(w->segment_choice))
#define gc_ChoiceStart (w->segment_choice)

#define gc_ReverseChoice(cp,prevcp,alt) { \
  try_node_t *m_alt = alt; \
  cp = prevcp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  prevcp = ChoiceCharOffset(cp,-alt->choice_offset); \
}

#define gc_UndoChoice(cp,prevcp,alt) { \
  try_node_t *m_alt = alt; \
  prevcp = cp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  cp = ChoiceCharOffset(cp,alt->choice_offset); \
}

/* =========================================================================== */

static CVOID__PROTO(shuntVariables);
static CVOID__PROTO(markTrail);
static CVOID__PROTO(markFrames, frame_t *frame, bcp_t l);
static CVOID__PROTO(markChoicepoints);
static CVOID__PROTO(markVariable, tagged_t *start);
static void updateRelocationChain(tagged_t *curr, tagged_t *dest);
static CVOID__PROTO(sweepTrail);
static CVOID__PROTO(sweepFrames, frame_t *frame, bcp_t l);
static CVOID__PROTO(sweepChoicepoints);
static CVOID__PROTO(compressHeap);

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
tagged_t *gc_trail_start;

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

static CVOID__PROTO(shuntVariables) {
  tagged_t *pt = w->trail_top;
  choice_t *cp = Gc_Aux_Node;
  choice_t *prevcp = w->choice;
  try_node_t *alt = fail_alt;
  intmach_t i;
  tagged_t *limit;
  frame_t *frame;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
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
    gc_ReverseChoice(cp,prevcp,alt);
  }

  while (ChoiceYounger(Gc_Aux_Node,cp)) {
    gc_UndoChoice(cp,prevcp,alt);
    limit = TaggedToPointer(cp->trail_top);
    pt = TaggedToPointer(prevcp->trail_top);
    while (TrailYounger(limit,pt)) {
      tagged_t v = *pt++;

      if (!gc_IsMarked(v)) {
        gc_UnmarkM(*TaggedToPointer(v));
      }
    }
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
        
    pt = cp->x+OffsetToArity(alt->choice_offset);
    while (pt!=cp->x) {
      --pt;
      gc_shuntVariable(*pt);
    }
  }
}

/* --------------------------------------------------------------------------- */
/* The Marking Phase **/

/* First mark all unbound/newly bound constraint variables,
   all old heap reference, all old stack reference.
   Must be done before any early reset is done.
*/
static CVOID__PROTO(markTrail) {
  tagged_t *tr = w->trail_top;
  tagged_t v;
  intmach_t wake_count = WakeCount();

  while (Cvas_Found!=atom_nil) { /* mark unbound CVAs */
    *tr = v = Cvas_Found;
    Cvas_Found = *TagpPtr(CVA,v);
    gc_UnmarkM(Cvas_Found);
    *TagpPtr(CVA,v) = v;
    markVariable(Arg, tr);
  }

  /* find_constraints(Arg,Gc_Choice_Start);
     markVariable(tr);
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
      markVariable(Arg, tr);
    }
  }

#if defined(SEGMENTED_GC)
  /* First mark all trailed old variables */
  tr = w->trail_top;
  while (TrailYounger(tr,Gc_Trail_Start)) {
    TrailDec(tr);
    tagged_t v = *tr; // (tr points to the popped element)
    tagged_t *p = TaggedToPointer(v);

    if (v!=0 && !gc_IsMarked(v) &&
        ((IsHeapVar(v) && !OffHeaptop(p,Gc_Heap_Start)) ||
         (IsStackVar(v) && !OffStacktop(p,Gc_Stack_Start)))) {
      gc_MarkM(*tr);                       /* so won't look at it again */
      if (IsHeapTerm(*p) && !gc_IsMarked(*p) &&
          OffHeaptop(TaggedToPointer(*p),Gc_Heap_Start)) {
        Gcgrey-= Total_Found;
        markVariable(Arg, p);
        Gcgrey+= Total_Found;
      }
    }
  }
#endif
}

/* A frame slot is marked iff it is in the chain of environments for
   one of the frozen execution states, regardless of contents. */
static CVOID__PROTO(markFrames, frame_t *frame, bcp_t l) {
  /* Mark frame chain */
  tagged_t *ev;

  while (OffStacktop(frame,Gc_Stack_Start)) {
    ev = (tagged_t *)StackCharOffset(frame,FrameSize(l));
    while (ev!=frame->x) {
      tagged_t v;
        
      StackDecr(ev);
      if (gc_IsMarked(v = *ev)) return;
      if (IsHeapTerm(v)) {
        markVariable(Arg, ev);
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
static CVOID__PROTO(markChoicepoints) {
  /* Mark choicepoints and corresponding chains of frames */
  /* and mark remaining trail entries */
  choice_t *cp = Gc_Aux_Node;
  tagged_t *tr = w->trail_top;
  tagged_t *limit;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    intmach_t n = cp->next_alt->choice_offset;
    intmach_t i = OffsetToArity(n);

    markFrames(Arg, cp->frame, cp->next_insn);
    while ((i--)>0) {
      if (IsHeapTerm(cp->x[i])) {
        markVariable(Arg, &cp->x[i]);
      }
    }
    cp = ChoiceCharOffset(cp, -n);

    /* mark goals and unbound constrained variables;
       reset unmarked bound variables
       between cp->trail_top and tr */

    limit = TaggedToPointer(cp->trail_top);
    while (TrailYounger(tr,limit)) {
      TrailDec(tr);
      tagged_t v = *tr; // (tr points to the popped element)
        
      if (v==(tagged_t)NULL || gc_IsMarked(v)) {
      } else if (!IsVar(v)) {
        markVariable(Arg, tr);
      }
#ifdef EARLY_RESET
      else if (TaggedIsCVA(v)) {
        if (!gc_IsMarked(*TagpPtr(CVA,v))) {
          *TagpPtr(CVA,v) = v;
          markVariable(Arg, tr);
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
          markVariable(Arg, tr);
      }
#endif
    }
  }
}

/* delete 0's from the trail */
CVOID__PROTO(compressTrail, bool_t from_gc) {
  tagged_t cv, *curr, *dest;
  tagged_t *limit;
  choice_t *cp = Gc_Aux_Node;
  choice_t *prevcp = w->choice;
  try_node_t *alt = fail_alt;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    gc_ReverseChoice(cp,prevcp,alt);
  }
  curr = dest = Gc_Trail_Start;
  while (ChoiceYounger(Gc_Aux_Node,cp)) {
    gc_UndoChoice(cp,prevcp,alt);
    limit = TaggedToPointer(cp->trail_top);
    while (TrailYounger(limit,curr)) {
      cv = *curr;
      curr++;
      if (cv != (tagged_t)0) {
        TrailPush(dest,cv);
      }
    }
    cp->trail_top -= limit-dest;
    if (from_gc) ChoiceptMarkPure(cp);
  }

  w->trail_top = dest;
}

static CVOID__PROTO(markVariable, tagged_t *start) {
  /* Mark a variable.
     start always points outside the heap.  Cyclic structs require this!
     (!gc_IsMarked(*start)) is true.
  */
  tagged_t *current, *next;

  current = start;
  next = TaggedToPointer(*current);
  gc_MarkF(*current);
  goto first_forward;
 forward:
  if (gc_IsMarked(*current)) goto backward;
  Total_Found++;
 first_forward:
  gc_MarkM(*current);
#if defined(SEGMENTED_GC)
  if (OffHeaptop(next,Gc_Heap_Start)) {
#endif
    switch(TagOf(*current)) {
    case SVA: /* No pointers from heap to stack */
      SERIOUS_FAULT("GC: stack variable in heap");
    case CVA: /* new 3-field CVA */
      /* N.B. there can be LST pointers to the second cell as well */
      if (!gc_IsForM(*(next+2)))
        {                     /* no marking in progress as CVA nor as LST */
          /* treat as 3-cell tuple */
          gc_MarkF(PreHeapRead(next));
          gc_MarkF(PreHeapRead(next));
          gc_Reverse(current,next);
          goto forward;
        }                     /* otherwise, just treat the value cell */
    case HVA:
      if (gc_IsForM(*next)) goto backward;
      gc_Reverse(current,next);
      goto forward;
    case LST:
      if (gc_IsFirst(*(next+1)) ||
          (gc_IsMarked(*next) &&
           gc_IsMarked(*(next+1))))
        goto backward;
      gc_MarkF(PreHeapRead(next));
      gc_Reverse(current,next);
      goto forward;
    case STR:
      if (gc_IsMarked(*next)) {
      } else if (*next&QMask) { /* box */
        intmach_t ar = LargeArity(*next);
        gc_MarkM(*next);
        Total_Found += ar+1;
      } else if (!gc_IsFirst(*(next+1))) {
        intmach_t n;
        for (n = Arity(*next); n>0; --n) {
          gc_MarkF(PreHeapRead(next));
        }
        gc_Reverse(current,next);
        goto forward;
      }
    default: /* all other treated as constants */
      ; /* goto backward */
    }
#if defined(SEGMENTED_GC)
  }
#endif
 backward:
  while (!gc_IsFirst(*current)) {
    /* internal cell */
    gc_Undo(current,next);
  }
  /* head of chain */
  gc_UnmarkF(*current);
  if (current!=start) {
    gc_Advance(current,next);
    goto forward;
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

static CVOID__PROTO(sweepTrail) {
  tagged_t *tr;
  tagged_t v, *p;

  tr = w->trail_top;
  while (TrailYounger(tr,Gc_Trail_Start)) {
    TrailDec(tr);
    v = *tr; // (tr points to the popped element)
    if (v==0) continue;
    gc_UnmarkM(*tr);
    p = TaggedToPointer(v);
#if defined(SEGMENTED_GC)
    if ((IsHeapVar(v) && !OffHeaptop(p,Gc_Heap_Start)) ||
        (IsStackVar(v) && !OffStacktop(p,Gc_Stack_Start))) {
      tagged_t *p1 = TaggedToPointer(*p);
      if (IsHeapTerm(*p) &&
          gc_IsMarked(*p) &&
          OffHeaptop(p1,Gc_Heap_Start)) {
        gc_UnmarkM(*p);
        intoRelocationChain(p1,p);
      }
    } else if (IsHeapTerm(v) && OffHeaptop(p,Gc_Heap_Start)) {
      intoRelocationChain(p,tr);
    }
#else
    if (IsHeapTerm(v)) {
      intoRelocationChain(p,tr);
    }
#endif
  }
}

static CVOID__PROTO(sweepFrames, frame_t *frame, bcp_t l) {
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
      if (IsHeapTerm(v)
#if defined(SEGMENTED_GC)
          && OffHeaptop(p,Gc_Heap_Start)
#endif
          ) {
        intoRelocationChain(p,ev);
      }
    }
    l = frame->next_insn;
    frame = frame->frame;
  }
}

static CVOID__PROTO(sweepChoicepoints) { /* sweep choicepoints and corresponding chains of frames */
  choice_t *cp = Gc_Aux_Node;

  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    intmach_t n = cp->next_alt->choice_offset;
    intmach_t i = OffsetToArity(n);

    sweepFrames(Arg, cp->frame, cp->next_insn);
    while ((i--)>0) {
      tagged_t v = cp->x[i];
      tagged_t *p = TaggedToPointer(v);
        
      gc_UnmarkM(cp->x[i]);
      if (IsHeapTerm(v)
#if defined(SEGMENTED_GC)
          && OffHeaptop(p,Gc_Heap_Start)
#endif
          ) {
        intoRelocationChain(p, &cp->x[i]);
      }
    }
    cp = ChoiceCharOffset(cp, -n);
  }
}

static CVOID__PROTO(compressHeap) {
  tagged_t cv;
  choice_t *cp = Gc_Aux_Node;
  tagged_t *curr = w->heap_top;
  tagged_t *dest = HeapOffset(Gc_Heap_Start,Total_Found);
  intmach_t garbage_words = 0;
  intmach_t extra;

  /* the upward phase */
  while (ChoiceYounger(cp,Gc_Choice_Start)) {
    cp->heap_top = dest;
    cp=ChoiceCont(cp);
        
    while (HeapYounger(curr,NodeGlobalTop(cp))) {
      cv = HeapPop(curr);
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
        HeapDecr(dest);
        if (gc_IsFirst(cv)) {
          updateRelocationChain(curr,dest);
          cv = *curr;
        }
        if (IsHeapTerm(cv)) {
          tagged_t *p = TaggedToPointer(cv);
                
          if (HeapYounger(curr,p)
#if defined(SEGMENTED_GC)
              && OffHeaptop(p,Gc_Heap_Start)
#endif
              ) {
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
  while (HeapYounger(w->heap_top,curr)) {
    cv = *curr;
    if (gc_IsMarked(cv)) {
      if (gc_IsFirst(cv)) {
        updateRelocationChain(curr,dest);
        cv = *curr;
      }
      gc_UnmarkM(cv);  /* M and F-flags off */
      {
        tagged_t *p = TaggedToPointer(cv);
        
        if (IsHeapTerm(cv) && HeapYounger(p,curr)) {              
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
      (void)HeapNext(dest);
    } else { /* skip a box---all garbage is boxed */
      curr += LargeArity(cv);
    }
    (void)HeapNext(curr);
  }
  w->heap_top = dest;
}


/**** The main garbage collection routine *****/

CVOID__PROTO(GarbageCollect) {
  /* The X registers have been saved already in an frame */
  ptrdiff_t hz;
#if 0
  ptrdiff_t hz, sz, cz, tz;
#endif
  flt64_t t1,t2;
  frame_t *newa;

  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " enters GarbageCollect\n", (intmach_t)Thread_Id);

  ComputeA(newa, w->choice);
  hz = HeapDifference(Heap_Start,w->heap_top); /* current heap size */
#if 0
  sz = StackDifference(Stack_Start,w->heap_top); /* current stack size */
  cz = ChoiceDifference(Choice_Start,w->heap_top); /*  choicep size */
  tz = TrailDifference(Trail_Start,w->heap_top); /* current trail size */
#endif
  if (current_gctrace != atom_off) {
    if (current_gctrace == atom_terse) {
      CVOID__CALL(print_string, Error_Stream_Ptr, "{GC}");
    } else {
      StreamPrintf(Error_Stream_Ptr, "\n{GC}  Heap GC started\n");
      StreamPrintf(Error_Stream_Ptr, "Heap:   from %p to %p (total size = %" PRIdm ")\n",
                   Heap_Start, 
                   Heap_End,
                   (intmach_t)HeapDifference(Heap_Start, Heap_End));
      StreamPrintf(Error_Stream_Ptr, "        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                   w->heap_top,  
                   (intmach_t)HeapDifference(Heap_Start, w->heap_top),
                   (intmach_t)HeapDifference(w->heap_top, Heap_End));
      StreamPrintf(Error_Stream_Ptr, "        GC start at %p\n", 
                   gc_HeapStart);

      StreamPrintf(Error_Stream_Ptr, "Stack:  from %p to %p (total size = %" PRIdm ")\n",
                   Stack_Start, 
                   Stack_End,
                   (intmach_t)StackDifference(Stack_Start, Stack_End));
      StreamPrintf(Error_Stream_Ptr, "        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                   w->local_top, 
                   (intmach_t)StackDifference(Stack_Start,w->local_top),
                   (intmach_t)StackDifference(w->local_top, Stack_End));
      StreamPrintf(Error_Stream_Ptr, "        GC start at %p\n", 
                   gc_StackStart);

      StreamPrintf(Error_Stream_Ptr, "Choice/Trail: from %p to %p (total size = %" PRIdm ")\n",
                   Choice_Start, 
                   Choice_End,
                   (intmach_t)ChoiceDifference(Choice_Start,Choice_End));
      StreamPrintf(Error_Stream_Ptr, "        Ch. top at %p (used = %" PRIdm ")\n", 
                   w->choice, 
                   (intmach_t)ChoiceDifference(Choice_Start, w->choice));
      StreamPrintf(Error_Stream_Ptr, "        Tr. top at %p (used = %" PRIdm ")\n", 
                   w->trail_top, 
                   (intmach_t)TrailDifference(Trail_Start,w->trail_top));
      StreamPrintf(Error_Stream_Ptr, "        Ch./Tr. free %" PRIdm "\n",
                   (intmach_t)ChoiceDifference(w->choice, w->trail_top));
    }
  }

  t1 = RunTickFunc();

#if defined(USE_GLOBAL_VARS)
  TrailPush(w->trail_top,GLOBAL_VARS_ROOT);
#endif
    
  /* push special registers on the trail stack */
  TrailPush(w->trail_top,Current_Debugger_State);

  Total_Found = 0;
  Gcgrey = 0;
  if (w->segment_choice == InitialNode) {
    Gc_Total_Grey = 0;
  }
  trail_gc(Arg); /* sets Gc_Aux_Node, gc_Choice_Start, Gc_Trail_Start */
  Gc_Aux_Node->local_top = newa;
  Gc_Aux_Node->heap_top = w->heap_top;
  Gc_Aux_Node->frame = w->frame;
  Gc_Aux_Node->next_insn = w->next_insn;
  Gc_Heap_Start = gc_HeapStart;
  Gc_Stack_Start = gc_StackStart;

  Cvas_Found = atom_nil;

  if (WakeCount()) {
    if (current_gctrace == atom_verbose) {
      StreamPrintf(Error_Stream_Ptr, "{GC}  Shunting disabled due to pending unifications\n");
    }
  } else {
    shuntVariables(Arg);
  }

  markTrail(Arg);
  markChoicepoints(Arg);
  compressTrail(Arg,TRUE);

  Gc_Total_Grey += Gcgrey;
  t1 = (t2 = RunTickFunc())-t1;
  if (current_gctrace == atom_verbose) {
    StreamPrintf(Error_Stream_Ptr, "        mark: %" PRIdm " cells marked in %.3f sec\n",
                 Total_Found,((flt64_t)(t1))/RunClockFreq(ciao_stats));
#if defined(SEGMENTED_GC)
    StreamPrintf(Error_Stream_Ptr, "        no more than %" PRIdm " garbage cells left\n",
                 Gcgrey);
#endif
  }

  sweepTrail(Arg);
  sweepChoicepoints(Arg);
  compressHeap(Arg);

  /* pop special regs from the trail stack */
  TrailDec(w->trail_top);
  Current_Debugger_State = *(w->trail_top); // (w->trail_top points to the popped element)
#if defined(USE_GLOBAL_VARS)
  TrailDec(w->trail_top);
  GLOBAL_VARS_ROOT = *(w->trail_top); // (w->trail_top points to the popped element)
#endif
    
  SetShadowregs(w->choice);     /* shadow regs may have changed */
                                /* statistics */
  t2 = RunTickFunc()-t2;
  ciao_stats.gc_tick   += t1+t2;
  ciao_stats.starttick += t1+t2;
  ciao_stats.lasttick  += t1+t2;
  ciao_stats.gc_count++;
  ciao_stats.gc_acc+= hz-HeapDifference(Heap_Start,w->heap_top);
  if (current_gctrace==atom_verbose) {
    StreamPrintf(Error_Stream_Ptr, "        Heap: %" PRIdm " cells reclaimed in %.3f sec\n",
                 (intmach_t)(hz-HeapDifference(Heap_Start,w->heap_top)),
                 ((flt64_t)(t2))/RunClockFreq(ciao_stats));
    StreamPrintf(Error_Stream_Ptr, "Heap:   from %p to %p (total size = %" PRIdm ")\n",
                 Heap_Start, 
                 Heap_End,
                 (intmach_t)HeapDifference(Heap_Start, Heap_End));
    StreamPrintf(Error_Stream_Ptr, "        top at %p (used = %" PRIdm ", free = %" PRIdm ")\n",
                 w->heap_top,  
                 (intmach_t)HeapDifference(Heap_Start, w->heap_top),
                 (intmach_t)HeapDifference(w->heap_top, Heap_End));
    StreamPrintf(Error_Stream_Ptr, "        GC start at %p\n", 
                 gc_HeapStart);

    StreamPrintf(Error_Stream_Ptr, "        Total: %" PRIdm " cells reclaimed in %" PRIdm " gc's\n",
                 ciao_stats.gc_acc,ciao_stats.gc_count);
    StreamPrintf(Error_Stream_Ptr, "        GC time = %.6f  Total = %.6f\n\n",
                 ((flt64_t)(t1+t2))/RunClockFreq(ciao_stats),
                 ((flt64_t)ciao_stats.gc_tick)/RunClockFreq(ciao_stats));
  }
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
    i=OffsetToArity(b->next_alt->choice_offset);
    if (i>0) {
      tagged_t *t = (tagged_t *)w->previous_choice;
      do {
        ChoicePush(t,X(--i));
      } while (i>0);
    }
    if (ChoiceYounger(ChoiceOffset(b,CHOICEPAD),w->trail_top)) {
      choice_overflow(Arg,CHOICEPAD);
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


/* Set w->segment_choice to most recent choicept which is marked as pure. */
static CVOID__PROTO(calculate_segment_choice) {
  choice_t *n;
  w->segment_choice = NULL;
  for (n=w->choice;
       w->segment_choice==NULL;
       n=ChoiceCont(n)) {
    if (ChoiceptTestPure(n)) {
      w->segment_choice = n;
    }
  }
}

#if defined(ANDPARALLEL)
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor, tagged_t *newh, bool_t remote_reloc, worker_t *remote_worker);
#else
CVOID__PROTO(heap_overflow_adjust_wam, intmach_t reloc_factor, tagged_t *newh);
#endif

#if defined(ANDPARALLEL)
#define IsHeapTermAndNeedsReloc(T) (IsHeapTerm((T)) && HeapTermNeedsReloc((T)))
#else
#define IsHeapTermAndNeedsReloc(T) IsHeapTerm((T))
#endif

#if defined(ANDPARALLEL)
/* Needs reloc if `remote_reloc` coincides with `is_remote_HeapTerm` for `T` */
#define HeapTermNeedsReloc(T) (remote_reloc == is_remote_HeapTerm((T),w,rem_w))
#endif

#if defined(ANDPARALLEL)
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
#endif

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

#define RelocPtr(P,Offset) ((typeof(P))((char *)(P)+(Offset)))
#define AssignRelocPtr(P,Offset) (P) = RelocPtr((P), (Offset))

#if defined(ANDPARALLEL)
#define AssignRelocPtrNotRemote(P,Offset) if (!remote_reloc) AssignRelocPtr((P),(Offset)) 
#else
#define AssignRelocPtrNotRemote(P,Offset) AssignRelocPtr((P),(Offset)) 
#endif

/* Here when w->choice and w->trail_top are within CHOICEPAD from each other. */
CVOID__PROTO(choice_overflow, intmach_t pad) {
  inttime_t tick0;
  tagged_t *choice_top;
  try_node_t *next_alt;

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

  tick0 = RunTickFunc();

  if (!(next_alt = w->choice->next_alt)) { /* ensure A', P' exist */
    w->choice->next_alt = w->next_alt;
    w->choice->local_top = w->local_top;
  }

  if (pad<0) {
    pad = -pad; /* in compile_term: disable trail_gc */
  } else {
    calculate_segment_choice(Arg);
    trail_gc(Arg);
    compressTrail(Arg,FALSE);
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
          ChoicePush(x,ChoiceNext(tr));
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

  ciao_stats.ss_control++;
  tick0 = RunTickFunc()-tick0;
  ciao_stats.starttick += tick0;
  ciao_stats.lasttick += tick0;
  ciao_stats.ss_tick += tick0;

#if defined(ANDPARALLEL)
  CVOID__CALL(resume_all);
  Release_slock(stackset_expansion_l);
#endif
}

/* Here when w->local_top and Stack_End are within STACKAD from each other. */
CVOID__PROTO(stack_overflow) {
  intmach_t count, reloc_factor;
  tagged_t *newh;
  flt64_t tick0 = RunTickFunc();
  
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

  count = 2*StackDifference(Stack_Start,Stack_End);
  newh = checkrealloc_ARRAY(tagged_t,
                            count/2,
                            count,
                            Stack_Start);
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing STACK from %p to %p\n", (intmach_t)Thread_Id, Stack_Start, newh);

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */

  stack_overflow_adjust_wam(w,reloc_factor);

  /* Final adjustments */
  Stack_Start = newh;           /* new low bound */
  Stack_End = newh+count;       /* new high bound */
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
  ciao_stats.ss_local++;
  tick0 = RunTickFunc()-tick0;
  ciao_stats.starttick += tick0;
  ciao_stats.lasttick += tick0;
  ciao_stats.ss_tick += tick0;

#if defined(ANDPARALLEL)
  CVOID__CALL(resume_all);
  Release_slock(stackset_expansion_l);
#endif
}

CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor) {
  choice_t *n, *n2;
  tagged_t t1;
  tagged_t *pt1;

  if (reloc_factor!=0) {
    choice_t *aux_node;
    frame_t *frame;
    intmach_t i;
    
    aux_node = ChoiceCharOffset(w->choice,ArityToOffset(0));
    aux_node->next_alt = fail_alt;
    aux_node->frame = RelocPtr(w->frame, reloc_factor);
    aux_node->next_insn = w->next_insn;
    aux_node->local_top = RelocPtr(w->local_top, reloc_factor);

    /* relocate pointers in trail */
    pt1 = Trail_Start;
    while (TrailYounger(w->trail_top,pt1)) {
      t1 = *pt1;
      pt1++;
      if (TaggedIsSVA(t1)) {
        *(pt1-1) += reloc_factor;
      }
    }

    /* relocate pointers in choice&env stks */
    for (n=aux_node; n!=InitialNode; n=n2){
      n2 = ChoiceCont(n);
      //Tabling --> How to translate?
      AssignRelocPtr(n2->local_top, reloc_factor);
      AssignRelocPtr(n2->frame, reloc_factor);

      for (pt1=n->x; pt1!=(tagged_t *)n2;) {
        t1 = ChoicePrev(pt1);
        if (TaggedIsSVA(t1)) {
          *(pt1-1) += reloc_factor;
        }
      }
      
      i = FrameSize(n->next_insn);
      frame = n->frame;
      while (frame >= (frame_t*) NodeLocalTop(n2)) {
        pt1 = (tagged_t *)StackCharOffset(frame,i);
        while (pt1!=frame->x){
          t1 = *(--pt1);
          if (TaggedIsSVA(t1)) {
            *pt1 += reloc_factor;
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
}


static bool_t gcexplicit = FALSE;       /* Shared, no locked --- global flag */

CBOOL__PROTO(gc_start) {
  gcexplicit = TRUE;
  heap_overflow(Arg,CALLPAD);
  return TRUE;
}

/* Here when w->heap_top and Heap_End are within CALLPAD from each other. */
CVOID__PROTO(heap_overflow, intmach_t pad)
{
  tagged_t *oldh = w->heap_top;
  tagged_t *newh = w->heap_top;
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
  calculate_segment_choice(Arg);
  if (gc ||
      (current_gcmode != atom_off &&
       HeapCharDifference(Heap_Start,oldh) >= GetSmall(current_gcmargin)*1024)) {
    GarbageCollect(Arg);
    newh = w->heap_top;
    lowboundh = newh-Gc_Total_Grey;
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*1024 ||
         HeapYounger(HeapCharOffset(newh,2*pad),Heap_End)) &&
        !(HeapCharDifference(lowboundh,oldh) < GetSmall(current_gcmargin)*1024 ||
          HeapYounger(HeapCharOffset(lowboundh,2*pad),Heap_End))) {
      /* garbage collect the entire heap */
      w->segment_choice = InitialNode;
      GarbageCollect(Arg);
      newh = w->heap_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*1024) ||
      HeapYounger(HeapCharOffset(newh,2*pad),Heap_End)) {
    flt64_t tick0 = RunTickFunc();
    /* increase heapsize */
    intmach_t mincount, newcount, oldcount, reloc_factor;
    tagged_t *newh;
    
    intmach_t wake_count = WakeCount();
    
    ComputeA(w->local_top,w->choice);
    
    mincount = 2*pad - HeapCharDifference(w->heap_top,Heap_End);
    oldcount = HeapCharDifference(Heap_Start,Heap_End);
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);

    newh = (tagged_t *)checkrealloc_ARRAY(char, oldcount, newcount, Heap_Start);

    DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is reallocing HEAP from %p to %p\n", (intmach_t)Thread_Id, Heap_Start, newh);

    reloc_factor = (char *)newh - (char *)Heap_Start;
    // fprintf(stderr, "reloc_factor=%x\n", reloc_factor);
    
    /* AA, HH and TR are free pointers;  BB is last used word. */

#if defined(ANDPARALLEL)
    /* Adjust remote pointers in other agents */
    heap_overflow_adjust_wam(w,reloc_factor,newh,FALSE,NULL);
    for (aux = Next_Wam_Of(w); aux != w; aux = Next_Wam_Of(aux)) {
      heap_overflow_adjust_wam(aux,reloc_factor,newh,TRUE,w);
    }
#else
    heap_overflow_adjust_wam(w,reloc_factor,newh);
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

    ciao_stats.ss_global++;
    tick0 = RunTickFunc()-tick0;
    ciao_stats.starttick += tick0;
    ciao_stats.lasttick += tick0;
    ciao_stats.ss_tick += tick0;
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
  choice_t *n, *n2 = NULL;
  tagged_t t1;
  tagged_t *pt1;

  if (reloc_factor!=0) {
    choice_t *aux_node;
    frame_t *frame;
    intmach_t i;

    aux_node = ChoiceCharOffset(w->choice,ArityToOffset(0));
    aux_node->next_alt = fail_alt;
    aux_node->frame = w->frame;
    aux_node->next_insn = w->next_insn;
    aux_node->heap_top = w->heap_top;
    aux_node->local_top = w->local_top; /* segfault patch -- jf */

    /* relocate pointers in global stk */
#if defined(ANDPARALLEL)
    pt1 = remote_reloc ? Heap_Start : newh;
#else
    pt1 = newh;
#endif

    AssignRelocPtrNotRemote(w->heap_top, reloc_factor);
    while (HeapYounger(w->heap_top,pt1)) {
      t1 = HeapNext(pt1);
      if (t1&QMask) {
        pt1 += LargeArity(t1);
      } else if (IsHeapTermAndNeedsReloc(t1)) {
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
      t1 = *pt1;
      pt1++;
      if (IsHeapTermAndNeedsReloc(t1)) {
        *(pt1-1) += reloc_factor;
      }
    }
    TrailDec(w->trail_top);
    Current_Debugger_State = *(w->trail_top); // (w->trail_top points to the popped element)

#if defined(ANDPARALLEL)
    /* relocate pointers in goal list */
    handler_entry_t *gle = Goal_List_Start;
    tagged_t x1 = (tagged_t)NULL;
    if (gle != NULL) {
      if (HeapTermNeedsReloc(gle->handler)) {
        DerefArg(x1,gle->handler,1);
        ((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
        gle->handler += reloc_factor;
      }
      while (gle != Goal_List_Top) {
        gle = gle->next;
        if (HeapTermNeedsReloc(gle->handler)) {
          DerefArg(x1,gle->handler,1);
          ((par_handler_t *) TermToPointer(x1))->goal += reloc_factor;
          gle->handler += reloc_factor;
        }
      }
    }

    /* relocate pointers in event queue */
    event_entry_t *eqe = Event_Queue_Start;
    if (eqe != NULL) {
      if (HeapTermNeedsReloc(eqe->handler)) {
        eqe->handler += reloc_factor;
      }
      while (eqe != Event_Queue_Top) {
        eqe = eqe->next;
        if (HeapTermNeedsReloc(eqe->handler)) {
          eqe->handler += reloc_factor;
        }
      }
    }
#endif

    /* relocate pointers in choice&env stks */
    for (n=aux_node; n!=InitialNode && n->next_alt!=NULL; n=n2) {
      if (n->next_alt != NULL) {
        n2 = ChoiceCont(n);
        for (pt1=n->x; pt1!=(tagged_t *)n2;) {
          t1 = ChoicePrev(pt1);
          if (IsHeapTermAndNeedsReloc(t1)) {
            *(pt1-1) += reloc_factor;
          }
        }
        i = FrameSize(n->next_insn);
        frame = n->frame;
        while ((frame >= (frame_t*) NodeLocalTop(n2)) && frame->next_insn != NULL) {
          pt1 = (tagged_t *)StackCharOffset(frame,i);
          while (pt1!=frame->x) {
            t1 = *(--pt1);
            if (IsHeapTermAndNeedsReloc(t1)) {
              *pt1 += reloc_factor;
            }
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
}

/* Tidy new half of trail exhaustively. */
CVOID__PROTO(trail_gc) {
  tagged_t *tr = w->trail_top;
  choice_t *b = w->choice;
  intmach_t wake_count = WakeCount();
  tagged_t heap_last = Tagp(HVA,HeapOffset(Heap_End,-1));
  /*extern choice_t *gc_aux_node;*/ /* Now in a register */
  /*extern choice_t *gc_choice_start;*/ /* No in a register */
  /*extern tagged_t *gc_trail_start;*/ /* now in a register */

  Gc_Aux_Node = ChoiceCharOffset(b,ArityToOffset(0));
  Gc_Aux_Node->next_alt = fail_alt;
  Gc_Aux_Node->trail_top = tr;
  Gc_Choice_Start = w->segment_choice;
  Gc_Trail_Start = TaggedToPointer(w->segment_choice->trail_top);
  
  if (current_gctrace == atom_verbose) {
    StreamPrintf(Error_Stream_Ptr, "{GC}  Trail GC started\n");
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
      tagged_t t1 /*, *pt */ ; /* unused */

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
  current_gcmode = atom_on;
  current_gctrace = atom_off;
  current_gcmargin = MakeSmall(500); /* Quintus has 1024 */
}

