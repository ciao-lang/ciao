/*
 *  term_basic.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <stdio.h>
#include <stdlib.h>

#include <ciao/eng.h>
#include <ciao/basiccontrol.h>
#include <ciao/dynamic_rt.h>
#include <ciao/dtoa_ryu.h>
#include <ciao/eng_start.h>
#include <ciao/eng_registry.h>
#include <ciao/eng_gc.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/io_basic.h>
#include <ciao/eng_bignum.h>

/* Unify with occurs-check, using inline checks (var-nonvar cases) */
#define UNIFY_OC_INLINE 1
/* Enable tail optimization in cyclic_term */
#define CYCLIC_TERM_TAIL_OPTIM 1

/* local declarations */

#define GCTEST(Pad) { \
    if (HeapCharDifference(w->heap_top,Heap_End) < (Pad)*sizeof(tagged_t)) \
      heap_overflow(Arg,2*((Pad)*sizeof(tagged_t))); \
    if (ChoiceDifference(w->choice,w->trail_top) < (Pad)) \
      choice_overflow(Arg,2*(Pad)*sizeof(tagged_t),TRUE); \
  }

static CVOID__PROTO(copy_it, tagged_t *loc);
static CVOID__PROTO(copy_it_nat, tagged_t *loc);

/* -------------------------------------------------------------------
   FRAME MANIPULATIONS
   ----------------------------------------------------------------------*/

CVOID__PROTO(pop_frame) {
  frame_t *e;
  SetE(w->frame);
  intmach_t arity = FrameSizeToCount(FrameSize(w->next_insn));
  for(intmach_t i=0; i<arity; i++) {
    X(i) = Y(i);
  }
  w->local_top = E;
  w->frame = E->frame;
  w->next_insn = E->next_insn;
}

/* this assumes w->local_top has been computed! */
CVOID__PROTO(push_frame, int arity) {
  frame_t *e;
  SetE(w->local_top);
  E->next_insn = w->next_insn;
  E->frame = w->frame;
  w->frame = E;
  w->next_insn = CONTCODE(arity);
  w->local_top = (frame_t *)Offset(E,EToY0+arity);
  for(intmach_t i=0; i<arity; i++) {
    Y(i) = X(i);
  }
}

/* --------------------------------------------------------------------------- */
/* copy term */

/* copy_term(?Old,?New):
 * Algorithm:
 * If Old is a plain variable, just return.
 * Otherwise allocate a frame containing Old and New.
 * The frame slot for Old will be progressively replaced by a copy of Old.
 * Thus all relevant parts of the old and new structures are reachable from
 * the frame slot, should GC occur.
 * While copying, all old variables encountered are bound to their copies.
 * This requires a choicepoint.
 * Finally untrail but trail any new CVA:s, deallocate frame & choicept,
 * and unify copy with New.
 */
CBOOL__PROTO(prolog_copy_term) {
  tagged_t t1, t2, *pt1, *pt2;

  t1 = X(0);
  SwitchOnVar(t1,t2,
              { return TRUE; },
              {},
              { return TRUE; },
              {});

  X(0) = t1;
  push_choicept(Arg,fail_alt);  /* try, arity=0 */
  push_frame(Arg,2);            /* allocate, size=2 */

  copy_it(Arg,&w->frame->x[0]); /* do the copying */

  pt1 = pt2 = TrailTopUnmark(w->choice->trail_top); /* untrail */
  while (TrailYounger(w->trail_top,pt2)) {
    t1 = *pt2; /* old var */
    pt2++;
    *TaggedToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_frame(Arg);
  pop_choicept(Arg);            /* trust */
  CBOOL__LASTUNIFY(X(0),X(1));
}

static CVOID__PROTO(copy_it, tagged_t *loc) {
  tagged_t t1, t2, *pt1, *pt2;
  int i;
  int term_so_far;              /* size of new heap before copying subterms */

 start:
  RefHeap(t1,loc);
  SwitchOnHeapVar(t1,t2,{goto copy_hva;},{goto copy_cva;},{});

  if (TaggedIsATM(t1) || IsNumber(t1)) {                           /* NUM, ATM */
    *loc = t1;
    return;
  } else if (t1 & TagBitFunctor) {                                 /* STR */
    pt1 = TagpPtr(STR,t1);
    pt2 = w->heap_top;
    *loc = Tagp(STR,pt2);
    t2 = HeapNext(pt1), HeapPush(pt2,t2);
    for (i=Arity(t2); i>0; --i) {
      RefHeapNext(t1,pt1);
      HeapPush(pt2,t1);
    }
    w->heap_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    for (i=Arity(t2); i>1; --i)
      copy_it(Arg,HeapOffset(TopOfOldHeap,term_so_far-i));
  } else {                                                         /* LST */
    pt1 = TagpPtr(LST,t1);
    pt2 = w->heap_top;
    *loc = Tagp(LST,pt2);
  copy_2_cells:
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    w->heap_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    copy_it(Arg,HeapOffset(w->heap_top,-2));
  }
  GCTEST(CHOICEPAD);
  loc = HeapOffset(TopOfOldHeap,term_so_far-1);
  goto start;

 copy_hva:
  if (OldHVA(t1)) { /* HVA */
    PreLoadHVA(*loc,loc);
    t2 = Tagp(HVA,loc);
    BindHVA(t1,t2);
  } else *loc = t1;
  return;

 copy_cva:
  if (OldCVA(t1)) { /* new 3-field CVA */
    pt1 = TaggedToGoal(t1);
    pt2 = w->heap_top;
    LoadCVA(t2,pt2);
    BindCVANoWake(t1,t2);
    *loc = t2;
    goto copy_2_cells;
  } else *loc = t1;
  return;
}

/* Do not copy attributes */
CBOOL__PROTO(prolog_copy_term_nat)
{
  tagged_t t1, t2, *pt1, *pt2;

  t1 = X(0);
  SwitchOnVar(t1,t2,
              { return TRUE; },
              { return TRUE; },
              { return TRUE; },
              {});

  X(0) = t1;
  push_choicept(Arg,fail_alt);  /* try, arity=0 */
  push_frame(Arg,2);            /* allocate, size=2 */

  copy_it_nat(Arg,&w->frame->x[0]); /* do the copying */

  pt1 = pt2 = TrailTopUnmark(w->choice->trail_top); /* untrail */
  while (TrailYounger(w->trail_top,pt2)) {
    t1 = *pt2; /* old var */
    pt2++;
    *TaggedToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_frame(Arg);
  pop_choicept(Arg);            /* trust */
  CBOOL__LASTUNIFY(X(0),X(1));
}


static CVOID__PROTO(copy_it_nat, tagged_t *loc)
{
  tagged_t t1, t2, *pt1, *pt2;
  int i;
  int term_so_far;              /* size of new heap before copying subterms */

 start:
  RefHeap(t1,loc);
  SwitchOnHeapVar(t1,t2,{goto copy_hva;},{goto skip_cva;},{});

  if (TaggedIsATM(t1) || IsNumber(t1)) {                           /* NUM, ATM */
    *loc = t1;
    return;
  } else if (t1 & TagBitFunctor) {                                 /* STR */
    pt1 = TagpPtr(STR,t1);
    pt2 = w->heap_top;
    *loc = Tagp(STR,pt2);
    t2 = HeapNext(pt1), HeapPush(pt2,t2);
    for (i=Arity(t2); i>0; --i) {
      RefHeapNext(t1,pt1);
      HeapPush(pt2,t1);
    }
    w->heap_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    for (i=Arity(t2); i>1; --i)
      copy_it_nat(Arg,HeapOffset(TopOfOldHeap,term_so_far-i));
  } else {                                                         /* LST */
    pt1 = TagpPtr(LST,t1);
    pt2 = w->heap_top;
    *loc = Tagp(LST,pt2);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    w->heap_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    copy_it_nat(Arg,HeapOffset(w->heap_top,-2));
  }
  GCTEST(CHOICEPAD);
  loc = HeapOffset(TopOfOldHeap,term_so_far-1);
  goto start;

 copy_hva:
  if (OldHVA(t1)) { /* HVA */
    PreLoadHVA(*loc,loc);
    t2 = Tagp(HVA,loc);
    BindHVA(t1,t2);
  } else *loc = t1;
  return;

 skip_cva:
  if (OldCVA(t1)) {
    /* This code is equivalent to taking out the attribute;
       xref bu1_detach_attribute() */
    PreLoadHVA(*loc,loc);
    t2 = Tagp(HVA,loc);
    BindCVANoWake(t1,t2);
  } else  *loc = t1;
  return;
}

#if defined(SAFE_CROSS_COPY)

CBOOL__PROTO(prolog_copy_term); /* term_basic.c */

/* Copy a term in a remote worker to the local worker.  Returns the local
   term pointer.  It has (nontermination) problems when copying structures
   with self references. */

CFUN__PROTO(cross_copy_term, tagged_t, tagged_t remote_term)
{
  X(0) = remote_term;
  LoadHVA(X(1), w->heap_top);
#if defined(DEBUG_TRACE)
  if (!prolog_copy_term(Arg))
    fprintf(stderr, "Could not copy term in cross_copy_term!!!!\n");
#else
  prolog_copy_term(Arg);
#endif
  return X(1);
}

#endif

/* --------------------------------------------------------------------------- */

/* c_cyclic_term: checks that the term t is cyclic.

   Calls c_cyclic_ptr with the reference of a tagged word, using GC
   bits as marks for already visited nodes. If the tagged word is a
   compound term:

     - If GC bit is on, exit. The term is cyclic.
     - Otherwise, set the GC bit, call recursively, and unset the GC
       bit.

   There are two implementations
     - One fully recursive (macro CYCLIC_TERM_TAIL_OPTIM undefined)
     - One with tail optimization  (macro CYCLIC_TERM_TAIL_OPTIM 
       defined), that avoid  using C stack for the last argument.

   In case of the tail optimized version the GC bits of the rightmost
   branch are removed together.

*/

static CBOOL__PROTO(c_cyclic_ptr, tagged_t *pt);
#if defined(CYCLIC_TERM_TAIL_OPTIM)
static CVOID__PROTO(unmark_rightmost_branch, tagged_t *ptr);
#endif

static CBOOL__PROTO(c_cyclic_term, tagged_t t) {
  tagged_t *ptr;
  int i;

  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    return c_cyclic_ptr(Arg, TaggedToPointer(t));
  case LST: 
    ptr = TagpPtr(LST,t);
    i = 2;
    goto args;
  case STR:
    ptr = TagpPtr(STR,t);
    t = *ptr;
    if (t&QMask) return FALSE; /* large number */
    i = Arity(t);
    ptr++;
    goto args;
  default:
    return FALSE;
  }
 args:
  for (; i >= 1; i--, ptr++) {
    if (c_cyclic_ptr(Arg, ptr)) return TRUE;
  }
  return FALSE;
}

CBOOL__PROTO(c_cyclic_ptr, tagged_t *pt) {
  tagged_t t;
  tagged_t *ptr, *ptr1;
  int i;

  ptr = pt;
 start:
  t = *ptr;
  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    ptr = TaggedToPointer(t);
    if (*ptr == t) goto acyclic; /* free variable */
    goto start;
  case LST: 
    if (gc_IsMarked(t)) goto cyclic;
    ptr1 = TagpPtr(LST,t);
    i = 2;
    goto args;
  case STR:
    if (gc_IsMarked(t)) goto cyclic;
    ptr1 = TagpPtr(STR,t);
    t = *ptr1;
    if (t&QMask) goto acyclic;  /* large number */
    i = Arity(t);
    ptr1++;
    goto args;
  default:
    goto acyclic;
  }
 args:
  gc_MarkM(*ptr); /* mark cell */
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  for (; i > 1; i--, ptr1++) {
    if (c_cyclic_ptr(Arg, ptr1)) goto cyclic;
  }
  ptr = ptr1;
  goto start;
#else
  for (; i >= 1; i--, ptr1++) {
    if (c_cyclic_ptr(Arg, ptr1)) { gc_UnmarkM(*ptr); goto cyclic; }
  }
  /* acyclic */
  gc_UnmarkM(*ptr);
  goto acyclic;
#endif

 acyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  unmark_rightmost_branch(Arg, pt); /* ensure marks are removed */
#endif
  return FALSE;
 cyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  unmark_rightmost_branch(Arg, pt); /* ensure marks are removed */
#endif
  return TRUE;
}

#if defined(CYCLIC_TERM_TAIL_OPTIM)
CVOID__PROTO(unmark_rightmost_branch, tagged_t *ptr) {
  tagged_t t;

 start:
  t = *ptr;
  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    ptr = TaggedToPointer(t);
    if (*ptr == t) return;
    goto start;
  case LST: 
    if (!gc_IsMarked(t)) return;
    gc_UnmarkM(*ptr);
    ptr = TagpPtr(LST,*ptr);
    ptr++;
    goto start;
  case STR:
    if (!gc_IsMarked(t)) return;
    gc_UnmarkM(*ptr);
    ptr = TagpPtr(STR,*ptr);
    ptr += Arity(*ptr);
    goto start;
  default:
    return;
  }
}
#endif

CBOOL__PROTO(prolog_cyclic_term)
{
  return c_cyclic_term(Arg, X(0));
}

/* unifiable(Term1, Term2, Unifier)
 *
 * Algortihm: tries unify Term1 and Term2, then untrail the
 * unification while saving the unifier into a prolog list which is
 * eventually unified with Unifier. The function treats attributed 
 * variable as classical ones.
 */
CBOOL__PROTO(prolog_unifiable)
{
  tagged_t t, t1;
  tagged_t * limit, *tr;

  /* Forces trailing of bindings and saves the top of the trail. */
  push_choicept(Arg,fail_alt);  
  /* Saves the arguments in case of GC. */
  push_frame(Arg,3); /* TODO: frame args used? */

  CBOOL__UNIFY(X(0), X(1));

  /* Makes sure there is enough place in the heap to construct the
     unfiers list. */
  GCTEST((w->trail_top - TrailTopUnmark(w->choice->trail_top)) * 5);

  t = atom_nil;
  tr = w->trail_top;
  limit = TrailTopUnmark(w->choice->trail_top);
   
  while (TrailYounger(tr, limit)) {
    TrailDec(tr);
    t1 = *tr; // (tr points to the popped element)

    HeapPush(w->heap_top, SetArity(atom_equal, 2));
    HeapPush(w->heap_top, t1);
    HeapPush(w->heap_top, *TaggedToPointer(t1));
    HeapPush(w->heap_top, Tagp(STR, HeapOffset(w->heap_top, -3)));
    HeapPush(w->heap_top, t);
    t = Tagp(LST, HeapOffset(w->heap_top, -2));

    *TaggedToPointer(t1) = t1;
  }

  /* Ignores possible wakes caused by unification of attributed
     variables */ 
  if (TestEvent()) UnsetEvent(); /* TODO: check */
  
  w->trail_top = limit;
  pop_frame(Arg);
  pop_choicept(Arg);    

  CBOOL__LASTUNIFY(X(2), t);
}  

/* ------------------------------------------------------------------------- */
/* Deref variable v occurs in term x */
/* (needed for unifyOC) */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1);

static CBOOL__PROTO(var_occurs_args_aux, 
                    tagged_t v,
                    int arity,
                    tagged_t *pt1,
                    tagged_t *x1) {
  tagged_t 
    t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && var_occurs(Arg,v,t1)) return TRUE;
    (void)HeapNext(pt1);
  }
  *x1 = t1;
  return FALSE;
}

static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1) {
  tagged_t u, t1;

 in:
  u=x1;

  SwitchOnVar(u,t1,
              { goto var; },
              { goto var; },
              { goto var; },
              { goto non_var; });

 non_var:
  if (TaggedIsATM(u)) goto lose;
  if (TaggedIsSmall(u)) goto lose;
  if (TaggedIsLST(u)) {
    if (!var_occurs_args_aux(Arg,v,2,TaggedToCar(u),&x1))
      goto in;
    else
      goto win;
  } else { /* structure. */
    t1=TaggedToHeadfunctor(u);
    if (t1&QMask) { /* large number */
          goto lose;
    } if (!var_occurs_args_aux(Arg,v,Arity(t1),TaggedToArg(u,1),&x1)) {
      goto in;
    } else {
      goto win;
    }
  }

 var:
  if (v == u) goto win; else goto lose;

 win:
  return TRUE;
 lose:
  return FALSE;
}
#endif

/* ------------------------------------------------------------------------- */
/* Unify with occurs-check */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(cunifyOC_args_aux,
                    int arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2);
static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2);

/* Unify the argument lists of two compund terms. (with occurs-check)
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
CBOOL__PROTO(cunifyOC_args, 
             int arity,
             tagged_t *pt1,
             tagged_t *pt2) {
  tagged_t x1, x2;
  return (cunifyOC_args_aux(Arg,arity,pt1,pt2,&x1,&x2) && cunifyOC_aux(Arg,x1,x2));
}

static CBOOL__PROTO(cunifyOC_args_aux, 
                    int arity,
                    tagged_t *pt1,
                    tagged_t *pt2,
                    tagged_t *x1,
                    tagged_t *x2) {
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;

  VALUETRAIL__TEST_OVERFLOW(2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (t1 != t2) {
      DerefSwitch0(t1,{ goto noforward; });
      DerefSwitch0(t2,{ goto noforward; });
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* NOTE: do forward args from pt2 to pt1 */ 
      noforward:
        if (arity>1 && !cunifyOC_aux(Arg,t1,t2))
          return FALSE;
      } else if (t1 != t2) {
        return FALSE;
      }
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1;
  *x2 = t2;

  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
  return TRUE;
}
#endif 

/* Unify two terms. (with occurs-checks)
 * x1 - first term
 * x2 - second term
 */

CBOOL__PROTO(cunifyOC, tagged_t x1, tagged_t x2) {
#if defined(UNIFY_OC_INLINE)
  /* Use a recursive version of Robinson's 1965 unification algorithm
     with inline occurs-check */
  CBOOL__LASTCALL(cunifyOC_aux,x1,x2);
#else
  /* Otherwise, check cyclic later. This may be less efficient than
     the first algorithm depending on cost of cyclic term checks,
     e.g., f(X,X,X)=f(...,...,...) redoes work for each arg */
  CBOOL__UNIFY(x1,x2);
  if (CBOOL__SUCCEED(c_cyclic_term, x1)) return FALSE;
  CBOOL__PROCEED;
#endif
}

#if defined(UNIFY_OC_INLINE)
#define OccurCheck(U,V,OCCUR) \
  { if (CBOOL__SUCCEED(var_occurs, (U), (V))) { OCCUR; } }

static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2) {
  tagged_t u, v, t1;

 in:
  u=x1, v=x2;

  SwitchOnVar(u,t1,
              {goto u_is_hva;},
              {goto u_is_cva;},
              {goto u_is_sva;},
              {});

                                /* one non variable */
  SwitchOnVar(v,t1,
              { OccurCheck(v,u,{goto lose;}); BindHVA(v,u); goto win; },
              { OccurCheck(v,u,{goto lose;}); BindCVA(v,u); goto win; },
              { OccurCheck(v,u,{goto lose;}); BindSVA(v,u); goto win; },
              {});

                                /* two non variables */
  if (!(v ^= u)) {              /* are they equal? */
    goto win;
  } else if (v>=QMask) {                /* not the same type? */
    goto lose;
  } else if (!(u & TagBitComplex)) { /* atomic? (& not LNUM)*/
    goto lose;
  } else if (!(u & TagBitFunctor)) { /* list? */
    v ^= u;                     /* restore v */
    if (cunifyOC_args_aux(Arg,2,TaggedToCar(u),TaggedToCar(v),&x1,&x2)) {
      goto in;
    } else {
      goto lose;
    }
  } else {                              /* structure. */
    v ^= u;                     /* restore v */
    if (TaggedToHeadfunctor(u) != (t1=TaggedToHeadfunctor(v))) {
      goto lose;
    } else if (t1&QMask) {      /* large number */
      int i;
        
      for (i = LargeArity(t1)-1; i>0; i--)
        if (*TaggedToArg(u,i) != *TaggedToArg(v,i)) goto lose;
      goto win;
    }
    if (cunifyOC_args_aux(Arg,Arity(t1),TaggedToArg(u,1),TaggedToArg(v,1),&x1,&x2)) {
      goto in;
    } else {
      goto lose;
    }
  }

 u_is_hva:
  SwitchOnVar(v,t1, {
      if (u==v) {
      } else if (YoungerHeapVar(TagpPtr(HVA,v),TagpPtr(HVA,u))) {
        BindHVA(v,u);
      } else {
        BindHVA(u,v); 
      } 
    }, {
      BindHVA(u,v);
    }, {
      BindSVA(v,u);
    }, {
      OccurCheck(u,v,{goto lose;}); BindHVA(u,v);
    });
  goto win;

 u_is_cva:
  SwitchOnVar(v,t1, {
      BindHVA(v,u);
    }, { if (u==v) {
      } else if (YoungerHeapVar(TagpPtr(CVA,v),TagpPtr(CVA,u))) {
        BindCVA(v,u);
      } else {
        BindCVA(u,v); 
      } 
    }, {
      BindSVA(v,u);
    }, {
      OccurCheck(u,v,{goto lose;}); BindCVA(u,v);
    });
  goto win;

 u_is_sva:
  for (; TaggedIsSVA(v); v = t1) {
    RefSVA(t1,v);
    if (v == t1) {
      if (u==v) {
      } else if (YoungerStackVar(TagpPtr(SVA,v),TagpPtr(SVA,u))) {
        BindSVA(v,u);
      } else {
        BindSVA(u,v);
      }
      goto win;
    }
  }
  OccurCheck(u,v,{ goto lose; }); BindSVA(u,v);

 win:
  return TRUE;

 lose:
  return FALSE;
}
#endif

CBOOL__PROTO(prolog_unifyOC) {
  return cunifyOC(Arg, X(0), X(1));
}

/* ------------------------------------------------------------------------- */

// TODO: required for ISO compatibility (see another TODO note below); but we need to update our codebase
// #define USE_FU2_ARG_EXCEPTIONS 1

CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t complex) {  
#if defined(USE_FU2_ARG_EXCEPTIONS)
  ERR__FUNCTOR("term_basic:arg", 3);
  intmach_t i;

  DerefSwitch0(number, BUILTIN_ERROR(INSTANTIATION_ERROR, number, 1););
  DerefSwitch0(complex, BUILTIN_ERROR(INSTANTIATION_ERROR, complex, 2););

  if (TaggedIsSmall(number)) {
    i = GetSmall(number);
  } else if (TaggedIsLarge(number) && !LargeIsFloat(number)) {
    return FALSE;
  } else {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), number, 1);
  }

  if (i < 0) {
    BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), number, 1);
  }

  if (TaggedIsSTR(complex)) {
    tagged_t f = TaggedToHeadfunctor(complex);
    if (i == 0 || i > Arity(f) || f&QMask) {
      return FALSE;
    }
    return *TaggedToArg(complex,i);
  } else if (IsComplex(complex)) { /* i.e. list */
    if (i == 1) {
      tagged_t t0;
      RefCar(t0,complex);
      return t0;
    } else if (i == 2) { 
      tagged_t t0;
      RefCdr(t0,complex);
      return t0;
    } else {
      return FALSE;
    }
  } else {
    if (TaggedIsATM(complex)) { // TODO: comment this case for full ISO compliance
      return FALSE;      
    } else {
      BUILTIN_ERROR(TYPE_ERROR(COMPOUND), complex, 2);
    }
  }
#else
  DerefSwitch0(number,{goto barf1;});
  DerefSwitch0(complex,{goto barf2;});

  if (TaggedIsSTR(complex)) {
    intmach_t i = GetSmall(number);
    tagged_t f = TaggedToHeadfunctor(complex);
    if (i<=0 || i>Arity(f) || f&QMask) {
      goto barf1;
    }
    return *TaggedToArg(complex,i);
  } else if (IsComplex(complex)) { // i.e. list 
    if (number==MakeSmall(1))   {
      tagged_t t0;
      RefCar(t0,complex);
      return t0;
    } else if (number==MakeSmall(2)) {
      tagged_t t0;
      RefCdr(t0,complex);
      return t0;
    } else {
      goto barf1;
    }
  } else {
    goto barf2;
  }

 barf1:
  MINOR_FAULT("arg/3: incorrect 1st argument");

 barf2:
  MINOR_FAULT("arg/3: incorrect 2nd argument");
#endif
}

/*---------------------------------------------------------------*/

// TODO: required for ISO compatibility; but we need to update our codebase
// #define USE_BU3_FUNCTOR_EXCEPTIONS 1

CBOOL__PROTO(bu3_functor, tagged_t term, tagged_t name, tagged_t arity) {
#if defined(USE_BU3_FUNCTOR_EXCEPTIONS)
  ERR__FUNCTOR("term_basic:functor", 3);

  DerefSwitch0(term,{goto construct;});
  {
    tagged_t tagarity;
    if (TermIsAtomic(term)) {
      tagarity = TaggedZero;
    } else if (!(term & TagBitFunctor)) {
      term = atom_list;
      tagarity = MakeSmall(2);
    } else {
      tagged_t f = TaggedToHeadfunctor(term);
      term = SetArity(f,0);
      tagarity = MakeSmall(Arity(f));
    }
    CBOOL__UnifyCons(tagarity,arity);
    CBOOL__LASTUNIFY(term,name);
  }
 construct:
  {
    DerefSwitch0(name, BUILTIN_ERROR(INSTANTIATION_ERROR, name, 2););
    DerefSwitch0(arity, BUILTIN_ERROR(INSTANTIATION_ERROR, arity, 3););

    if (TermIsAtomic(name)) {
      if (arity == TaggedZero) {
        CBOOL__LASTUNIFY(name,term);
      } else if (arity > TaggedZero) {
        if (TaggedIsATM(name)) {
          if (arity < MakeSmall(ARITYLIMIT)) {
            return cunify(Arg, make_structure(Arg, SetArity(name,GetSmall(arity))), term);
          } else if (IsInteger(arity)) {
            BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), arity, 3);
          } else {
            BUILTIN_ERROR(TYPE_ERROR(INTEGER),arity, 3);
          }
        } else if (IsInteger(arity)) {
          BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), name, 2);
        } else {
          BUILTIN_ERROR(TYPE_ERROR(INTEGER), arity, 3);
        }
      } else {
        BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), arity, 3);
      }
    } else {
      BUILTIN_ERROR(TYPE_ERROR(ATOMIC), name, 2);
    }
  }
#else
  DerefSwitch0(term,{goto construct;});
  {
    tagged_t tagarity;
    if (TermIsAtomic(term)) {
      tagarity = TaggedZero;
    } else if (!(term & TagBitFunctor)) {
      term = atom_list;
      tagarity = MakeSmall(2);
    } else {
      tagged_t f = TaggedToHeadfunctor(term);
      term = SetArity(f,0);
      tagarity = MakeSmall(Arity(f));
    }
    CBOOL__UnifyCons(tagarity,arity);
    CBOOL__LASTUNIFY(term,name);
  }
  construct:
  {
    DerefSwitch0(name,;);
    DerefSwitch0(arity,;);
    if (TermIsAtomic(name) && (arity==TaggedZero)) {
      CBOOL__LASTUNIFY(name,term);
    } else if (TaggedIsATM(name) && (arity>TaggedZero) && (arity<MakeSmall(ARITYLIMIT))) {
      CBOOL__LASTUNIFY(make_structure(Arg, SetArity(name,GetSmall(arity))), term);
    } else {
      return FALSE;
    }
  }
#endif
}

/*---------------------------------------------------------------*/

// TODO: required for ISO compatibility; this seems to be OK with our codebase
#define USE_BU2_UNIV_EXCEPTIONS 1

CBOOL__PROTO(bu2_univ, tagged_t term, tagged_t list) { 
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  ERR__FUNCTOR("term_basic:=..", 2);
#endif
  tagged_t car;
  tagged_t cdr;
  tagged_t *argp;
  tagged_t *argq;
  int arity;
  tagged_t f;

  DerefSwitch0(term,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term)) {
    MakeLST(cdr,term,cdr);
    CBOOL__LASTUNIFY(cdr,list);
  }
  
  if (term & TagBitFunctor) {
    f = TaggedToHeadfunctor(term);
    argp = TaggedToArg(term,1);
    argq = HeapOffset(argp,Arity(f));
  } else {
    f = functor_lst;
    argp = TaggedToCar(term);
    argq = HeapOffset(argp,2);
  }
  while (HeapYounger(argq,argp)) {
    HeapDecr(argq);
    RefHeap(car,argq);
    MakeLST(cdr,car,cdr);
  }
  MakeLST(cdr,SetArity(f,0),cdr);
  CBOOL__LASTUNIFY(cdr,list);

 construct:
  cdr = list;
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  DerefSwitch0(cdr, BUILTIN_ERROR(INSTANTIATION_ERROR, list, 2););
#else
  DerefSwitch0(cdr,;);
#endif
  arity = 0;

  if (IsVar(cdr)) goto bomb;
  if (!TaggedIsLST(cdr)) {
#if defined(USE_BU2_UNIV_EXCEPTIONS)
    if (cdr == atom_nil) {
      BUILTIN_ERROR(DOMAIN_ERROR(NON_EMPTY_LIST), list, 2); 
    } else {
      BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2); 
    }
#else
    MINOR_FAULT("=../2: incorrect 2nd argument");
#endif
  }
  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  if (cdr==atom_nil) {
    if (TermIsAtomic(f)) {
      CBOOL__LASTUNIFY(f,term);
    } else {
      BUILTIN_ERROR(TYPE_ERROR(ATOMIC), f, 2); 
    }
  } else if (IsVar(f)) {
    goto bomb;
  } else if (!TaggedIsATM(f)) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), f, 2); 
  }
#else
  if (TermIsAtomic(f) && (cdr==atom_nil)) {
    CBOOL__LASTUNIFY(f,term);
  } else if (IsVar(f)) {
    goto bomb;
  } else if (!TaggedIsATM(f)) {
    MINOR_FAULT("=../2: incorrect 2nd argument");
  }
#endif

  argp = w->heap_top;
  HeapPush(w->heap_top,f);
  while (TaggedIsLST(cdr) && arity<ARITYLIMIT) {
    DerefCar(car,cdr);
    DerefCdr(cdr,cdr);
    HeapPush(w->heap_top,car);
    arity++;
  }
  if (IsVar(cdr)) goto bomb;
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  if (arity==ARITYLIMIT) {
    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), list, 2);
  }
  if (cdr!=atom_nil) {
    BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2);
  }
#else
  if (cdr!=atom_nil || arity==ARITYLIMIT) {
    MINOR_FAULT("=../2: incorrect 2nd argument");
  }
#endif
  
  f = SetArity(f,arity);
  if (f==functor_lst) {
    w->heap_top = argp;
    argq = HeapOffset(w->heap_top,1);
    RefHeapNext(car,argq);
    RefHeapNext(cdr,argq);
    HeapPush(w->heap_top,car);
    HeapPush(w->heap_top,cdr);
    CBOOL__LASTUNIFY(Tagp(LST,argp),term);
  } else {
    *argp = f;
    CBOOL__LASTUNIFY(Tagp(STR,argp),term);
  }

 bomb:
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  BUILTIN_ERROR(INSTANTIATION_ERROR,list, 2);
#else
  USAGE_FAULT("=../2: illegal arguments");
#endif
}

