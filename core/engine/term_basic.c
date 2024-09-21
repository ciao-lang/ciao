/*
 *  term_basic.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/basiccontrol.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_bignum.h> /* bn_positive */
#endif

/* Unify with occurs-check, using inline checks (var-nonvar cases) */
#define UNIFY_OC_INLINE 1
/* Enable tail optimization in cyclic_term */
#define CYCLIC_TERM_TAIL_OPTIM 1
/* Enable copying of terms between different heaps in copy_term */
/* (it does not have any significant impact on performance) */
#define SAFE_CROSS_COPY 1

/* --------------------------------------------------------------------------- */

/* Detect if a variable is old during copy_term */
#if defined(SAFE_CROSS_COPY)
/* Use w->global_uncond as a memory barrier
   *and* OnHeap (for safe cross copy_term, since we cannot not assume
   anything about the order of different heaps */
#if defined(PARBACK) || defined(ANDPARALLEL)
// TODO: CondHVA and CondCVA already check heap/stack boundaries
#define OldHVA(X) CondHVA((X))
#define OldCVA(X) CondCVA((X))
#else
#define OldHVA(X) (CondHVA((X)) || !OnHeap(TaggedToPointer(X)))
#define OldCVA(X) (CondCVA((X)) || !OnHeap(TaggedToPointer(X)))
#endif
#else
/* Use w->global_uncond as a memory barrier */
#define OldHVA(X) CondHVA((X))
#define OldCVA(X) CondCVA((X))
#endif

/* --------------------------------------------------------------------------- */
/* copy term */

/* copy_term(?Old,?New) implements this algorithm:

   - If Old is a plain variable, just return.
   - Allocate a choicepoint (to create a memory barrier, as we'll see later,
     to distinguish new and old variables).
   - Allocate a frame containing Old and New (for keeping GC roots).
   - Progressively replace the frame slot for Old with a copy (CopyTerm##__it).
   - While copying, all old variables encountered are bound to their copies.
   - Finally untrail but trail any new CVA:s, deallocate frame & choicept,
     and unify copy with New.

  Different versions of copy_term w.r.t. attributes:

   - copy_term/2: Make a copy of the attributes (recursively):

   ?- attach_attribute(X, fobar(W,W)), copy_term(f(X,X,W,W), Z).
   Z = f(_A,_A,_B,_B),
   X attributed fobar(W,W),
   _A attributed fobar(_B,_B) ? 
    
   - copy_term_nat/2: Do not copy attributes (equivalent to make a copy and remove attributes?)

   ?- attach_attribute(X, fobar(W,W)), copy_term_nat(f(X,X,W,W), Z).
   Z = f(_A,_A,_B,_B),
   X attributed fobar(W,W) ? 
    
   - copy_term_shattr/2: Share all attributed variables:

   ?- attach_attribute(X, fobar(W,W)), copy_term_shattr(f(X,X,W,W), Z).
   Z = f(X,X,_A,_A),
   X attributed fobar(W,W) ? 
*/

/* TODO: make it work with cyclic terms */

// TODO:[oc-merge] can w->global_uncond be other heap pointer?
// generalize for other uses?

/* In copy_term used as a heap pointer whose relative position
   w.r.t. the newly created term is invariant in the presence of
   GC. Note that it requires the code to be surrounded by
   push_choicept/pop_choicept. */
#define TopOfOldHeap w->global_uncond
#if defined(OPTIM_COMP)
#define GetRelPtrOldHeap(X) HeapCharDifference(TopOfOldHeap,(X))
#define GetAbsPtr(X) HeapCharOffset(TopOfOldHeap,(X))
#else
#define GetRelPtrOldHeap(X) HeapDifference(TopOfOldHeap,(X))
#define GetAbsPtr(X) HeapOffset(TopOfOldHeap,(X))
#endif

#if defined(OPTIM_COMP)
// TODO:[oc-merge] 2* was missing in OC
#define GCTEST(Pad) { \
  if (HeapCharAvailable(G->heap_top) < (Pad)) \
    CVOID__CALL(heap_overflow,2*(Pad)); \
  if (ChoiceCharDifference(w->choice,G->trail_top) < (Pad)) \
    CVOID__CALL(choice_overflow,(Pad),TRUE); \
}
#else
#define GCTEST(Pad) { \
  if (HeapCharDifference(G->heap_top,Heap_End) < (Pad)*sizeof(tagged_t)) \
    CVOID__CALL(heap_overflow,2*((Pad)*sizeof(tagged_t))); \
  if (ChoiceDifference(w->choice,G->trail_top) < (Pad)) \
    CVOID__CALL(choice_overflow,2*(Pad)*sizeof(tagged_t),TRUE); \
}
#endif

#define TMPL_copy_term(CopyTerm, ROOT_CVA, COPY_CVA) \
static CVOID__PROTO(CopyTerm##__it, tagged_t *loc); \
CBOOL__PROTO(CopyTerm) { \
  tagged_t t1 = X(0); \
  /* returning now is equivalent to unify X(1) with a fresh variable */ \
  DerefSw_HVA_CVA_SVA_Other(t1,{ \
    CBOOL__PROCEED; \
  }, { \
    ROOT_CVA; \
  }, { \
    CBOOL__PROCEED; \
  }, { \
  }); \
  /* otherwise, create a choicept+frame and start copying */ \
  X(0) = t1; \
  CVOID__CALL(push_choicept,fail_alt); /* try, arity=0 */ \
  CVOID__CALL(push_frame,2); /* allocate, size=2 */ \
  CVOID__CALL(CopyTerm##__it,&G->frame->x[0]); /* do the copying */ \
  UntrailVals(); /* untrail */ \
  CVOID__CALL(pop_frame); /* X(0) is now the copy! */ \
  CVOID__CALL(pop_choicept); \
  CBOOL__LASTUNIFY(X(0),X(1)); \
} \
\
/* create a copy of the term located at 'loc' */ \
static CVOID__PROTO(CopyTerm##__it, tagged_t *loc) { \
  tagged_t t1, *pt1, *pt2; \
  arity_t i; \
  intmach_t pt2rel; \
\
 start: \
  t1 = *loc; \
  HeapDerefSw_HVA_CVA_NUMorATM_LST_STR(t1,{ /* HVA */ \
    if (OldHVA(t1)) { \
      *loc = Tagp(HVA, loc); \
      tagged_t t2 = Tagp(HVA, loc); \
      BindHVA(t1,t2); \
      return; \
    } else { \
      goto keep_old; \
    } \
  }, { /* CVA */ \
    COPY_CVA; \
  }, { /* NUM ATM */ \
    goto keep_old; \
  }, { /* LST */ \
    pt1 = TagpPtr(LST,t1); \
    pt2 = G->heap_top; \
    *loc = Tagp(LST,pt2); \
    goto copy_2_cells; \
  }, { /* STR */ \
    SwStruct(hf, t1, { /* STR(blob) */ \
      /* TODO: fixme, cross copy term requires copying the blob */ \
      goto keep_old; \
    },{ /* STR(struct) */ \
      /* copy the structure (first with same arguments) */ \
      pt1 = TaggedToArg(t1,1); \
      pt2 = G->heap_top; \
      *loc = Tagp(STR,pt2); \
      HeapPush(pt2,hf); \
      for (i=Arity(hf); i>0; --i) { \
        t1 = *pt1; \
        pt1++; \
        HeapPush(pt2,t1); \
      } \
      G->heap_top = pt2; \
      /* now make copies for each of them */ \
      pt2rel = GetRelPtrOldHeap(pt2); \
      GCTEST(CHOICEPAD); \
      for (i=Arity(hf); i>1; --i) { \
        CVOID__CALL(CopyTerm##__it,GetAbsPtr(pt2rel)-i); \
      } \
      goto last_arg; \
    }); \
  }); \
  return; \
 keep_old: \
  *loc = t1; \
  return; \
 copy_2_cells: \
  /* special case for 2 cells (LST or CVA) */ \
  t1 = *pt1; \
  pt1++; \
  HeapPush(pt2,t1); \
  t1 = *pt1; \
  pt1++; \
  HeapPush(pt2,t1); \
  G->heap_top = pt2; \
  pt2rel = GetRelPtrOldHeap(pt2); \
  GCTEST(CHOICEPAD); \
  CVOID__CALL(CopyTerm##__it, G->heap_top - 2); \
  goto last_arg; \
 last_arg: \
  GCTEST(CHOICEPAD); \
  /* (tail call) */ \
  loc = GetAbsPtr(pt2rel)-1; \
  goto start; \
}

/* copy_term/2 */
TMPL_copy_term(prolog_copy_term, {}, {
  if (OldCVA(t1)) { /* new 3-field CVA */
    tagged_t t2;
    pt1 = TaggedToGoal(t1);
    pt2 = G->heap_top;
    LoadCVA(t2,pt2);
    BindCVANoWake(t1,t2);
    *loc = t2;
    goto copy_2_cells;
  } else {
    goto keep_old;
  }
});

// TODO:[oc-merged] keep both versions?
#if defined(OPTIM_COMP)
// TODO:[oc-merged] check if it is still needed
/* copy_term_shattr/2: like copy_term/2 but share attributed variables
   (JFMC) */
TMPL_copy_term(prolog_copy_term_shattr, {}, ({
  goto keep_old;
}));
#else
/* Do not copy attributes (CVA) */
/* (equivalent to taking out the attributes from the copy) */
TMPL_copy_term(prolog_copy_term_nat, {
  CBOOL__PROCEED; /* CVA on root, do nothing */
}, {
  if (OldCVA(t1)) {
    /* This code is equivalent to taking out the attribute;
       xref bu1_detach_attribute() */
    *loc = Tagp(HVA,loc);
    tagged_t t2 = Tagp(HVA,loc);
    BindCVANoWake(t1,t2);
    return;
  } else {
    goto keep_old;
  }
});
#endif

/* ------------------------------------------------------------------------- */

#if defined(SAFE_CROSS_COPY)
/* Copy a term in a remote worker to the local worker.  Returns the local
   term pointer.  It has (nontermination) problems when copying structures
   with self references. */

// TODO: see bugs/Pending/cross_copy_term/README.txt
//  - blobs are not copied across heaps!

CFUN__PROTO(cross_copy_term, tagged_t, tagged_t remote_term) {
  bool_t ok MAYBE_UNUSED;
  X(0) = remote_term;
  LoadHVA(X(1), G->heap_top);
  ok = CBOOL__SUCCEED(prolog_copy_term);
  RTCHECK({
    /* TODO: raise an exception! */
    if (!ok) fprintf(stderr, "Could not copy term in cross_copy_term!!!!\n");
  });
  CFUN__PROCEED(X(1));
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
  arity_t i;

  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    CBOOL__LASTCALL(c_cyclic_ptr, TaggedToPointer(t));
  case LST: 
    ptr = TagpPtr(LST,t);
    i = 2;
    goto args;
  case STR:
    ptr = TagpPtr(STR,t);
    t = *ptr;
    if (FunctorIsBlob(t)) CBOOL__FAIL; /* large number */
    i = Arity(t);
    ptr++;
    goto args;
  default:
    CBOOL__FAIL;
  }
 args:
  for (; i >= 1; i--, ptr++) {
    if (CBOOL__SUCCEED(c_cyclic_ptr, ptr)) CBOOL__PROCEED;
  }
  CBOOL__FAIL;
}

CBOOL__PROTO(c_cyclic_ptr, tagged_t *pt) {
  tagged_t *ptr1;
  arity_t i;
  TG_Let(ptr, pt);
 start:
  TG_Fetch(ptr);
  switch(TagOf(TG_Val(ptr))){
  case SVA:
  case HVA:
  case CVA:
    ptr = TaggedToPointer(TG_Val(ptr));
    if (*ptr == TG_Val(ptr)) goto acyclic; /* free variable */
    goto start;
  case LST: 
    if (TG_IsM(ptr)) goto cyclic;
    ptr1 = TagpPtr(LST,TG_Val(ptr));
    i = 2;
    goto args;
  case STR:
    if (TG_IsM(ptr)) goto cyclic;
    ptr1 = TagpPtr(STR,TG_Val(ptr));
    tagged_t hf = *ptr1;
    if (FunctorIsBlob(hf)) {
      goto acyclic; /* large number */
    } else {
      i = Arity(hf);
      ptr1++;
      goto args;
    }
  default:
    goto acyclic;
  }
 args:
  TG_SetM(ptr); /* mark cell */
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  for (; i > 1; i--, ptr1++) {
    if (CBOOL__SUCCEED(c_cyclic_ptr, ptr1)) goto cyclic;
  }
  ptr = ptr1;
  goto start;
#else
  for (; i >= 1; i--, ptr1++) {
    if (CBOOL__SUCCEED(c_cyclic_ptr, ptr1)) { TG_UnsetM(ptr); goto cyclic; }
  }
  /* acyclic */
  TG_UnsetM(ptr);
  goto acyclic;
#endif

 acyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  CVOID__CALL(unmark_rightmost_branch, pt); /* ensure marks are removed */
#endif
  CBOOL__FAIL;
 cyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  CVOID__CALL(unmark_rightmost_branch, pt); /* ensure marks are removed */
#endif
  CBOOL__PROCEED;
}

#if defined(CYCLIC_TERM_TAIL_OPTIM)
CVOID__PROTO(unmark_rightmost_branch, tagged_t *ptr0) {
  TG_Let(ptr, ptr0);

 start:
  TG_Fetch(ptr);
  switch(TagOf(TG_Val(ptr))){
  case SVA:
  case HVA:
  case CVA:
    ptr = TaggedToPointer(TG_Val(ptr));
    if (*ptr == TG_Val(ptr)) return;
    goto start;
  case LST: 
    if (!TG_IsM(ptr)) return;
    TG_UnsetM(ptr);
    ptr = TagpPtr(LST,*ptr);
    ptr++;
    goto start;
  case STR:
    if (!TG_IsM(ptr)) return;
    TG_UnsetM(ptr);
    ptr = TagpPtr(STR,*ptr);
    ptr += Arity(*ptr);
    goto start;
  default:
    return;
  }
}
#endif

CBOOL__PROTO(prolog_cyclic_term) {
  CBOOL__LASTCALL(c_cyclic_term, X(0));
}

/* --------------------------------------------------------------------------- */
/* unifiable(Term1, Term2, Unifier)
 *
 * Algortihm: tries unify Term1 and Term2, then untrail the
 * unification while saving the unifier into a prolog list which is
 * eventually unified with Unifier. The function treats attributed 
 * variable as classical ones.
 */
CBOOL__PROTO(prolog_unifiable) {
  tagged_t t, t1;
  tagged_t * limit, *tr;

  /* Forces trailing of bindings and saves the top of the trail. */
  CVOID__CALL(push_choicept,fail_alt);  
  /* Saves the arguments in case of GC. */
  CVOID__CALL(push_frame,3);

  CBOOL__UNIFY(X(0), X(1));

  /* Makes sure there is enough place in the heap to construct the
     unfiers list. */
  GCTEST((G->trail_top - TrailTopUnmark(w->choice->trail_top)) * 5);

  t = atom_nil;
  tr = G->trail_top;
  limit = TrailTopUnmark(w->choice->trail_top);
   
  while (TrailYounger(tr, limit)) {
    TrailDec(tr);
    t1 = *tr; // (tr points to the popped element)

    HeapPush(G->heap_top, SetArity(atom_equal, 2));
    HeapPush(G->heap_top, t1);
    HeapPush(G->heap_top, *TaggedToPointer(t1));
    HeapPush(G->heap_top, Tagp(STR, G->heap_top - 3));
    HeapPush(G->heap_top, t);
    t = Tagp(LST, G->heap_top - 2);

    *TaggedToPointer(t1) = t1;
  }

  /* Ignores possible wakes caused by unification of attributed
     variables */ 
  if (TestEvent()) UnsetEvent(); /* TODO: check */
  
  G->trail_top = limit;
  CVOID__CALL(pop_frame);
  CVOID__CALL(pop_choicept);    

  CBOOL__LASTUNIFY(X(2), t);
}  

/* ------------------------------------------------------------------------- */
/* Deref variable v occurs in term x */
/* (needed for unifyOC) */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1);

static CBOOL__PROTO(var_occurs_args_aux, 
                    tagged_t v,
                    arity_t arity,
                    tagged_t *pt1,
                    tagged_t *x1) {
  tagged_t t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && CBOOL__SUCCEED(var_occurs,v,t1)) CBOOL__PROCEED;
    pt1++;
  }
  *x1 = t1;
  CBOOL__FAIL;
}

static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1) {
  tagged_t u;

 in:
  u=x1;

  DerefSw_HVA_CVA_SVA_Other(u,
              { goto var; },
              { goto var; },
              { goto var; },
              { goto non_var; });

 non_var:
  if (TaggedIsATM(u)) goto lose;
  if (TaggedIsSmall(u)) goto lose;
  if (TaggedIsLST(u)) {
    if (!CBOOL__SUCCEED(var_occurs_args_aux,v,2,TaggedToCar(u),&x1)) {
      goto in;
    } else {
      goto win;
    }
  } else { /* structure. */
    tagged_t t1;
    t1=TaggedToHeadfunctor(u);
    if (FunctorIsBlob(t1)) { /* large number */
      goto lose;
    } if (!CBOOL__SUCCEED(var_occurs_args_aux,v,Arity(t1),TaggedToArg(u,1),&x1)) {
      goto in;
    } else {
      goto win;
    }
  }

 var:
  if (v == u) goto win; else goto lose;

 win:
  CBOOL__PROCEED;
 lose:
  CBOOL__FAIL;
}
#endif

/* ------------------------------------------------------------------------- */
/* Unify with occurs-check */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(cunifyOC_args_aux,
                    arity_t arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2);
static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2);

/* Unify the argument lists of two compund terms. (with occurs-check)
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
CBOOL__PROTO(cunifyOC_args, 
             arity_t arity,
             tagged_t *pt1,
             tagged_t *pt2) {
  tagged_t x1, x2;
  return (CBOOL__SUCCEED(cunifyOC_args_aux,arity,pt1,pt2,&x1,&x2) && CBOOL__SUCCEED(cunifyOC_aux,x1,x2));
}

// TODO:[oc-merge] which one is right?
#if defined(OPTIM_COMP)
#define UNIF_DerefVar HeapDerefSw_HVAorCVA_Other
#else
#define UNIF_DerefVar DerefSw_HVAorCVAorSVA_Other
#endif

static CBOOL__PROTO(cunifyOC_args_aux, 
                    arity_t arity,
                    tagged_t *pt1,
                    tagged_t *pt2,
                    tagged_t *x1,
                    tagged_t *x2) {
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;

#if defined(OPTIM_COMP)
  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
#else
  VALUETRAIL__TEST_OVERFLOW(2*CHOICEPAD);
#endif
  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (t1 != t2) {
      UNIF_DerefVar(t1, { goto noforward; }, {});
      UNIF_DerefVar(t2, { goto noforward; }, {});
      if (t1!=t2) {
        Sw_2xLSTorSTR_Other(t1, t2, { /* LST or STR */
          /* NOTE: do forward args from pt2 to pt1 */ 
          goto noforward;
        }, {
          CBOOL__FAIL;
        });
      } else { 
        goto next;
      }
    noforward:
      if (arity>1 && !CBOOL__SUCCEED(cunifyOC_aux,t1,t2))
        CBOOL__FAIL;
      goto next;
    }
  next:
    pt1++;
    pt2++;
  }

  *x1 = t1;
  *x2 = t2;

#if !defined(OPTIM_COMP) // TODO:[oc-merge] needed?
  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
#endif
  CBOOL__PROCEED;
}
#endif 

#if defined(OPTIM_COMP) // TODO:[oc-merge] merge oc macros (they are different, assume same tags, more efficient?)
#define YoungerHVA(U,V) YoungerHeapVar((U),(V))
#define YoungerCVA(U,V) YoungerHeapVar((U),(V))
#define YoungerSVA(U,V) YoungerStackVar((U),(V))
#else
#define YoungerHVA(U,V) YoungerHeapVar(TagpPtr(HVA,(U)),TagpPtr(HVA,(V)))
#define YoungerCVA(U,V) YoungerHeapVar(TagpPtr(CVA,(U)),TagpPtr(CVA,(V)))
#define YoungerSVA(U,V) YoungerStackVar(TagpPtr(SVA,(U)),TagpPtr(SVA,(V)))
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
  if (CBOOL__SUCCEED(c_cyclic_term, x1)) CBOOL__FAIL;
  CBOOL__PROCEED;
#endif
}

#if defined(UNIFY_OC_INLINE)
#define OccurCheck(U,V,OCCUR) \
  { if (CBOOL__SUCCEED(var_occurs, (U), (V))) { OCCUR; } }

static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2) {
  tagged_t u, v;

 in:
  u=x1;
  v=x2;

  DerefSw_HVA_CVA_SVA_Other(u,
              {goto u_is_hva;},
              {goto u_is_cva;},
              {goto u_is_sva;},
              {});

  /* one non variable */
  DerefSw_HVA_CVA_SVA_Other(v,
              { OccurCheck(v,u,{goto lose;}); BindHVA(v,u); goto win; },
              { OccurCheck(v,u,{goto lose;}); BindCVA(v,u); goto win; },
              { OccurCheck(v,u,{goto lose;}); BindSVA(v,u); goto win; },
              {});

  /* two non variables */
  if (u == v) { /* are they equal? */
    goto win;
  } else if (!TaggedSameTag(u, v)) { /* not the same type? */
    goto lose;
  } else {
    Sw_NUMorATM_LST_STR(u, { /* NUM ATM */
      goto lose; /* fail */
    }, {
      /* LST x LST */
      CBOOL__CALL(cunifyOC_args_aux,2,TaggedToCar(u),TaggedToCar(v),&x1,&x2);
      goto in;
    }, {
      /* STR x STR */
      tagged_t t1 = TaggedToHeadfunctor(v);
      if (TaggedToHeadfunctor(u) != t1) {
        goto lose;
      } else {
        if (FunctorIsBlob(t1)) { /* STRBlob x STRBlob */
#if defined(OPTIM_COMP)
          CBOOL__TEST(compare_blob(TagpPtr(STR, u), TagpPtr(STR, v)));
#else
          for (int i = LargeArity(t1)-1; i>0; i--) {
            if (*TaggedToArg(u,i) != *TaggedToArg(v,i)) CBOOL__FAIL;
          }
#endif
          goto win;
        } else { /* STRStruct x STRStruct */
          CBOOL__CALL(cunifyOC_args_aux, Arity(t1), TaggedToArg(u,1), TaggedToArg(v,1), &x1, &x2);
          goto in;
        }
      }
    });
  }

 u_is_hva:
  DerefSw_HVA_CVA_SVA_Other(v, {
    if (u==v) {
    } else if (YoungerHVA(v, u)) {
      BindHVA(v,u);
    } else {
      BindHVA(u,v); 
    } 
  }, {
    BindHVA(u,v);
  }, {
    BindSVA(v,u);
  }, {
    OccurCheck(u,v,{goto lose;});
    BindHVA(u,v);
  });
  goto win;

 u_is_cva:
  DerefSw_HVA_CVA_SVA_Other(v, {
    BindHVA(v,u);
  }, {
    if (u==v) {
    } else if (YoungerCVA(v,u)) {
      BindCVA(v,u);
    } else {
      BindCVA(u,v); 
    } 
  }, {
    BindSVA(v,u);
  }, {
    OccurCheck(u,v,{goto lose;});
    BindCVA(u,v);
  });
  goto win;

 u_is_sva:
  { tagged_t t1;
    for (; TaggedIsSVA(v); v = t1) {
      t1 = *TagpPtr(SVA,v);
      if (v == t1) {
        if (u==v) {
        } else if (YoungerSVA(v,u)) {
          BindSVA(v,u);
        } else {
          BindSVA(u,v);
        }
        goto win;
      }
    }
  }
  OccurCheck(u,v,{ goto lose; });
  BindSVA(u,v);

 win:
  CBOOL__PROCEED;

 lose:
  CBOOL__FAIL;
}
#endif

CBOOL__PROTO(prolog_unifyOC) {
  CBOOL__LASTCALL(cunifyOC, X(0), X(1));
}

/* --------------------------------------------------------------------------- */

#define USE_FU2_ARG_EXCEPTIONS 1 /* ISO compatibility */
// TODO: deprecate old behavior

CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t term) {
#if defined(USE_FU2_ARG_EXCEPTIONS)
  ERR__FUNCTOR("term_basic:arg", 3);

  intval_t i;
  DerefSw_HVAorCVAorSVA_Other(number, {
    BUILTIN_ERROR(ERR_instantiation_error, number, 1);
  }, {
    Sw_NUM_Large_Other(number, {
      i = GetSmall(number);
    }, {
      /* large number, saturate to max arity plus 1 so that we can
         continue checking the other arguments */
      i = ARITYLIMIT + 1;
    }, {
      BUILTIN_ERROR(ERR_type_error(integer), number, 1);
    });
    if (i < 0) {
      BUILTIN_ERROR(ERR_domain_error(not_less_than_zero), number, 1);
    }
  });

  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    BUILTIN_ERROR(ERR_instantiation_error, term, 2);
  }, { /* NUM ATM */
    // if (TaggedIsATM(term)) CBOOL__FAIL; // TODO: comment this case for full ISO compliance
    BUILTIN_ERROR(ERR_type_error(compound), term, 2);
  }, { /* LST */ 
    if (i == 1) {
      tagged_t t0 = *TaggedToCar(term);
      CFUN__PROCEED(t0);
    } else if (i == 2) { 
      tagged_t t0 = *TaggedToCdr(term);
      CFUN__PROCEED(t0);
    } else {
      CFUN__PROCEED(ERRORTAG); /* (a failure) */
    }
  }, { /* STR */
    SwStruct(f, term, { /* STR(blob) */
      CFUN__PROCEED(ERRORTAG); /* (a failure) */
    }, { /* STR(struct) */
      if (i == 0 || i > Arity(f)) {
        CFUN__PROCEED(ERRORTAG); /* (a failure) */
      }
      tagged_t t0 = *TaggedToArg(term,i);
      CFUN__PROCEED(t0);
    });
  });
#else
  DerefSw_HVAorCVAorSVA_Other(number,{
    goto barf1;
  }, {
  });
  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    goto barf2;
  }, { /* NUM ATM */
    goto barf2;
  }, { /* LST */ 
    if (number==MakeSmall(1)) {
      tagged_t t0 = *TaggedToCar(term);
      CFUN__PROCEED(t0);
    } else if (number==MakeSmall(2)) {
      tagged_t t0 = *TaggedToCdr(term);
      CFUN__PROCEED(t0);
    } else {
      goto barf1;
    }
  }, { /* STR */
    SwStruct(f, term, { /* STR(blob) */
      goto barf1;
    }, { /* STR(struct) */
      /* TODO: do we check that 'number' is a small integer? */
      intval_t i = GetSmall(number);
      if (i<=0 || i>Arity(f)) goto barf1;
      tagged_t t0 = *TaggedToArg(term,i);
      CFUN__PROCEED(t0);
    });
  });

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

  tagged_t tagarity;
  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term,{ /* HVA CVA SVA */
    /* (construct) */

    DerefSw_HVAorCVAorSVA_Other(name, {
      BUILTIN_ERROR(ERR_instantiation_error, name, 2);
    }, {});
    DerefSw_HVAorCVAorSVA_Other(arity, {
      BUILTIN_ERROR(ERR_instantiation_error, arity, 3);
    },{});

    /* Get and check arity */
    intval_t arity_n;
    Sw_NUM_Large_Other(arity, { /* NUM */
      arity_n = GetSmall(arity);
    }, { /* Large */
      /* large number, saturate to -1 or max arity plus 1 so that we can
         continue checking the other arguments */
      if (bn_positive(TaggedToBignum(arity))) {
        arity_n = ARITYLIMIT + 1;
      } else {
        arity_n = -1;
      }
    }, { /* Other */
      BUILTIN_ERROR(ERR_type_error(integer), arity, 3);
    });
    if (arity_n < 0) {
      BUILTIN_ERROR(ERR_domain_error(not_less_than_zero), arity, 3);
    }
    if (arity_n >= ARITYLIMIT) {
      BUILTIN_ERROR(ERR_representation_error(max_arity), arity, 3);
    }

    if (arity_n==0) { /* if arity_n==0, 'name' can be any atomic */
      if (!TermIsAtomic(name)) {
        BUILTIN_ERROR(ERR_type_error(atomic), name, 2);
      }
      CBOOL__LASTUNIFY(name,term);
    } else { /* otherwise 'name' must be an atom */
      if (!TaggedIsATM(name)) {
        BUILTIN_ERROR(ERR_type_error(atom), name, 2);
      }
      CBOOL__LASTUNIFY(CFUN__EVAL(make_structure, SetArity(name,arity_n)), term);
    }
  },{ /* NUM ATM */
    tagarity = MakeSmall(0);
    goto unif;
  }, { /* LST */
#if defined(OPTIM_COMP)
    term = atom_lst;
#else
    term = atom_list;
#endif
    tagarity = MakeSmall(2);
    goto unif;
  }, { /* STR */
    SwStruct(f, term, { /* STR(blob) */
      tagarity = MakeSmall(0);
      goto unif;
    }, { /* STR(struct) */
      term = FUNCTOR_NAME(f);
      tagarity = MakeSmall(Arity(f));
      goto unif;
    });
  });
 unif:
  CBOOL__UnifyCons(tagarity,arity);
  CBOOL__LASTUNIFY(term,name);
#else
  tagged_t tagarity;
  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    DerefSw_HVAorCVAorSVA_Other(name,;,{});
    DerefSw_HVAorCVAorSVA_Other(arity,;,{});
    if (TermIsAtomic(name) && (arity==MakeSmall(0))) {
      CBOOL__LASTUNIFY(name,term);
    } else if (TaggedIsATM(name) &&
               (arity>MakeSmall(0)) && (arity<MakeSmall(ARITYLIMIT))) {
      CBOOL__LASTUNIFY(CFUN__EVAL(make_structure, SetArity(name,GetSmall(arity))), term);
    } else {
      CBOOL__FAIL;
    }
  }, { /* NUM ATM */
    tagarity = MakeSmall(0);
    goto unif;
  }, { /* LST */
#if defined(OPTIM_COMP)
    term = atom_lst;
#else
    term = atom_list;
#endif
    tagarity = MakeSmall(2);
    goto unif;
  }, { /* STR */
    SwStruct(f, term, { /* STR(blob) */
      tagarity = MakeSmall(0);
      goto unif;
    }, { /* STR(struct) */
      term = FUNCTOR_NAME(f);
      tagarity = MakeSmall(Arity(f));
      goto unif;
    });
  });
 unif:
  CBOOL__UnifyCons(tagarity,arity);
  CBOOL__LASTUNIFY(term,name);
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
  arity_t arity;
  tagged_t f;

  DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(term, { /* HVA CVA SVA */
    goto construct;
  }, { /* NUM ATM */
    goto nil_list;
  }, { /* LST */
    f = functor_lst;
    argp = TaggedToCar(term);
    argq = HeapCharOffset(argp,2*sizeof(tagged_t));
    goto build_list;
  }, { /* STR */
    SwStruct(hf, term, { /* STR(blob) */
      goto nil_list;
    }, { /* STR(struct) */
      f = hf;
      argp = TaggedToArg(term,1);
      argq = HeapCharOffset(argp,Arity(f)*sizeof(tagged_t));
      goto build_list;
    });
  });

 nil_list:
  MakeLST(cdr,term,atom_nil);
  CBOOL__LASTUNIFY(cdr,list);

 build_list:
  cdr = atom_nil;
  while (HeapYounger(argq,argp)) {
    argq--;
    car = *argq;
    MakeLST(cdr,car,cdr);
  }
  MakeLST(cdr,FUNCTOR_NAME(f),cdr);
  CBOOL__LASTUNIFY(cdr,list);

 construct:
  cdr = list;
  DerefSw_HVAorCVAorSVA_Other(cdr, {
#if defined(USE_BU2_UNIV_EXCEPTIONS)
    BUILTIN_ERROR(ERR_instantiation_error, list, 2);
#else
    goto bomb;
#endif
  }, {
  });
  if (!TaggedIsLST(cdr)) {
#if defined(USE_BU2_UNIV_EXCEPTIONS)
    if (cdr == atom_nil) {
      BUILTIN_ERROR(ERR_domain_error(non_empty_list), list, 2); 
    } else {
      BUILTIN_ERROR(ERR_type_error(list), list, 2); 
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
      BUILTIN_ERROR(ERR_type_error(atomic), f, 2); 
    }
  } else if (IsVar(f)) {
    goto bomb;
  } else if (!TaggedIsATM(f)) {
    BUILTIN_ERROR(ERR_type_error(atom), f, 2); 
  }
#else
  if ((cdr==atom_nil) && TermIsAtomic(f)) {
    CBOOL__LASTUNIFY(f,term);
  } else if (IsVar(f)) {
    goto bomb;
  } else if (!TaggedIsATM(f)) {
    MINOR_FAULT("=../2: incorrect 2nd argument");
  }
#endif

  arity = 0;
  argp = G->heap_top;
  HeapPush(G->heap_top,f);
  while (TaggedIsLST(cdr) && arity<ARITYLIMIT) {
    DerefCar(car,cdr);
    DerefCdr(cdr,cdr);
    HeapPush(G->heap_top,car);
    arity++;
  }
  if (IsVar(cdr)) goto bomb;
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  if (arity==ARITYLIMIT) {
    BUILTIN_ERROR(ERR_representation_error(max_arity), list, 2);
  }
  if (cdr!=atom_nil) {
    BUILTIN_ERROR(ERR_type_error(list), list, 2);
  }
#else
  if (cdr!=atom_nil || arity==ARITYLIMIT) {
    MINOR_FAULT("=../2: incorrect 2nd argument");
  }
#endif

  f = SetArity(f,arity);
  if (f==functor_lst) { /* rewrite as a list; note: cannot handle LST cast directly because arity is not known until all the input is processed */
    G->heap_top = argp;
    argq = HeapCharOffset(G->heap_top,1*sizeof(tagged_t));
    car = *argq;
    argq++;
    cdr = *argq;
    argq++;
    HeapPush(G->heap_top,car);
    HeapPush(G->heap_top,cdr);
    CBOOL__LASTUNIFY(Tagp(LST,argp),term);
  } else {
    *argp = f;
    CBOOL__LASTUNIFY(Tagp(STR,argp),term);
  }

 bomb:
#if defined(USE_BU2_UNIV_EXCEPTIONS)
  BUILTIN_ERROR(ERR_instantiation_error,list, 2);
#else
  USAGE_FAULT("=../2: illegal arguments");
#endif
}

