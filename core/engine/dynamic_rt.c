/*
 *  dynamic_rt.c
 *
 *  Database management support code.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 */

#include <unistd.h>
#include <stddef.h> /* ptrdiff_t */

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/dynamic_rt.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>
#include <ciao/basiccontrol.h>

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       $CURRENT_INSTANCE/5
   -----------------------------------------------------------------------*/

typedef enum {BLOCK, NO_BLOCK} BlockingType;
typedef enum {X5, X2} WhichChain;

static bool_t wait_for_an_instance_pointer(instance_t **ins_pptr1,
                                           instance_t **ins_pptr2,
                                           int_info_t *root,
                                           BlockingType block);

static instance_t *first_possible_instance(tagged_t head,
                                           int_info_t *root,
                                           instance_t **x2_n,
                                           instance_t **x5_n);

static instance_handle_t *make_handle_to(instance_t *inst,
                                         int_info_t *root,
                                         tagged_t head,
                                         WhichChain chain);

static void remove_handle(instance_handle_t *xi,
                          int_info_t *root,
                          WhichChain chain);

static void change_handle_to_instance(instance_handle_t *handle,
                                      instance_t *new_inst,
                                      int_info_t *root,
                                      WhichChain chain);

static void unlink_handle(instance_handle_t *xi, 
                          int_info_t *rt, 
                          WhichChain chain);

static void link_handle(instance_handle_t *handle,
                        instance_t *inst,
                        int_info_t *root,
                        WhichChain chain);

static CFUN__PROTO(current_instance_noconc, instance_t *);
static CFUN__PROTO(current_instance_conc, instance_t *, BlockingType block);

/* ASSERT: X(2) is a dereferenced small integer */

/* MCL: a typical call is '$current_instance'(Head, Body, Root, Ptr,
   Blocking) where Head is the head clause, Body is the body clause ("true"
   for facts), Root is a small integer denoting the root of the predicate
   (obtained through '$current_clauses'/2), and Ptr is a number denoting the
   particular clause (i.e., a pointer to the instance_t). "Blocking"
   specifies whether the predicate must block or not on concurrent
   predicates. */

CFUN__PROTO(current_instance, instance_t *)
{
  int_info_t *root;
  BlockingType block;
#if defined(PROFILE)
  tagged_t *junk;
  w->choice->functor=find_definition(predicates_location,X(0),&junk,FALSE);
#endif
  PredTrace("I",w->choice->functor);
  root = TaggedToRoot(X(2));
  if (root->behavior_on_failure == DYNAMIC)
    return current_instance_noconc(Arg);
  else {
    DEREF(X(4), X(4));                                   /* Blocking? (MCL) */
#if defined(DEBUG)
    if (X(4) != atom_block && X(4) != atom_no_block) {
      failc("$current_instance called with unknown 5th argument");
      return NULL;                         
    }
#endif
    block = X(4) == atom_block ? BLOCK : NO_BLOCK;
    return current_instance_conc(Arg, block);
  }
}


 /* Take time into account, do not wait for predicates. */

static CFUN__PROTO(current_instance_noconc, instance_t *)
{
  instance_t *x2_chain, *x5_chain;
  instance_t *x2_next=NULL, *x5_next=NULL;
  int_info_t *root = TaggedToRoot(X(2));
  tagged_t head;


  Wait_Acquire_Cond_lock(root->clause_insertion_cond);
  head=X(0); 
  DerefSwitch(head,X(0),goto var_case_switch;);
  if (TaggedIsSTR(head)) {
    DerefArg(head,head,1);
    if (IsVar(head)) {
    var_case_switch:
      x5_chain = x2_chain = ACTIVE_INSTANCE(Arg,root->first,use_clock,TRUE);
      if (x2_chain)
        x5_next = x2_next =
          ACTIVE_INSTANCE(Arg,x2_chain->forward,use_clock,TRUE);
      else {
        Release_Cond_lock(root->clause_insertion_cond);
        return NULL;
      }
    }
    else if (TaggedIsLST(head)){
      x5_chain = ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE);
    xn_switch:
      x2_chain = ACTIVE_INSTANCE(Arg,root->varcase,use_clock,FALSE);

      if (x2_chain && x5_chain) {
        if (x2_chain->rank < x5_chain->rank){
          x2_next =
            ACTIVE_INSTANCE(Arg,x2_chain->next_forward,use_clock,FALSE);
          x5_next = x5_chain;
          x5_chain = NULL;
        } else {
          x5_next =
            ACTIVE_INSTANCE(Arg,x5_chain->next_forward,use_clock,FALSE);
          x2_next = x2_chain;
          x2_chain = NULL;
        }
      } else if (x2_chain)
        x2_next = ACTIVE_INSTANCE(Arg,x2_chain->next_forward,use_clock,FALSE);
      else if (x5_chain)
        x5_next = ACTIVE_INSTANCE(Arg,x5_chain->next_forward,use_clock,FALSE);
      else {
        Release_Cond_lock(root->clause_insertion_cond);
        return NULL;                                    /* No solution */
      }
    } else {
      sw_on_key_node_t *hnode;

      if (TaggedIsSTR(head))
        hnode = incore_gethash(root->indexer,TaggedToHeadfunctor(head));
      else
        hnode = incore_gethash(root->indexer,head);
        
      x5_chain = ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);
      goto xn_switch;
    }
  }
  else goto var_case_switch;

  /* NOTE: We must cleanup unused registers up to DynamicPreserved so
     that HEAPMARGIN_CALL does not break during GC */

  if (x2_next || x5_next) {
    X(X2_CHN) = PointerToTermOrZero(x2_next);
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerToTermOrZero(x5_next);
    X(RootArg) = PointerToTermOrZero(root);
    /* Cleanup unused registers (JF & MCL) */
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = MakeSmall(0); 

    w->previous_choice = w->choice;

    CODE_CHOICE_NEW(w->choice, address_nd_current_instance); /* establish skeletal node */
  } else {
    /* Cleanup unused registers */
    X(X5_CHN) = MakeSmall(0);
    X(RootArg) = MakeSmall(0);
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = MakeSmall(0); 
  }

  Release_Cond_lock(root->clause_insertion_cond);

  if (!x2_chain)
    return x5_chain;
  else
    return x2_chain;
}


/* First-solution special case of the above. */
/* ASSERT: X(0) is a dereferenced small integer,
           X(1) is a dereferenced unconditional unbound */
/* Adapted to concurrent predicates (MCL) */
CBOOL__PROTO(first_instance)
{
  int_info_t *root = TaggedToRoot(X(0));
  instance_t *inst;

  if (root->behavior_on_failure == DYNAMIC) 
    inst = ACTIVE_INSTANCE(Arg,root->first,use_clock,TRUE);
  else {
    Cond_Begin(root->clause_insertion_cond);
    inst = root->first;
    Broadcast_Cond(root->clause_insertion_cond);
  }

  if (!inst)
    return FALSE;                                         /* no solutions */
  *TaggedToPointer(X(1)) = PointerToTerm(inst);

  return TRUE;
}

/* ------------------------------------------------------------------
          NEXT_INSTANCE
   -----------------------------------------------------------------------*/

CBOOL__PROTO(next_instance, instance_t **ipp)
{
    CIAO_REG_1(instance_t *, x2_insp);
    CIAO_REG_2(instance_t *, x5_insp);
    CIAO_REG_3(int_info_t *, root);
    instance_clock_t clock = GetSmall(X(4));

    x2_insp = *ipp = TaggedToInstance(X(2));
    x5_insp = TaggedToInstance(X(5));
    root = TaggedToRoot(X(6));

    Wait_Acquire_Cond_lock(root->clause_insertion_cond);

    if (x2_insp == x5_insp)
        x2_insp = x5_insp = ACTIVE_INSTANCE(Arg,x2_insp->forward,clock,TRUE);
    else if (!x2_insp)
    {
    x5_alt:
        *ipp = x5_insp;
        x5_insp = ACTIVE_INSTANCE(Arg,x5_insp->next_forward,clock,FALSE);
    }
    else if (!x5_insp)
    x2_alt:
        x2_insp = ACTIVE_INSTANCE(Arg,x2_insp->next_forward,clock,FALSE);

    else if (x2_insp->rank < x5_insp->rank)
        goto x2_alt;
    else
        goto x5_alt;

    Release_Cond_lock(root->clause_insertion_cond);

    if (!x2_insp && !x5_insp)
        return FALSE;
    else {
      w->choice->x[X2_CHN] = X(X2_CHN) = PointerToTermOrZero(x2_insp);
      w->choice->x[X5_CHN] = X(X5_CHN) = PointerToTermOrZero(x5_insp);
      return  TRUE;
    }
}


#if defined(OLD_DATABASE)
/* ------------------------------------------------------------------
        $CURRENT_KEY/4
   -----------------------------------------------------------------------*/

tagged_t decode_instance_key(instance_t *);

CBOOL__PROTO(current_key)
{
  int_info_t *root = TaggedToRoot(X(0));
  sw_on_key_t *swp = root->indexer;
  intmach_t j = SwitchSize(swp);
  tagged_t mask;

  DEREF(X(2),X(2));
  mask = (IsVar(X(2)) ? 0 : INDEXMASK);
  DEREF(X(3),X(3));
  X(4) = atom_nil;

  if (!IsVar(X(3))){
    if (TaggedIsLST(X(3))) {
      if (ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE))
        MakeLST(X(4),make_structure(Arg,functor_list),X(4));
    } else if (TaggedIsSTR(X(3))) {
      sw_on_key_node_t *hnode =
        incore_gethash(swp,TaggedToHeadfunctor(X(3)));
      instance_t *inst =
        ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

      if (inst && !(hnode->key & QMask)) {
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
      } else
        while (inst){
          intmach_t ar = LargeArity(hnode->key);

          if (HeapCharDifference(w->heap_top,Heap_End)<CONTPAD+ar*sizeof(tagged_t)+3*sizeof(tagged_t)) {
            explicit_heap_overflow(Arg,(CALLPAD+ar*sizeof(tagged_t))*2,5);
          }

          MakeLST(X(4),decode_instance_key(inst),X(4));
          inst = ACTIVE_INSTANCE(Arg,inst->next_forward,use_clock,FALSE);
        }
    } else {
      sw_on_key_node_t *hnode = incore_gethash(swp,X(3));
      instance_t *inst =
        ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

      if (inst)
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
    }
    *TaggedToPointer(X(1)) = X(4);
    return TRUE;
  }

  if (ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE) &&
      (functor_list & mask) == (X(2) & mask))
    MakeLST(X(4),make_structure(Arg,functor_list),X(4));
  for (--j; j>=0; --j) {
    sw_on_key_node_t *hnode = &swp->node[j];
    instance_t *inst =
      ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

    if (!(hnode->key & QMask)){
      if (inst && (hnode->key & mask) == (X(2) & mask)) {
        if (HeapCharDifference(w->heap_top,Heap_End)<CONTPAD+ARITYLIMIT*sizeof(tagged_t)+3*sizeof(tagged_t)) {
          explicit_heap_overflow(Arg,CALLPAD*2,5);
        }

        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
      }
    } else {
      if (IsVar(X(2)) ||
          (TaggedIsSTR(X(2)) && hnode->key==TaggedToHeadfunctor(X(2))))
        while (inst){
          intmach_t ar = LargeArity(hnode->key);

          if (HeapCharDifference(w->heap_top,Heap_End)<CONTPAD+ar*sizeof(tagged_t)+3*sizeof(tagged_t)) {
            explicit_heap_overflow(Arg,(CALLPAD+ar*sizeof(tagged_t))*2,5);
          }

          MakeLST(X(4),decode_instance_key(inst),X(4));
          inst = ACTIVE_INSTANCE(Arg,inst->next_forward,use_clock,FALSE);
        }
    }
  }
  *TaggedToPointer(X(1)) = X(4);
  return TRUE;
}
#endif

#if defined(USE_THREADS)
CBOOL__PROTO(close_predicate)
{
  int_info_t *root = TaggedToRoot(X(0));

  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_OPEN) 
    root->behavior_on_failure = CONC_CLOSED;
  Broadcast_Cond(root->clause_insertion_cond);

  return TRUE;
}

CBOOL__PROTO(open_predicate)
{
  int_info_t *root = TaggedToRoot(X(0));


  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_CLOSED) 
    root->behavior_on_failure = CONC_OPEN;
  Broadcast_Cond(root->clause_insertion_cond);

  return TRUE;
}
#else /* !defined(USE_THREADS) */
CBOOL__PROTO(close_predicate)
{
  return TRUE;
}

CBOOL__PROTO(open_predicate)
{
  return TRUE;
}
#endif


/* Similar to current_instance for concurrent predicates: wait if no clauses
   match and the predicate is not closed; also, handle the cases of
   predicate retraction, assertion, etc. by maintaining and moving around a
   list with the calls pending on every clause of a predicate.  When the end
   of the list is reached and the predicate is still "open" (i.e., more
   clauses might be added to it), the call does not fail: it is instead
   enqueued in a list reacheable from the root of the predicate. (MCL) 

   The case for non-blocking calls is interesting: there are three
   possibilities:

   a) There is no clause for the predicate
   b) There are clauses, but indexing dictates that none of them are 
      applicable for the current call.
   c) There are clauses, and at least one of them can match the current call.

   In cases (a) and (b) a non-blocking call does *not* need choicepoint.
   Case (c) needs a choicepoint, and the clause remains locked while it is
   being executed.  The lock is removed when execution finishes, or when the
   choicepoint is deallocated.

*/

static CFUN__PROTO(current_instance_conc, instance_t *, BlockingType block)
{
  instance_t *x2_n = NULL, *x5_n = NULL;
  instance_t *current_one;
  bool_t try_instance;
  instance_handle_t *x2_next, *x5_next;
  int_info_t *root = TaggedToRoot(X(2));

#if defined(DEBUG)
  if (debug_concchoicepoints) 
    fprintf(stderr, "** Entering current_instance_conc, node = 0x%p, previous_choice = 0x%p, conc. = 0x%p\n",
            w->choice, w->previous_choice, TopConcChpt);
#endif

  do {
    try_instance =
     wait_for_an_instance_pointer(&(root->first), &(root->first), root, block);

 /* That was a non-blocking or concurrent predicate with no instance at all */
    if (!try_instance) {
      Wait_For_Cond_End(root->clause_insertion_cond);
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%" PRIdm "d)(%" PRIdm ") Exiting current_instance_conc with failure, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              w->choice, w->previous_choice, TopConcChpt);
#endif
      return NULL;
    }

/* If we are here, we have a lock for the predicate.  Get first possibly
   matching instance */

    current_one = first_possible_instance(X(0), root, &x2_n, &x5_n);
    if (current_one == NULL) { /* Let others enter and update */
        Wait_For_Cond_End(root->clause_insertion_cond);
    }
  } while (!current_one && block == BLOCK);

  /* Here with (current_one || block == NON_BLOCK).  current_one == NULL
     implies that we do not have a lock --- but then block == NON_BLOCK, and
     therefore we can return with a NULL */
     
  if (!current_one) {
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%" PRIdm ")(%" PRIdm ") Exiting current_instance_conc with failure, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              w->choice, w->previous_choice, TopConcChpt);
#endif
    return NULL;
  }

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_concchoicepoints && 
      Cond_Lock_is_unset(root->clause_insertion_cond))
      fprintf(stderr, 
              "***%" PRIdm "(%" PRIdm ") current_instance_conc: putting chpt without locks!\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
  if (debug_conc && !root->first)
    fprintf(stderr,"*** current_instance_conc: no first instance!\n");
#endif

 /* We do NOT release the clause lock here: we must retain it until we
    finish executing it.  It is released at Prolog level.  The same
    for next instance: when it provides a possible solution, it leaves
    a lock set, to be unset from Prolog.  If the Prolog unification
    fails, the lock is anyway unset from wam(), right before
    failure. */

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_concchoicepoints)
    fprintf(stderr,
            "*** %" PRIdm "(%" PRIdm ") in c_i making chpt (now: node = 0x%p)., inst. is 0x%p\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
            w->choice, current_one);
#endif

    x2_next = make_handle_to(x2_n, root, X(0), X2);
    x5_next = make_handle_to(x5_n, root, X(0), X5);
    X(X2_CHN) = PointerToTermOrZero(x2_next);
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerToTermOrZero(x5_next);

    /* pass root to RETRY_INSTANCE (MCL) */
    X(RootArg) = PointerToTermOrZero(root);  
    X(InvocationAttr) = MakeSmall(0); /* avoid garbage */
    if (block == BLOCK) {
      SET_BLOCKING(X(InvocationAttr));
    } else {
      SET_NONBLOCKING(X(InvocationAttr));
    }
    SET_EXECUTING(X(InvocationAttr));

    /* Save last dynamic top */
    X(PrevDynChpt) = PointerToTermOrZero(TopConcChpt); 
    w->previous_choice = w->choice;
    //
    CODE_CHOICE_NEW(w->choice, address_nd_current_instance); /* establish skeletal node */
    //
    TopConcChpt = (choice_t *)w->choice;  /* Update dynamic top */

#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%" PRIdm ")(%" PRIdm ") Exiting current_instance_conc, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              w->choice, w->previous_choice, TopConcChpt);
#endif

  return current_one;
}

static bool_t wait_for_an_instance_pointer(instance_t **inst_pptr1,
                                           instance_t **inst_pptr2,
                                           int_info_t *root,
                                           BlockingType block)
{

  volatile instance_t *pptr1 = NULL, *pptr2 = NULL;

   /* We have to wait for a new clause only if we are blocking */

    while(TRUE){  
    /* Wait until a change is signaled, and test that the change affects us */

      if (block == BLOCK) {
        Wait_For_Cond_Begin( \
                             ((*inst_pptr1 == NULL) && \
                              (*inst_pptr2 == NULL) && \
                              root->behavior_on_failure == CONC_OPEN ), \
                              root->clause_insertion_cond \
                             )
      } else { /* In any case, leave the predicate locked */
        Cond_Begin(root->clause_insertion_cond);
      }
          
      /* Test again to find out which was the case */
      
    pptr1 = *inst_pptr1;
    pptr2 = *inst_pptr2;
    if (pptr1 || pptr2) 
      return TRUE;
    else if (block == NO_BLOCK || root->behavior_on_failure == CONC_CLOSED)
      return FALSE;
    else Wait_For_Cond_End(root->clause_insertion_cond); /*Let others update*/
    }
}

static instance_t *first_possible_instance(tagged_t x0,
                                           int_info_t *root,
                                           instance_t **x2_n,
                                           instance_t **x5_n)
{
  instance_t *x2_chain, *x5_chain;
  instance_t *x2_next, *x5_next;
  sw_on_key_node_t *hnode;
  tagged_t head;

  head = x0;
  DerefSwitch(head,x0,goto var_case_switch;);

  x2_next = x5_next = NULL;
  if (TaggedIsSTR(head)) {
    DerefArg(head,head,1);
    if (IsVar(head)) {
    var_case_switch:
      x5_chain = x2_chain = root->first;                 /* normal = TRUE */
      if (x2_chain)
        x5_next = x2_next = x2_chain->forward;           /* normal = TRUE */
      else return NULL;
    }
    else if (TaggedIsLST(head)){
      x5_chain = root->lstcase;                         /* normal = FALSE */
    xn_switch:
      x2_chain = root->varcase;                         /* normal = FALSE */
       if (x2_chain && x5_chain) {
        if (x2_chain->rank < x5_chain->rank){
          x2_next = x2_chain->next_forward;             /* normal = FALSE */
          x5_next = x5_chain;
          x5_chain = NULL;
        } else {
          x5_next = x5_chain->next_forward;             /* normal = FALSE */
          x2_next = x2_chain;
          x2_chain = NULL;
        }
      } else if (x2_chain)
        x2_next = x2_chain->next_forward;               /* normal = FALSE */
      else if (x5_chain)
        x5_next = x5_chain->next_forward;               /* normal = FALSE */
      else return NULL;                                    /* No solution */
    } else {
      hnode = TaggedIsSTR(head) ?
              incore_gethash(root->indexer,TaggedToHeadfunctor(head)) :
              incore_gethash(root->indexer,head);
      x5_chain = hnode->value.instp;                    /* normal = FALSE */
      goto xn_switch;
    }
  }
  else goto var_case_switch;

  *x2_n  = x2_next;
  *x5_n  = x5_next;

  return x2_chain ? x2_chain : x5_chain;
}

CBOOL__PROTO(next_instance_conc, instance_t **ipp)
{
  int_info_t *root = TaggedToRoot(X(RootArg));
  BlockingType block;      
  instance_handle_t *x2_ins_h, *x5_ins_h;
  bool_t next_instance_pointer;
  instance_t *x2_insp, *x5_insp;

#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"*** Entering next_instance_conc, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              w->choice, w->previous_choice, TopConcChpt
);
#endif

  /* = X(7) == atom_block ? BLOCK : NO_BLOCK;*/
  block = IS_BLOCKING(X(InvocationAttr)) ? BLOCK : NO_BLOCK; 

  /* When we baktrack after a call which did not finally succeed, the lock
     is still set. Unlock it before proceeding to the next clause. */

  if (EXECUTING(X(InvocationAttr))){
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr, "*** in next_instance_conc changing to nonexecuting\n");
#endif
    SET_NONEXECUTING(X(InvocationAttr));
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
  
  x2_ins_h = TaggedToInstHandle(X(X2_CHN));
  x5_ins_h = TaggedToInstHandle(X(X5_CHN));

/* x2_ins_h->inst_ptr and x5_ins_h->inst_ptr may be both NULL; that means no
   current instance is available.  Just wait for one.  If any of x2_insp or
   x5_insp are NULL pointer, they are automatically enqueued in root by
   change_handle_to_instance */

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc && !root->first)
    fprintf(stderr, 
            "*** %" PRIdm "(%" PRIdm ") in next_instance_conc without first instance.\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
  if (debug_conc) {
    fprintf(stderr,
      "*** %" PRIdm "(%" PRIdm ") in next_instance_conc with x2 = 0x%p, x5 = 0x%p, block = %d\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
            x2_ins_h->inst_ptr, x5_ins_h->inst_ptr, (int)block);
  }
#endif

  do {
    next_instance_pointer =
      wait_for_an_instance_pointer(&(x2_ins_h->inst_ptr), 
                                   &(x5_ins_h->inst_ptr),
                                   root, block);
    if (!next_instance_pointer) {           /* Closed or non-waiting call */
      remove_handle(x2_ins_h, root, X2);
      remove_handle(x5_ins_h, root, X5);
      *ipp = NULL;                                       /* Cause failure */
      /* Time for new assertions */
      Wait_For_Cond_End(root->clause_insertion_cond);
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%" PRIdm ")(%" PRIdm ") Exiting current_instance_conc with failure, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              w->choice, w->previous_choice, TopConcChpt);
#endif
      return FALSE;                                 /* Remove choicepoint */
    }

    /* Locate a satisfactory instance. */
    jump_to_next_instance(x2_ins_h->inst_ptr, x5_ins_h->inst_ptr,
                          ipp, &x2_insp, &x5_insp);

    /* Move handle forwards to re-start (if necesary) in a new clause */
    change_handle_to_instance(x2_ins_h, x2_insp, root, X2);
    change_handle_to_instance(x5_ins_h, x5_insp, root, X5);

#if defined(DEBUG) && defined(USE_THREADS)
    if (debug_conc && !root->first)
      fprintf(stderr, 
              "*** %" PRIdm "(%" PRIdm ") after jumping without first instance.\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
#endif

    if (!*ipp) /* Not instance -> release lock, continue in loop */
      Wait_For_Cond_End(root->clause_insertion_cond);
  } while (!*ipp);

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc && !root->first)
    fprintf(stderr, 
            "*** %" PRIdm "(%" PRIdm ") exiting n_i without first instance.\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
  if (debug_conc)
    fprintf(stderr, 
            "*** %" PRIdm "(%" PRIdm ") exiting n_i with instance 0x%p.\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, *ipp);
#endif  

  /* Here with a possibly matching instance,
     a possibly empty next instance,
     and the lock on the instance. */

  w->choice->x[X2_CHN] = X(X2_CHN) = PointerToTermOrZero(x2_ins_h);
  w->choice->x[X5_CHN] = X(X5_CHN) = PointerToTermOrZero(x5_ins_h);
  SET_EXECUTING(X(InvocationAttr));
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%" PRIdm ")(%" PRIdm ") Exiting current_instance_conc, node = 0x%p, previous_choice = 0x%p, conc. node = 0x%p\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              w->choice, w->previous_choice, TopConcChpt);
#endif

  return TRUE;
}


 /* Current pointers to instances are x2_insp and x5_insp; look for a new
    pointer which presumably matches the query. */

void jump_to_next_instance(x2_insp, x5_insp,
                           ipp, x2_next, x5_next)
    instance_t *x2_insp, *x5_insp, **ipp, **x2_next, **x5_next;
{
  *ipp = *x2_next = x2_insp;
  *x5_next = x5_insp;

  if (!x2_insp && !x5_insp)                                        /* MCL */
    return;

  if (x2_insp == x5_insp)
    x2_insp = x5_insp = x2_insp->forward;               /* normal = TRUE */
  else if (!x2_insp) {
  x5_alt:
    *ipp = x5_insp;
    x5_insp = x5_insp->next_forward;                    /* normal = FALSE */
  } else if (!x5_insp)
    x2_alt:
  x2_insp = x2_insp->next_forward;                      /* normal = FALSE */
  else if (x2_insp->rank < x5_insp->rank)
    goto x2_alt;
  else
    goto x5_alt;

  *x2_next = x2_insp;
  *x5_next = x5_insp;
}


/* ------------------------------------------------------------------------- */
/* Add an invocation as pending from an instance; if there is anyone else
   pending on that instance, add ourselves to the list.

   inst: Pending to here
   root: Predicate root
   head:
   chain: Is that X2 or X5?
*/

instance_handle_t *make_handle_to(instance_t *inst,
                                  int_info_t *root,
                                  tagged_t head,
                                  WhichChain chain)
{
  instance_handle_t *this_handle;
                                                     /* Create the handle */
  this_handle = checkalloc_TYPE(instance_handle_t);

  this_handle->head = head;                        /* Allow re-indexation */
  link_handle(this_handle, inst, root, chain);
#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr, 
            "*** %" PRIdm "(%" PRIdm ") made handle 0x%p to instance 0x%p\n", 
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, this_handle, inst);
#endif

  return this_handle;
}


/* Remove handle from list.  xi might be pointed to directly from the root,
   or from an instance record: need to update the pointer itself. */

void remove_handle(instance_handle_t *xi,
                   int_info_t *root,
                   WhichChain chain)
{
  unlink_handle(xi, root, chain);
  checkdealloc_TYPE(instance_handle_t, xi);

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") removed handle 0x%p (to instance 0x%p)\n", 
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, xi, xi->inst_ptr);
#endif

}



/* Make a handle to point to a new instance. */

static void change_handle_to_instance(instance_handle_t *handle,
                                      instance_t *new_inst,
                                      int_info_t *root,
                                      WhichChain chain)
{
  if (handle->inst_ptr != new_inst) {      /* Do not move if not necessary */
#if defined(DEBUG)
    if (debug_conc)
      fprintf(stderr, 
              "*** %" PRIdm "(%" PRIdm ") changes handle 0x%p from instance 0x%p to 0x%p\n", 
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER,
              handle, handle->inst_ptr, new_inst);
#endif
    unlink_handle(handle, root, chain);
    link_handle(handle, new_inst, root, chain);
  }
}

/*
  inst: Pending to here
  root: Predicate root
  chain: Is that X2 or X5?
 */
static void link_handle(instance_handle_t *handle,
                        instance_t *inst,
                        int_info_t *root,
                        WhichChain chain)
{
#if defined(DEBUG) 
  if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** Thread %" PRIdm "(%" PRIdm ") in link_handle() with lock unset!\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
#endif

  handle->inst_ptr = inst;             /* Instance we are looking at */
  handle->previous_handle = NULL;
  if (inst) {           /* Non-null instances go either to X2 or to X5... */
    if (chain == X2) {
      handle->next_handle = inst->pending_x2;
      inst->pending_x2 = handle;
    } else {
      handle->next_handle = inst->pending_x5;
      inst->pending_x5 = handle;
    }
  } else {                    /* handles to NULL instances go to the root */
    if (chain == X2) {
      handle->next_handle = root->x2_pending_on_instance;
      root->x2_pending_on_instance = handle;
    } else {
      handle->next_handle = root->x5_pending_on_instance;
      root->x5_pending_on_instance = handle;
    }
  }
  if (handle->next_handle)
    handle->next_handle->previous_handle = handle;
}

static void unlink_handle(instance_handle_t *xi,
                          int_info_t *root,
                          WhichChain chain)
{
  instance_t *inst;

#if defined(DEBUG)
  if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** Thread_Id %" PRIdm "(%" PRIdm ") in unlink_handle() with lock unset!\n", 
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
#endif

/* A handle is enqueued in X2 (unindexed links) or X5 (indexed links) iff it
   has a non-null instance pointer; otherwise, it must be enqueued in the
   root queue. */

  if ((inst = xi->inst_ptr)) {

    if (chain == X2 && inst->pending_x2 == xi)          /* First in queue */
      inst->pending_x2 = xi->next_handle;

    if (chain == X5 && inst->pending_x5 == xi)
      inst->pending_x5 = xi->next_handle;

  } else if (chain == X2) {             /* xi->inst_ptr is a NULL pointer */
    if (root->x2_pending_on_instance == xi)
      root->x2_pending_on_instance = xi->next_handle;
  } else {
    if (root->x5_pending_on_instance == xi)
      root->x5_pending_on_instance = xi->next_handle;
  }

  if (xi->next_handle)
    xi->next_handle->previous_handle = xi->previous_handle;
  if (xi->previous_handle)
    xi->previous_handle->next_handle = xi->next_handle;
}

 /* Move all elements of a queue to another queue, and make all of them to
    point to the instance destinst */

void move_queue(instance_handle_t **srcq,
                instance_handle_t **destq,
                instance_t *destinst)
{
  instance_handle_t *last, *running = *srcq;

#if defined(DEBUG) && defined(USE_THREADS)
  int_info_t *root =
    *srcq && (*srcq)->inst_ptr ? (*srcq)->inst_ptr->root : NULL;
  int counter = 0;

  if (debug_conc && root && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** in move_queue() with lock unset!\n");

  if (debug_conc)
    fprintf(stderr,
            "*** %" PRIdm "(%" PRIdm ") moving queue from 0x%p to 0x%p (-> instance 0x%p)\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
            srcq, destq, destinst);
#endif

  if (running){
    while(running) {
#if defined(DEBUG) && defined(USE_THREADS)
      counter++;
#endif
      running->inst_ptr = destinst;
      last = running;
      running = running->next_handle;
    }
    last->next_handle = *destq;
    if (last->next_handle)
      last->next_handle->previous_handle = last;
    *destq = *srcq;
    *srcq = NULL;
  }
#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr,
            "*** %" PRIdm "(%" PRIdm ") after moving queue made %d steps\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, counter);
#endif
}


/* Remove the linked chains which point to the calls to concurrent
   predicates which were suspended.  Start at topdynamic (topmost dynamic
   choicepoint) and go down the choicepoint stack until the next dynamic
   choicepoint to be considered is older than chpttoclear.  Then, return the
   value of that dynamic choicepoint in the variable topdynamic (it is the
   topmost dynamic choicepoint after the call!). */

void remove_link_chains(choice_t **topdynamic,
                        choice_t *chpttoclear)
{
  choice_t *movingtop = *topdynamic;
#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") removing from 0x%p until 0x%p\n", 
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
            *topdynamic,
            chpttoclear);
#endif
  
  while (ChoiceYounger(movingtop, chpttoclear)){
#if defined(DEBUG) && defined(USE_THREADS)
    if (debug_conc)
      fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") removing handle at (dynamic) node 0x%p\n", 
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
              movingtop);
#endif

    Cond_Begin(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

#if defined(DEBUG)
    if (TaggedToInstHandle(movingtop->x[X2_CHN]) == NULL)
      fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") remove_link_chains: X2 handle is NULL!!\n",
              (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
    if (TaggedToInstHandle(movingtop->x[X5_CHN]) == NULL)
      fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") remove_link_chains: X5 handle is NULL!!\n", (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER);
#endif
    remove_handle(TaggedToInstHandle(movingtop->x[X2_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X2);
    remove_handle(TaggedToInstHandle(movingtop->x[X5_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X5);

    Broadcast_Cond(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

    movingtop=TermToPointerOrNull(choice_t, movingtop->x[PrevDynChpt]);
  }
#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %" PRIdm "(%" PRIdm ") remove_link_chains: done at 0x%p\n", 
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, 
            movingtop);
#endif
  *topdynamic = movingtop;
}

/* --------------------------------------------------------------------------- */

static void relocate_table_clocks(sw_on_key_t *sw, instance_clock_t *clocks);

/* MCL: make sure we have a lock before expunging instances; this might 
   be done concurrently */

CBOOL__PROTO(prolog_purge)
{
  instance_t *inst;
  intmach_t current_mem = total_mem_count;
  
  DEREF(X(0),X(0));
  inst = TaggedToInstance(X(0));


  Cond_Begin(inst->root->clause_insertion_cond);
  expunge_instance(inst);
  Broadcast_Cond(inst->root->clause_insertion_cond);

  INC_MEM_PROG(total_mem_count - current_mem);

  return TRUE;
}

 /* Erase an instance. In the case of instances from concurrent predicates
    which are being pointed at, move the handle to the next available
    instance.  For now, fail if no matching instances are available.  When
    called for a concurrent predicate, it must be protected by a clause lock
    set at Prolog level.  Memory accounting: delay until we are done with
    concurrency-related pointer juggling. */

#define InstOrNull(Handle) Handle ? Handle->inst_ptr : NULL

CBOOL__PROTO(prolog_erase)
{
  instance_t *node;
  int_info_t *root;
  intmach_t current_mem;

#if defined(USE_THREADS)
  instance_handle_t *x2_ins_h, *x5_ins_h;
  instance_t *ipp, *x2_insp, *x5_insp;
#endif

  DEREF(X(0),X(0));
  node = TaggedToInstance(X(0));
  root = node->root;

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc) {
    fprintf(stderr, "*** %d(%d) entering prolog_erase()!\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
    if (!root->first)
      fprintf(stderr, "*** %d(%d) prolog_erase() without first instance!\n",
              (int)Thread_Id, (int)GET_INC_COUNTER);
  }
#endif

#if defined(USE_THREADS)                                               /* MCL */

 /* An instance is about to be deleted.  If the predicate is
    concurrent, and there are calls pointing at that instance, move
    the queue of pending calls to the new available instance.  In
    order to choose which clause is to be pointed at, any handle is
    equally valid; we use the first one.  This call must not block if
    no next instance exists: blocking is performed by
    '$current_instance'/1 . */

  if (root->behavior_on_failure != DYNAMIC) {
#if defined(DEBUG) && defined(USE_THREADS)
    if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
     fprintf(stderr, "prolog_erase: entering for conc. pred. without lock!\n");
#endif
    x2_ins_h = node->pending_x2;
    x5_ins_h = node->pending_x5;
    if (x2_ins_h || x5_ins_h) {
      jump_to_next_instance(InstOrNull(x2_ins_h), 
                            InstOrNull(x5_ins_h), 
                            &ipp, &x2_insp, &x5_insp);

#if defined(DEBUG) && defined(USE_THREADS)
      if (debug_conc)
        fprintf(stderr,
                "*** %d(%d) moving handles hanging from %x\n",
                (int)Thread_Id, (int)GET_INC_COUNTER, (int)node);
#endif

      if (ipp && (x2_insp || x5_insp)) {
        /* Make all the queues point to the next instance.  if x2_insp or
           x5_insp are null, make both to point to the same queue */
        if (x2_insp && x5_insp) {
          move_queue(&node->pending_x2, &x2_insp->pending_x2, x2_insp);
          move_queue(&node->pending_x5, &x5_insp->pending_x5, x5_insp);
        } else if (!x2_insp) {
          move_queue(&node->pending_x2, &x5_insp->pending_x5, x5_insp);
          move_queue(&node->pending_x5, &x5_insp->pending_x5, x5_insp);
        } else if (!x5_insp) {
          move_queue(&node->pending_x2, &x2_insp->pending_x2, x2_insp);
          move_queue(&node->pending_x5, &x2_insp->pending_x2, x2_insp);
        }
      } else {
        /* No next instance.  If the predicate is still concurrent,
           enqueue the list at the root and make it wait --- set the
           instance pointed to to NULL.  If the predicate is not
           concurrent, the call will faill because of the NULL
           pointer, and the handles will by deallocated by it.  */
        move_queue(&node->pending_x2, &root->x2_pending_on_instance, NULL);
        move_queue(&node->pending_x5, &root->x5_pending_on_instance, NULL);
      }
    }
  }
#endif

  current_mem = total_mem_count;
  node->death = use_clock = def_clock;
  if (root->behavior_on_failure != DYNAMIC ||                      /* MCL */
      node->birth == node->death)
    expunge_instance(node);
  else
    (void)active_instance(Arg,node,use_clock,TRUE);

  INC_MEM_PROG(total_mem_count - current_mem);

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc) 
    fprintf(stderr, "*** %d(%d) exiting prolog_erase()!\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
#endif

  return TRUE;
}


/*-----------------------------------------------------------------*/

/* Convert between internal and external instance ID's, i.e.
 * between integers and '$ref'(_,_).
 */
CBOOL__PROTO(prolog_ptr_ref)
{
  DEREF(X(0),X(0));
  if (TaggedIsSmall(X(0)))
    {
      tagged_t *pt1 = w->heap_top;

      HeapPush(pt1,functor_Dref);
      HeapPush(pt1,X(0));
      HeapPush(pt1,TaggedToInstance(X(0))->rank);
      w->heap_top=pt1;
      CBOOL__LASTUNIFY(Tagp(STR,HeapOffset(pt1,-3)),X(1));
    }
  else
    {
      tagged_t x1, x2;
      instance_t *n;

      x2=X(1); DerefSwitch(x2,x1,;);
      if (!TaggedIsSTR(x2) || (TaggedToHeadfunctor(x2) != functor_Dref))
        return FALSE;

      DerefArg(x1,x2,1);
      DerefArg(x2,x2,2);
      if (!TaggedIsSmall(x1) ||
          !(n=TaggedToInstance(x1)) ||
           n->rank != x2 ||
           n->death != 0xffff)    
        return FALSE;

      CBOOL__UnifyCons(PointerToTerm(n),X(0));
      return TRUE;
    }
}

/* ASSERT: X(0) is a dereferenced integer.  

   If the predicate is concurrent, it is still open, it has no clauses
   (i.e., this first clause is also the last one) and there are invocations
   waiting for a clause, wake them up and make them point to the new clause.
   Unfortunately, this results in a lack of indexing. */

CBOOL__PROTO(inserta)
{
    instance_t *n, **loc;
    int_info_t *root = TaggedToRoot(X(0));
    intmach_t current_mem = total_mem_count;
#if defined(USE_THREADS)                                               /* MCL */
    bool_t move_insts_to_new_clause = FALSE;
#endif

    Cond_Begin(root->clause_insertion_cond);

#if defined(DEBUG)
    if (debug_conc) fprintf(stderr,
              "*** %d(%d) in inserta (root = %x, first = %x, &first = %x)\n",
              (int)Thread_Id, (int)GET_INC_COUNTER, (unsigned int)root,
              (unsigned int)root->first, (unsigned int)&(root->first));
#endif

#if defined(USE_THREADS)
    if (root->behavior_on_failure == CONC_CLOSED){                 /* MCL */
      Broadcast_Cond(root->clause_insertion_cond);
      USAGE_FAULT("$inserta in an already closed concurrent predicate");
    }
#endif

    DEREF(X(1),X(1));
    n = TaggedToInstance(X(1));
    
    /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
    if (!root->first){
      n->rank = TaggedZero;
      n->forward = NULL;
      n->backward = n;
#if defined(USE_THREADS)
      if (root->behavior_on_failure == CONC_OPEN)
        move_insts_to_new_clause = TRUE;    /* 'n' will be the new clause */
#endif
    } else if (root->first->rank == TaggedLow)
      SERIOUS_FAULT("database node full in assert or record")
    else {
      n->rank = root->first->rank-MakeSmallDiff(1);
      n->forward = root->first;
      n->backward = root->first->backward;
      root->first->backward = n;
    }
    root->first = n;    
    
    n->root = root;
    n->birth = use_clock = def_clock;
    n->death = 0xffff;

#if defined(USE_THREADS)                                               /* MCL */
    n->pending_x5 = n->pending_x2 = NULL;
#endif
    
    loc = (n->key==ERRORTAG ? &root->varcase :
           n->key==functor_list ? &root->lstcase :
           &dyn_puthash(&root->indexer,n->key)->value.instp);
    
    if (!(*loc))
        n->next_forward = NULL, n->next_backward = n;
    else
        n->next_forward = (*loc),
        n->next_backward = (*loc)->next_backward,
        (*loc)->next_backward = n;
    (*loc) = n;
    
#if defined(USE_THREADS)
    if (move_insts_to_new_clause) {
      if (root->x2_pending_on_instance)
        move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
      if (root->x5_pending_on_instance)
        move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
    }
#endif

#if defined(DEBUG)
    if (debug_conc)
      fprintf(stderr,
           "*** %d(%d) leaving inserta (root = %x, first = %x, &first = %x)\n",
              (int)Thread_Id, (int)GET_INC_COUNTER, (unsigned int)root,
              (unsigned int)root->first, (unsigned int)&(root->first));
#endif

    Broadcast_Cond(root->clause_insertion_cond);
    
    INC_MEM_PROG(total_mem_count - current_mem);
    return TRUE;
}


/* ASSERT: X(0) is a dereferenced integer */
CBOOL__PROTO(insertz)
{
    instance_t *n, **loc;
    int_info_t *root = TaggedToRoot(X(0));
    intmach_t current_mem = total_mem_count;

    Cond_Begin(root->clause_insertion_cond);

#if defined(DEBUG)
    if (debug_conc)
      fprintf(stderr,
              "*** %d(%d) in insertz (root = %x, first = %x, &first = %x)\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (unsigned int)root,
              (unsigned int)root->first,
              (unsigned int)&(root->first));
#endif

    if (root->behavior_on_failure == CONC_CLOSED){                 /* MCL */
      Broadcast_Cond(root->clause_insertion_cond);
      USAGE_FAULT("$inserta in an already closed concurrent predicate");
    }

    DEREF(X(1),X(1));
    n = TaggedToInstance(X(1));
    
    /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
    if (!root->first){
      n->rank = TaggedZero;
      n->backward = n;
      root->first = n;
    }
    else if (root->first->backward->rank == TaggedIntMax)
      SERIOUS_FAULT("database node full in assert or record")
    else {
      n->rank = root->first->backward->rank+MakeSmallDiff(1);
      n->backward = root->first->backward;
      root->first->backward->forward = n;
      root->first->backward = n;
    }

    n->root = root;
    n->birth = use_clock = def_clock;
    n->death = 0xffff;
    n->forward = NULL;
    n->next_forward = NULL;

#if defined(USE_THREADS)                                               /* MCL */
    n->pending_x5 = n->pending_x2 = NULL;
#endif

    loc = (n->key==ERRORTAG ? &root->varcase :
           n->key==functor_list ? &root->lstcase :
           &dyn_puthash(&root->indexer,n->key)->value.instp);
    
    if (!(*loc)) {
        n->next_backward = n;
        (*loc) = n;
    } else {
      n->next_backward = (*loc)->next_backward;
      (*loc)->next_backward->next_forward = n;
      (*loc)->next_backward = n;
    }

#if defined(DEBUG) && defined(USE_THREADS)
    if (debug_conc && root->behavior_on_failure != DYNAMIC)
      fprintf(stderr, "*** %d(%d) insertz'ed clause %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER, (int)n);
#endif
    
#if defined(USE_THREADS)
    if (root->behavior_on_failure == CONC_OPEN){
      if (root->x2_pending_on_instance)
        move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
      if (root->x5_pending_on_instance)
        move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
    }
#endif

#if defined(DEBUG)
    if (debug_conc)
      fprintf(stderr,
              "*** %d(%d) leaving insertz (root = %x, first = %x, &first = %x)\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (unsigned int)root,
              (unsigned int)root->first,
              (unsigned int)&(root->first));
#endif

    Broadcast_Cond(root->clause_insertion_cond);

    INC_MEM_PROG(total_mem_count - current_mem);
    return TRUE;
}

/* ------------------------------------------------------------------------- */

/* TODO: reuse for copying Large (see globalize_bn) */
size_t compile_large(tagged_t t, bcp_t p) {
  intmach_t i;
  intmach_t ar = LargeArity(TaggedToHeadfunctor(t));
  tagged_t *tp = TagpPtr(STR,t);
  tagged_t *pp = (tagged_t *)p;

  for (i = 0; i < ar; i++)
    *pp++ = *tp++;
  return ar*sizeof(tagged_t);
}

#if BC_SCALE==2
/* Copy a large into bytecode, scaling if needed (see
   bn_scale_bc32()) */
size_t compile_large_bc32(tagged_t t, bcp_t p) {
  intmach_t sz;
  // fprintf(stderr, "trace: compile_large_bc32\n");
  if (TaggedIsSmall(t)) {
    /* Force into a large, even if it fits in a small */
    // fprintf(stderr, "trace: bc32 large stored as small needs fix\n");
    intmach_t i = GetSmall(t);
    if (IsInSmiValRange_BC32(i)) {
      SERIOUS_FAULT("compile_large_bc32: int32 found in large!");
    }
    tagged_t xx[2];
    xx[0] = MakeFunctorFix;
    xx[1] = i;
    tagged_t t1 = Tagp(STR, xx);
    (void)compile_large(t1, p);
    sz = bn_scale_bc32((bignum_t *)p);
  } else if (LargeIsFloat(t)) {
    sz = compile_large(t, p);
  } else {
    (void)compile_large(t, p);
    sz = bn_scale_bc32((bignum_t *)p);
  }
  return sz;
}
#endif

CBOOL__PROTO(make_bytecode_object)
{
  tagged_t num,list;
  emul_info_t *object;
  bcp_t P;
#if defined(GAUGE)
  tagged_t num1;
#endif
  /*unsigned int counter_cnt;*/
  /*intmach_t *current_counter;*/
  /*int i;*/
  intmach_t current_mem = total_mem_count;
  intmach_t bsize;

  DEREF(num,X(0));              /* Must be PHYSICAL size in characters! */
#if defined(GAUGE)
  DEREF(num1,X(1));             /* Number of Counters */
#endif
  DEREF(list,X(2));

  bsize = TaggedToIntmach(num) * BC_SCALE;

#if defined(GAUGE)
  counter_cnt = TaggedToIntmach(num1);
  checkalloc_FLEXIBLE_S(emul_info_t,
                        objsize,
                        char,
                        (bsize + counter_cnt*sizeof(intmach_t)),
                        object);
#else
  checkalloc_FLEXIBLE_S(emul_info_t,
                        objsize,
                        char,
                        bsize,
                        object);
#endif

  object->next.ptr = NULL;
  object->subdefs = NULL;
#if defined(GAUGE)
  object->counters = (intmach_t *)((char *)object+object->objsize)-counter_cnt;
  for (i=0; i<counter_cnt; i++)
    object->counters[i] = 0;
  current_counter = object->counters + 2; /* Entry Counters */
#endif
  P = (bcp_t)object->emulcode;
  while (list!=atom_nil) {
    tagged_t car;

    DerefCar(car,list);
    DerefCdr(list,list);
    switch(TagOf(car)) {
    case NUM: /* TODO: assumes that f_o, f_x, f_y, etc. have all the same size */
      {
        EMIT_o((FTYPE_ctype(f_o))GetSmall(car));
        break;
      }
#if defined(GAUGE)
    case ATM:
      {
        if (car == atom_counter) {
          /* NOTE: this is a pointer */
          EMITtok(f_counter, (char *)current_counter);
          current_counter++;
          --counter_cnt;
        } else {
          USAGE_FAULT("make_bytecode_object: bad spec");
        }
        break;
      }
#endif
    case STR:
      {
        tagged_t func;
        
        func=TaggedToHeadfunctor(car);
        if(func==functor_functor) {
          /* functor(Name/Arity) */
          DerefArg(car,car,1);
          if (TaggedIsSTR(car) && (TaggedToHeadfunctor(car)==functor_slash)) {
            tagged_t t1, t2;
            DerefArg(t1,car,1);
            DerefArg(t2,car,2);
            EMIT_f(SetArity(t1,GetSmall(t2)));
          }
          break;
        }
        
        if(func==functor_tagged) {
          /* TAGGED(Term) */
          tagged_t t;
          DerefArg(t,car,1);
          EMIT_t(t);
          break;
        }
        if(func==functor_emul_entry) {
          /* label(PredicateSpec) */
          DerefArg(car,car,1);
          EMIT_E(parse_definition(car));
          break;
        }
        if(func==functor_builtin) {
          /* builtin(Integer) */
          tagged_t t1;
          DerefArg(t1,car,1);
          EMIT_C(builtintab[GetSmall(t1)]);
          break;
        }
        if (func==functor_large) {
          DerefArg(car,car,1);
          int sz;
#if BC_SCALE==2
          sz = compile_large_bc32(car, P);
#else
          sz = compile_large(car, P);
#endif
          P = BCoff(P, sz);
          break;
        }
        if(func==functor_long) {
          /* long(Num) */
          tagged_t t1;
          DerefArg(t1,car,1);
          EMIT_l(TaggedToIntmach(t1));
          break;
        }
        USAGE_FAULT("make_bytecode_object: bad spec");
      }
    }
  }

  ptrdiff_t truesize = (char *)P - (char *)object->emulcode;
  if (truesize > bsize) {
    SERIOUS_FAULT("bug: memory overrun in make_bytecode_object()");
  }

/* TODO: rename by patch_bytecode32 */
#if defined(BC64)
#define USE_REWRITE_BYTECODE 1
#endif
#if defined(USE_REWRITE_BYTECODE)
  CVOID__PROTO(bytecode_rewrite, bcp_t begin, bcp_t end);
  CVOID__CALL(bytecode_rewrite, object->emulcode, P);
#endif

#if defined(GAUGE)
  if (counter_cnt != 2)
    SERIOUS_FAULT("$make_bytecode_object: counter counts don't match");
#endif
  CBOOL__UnifyCons(PointerToTerm(object),X(3));
  INC_MEM_PROG(total_mem_count - current_mem);
  return TRUE;
}


/* A LOGICAL VIEW OF DYNAMIC CODE UPDATES.
   Scheme adapted from
        T. Lindholm, R. A. O'Keefe, ``Efficient Implementation of a
        Defensible Semantics for Dynamic PROLOG Code'', Proc. 4th International
        Conference on Logic Programming, Melbourne, 1987.
   All modifications are my own inventions.
   Mats Carlsson.

   The idea is that an invocation of a dynamic predicate should not be
   affected by asserts/retracts until that invocation has finitely failed.
   This goes for record/recorded as well.

   Implementation: Each instance has an interval [birth,death) and there are
   two clocks DC and UC, initially zero.
   to add an instance: set its interval to [DC,0xffff), set UC=DC.
   to erase an instance: set its death to DC, set UC=DC,
                         if (birth==death) reclaim space.
   to use an instance: UC must be in its interval.
   to create a dynamic chpt: save UC in chpt, set DC=UC+1,
                             if (DC==0xffff) clock_overflow().

   Reclamation of space is "lazy": whenever we come across a doomed 
   instance, we try to delete a sequence of doomed instances.
   We have to inspect the chpt stack to find out whether it's safe
   to delete.

   N.B.  Instances with an empty interval can be deleted right away,
         provided that dynamic chpts take care to set the "next alt."
         at an instance relevant for the chpt.

   To reduce scanning costs, the chpts are marked as "static" up to
   the first dynamic chpt, starting from the root.
*/

/* Given an instance and a time T, skip over a sequence of instances
   not active at time T.  Return first active or NULL.
   While skipping, deallocate DEAD instances satisfying:
   either (1a) it is not downstream of any chpt for same root, or
          (1b) it was born after any such chpt, or
          (1c) it died before any such chpt.
          */

/*
  normal: if normal==TRUE follow first-chain else next_forward
*/

CFUN__PROTO(active_instance,
            instance_t *,
            instance_t *i,
            int itime,
            bool_t normal)
{
  CIAO_REG_2(choice_t *, b);
  CIAO_REG_3(instance_t *, j);
  choice_t *b2;
  choice_t *latest_static = w->choice;
  instance_clock_t time = itime;
  instance_clock_t lotime = time;
  tagged_t lorank = TaggedIntMax;

#if defined(OPTIM_COMP) || defined(USE_DEEP_FLAGS)
  /* ensure that w->choice is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* We are still in "shallow mode" */
  /* Pre: !IsDeep() */
  CODE_MAYBE_NECK_TRY();
#endif

  if (latest_static->next_alt == NULL) { /* if called from wam() */ // TODO: IsShallowTry0(latest_static)? // TODO: [oc-merge] disable, never happens with 'flags'
    latest_static = w->previous_choice;
  }

  for (b=latest_static; !ChoiceptTestStatic(b); b=b2)  {
    b2=ChoiceCont(b);
    if (b->next_alt==address_nd_current_instance) {
      latest_static = b2;
      j = TaggedToInstance(b->x[2]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[4]);
        if (lorank>j->rank) lorank=j->rank;
      }
      j = TaggedToInstance(b->x[5]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[4]);
        if (lorank>j->rank) lorank=j->rank;
      }   
    }
  }
  
                          /* Mark all chpt before latest_static as static */

  for (b=latest_static;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b))
    ChoiceptMarkStatic(b);
  
  if (normal) {                                 /* Follow forward-chain ? */
    while (i &&
           i->death != 0xffff &&
           (lotime >= i->death ||
            time < i->birth ||
            (time >= i->death && lorank > i->rank)))  {
      j=i->forward;
      expunge_instance(i);
      i=j;
    }
    
    while (i && (time < i->birth || time >= i->death)) i=i->forward;
  }
  else {                                   /* follow next_forward-chain ! */
    while (i &&
           i->death != 0xffff &&
           (lotime >= i->death ||
            time < i->birth ||
            (time >= i->death && lorank > i->rank))) {
      j=i->next_forward;
      expunge_instance(i);
      i=j;
    }
    
    while (i && (time < i->birth || time >= i->death)) i=i->next_forward;
  }
  return i;
}



/* Called from wam() when X(4) = use_clock = 0xfffe;
   All timestamps have to be compressed.
   Collect in T0..Tn distinct clock values existing in choicepoints,
   counting Tn=0xfffe.  Then compress all values in choicepoints and
   instances as:
   
   x in [0..T0] => 0
   x in (T0..T1] => 1
   ...

   Instances may get an empty lifetime; then they are expunged.

   Set use_clock = X(4) = n,
       def_clock = n+1
*/

CVOID__PROTO(clock_overflow)
{
  instance_clock_t *clocks, *clockp;
  instance_clock_t t, current = 0xfffe;
  int count = 1;
  choice_t *b;

#if defined(DEBUG) && defined(USE_THREADS)
  if (debug_conc)
    fprintf(stderr, "*** in clock_overflow()\n");
#endif

  /* count # distinct clock values existing in choicepoints */
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    if (b->next_alt==address_nd_current_instance) {
      t = GetSmall(b->x[4]);
      if (current!=t) {
        current=t;
        count++;
      }
    }
  }

  /* grab space for temporary array of clock values */
  TEST_HEAP_OVERFLOW(G->heap_top, count*sizeof(instance_clock_t), DynamicPreserved);
  clocks = (instance_clock_t *)w->heap_top;

  /* fill in distinct chpt clock values, relocating them as we go */
  clockp = clocks+count;
  *(--clockp) = 0xfffe;
  def_clock = count;
  use_clock = count-1;
  X(4) = MakeSmall(count-1);
  
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b))
    if (b->next_alt==address_nd_current_instance) {
      t = GetSmall(b->x[4]);
      if ((*clockp)!=t) *(--clockp)=t;
      b->x[4] = MakeSmall(clockp-clocks);
    }
  /* relocate all instance clocks */
  relocate_table_clocks(prolog_predicates,clocks);
  relocate_gcdef_clocks(clocks);
}

static void relocate_table_clocks(sw_on_key_t *sw,
                                  instance_clock_t *clocks)
{
  sw_on_key_node_t *keyval;
  definition_t *d;
  intmach_t j = SwitchSize(sw);
  
  for (--j; j>=0; --j) {
    keyval = &sw->node[j];
    if ((d = keyval->value.def) &&
        d->predtyp==ENTER_INTERPRETED)
      relocate_clocks(d->code.intinfo->first,clocks);   
  }
}

void relocate_clocks(instance_t *inst,
                     instance_clock_t *clocks)
{
  int i, j;
  instance_t *next;

  for (; inst; inst=next)
    {
      next = inst->forward;
      for (i=0; inst->birth>clocks[i]; i++)
        ;
      inst->birth = i;
      if (inst->death!=0xffff)
        {
          for (j=i; inst->death>clocks[j]; j++)
            ;
          inst->death = j;
          if (i==j)
            expunge_instance(inst);
        }
    }
}


void expunge_instance(instance_t *i)
{
    instance_t **loc;
    int_info_t *root = i->root;

#if defined(DEBUG)  && defined(USE_THREADS)
    if (root->behavior_on_failure != DYNAMIC && debug_conc) 
        fprintf(stderr, "*** %d(%d) expunge_instance: deleting instance %x!\n",
                (int)Thread_Id, (int)GET_INC_COUNTER, (int)i);
      if (root->behavior_on_failure != DYNAMIC &&
          Cond_Lock_is_unset(root->clause_insertion_cond))
        fprintf(stderr,
                "*** %d(%d) expunge_instance: lock not set!\n",
                (int)Thread_Id, (int)GET_INC_COUNTER);
      if (!root->first)
        fprintf(stderr, "*** %d(%d) expunge_instance: no first instance!\n",
                (int)Thread_Id, (int)GET_INC_COUNTER);
#endif
  
    if (!i->forward)            /* last ? */
        root->first->backward = i->backward;
    else
        i->forward->backward = i->backward;

    if (i == root->first)       /* first ? */
        root->first = i->forward;
    else
      i->backward->forward = i->forward;
    
    loc = (i->key==ERRORTAG ? &root->varcase :
           i->key==functor_list ? &root->lstcase :
           &incore_gethash(root->indexer,i->key)->value.instp);
  
    if (!i->next_forward)       /* last ? */
        (*loc)->next_backward = i->next_backward;
    else
        i->next_forward->next_backward = i->next_backward;
    
    if (i == (*loc))            /* first ? */
        (*loc) = i->next_forward;
    else
        i->next_backward->next_forward = i->next_forward;
    
    i->rank = ERRORTAG;

    checkdealloc_FLEXIBLE_S(instance_t, objsize, i);
}
