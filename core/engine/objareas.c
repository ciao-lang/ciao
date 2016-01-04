/*
 *  objareas.c
 *
 *  Database management support code.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <unistd.h>
#include <stddef.h> /* ptrdiff_t */
#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/initial.h>
#include <ciao/wam_alloc.h>

#include <ciao/nondet.h>
#include <ciao/objareas.h>
#include <ciao/bignum.h>
#include <ciao/stacks.h>
#include <ciao/indexing.h>
#include <ciao/alloc.h>
#include <ciao/locks.h>
#include <ciao/start.h>

static void relocate_table_clocks(sw_on_key_t *sw, instance_clock_t *clocks);

/* MCL: make sure we have a lock before expunging instances; this might 
   be done concurrently */

CBOOL__PROTO(prolog_purge)
{
  instance_t *inst;
  intmach_t current_mem = total_mem_count;
  
  DEREF(X(0),X(0));
  inst = TagToInstance(X(0));


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

#if defined(THREADS)
  instance_handle_t *x2_ins_h, *x5_ins_h;
  instance_t *ipp, *x2_insp, *x5_insp;
#endif

  DEREF(X(0),X(0));
  node = TagToInstance(X(0));
  root = node->root;

#if defined(DEBUG) && defined(THREADS)
  if (debug_conc) {
    fprintf(stderr, "*** %d(%d) entering prolog_erase()!\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
    if (!root->first)
      fprintf(stderr, "*** %d(%d) prolog_erase() without first instance!\n",
              (int)Thread_Id, (int)GET_INC_COUNTER);
  }
#endif

#if defined(THREADS)                                               /* MCL */

 /* An instance is about to be deleted.  If the predicate is
    concurrent, and there are calls pointing at that instance, move
    the queue of pending calls to the new available instance.  In
    order to choose which clause is to be pointed at, any handle is
    equally valid; we use the first one.  This call must not block if
    no next instance exists: blocking is performed by
    '$current_instance'/1 . */

  if (root->behavior_on_failure != DYNAMIC) {
#if defined(DEBUG) && defined(THREADS)
    if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
     fprintf(stderr, "prolog_erase: entering for conc. pred. without lock!\n");
#endif
    x2_ins_h = node->pending_x2;
    x5_ins_h = node->pending_x5;
    if (x2_ins_h || x5_ins_h) {
      jump_to_next_instance(InstOrNull(x2_ins_h), 
                            InstOrNull(x5_ins_h), 
                            &ipp, &x2_insp, &x5_insp);

#if defined(DEBUG) && defined(THREADS)
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

#if defined(DEBUG) && defined(THREADS)
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
  if (TagIsSmall(X(0)))
    {
      tagged_t *pt1 = w->global_top;

      HeapPush(pt1,functor_Dref);
      HeapPush(pt1,X(0));
      HeapPush(pt1,TagToInstance(X(0))->rank);
      w->global_top=pt1;
      return cunify(Arg,Tag(STR,HeapOffset(pt1,-3)),X(1));
    }
  else
    {
      tagged_t x1, x2;
      instance_t *n;

      x2=X(1); DerefSwitch(x2,x1,;);
      if (!TagIsSTR(x2) || (TagToHeadfunctor(x2) != functor_Dref))
	return FALSE;

      DerefArg(x1,x2,1);
      DerefArg(x2,x2,2);
      if (!TagIsSmall(x1) ||
	  !(n=TagToInstance(x1)) ||
           n->rank != x2 ||
	   n->death != 0xffff)	  
	return FALSE;

      Unify_constant(PointerToTerm(n),X(0));
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
    int_info_t *root = TagToRoot(X(0));
    intmach_t current_mem = total_mem_count;
#if defined(THREADS)                                               /* MCL */
    bool_t move_insts_to_new_clause = FALSE;
#endif

    Cond_Begin(root->clause_insertion_cond);

#if defined(DEBUG)
    if (debug_conc) fprintf(stderr,
              "*** %d(%d) in inserta (root = %x, first = %x, &first = %x)\n",
              (int)Thread_Id, (int)GET_INC_COUNTER, (unsigned int)root,
              (unsigned int)root->first, (unsigned int)&(root->first));
#endif

#if defined(THREADS)
    if (root->behavior_on_failure == CONC_CLOSED){                 /* MCL */
      Broadcast_Cond(root->clause_insertion_cond);
      USAGE_FAULT("$inserta in an already closed concurrent predicate");
    }
#endif

    DEREF(X(1),X(1));
    n = TagToInstance(X(1));
    
    /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
    if (!root->first){
      n->rank = TaggedZero;
      n->forward = NULL;
      n->backward = n;
#if defined(THREADS)
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

#if defined(THREADS)                                               /* MCL */
    n->pending_x5 = n->pending_x2 = NULL;
#endif
    
    loc = (n->key==ERRORTAG ? &root->varcase :
	   n->key==functor_list ? &root->lstcase :
	   &dyn_puthash(&root->indexer,n->key)->value.instp);
    
    if (!(*loc))
	n->next_forward = NULL,	n->next_backward = n;
    else
	n->next_forward = (*loc),
	n->next_backward = (*loc)->next_backward,
	(*loc)->next_backward = n;
    (*loc) = n;
    
#if defined(THREADS)
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
    int_info_t *root = TagToRoot(X(0));
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
    n = TagToInstance(X(1));
    
    /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
    if (!root->first){
      n->rank = TaggedZero;
      n->backward = n;
      root->first = n;
    }
    else if (root->first->backward->rank == TaggedHigh)
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

#if defined(THREADS)                                               /* MCL */
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

#if defined(DEBUG) && defined(THREADS)
    if (debug_conc && root->behavior_on_failure != DYNAMIC)
      fprintf(stderr, "*** %d(%d) insertz'ed clause %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER, (int)n);
#endif
    
#if defined(THREADS)
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
  intmach_t ar = LargeArity(TagToHeadfunctor(t));
  tagged_t *tp = TagToSTR(t);
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
  if (TagIsSmall(t)) {
    /* Force into a large, even if it fits in a small */
    // fprintf(stderr, "trace: bc32 large stored as small needs fix\n");
    intmach_t i = GetSmall(t);
    if (IntIsSmall_BC32(i)) {
      SERIOUS_FAULT("compile_large_bc32: int32 found in large!");
    }
    tagged_t xx[2];
    xx[0] = MakeFunctorFix;
    xx[1] = i;
    tagged_t t1 = Tag(STR, xx);
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

  DEREF(num,X(0));		/* Must be PHYSICAL size in characters! */
#if defined(GAUGE)
  DEREF(num1,X(1));		/* Number of Counters */
#endif
  DEREF(list,X(2));

  bsize = GetInteger(num) * BC_SCALE;

#if defined(GAUGE)
  counter_cnt = GetInteger(num1);
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
        
        func=TagToHeadfunctor(car);
        if(func==functor_functor) {
          /* functor(Name/Arity) */
          DerefArg(car,car,1);
          if (TagIsSTR(car) && (TagToHeadfunctor(car)==functor_slash)) {
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
          EMIT_C((char *)builtintab[GetSmall(t1)]);
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
          EMIT_l(GetInteger(t1));
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
  Unify_constant(PointerToTerm(object),X(3));
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
  CIAO_REG_2(node_t *, b);
  CIAO_REG_3(instance_t *, j);
  node_t *b2;
  node_t *latest_static = w->node;
  instance_clock_t time = itime;
  instance_clock_t lotime = time;
  tagged_t lorank = TaggedHigh;

  if (!latest_static->next_alt)		          /* if called from wam() */
    latest_static = w->next_node;

  for (b=latest_static; !ChoiceptTestStatic(b); b=b2)  {
    b2=ChoiceCharOffset(b,-b->next_alt->node_offset);
    if (b->next_alt==address_nd_current_instance) {
      latest_static = b2;
      j = TagToInstance(b->term[2]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->term[4]);
        if (lorank>j->rank) lorank=j->rank;
      }
      j = TagToInstance(b->term[5]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->term[4]);
        if (lorank>j->rank) lorank=j->rank;
      }	  
    }
  }
  
                          /* Mark all chpt before latest_static as static */

  for (b=latest_static;
       !ChoiceptTestStatic(b);
       b=ChoiceCharOffset(b,-b->next_alt->node_offset))
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
  node_t *b;

#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr, "*** in clock_overflow()\n");
#endif

  /* count # distinct clock values existing in choicepoints */
  for (b=w->next_node;
       !ChoiceptTestStatic(b);
       b=ChoiceCharOffset(b,-b->next_alt->node_offset)) {
    if (b->next_alt==address_nd_current_instance) {
      t = GetSmall(b->term[4]);
      if (current!=t) {
        current=t;
	count++;
      }
    }
  }

  /* grab space for array of clock values */
  ENSURE_HEAP_BYTES(count*sizeof(instance_clock_t), DynamicPreserved);
  clocks = (instance_clock_t *)w->global_top;

  /* fill in distinct chpt clock values, relocating them as we go */
  clockp = clocks+count;
  *(--clockp) = 0xfffe;
  def_clock = count;
  use_clock = count-1;
  X(4) = MakeSmall(count-1);
  
  for (b=w->next_node;
       !ChoiceptTestStatic(b);
       b=ChoiceCharOffset(b,-b->next_alt->node_offset))
    if (b->next_alt==address_nd_current_instance) {
      t = GetSmall(b->term[4]);
      if ((*clockp)!=t) *(--clockp)=t;
      b->term[4] = MakeSmall(clockp-clocks);
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

#if defined(DEBUG)  && defined(THREADS)
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
  
    if (!i->forward)		/* last ? */
	root->first->backward = i->backward;
    else
	i->forward->backward = i->backward;

    if (i == root->first)	/* first ? */
    	root->first = i->forward;
    else
      i->backward->forward = i->forward;
    
    loc = (i->key==ERRORTAG ? &root->varcase :
	   i->key==functor_list ? &root->lstcase :
	   &incore_gethash(root->indexer,i->key)->value.instp);
  
    if (!i->next_forward)	/* last ? */
        (*loc)->next_backward = i->next_backward;
    else
        i->next_forward->next_backward = i->next_backward;
    
    if (i == (*loc))		/* first ? */
        (*loc) = i->next_forward;
    else
        i->next_backward->next_forward = i->next_forward;
    
    i->rank = ERRORTAG;

    checkdealloc_FLEXIBLE_S(instance_t, objsize, i);
}
