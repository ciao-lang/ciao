/*
 *  dynamic_rt.c
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#include <ciao/dynamic_rt.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_registry.h>
#include <ciao/rt_exp.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>
#include <ciao/basiccontrol.h>
#include <unistd.h>
#include <stddef.h> /* ptrdiff_t */
#endif

// #if !defined(OPTIM_COMP) /* due to PRED_HOOK() */
// #include <ciao/eng_profile.h>
// #endif

#if defined(OPTIM_COMP)
#define DYNAMIC BEHAVIOR(dynamic)
#define CONC_OPEN BEHAVIOR(conc_open)
#define CONC_CLOSED BEHAVIOR(conc_closed)
#endif

#if !defined(OPTIM_COMP)
#define PointerOrNullToTerm PointerToTermOrZero
#define TermNull MakeSmall(0)
#endif

/* --------------------------------------------------------------------------- */
/* Debug/trace messages (for concurrency,show Thread_Id) */

#define TRACEconc(FMT, ...) \
  TRACE_PRINTF("***(%" PRIdm "d)(%" PRIdm ") %s:%d: " FMT, \
               (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, __func__, __LINE__, \
               ## __VA_ARGS__);

#if defined(DEBUG_TRACE)
#define DEBUG__TRACEconc(COND, ...) do { if ((COND)) { TRACEconc(__VA_ARGS__); } } while(0)
#else
#define DEBUG__TRACEconc(COND, ...)
#endif

/* TODO: only when defined(USE_THREADS)? */
#if defined(USE_THREADS)
#define RTCHECK_concwarn(COND, FMT, ...) RTCHECK({ if ((COND)) TRACEconc(FMT, ## __VA_ARGS__); })
#else
#define RTCHECK_concwarn(COND, FMT, ...)
#endif
#define RTCHECK_lockset(ROOT) RTCHECK_concwarn(Cond_Lock_is_unset((ROOT)->clause_insertion_cond), "lock is unset!\n")
#define RTCHECK_firstset(ROOT) RTCHECK_concwarn(!(ROOT)->first, "no first instance!\n")

/* --------------------------------------------------------------------------- */
/* current_instance */

/* Shared? Not easy: they have to do with the lifetime of dyn. predicates  */
instance_clock_t def_clock=0;
instance_clock_t use_clock=0;

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

/* --------------------------------------------------------------------------- */

/* Define an interpreted predicate.  It is open iff it is concurrent. */
#if defined(OPTIM_COMP)
void predicate_def__interpreted(definition_t *f, bool_t concurrent) {
  int_info_t *d = checkalloc_TYPE(int_info_t);

  Init_Cond(d->clause_insertion_cond);

  /*  MCL added on 26 Nov 98 */
  d->x2_pending_on_instance = NULL;
  d->x5_pending_on_instance = NULL;

  d->first = NULL;
  d->varcase = NULL;
  d->lstcase = NULL;
  d->indexer = HASHTAB_NEW(2);

  f->code.intinfo = d;

  f->properties.dynamic = 1;
  f->properties.concurrent = (concurrent != 0);
  /* set the runtime behavior */
  f->code.intinfo->behavior_on_failure =
#if defined(USE_THREADS)
    concurrent ? CONC_OPEN : DYNAMIC;
#else
    DYNAMIC;
#endif

  f->predtyp = ENTER_INTERPRETED;
  f->enterop = OPCODEenc(func__enter_interpreted);
}
#else
void init_interpreted(definition_t *f) {
  int_info_t *d = checkalloc_TYPE(int_info_t);

  Init_Cond(d->clause_insertion_cond);

  /* By default, make it DYNAMIC.  
     set_property() may change this behavior later. MCL. */
  d->behavior_on_failure = DYNAMIC;

  /*  MCL added on 26 Nov 98 */
  d->x2_pending_on_instance = NULL;
  d->x5_pending_on_instance = NULL;

  d->first = NULL;
  d->varcase = NULL;
  d->lstcase = NULL;
  d->indexer = new_switch_on_key(2,NULL);

  f->code.intinfo = d;

  SetEnterInstr(f,ENTER_INTERPRETED);
}
#endif

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP)
int_info_t *find_int_info(tagged_t head);

/* builtin '$current_clauses'/2 */
/* This used to be nondeterministic. */
/* ASSERT: X1 is always unbound, unconditional. */
CBOOL__PROTO(current_clauses) {
  int_info_t *root;
  DEREF(X(0),X(0));
  root = find_int_info(X(0));
  CBOOL__TEST(root != NULL); /* Fails if not interpreted */
  *TaggedToPointer(X(1)) = PointerToTerm(root);
  CBOOL__PROCEED;
}

int_info_t *find_int_info(tagged_t head) {
  if (!IsVar(head)) {
    tagged_t *junk;
    definition_t *d;

    d = find_definition(head, &junk, FALSE);
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED)) {
      return d->code.intinfo;
    }
  }
  return NULL; 
}
#endif

#if !defined(OPTIM_COMP)
/* builtin '$current_clauses'/2 */
/* This used to be nondeterministic. */
/* ASSERT: X1 is always unbound, unconditional. */
CBOOL__PROTO(current_clauses) {
  tagged_t *junk;
  definition_t *d;

  DEREF(X(0),X(0));
  if (! IsVar(X(0))) {
    d = find_definition(predicates_location,X(0),&junk,FALSE);
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED)) {
      return (*TaggedToPointer(X(1))=PointerToTerm(d->code.intinfo), TRUE);
    } else {
      return FALSE;
    }
  }
  else {
    MINOR_FAULT("$current_clauses/2: incorrect 1st arg");
  }
}
#endif

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP)
static CINSNP__PROTO(next_instance_conc);
static CINSNP__PROTO(next_instance_noconc);
#endif

try_node_t *address_nd_current_instance;

static CFUN__PROTO(active_instance, instance_t *, instance_t *i, int/*instance_clock_t*/ itime, bool_t normal);

/* TODO: better name? */
#define DUMMY_ACTIVE_INSTANCE(I,TIME,CHAINP) (I)

#define ACTIVE_INSTANCE(I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   CFUN__EVAL(active_instance,I,TIME,CHAINP))

#if defined(OPTIM_COMP)
/* TODO: trick to avoid incorrect nondet macros - do not emit a macro! */
CINSNP__PROTO(next_instance) {
  CODE_NEXT_INSTANCE();
}
#endif

/* --------------------------------------------------------------------------- */
/* current_instance */

/* ASSERT: X(0) is head, X(1) is body */

#if defined(OPTIM_COMP)
#define BLOCKIDX (1<<0)
#define EXECIDX  (1<<1)
#else /* TODO: backport? */
/* NOTE: was 1<<0 and 1<<1; avoid reserved bits */
//#define BLOCKIDX ((intmach_t)1<<tagged__num_offset)
#define EXECIDX  ((intmach_t)1<<(tagged__num_offset+1))
#endif

/* MCL: "Blocking" specifies whether the predicate must block or not
   on concurrent predicates. */
#define SET_BLOCKING(arg) (arg) = ((arg) | BLOCKIDX)
#define SET_NONBLOCKING(arg) (arg) = ((arg) & ~BLOCKIDX)
#if defined(OPTIM_COMP)
#define IS_BLOCKING(arg) ((arg) & BLOCKIDX)
#endif
#define IS_NONBLOCKING(arg) !((arg) & BLOCKIDX)

#define SET_EXECUTING(arg) (arg) = ((arg) | EXECIDX)
#define SET_NONEXECUTING(arg) (arg) = ((arg) & ~EXECIDX)
#define EXECUTING(arg) ((arg) & EXECIDX)
#define NONEXECUTING(arg) !((arg) & EXECIDX)

#if defined(OPTIM_COMP)
static CINSNP__PROTO(current_instance_noconc, int_info_t *root);
static CINSNP__PROTO(current_instance_conc, int_info_t *root, BlockingType block);
#else
static CFUN__PROTO(current_instance_noconc, instance_t *);
static CFUN__PROTO(current_instance_conc, instance_t *, BlockingType block);
#endif

#if defined(OPTIM_COMP)
/* precondition: X(3) is set */
CINSNP__PROTO(current_instance, int_info_t *root, BlockingType block) {
  if (root->behavior_on_failure == DYNAMIC) {
    CINSNP__LASTCALL(current_instance_noconc, root);
  } else {
    CINSNP__LASTCALL(current_instance_conc, root, block);
  }
}
#else
/* ASSERT: X(2) is a dereferenced small integer */
/* MCL: a typical call is '$current_instance'(Head, Body, Root, Ptr,
   Blocking) where Head is the head clause, Body is the body clause ("true"
   for facts), Root is a small integer denoting the root of the predicate
   (obtained through '$current_clauses'/2), and Ptr is a number denoting the
   particular clause (i.e., a pointer to the instance_t). "Blocking"
   specifies whether the predicate must block or not on concurrent
   predicates. */
CFUN__PROTO(current_instance0, instance_t *) {
  int_info_t *root;
  BlockingType block;
  /* TODO:[JF] func is sometimes NULL; PRED_HOOK should not be used
     here, since we are not really interpreting any code at this point;
     profiling of current_fact/1, may need special code?
   */
//#if defined(DEBUG_NODE) || defined(ABSMACH_OPT__profile_calls) || defined(ABSMACH_OPT__profilecc)
//  tagged_t *junk;
//  definition_t *func=find_definition(predicates_location,X(0),&junk,FALSE);
//#endif
//#if defined(ABSMACH_OPT__profile_calls) || defined(ABSMACH_OPT__profilecc)
//  if (func == NULL) {
//    fprintf(stderr, "func=%p\n", func); DerefDisplayTerm(X(0), Error_Stream_Ptr, TRUE); fprintf(stderr, "\n");
//  }
//#endif
//  ON_DEBUG_NODE({ w->choice->functor=func; });
//  PRED_HOOK("I", func);
  root = TaggedToRoot(X(2));
  if (root->behavior_on_failure == DYNAMIC) {
    return CFUN__EVAL(current_instance_noconc);
  } else {
    DEREF(X(4), X(4));                                   /* Blocking? (MCL) */
    RTCHECK({
      if (X(4) != atom_block && X(4) != atom_no_block) {
        failc("$current_instance called with unknown 5th argument");
        return NULL;                         
      }
    });
    block = X(4) == atom_block ? BLOCK : NO_BLOCK;
    return CFUN__EVAL(current_instance_conc, block);
  }
}
#endif

// TODO: always update (Head) = head? before OnVar too?
#define CURRENT_INSTANCE(Head, Root, ActiveInstance, CODE_FAIL) do { \
  __label__ var_case_switch; \
  __label__ xn_switch; \
  tagged_t head = (Head); \
  DerefSw_HVAorCVAorSVA_Other(head, { (Head) = head; goto var_case_switch; }, {}); \
  (Head) = head; \
  x2_next = NULL; \
  x5_next = NULL; \
  if (TaggedIsSTR(head)) { \
    DerefArg(head,head,1); \
    if (IsVar(head)) { \
    var_case_switch: \
      x5_chain = x2_chain = ActiveInstance((Root)->first,use_clock,TRUE); \
      if (x2_chain) { \
        x5_next = x2_next = ActiveInstance(x2_chain->forward,use_clock,TRUE); \
      } else { \
        CODE_FAIL; \
      } \
    } else if (TaggedIsLST(head)) { \
      x5_chain = ActiveInstance((Root)->lstcase,use_clock,FALSE); \
    xn_switch: \
      x2_chain = ActiveInstance((Root)->varcase,use_clock,FALSE); \
      if (x2_chain && x5_chain) { \
        if (x2_chain->rank < x5_chain->rank){ \
          x2_next = ActiveInstance(x2_chain->next_forward,use_clock,FALSE); \
          x5_next = x5_chain; \
          x5_chain = NULL; \
        } else { \
          x5_next = ActiveInstance(x5_chain->next_forward,use_clock,FALSE); \
          x2_next = x2_chain; \
          x2_chain = NULL; \
        } \
      } else if (x2_chain) { \
        x2_next = ActiveInstance(x2_chain->next_forward,use_clock,FALSE); \
      } else if (x5_chain) { \
        x5_next = ActiveInstance(x5_chain->next_forward,use_clock,FALSE); \
      } else { \
        CODE_FAIL; /* No solution */ \
      } \
    } else { \
      hashtab_key_t k = TaggedIsSTR(head) ? \
                        (hashtab_key_t)TaggedToHeadfunctor(head) : \
                        (hashtab_key_t)head; \
      hashtab_node_t *hnode = hashtab_get((Root)->indexer, k); \
      x5_chain = ActiveInstance((instance_t *)hnode->value.as_ptr,use_clock,FALSE); \
      goto xn_switch; \
    } \
  } else { \
    goto var_case_switch; \
  } \
} while(0);

/* Take time into account, do not wait for predicates. */
#if defined(OPTIM_COMP)
/* precondition: X(3) is set */
static CINSNP__PROTO(current_instance_noconc, int_info_t *root)
#else
static CFUN__PROTO(current_instance_noconc, instance_t *)
#endif
{
  instance_t *x2_chain;
  instance_t *x5_chain;
  instance_t *x2_next;
  instance_t *x5_next;
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(2));
#endif

  Wait_Acquire_Cond_lock(root->clause_insertion_cond);
  CURRENT_INSTANCE(X(0), root, ACTIVE_INSTANCE, { /* fail */
    Release_Cond_lock(root->clause_insertion_cond);
#if defined(OPTIM_COMP)
    CINSNP__FAIL;
#else
    return NULL;
#endif
  });

  /* NOTE: We must cleanup unused registers up to DynamicPreserved so
     that HEAPMARGIN_CALL does not break during GC */

  if (x2_next || x5_next) {
    /* X(0) must be the head */
    /* X(1) must be the body */
    X(X2_CHN) = PointerOrNullToTerm(x2_next);
    /* X(3) - */
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerOrNullToTerm(x5_next);
    X(RootArg) = PointerOrNullToTerm(root);
    /* Cleanup unused registers (JF & MCL) */
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = TermNull;

    w->previous_choice = w->choice;

    CODE_CHOICE_NEW(w->choice, address_nd_current_instance); /* establish skeletal node */
  } else {
    /* Initialize dynpreserved (to ensure heapmargin correctness) */
    /* X(0) must be the head */
    /* X(1) must be the body */
    /* TODO: necessary? */
#if defined(OPTIM_COMP)
    X(X2_CHN) = TermNull;
    /* X(3) - */
    X(ClockSlot) = MakeSmall(use_clock);
#endif
    X(X5_CHN) = TermNull;
#if defined(OPTIM_COMP)
    X(RootArg) = PointerOrNullToTerm(root); /* necessary */
#else
    X(RootArg) = MakeSmall(0);
#endif
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = TermNull;
  }

  Release_Cond_lock(root->clause_insertion_cond);

#if defined(OPTIM_COMP)
  if (x2_chain != NULL) {
    w->ins = x2_chain;
    CINSNP__GOTO(w->ins->emulcode);
  } else if (x5_chain != NULL) {
    w->ins = x5_chain;
    CINSNP__GOTO(w->ins->emulcode);
  } else {
    CINSNP__FAIL; /* fail */ /* TODO: is this case possible? */
  }
#else
  if (x2_chain != NULL) {
    return x2_chain;
  } else {
    return x5_chain;
  }
#endif
}

/* First-solution special case of the above. */
#if defined(OPTIM_COMP)
/* ASSERT: X(0) is a small integer,
           X(1) is a dereferenced unconditional unbound */
#else
/* ASSERT: X(0) is a dereferenced small integer,
           X(1) is a dereferenced unconditional unbound */
#endif
/* Adapted to concurrent predicates (MCL) */
CBOOL__PROTO(first_instance) {
  int_info_t *root;
  instance_t *inst;

#if defined(OPTIM_COMP) /* TODO: backport */
  DEREF(X(0), X(0));
#endif
  root = TaggedToRoot(X(0));
  if (root->behavior_on_failure == DYNAMIC) {
    inst = ACTIVE_INSTANCE(root->first,use_clock,TRUE);
  } else {
    Cond_Begin(root->clause_insertion_cond);
    inst = root->first;
    Broadcast_Cond(root->clause_insertion_cond);
  }

  CBOOL__TEST(inst != NULL); /* has solutions */
  *TaggedToPointer(X(1)) = PointerToTerm(inst);

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------
          NEXT_INSTANCE
   -----------------------------------------------------------------------*/

#if defined(OPTIM_COMP)
CINSNP__PROTO(next_instance_noconc)
#else
CBOOL__PROTO(next_instance, instance_t **ipp)
#endif
{
  instance_t *x2_insp = TaggedToInstance(X(2));
  instance_t *x5_insp = TaggedToInstance(X(5));
  instance_clock_t clock = GetSmall(X(4));
#if defined(USE_THREADS)
  int_info_t *root = TaggedToRoot(X(6));
#endif

  Wait_Acquire_Cond_lock(root->clause_insertion_cond);

  if (x2_insp == x5_insp) {
#if defined(OPTIM_COMP)
    w->ins = x2_insp;
#else
    *ipp = x2_insp;
#endif
    x2_insp = x5_insp = ACTIVE_INSTANCE(x2_insp->forward,clock,TRUE);
  } else if (!x2_insp) {
  x5_alt:
#if defined(OPTIM_COMP)
    w->ins = x5_insp;
#else
    *ipp = x5_insp;
#endif
    x5_insp = ACTIVE_INSTANCE(x5_insp->next_forward,clock,FALSE);
  } else if (!x5_insp) {
  x2_alt:
#if defined(OPTIM_COMP)
    w->ins = x2_insp;
#else
    *ipp = x2_insp;
#endif
    x2_insp = ACTIVE_INSTANCE(x2_insp->next_forward,clock,FALSE);
  } else if (x2_insp->rank < x5_insp->rank) {
    goto x2_alt;
  } else {
    goto x5_alt;
  }

  Release_Cond_lock(root->clause_insertion_cond);

#if defined(OPTIM_COMP)
  if (!x2_insp && !x5_insp) {
    /* If there is *definitely* no next instance, remove choicepoint */
    SetDeep();
    SetChoice(w->previous_choice);
  } else {
    w->choice->x[X2_CHN] = X(X2_CHN) = PointerOrNullToTerm(x2_insp);
    w->choice->x[X5_CHN] = X(X5_CHN) = PointerOrNullToTerm(x5_insp);
  }
  if (w->ins == NULL) {
    TopConcChpt = TermToPointerOrNull(choice_t, X(PrevDynChpt));
    CINSNP__FAIL;
  } else {
    CINSNP__GOTO(w->ins->emulcode);
  }
#else
  if (!x2_insp && !x5_insp) {
    CBOOL__FAIL;
  } else {
    w->choice->x[X2_CHN] = X(X2_CHN) = PointerOrNullToTerm(x2_insp);
    w->choice->x[X5_CHN] = X(X5_CHN) = PointerOrNullToTerm(x5_insp);
    CBOOL__PROCEED;
  }
#endif
}

#if !defined(OPTIM_COMP) && defined(OLD_DATABASE)
/* $CURRENT_KEY/4 */

tagged_t decode_instance_key(instance_t *);

CBOOL__PROTO(current_key) {
  int_info_t *root = TaggedToRoot(X(0));
  hashtab_t *swp = root->indexer;
  intmach_t j = HASHTAB_SIZE(swp);
  tagged_t mask;

  DEREF(X(2),X(2));
  mask = (IsVar(X(2)) ? 0 : INDEXMASK);
  DEREF(X(3),X(3));
  X(4) = atom_nil;

  if (!IsVar(X(3))){
    if (TaggedIsLST(X(3))) {
      if (ACTIVE_INSTANCE(root->lstcase,use_clock,FALSE))
        MakeLST(X(4),make_structure(Arg,functor_lst),X(4));
    } else if (TaggedIsSTR(X(3))) {
      hashtab_node_t *hnode =
        hashtab_get(swp,TaggedToHeadfunctor(X(3)));
      instance_t *inst =
        ACTIVE_INSTANCE(hnode->value.instp,use_clock,FALSE);

      if (inst && !(hnode->key & QTAGMASK)) {
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
      } else
        while (inst){
          intmach_t ar = LargeArity(hnode->key);

          if (HeapCharDifference(w->heap_top,Heap_End)<CONTPAD+ar*sizeof(tagged_t)+3*sizeof(tagged_t)) {
            explicit_heap_overflow(Arg,(CALLPAD+ar*sizeof(tagged_t))*2,5);
          }

          MakeLST(X(4),decode_instance_key(inst),X(4));
          inst = ACTIVE_INSTANCE(inst->next_forward,use_clock,FALSE);
        }
    } else {
      hashtab_node_t *hnode = hashtab_get(swp,X(3));
      instance_t *inst =
        ACTIVE_INSTANCE(hnode->value.instp,use_clock,FALSE);

      if (inst)
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
    }
    *TaggedToPointer(X(1)) = X(4);
    return TRUE;
  }

  if (ACTIVE_INSTANCE(root->lstcase,use_clock,FALSE) &&
      (functor_lst & mask) == (X(2) & mask))
    MakeLST(X(4),make_structure(Arg,functor_lst),X(4));
  for (--j; j>=0; --j) {
    hashtab_node_t *hnode = &swp->node[j];
    instance_t *inst =
      ACTIVE_INSTANCE(hnode->value.instp,use_clock,FALSE);

    if (!(hnode->key & QTAGMASK)){
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
          inst = ACTIVE_INSTANCE(inst->next_forward,use_clock,FALSE);
        }
    }
  }
  *TaggedToPointer(X(1)) = X(4);
  CBOOL__PROCEED;
}
#endif

#if defined(OPTIM_COMP)
CVOID__PROTO(close_predicate, int_info_t *root)
#else
CBOOL__PROTO(close_predicate)
#endif
{
#if defined(USE_THREADS)
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(0));
#endif
  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_OPEN) 
    root->behavior_on_failure = CONC_CLOSED;
  Broadcast_Cond(root->clause_insertion_cond);
#endif
#if !defined(OPTIM_COMP)
  CBOOL__PROCEED;
#endif
}
#if defined(OPTIM_COMP)
CVOID__PROTO(open_predicate, int_info_t *root)
#else
CBOOL__PROTO(open_predicate)
#endif
{
#if defined(USE_THREADS)
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(0));
#endif
  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_CLOSED) 
    root->behavior_on_failure = CONC_OPEN;
  Broadcast_Cond(root->clause_insertion_cond);
#endif
#if !defined(OPTIM_COMP)
  CBOOL__PROCEED;
#endif
}

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

#if defined(OPTIM_COMP)
/* precondition: X(3) is set */
static CINSNP__PROTO(current_instance_conc, int_info_t *root, BlockingType block)
#else
static CFUN__PROTO(current_instance_conc, instance_t *, BlockingType block)
#endif
{
  instance_t *x2_n = NULL, *x5_n = NULL;
  instance_t *current_one;
  bool_t try_instance;
  instance_handle_t *x2_next, *x5_next;
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(2));
#endif

  DEBUG__TRACEconc(debug_concchoicepoints,
                   "entering, choice = %p, previous_choice = %p, conc. = %p\n",
                   w->choice, w->previous_choice, TopConcChpt);

  do {
    try_instance = wait_for_an_instance_pointer(&(root->first), &(root->first), root, block);

    /* That was a non-blocking or concurrent predicate with no instance at all */
    if (!try_instance) {
      Wait_For_Cond_End(root->clause_insertion_cond);
      DEBUG__TRACEconc(debug_concchoicepoints,
                       "exiting with failure, choice = %p, previous_choice = %p, conc. choice = %p\n",
                       w->choice, w->previous_choice, TopConcChpt);
#if defined(OPTIM_COMP)
      CINSNP__FAIL; /* fail */
#else
      return NULL;
#endif
    }

    /* If we are here, we have a lock for the predicate.  Get first
       possibly matching instance */
    current_one = first_possible_instance(X(0), root, &x2_n, &x5_n);
    if (current_one == NULL) { /* Let others enter and update */
      Wait_For_Cond_End(root->clause_insertion_cond);
    }
  } while (!current_one && block == BLOCK);

  /* Here with (current_one || block == NON_BLOCK).  current_one ==
     NULL implies that we do not have a lock --- but then block ==
     NON_BLOCK, and therefore we can return with a NULL */
  if (!current_one) {
    DEBUG__TRACEconc(debug_concchoicepoints,
                     "exiting with failure, choice = %p, previous_choice = %p, conc. choice = %p\n",
                     w->choice, w->previous_choice, TopConcChpt);
#if defined(OPTIM_COMP)
    CINSNP__FAIL; /* fail */
#else
    return NULL;
#endif
  }

  RTCHECK_lockset(root);
  RTCHECK_firstset(root);

  /* We do NOT release the clause lock here: we must retain it until
     we finish executing it.  It is released at Prolog level.  The
     same for next instance: when it provides a possible solution, it
     leaves a lock set, to be unset from Prolog.  If the Prolog
     unification fails, the lock is anyway unset from wam(), right
     before failure. */

  DEBUG__TRACEconc(debug_concchoicepoints,
                   "making chpt (now: choice = %p), inst. is %p\n",
                   w->choice, current_one);

  x2_next = make_handle_to(x2_n, root, X(0), X2);
  x5_next = make_handle_to(x5_n, root, X(0), X5);
  X(X2_CHN) = PointerOrNullToTerm(x2_next);
  X(ClockSlot) = MakeSmall(use_clock);
  X(X5_CHN) = PointerOrNullToTerm(x5_next);

  /* pass root to RETRY_INSTANCE (MCL) */
  X(RootArg) = PointerOrNullToTerm(root);  
#if !defined(OPTIM_COMP)
  // TODO:[oc-merge] also in OPTIM_COMP?
  X(InvocationAttr) = MakeSmall(0); /* avoid garbage */
#endif
  if (block == BLOCK) {
    SET_BLOCKING(X(InvocationAttr));
  } else {
    SET_NONBLOCKING(X(InvocationAttr));
  }
  SET_EXECUTING(X(InvocationAttr));

  /* Save last dynamic top */
  X(PrevDynChpt) = PointerOrNullToTerm(TopConcChpt); 
  w->previous_choice = w->choice;
  //
  CODE_CHOICE_NEW(w->choice, address_nd_current_instance); /* establish skeletal node */
  //
  TopConcChpt = (choice_t *)w->choice;  /* Update dynamic top */

  DEBUG__TRACEconc(debug_concchoicepoints,
                   "exiting, choice = %p, previous_choice = %p, conc. choice = %p\n",
                   w->choice, w->previous_choice, TopConcChpt);

#if defined(OPTIM_COMP)
  w->ins = current_one;
  CINSNP__GOTO(w->ins->emulcode);
#else
  return current_one;
#endif
}

/* Releases the lock on a predicate; this is needed to ensure that a
   clause will not be changed while it is being executed. */
#if defined(OPTIM_COMP)
CVOID__PROTO(prolog_unlock_predicate, int_info_t *root)
#else
CBOOL__PROTO(prolog_unlock_predicate)
#endif
{
#if defined(USE_LOCKS)
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(0));
#endif

  DEBUG__TRACEconc(debug_conc, "root is %p\n", root);
  RTCHECK({ /* TODO: trace or rtcheck? */
    if (root->behavior_on_failure == CONC_OPEN) {
      RTCHECK_lockset(root);
    }
  });
  
  /* We have just finished executing an operation on a locked
     predicate; unlock the predicate and make sure the choicepoint is
     not marked as executing. */
  if (root->behavior_on_failure != DYNAMIC) {
    SET_NONEXECUTING((TopConcChpt->x[InvocationAttr])); 
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
#else
  DEBUG__TRACEconc(debug_conc, "using fake unlock_predicate!\n");
#endif /* USE_LOCKS */ 
#if !defined(OPTIM_COMP)
  CBOOL__PROCEED;
#endif
}

static bool_t wait_for_an_instance_pointer(instance_t **inst_pptr1,
                                           instance_t **inst_pptr2,
                                           int_info_t *root,
                                           BlockingType block) {
  volatile instance_t *pptr1 = NULL, *pptr2 = NULL;

  /* We have to wait for a new clause only if we are blocking */

  while(TRUE) {
    /* Wait until a change is signaled, and test that the change affects us */

    if (block == BLOCK) {
      Wait_For_Cond_Begin(((*inst_pptr1 == NULL) &&
                           (*inst_pptr2 == NULL) &&
                           root->behavior_on_failure == CONC_OPEN),
                          root->clause_insertion_cond);
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
                                           instance_t **x2_n, instance_t **x5_n) {
  instance_t *x2_chain;
  instance_t *x5_chain;
  instance_t *x2_next;
  instance_t *x5_next;

  CURRENT_INSTANCE(x0, root, DUMMY_ACTIVE_INSTANCE, { /* fail */
    return NULL;
  });

  *x2_n = x2_next;
  *x5_n = x5_next;

  return x2_chain ? x2_chain : x5_chain;
}

/* Current pointers to instances are x2_insp and x5_insp; look for a new
   pointer which presumably matches the query. */
#define LOCATE_NEXT_INSTANCE(X2_INSP, X5_INSP, IPP) { \
  if (!X2_INSP && !X5_INSP) { /* MCL */ \
    IPP = X2_INSP; \
  } else if (X2_INSP == X5_INSP) { /* normal = TRUE */ \
    IPP = X2_INSP; \
    X2_INSP = X5_INSP = X2_INSP->forward; \
  } else if (!X2_INSP) { /* normal = FALSE */ \
  x5_alt: \
    IPP = X5_INSP; \
    X5_INSP = X5_INSP->next_forward; \
  } else if (!x5_insp) { /* normal = FALSE */ \
  x2_alt: \
    IPP = X2_INSP; \
    X2_INSP = X2_INSP->next_forward; \
  } else if (X2_INSP->rank < X5_INSP->rank) { \
    goto x2_alt; \
  } else { \
    goto x5_alt; \
  } \
}
#if !defined(OPTIM_COMP)
static void jump_to_next_instance(instance_t *x2_insp, instance_t *x5_insp, instance_t **ipp, instance_t **x2_next, instance_t **x5_next) {
  LOCATE_NEXT_INSTANCE(x2_insp, x5_insp, *ipp);
  *x2_next = x2_insp;
  *x5_next = x5_insp;
}
#endif

#if defined(OPTIM_COMP)
CINSNP__PROTO(next_instance_conc)
#else
CBOOL__PROTO(next_instance_conc, instance_t **ipp)
#endif
{
  int_info_t *root = TaggedToRoot(X(RootArg));
  BlockingType block;      
  instance_handle_t *x2_ins_h, *x5_ins_h;
  bool_t next_instance_pointer;
  instance_t *x2_insp, *x5_insp;

  DEBUG__TRACEconc(debug_concchoicepoints,
                   "entering, choice = %p, previous_choice = %p, conc. choice = %p\n",
                   w->choice, w->previous_choice, TopConcChpt);

  /* = X(7) == atom_block ? BLOCK : NO_BLOCK;*/
  block = IS_BLOCKING(X(InvocationAttr)) ? BLOCK : NO_BLOCK; 

  /* When we baktrack after a call which did not finally succeed, the lock
     is still set. Unlock it before proceeding to the next clause. */

  if (EXECUTING(X(InvocationAttr))) {
    DEBUG__TRACEconc(debug_concchoicepoints, "changing to nonexecuting\n");
    SET_NONEXECUTING(X(InvocationAttr));
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
  
  x2_ins_h = TaggedToInstHandle(X(X2_CHN));
  x5_ins_h = TaggedToInstHandle(X(X5_CHN));

/* x2_ins_h->inst_ptr and x5_ins_h->inst_ptr may be both NULL; that means no
   current instance is available.  Just wait for one.  If any of x2_insp or
   x5_insp are NULL pointer, they are automatically enqueued in root by
   change_handle_to_instance */

  RTCHECK_firstset(root);
  DEBUG__TRACEconc(debug_conc,
                   "with x2 = %p, x5 = %p, block = %d\n",
                   x2_ins_h->inst_ptr, x5_ins_h->inst_ptr, (int)block);

  do {
    next_instance_pointer =
      wait_for_an_instance_pointer(&(x2_ins_h->inst_ptr), 
                                   &(x5_ins_h->inst_ptr),
                                   root, block);
    if (!next_instance_pointer) { /* Closed or non-waiting call */
      remove_handle(x2_ins_h, root, X2);
      remove_handle(x5_ins_h, root, X5);
#if !defined(OPTIM_COMP)
      *ipp = NULL;                                       /* Cause failure */
#endif
      /* Time for new assertions */
      Wait_For_Cond_End(root->clause_insertion_cond);
      DEBUG__TRACEconc(debug_concchoicepoints,
                       "exiting with failure, choice = %p, previous_choice = %p, conc. choice = %p\n",
                       w->choice, w->previous_choice, TopConcChpt);
#if defined(OPTIM_COMP)
      /* Remove choicepoint and fail */
      /* TODO: hmmm redundant? failure already manages choice points... (perhaps TopConcChpt is not... ) <- the code seems to be correct... */
      SetDeep();
      SetChoice(w->previous_choice);
      TopConcChpt = TermToPointerOrNull(choice_t, X(PrevDynChpt));
      CINSNP__FAIL;
#else
      return FALSE; /* Remove choicepoint */
#endif
    }

    /* Locate a satisfactory instance. */
#if defined(OPTIM_COMP)
    x2_insp = x2_ins_h->inst_ptr;
    x5_insp = x5_ins_h->inst_ptr;
    LOCATE_NEXT_INSTANCE(x2_insp, x5_insp, w->ins);
#else
    jump_to_next_instance(x2_ins_h->inst_ptr, x5_ins_h->inst_ptr,
                          ipp, &x2_insp, &x5_insp);
#endif

    /* Move handle forwards to re-start (if necesary) in a new clause */
    change_handle_to_instance(x2_ins_h, x2_insp, root, X2);
    change_handle_to_instance(x5_ins_h, x5_insp, root, X5);

    RTCHECK_firstset(root); /* after jumping */

#if defined(OPTIM_COMP)
    if (!w->ins)
#else
    if (!*ipp)
#endif
    { /* Not instance -> release lock, continue in loop */
      Wait_For_Cond_End(root->clause_insertion_cond);
    }
  }
#if defined(OPTIM_COMP)
  while (!w->ins);
#else
  while (!*ipp);
#endif

  RTCHECK_firstset(root); /* exiting */
#if defined(OPTIM_COMP)
  DEBUG__TRACEconc(debug_conc, "exiting with instance %p\n", w->ins);
#else
  DEBUG__TRACEconc(debug_conc, "exiting with instance %p\n", *ipp);
#endif

  /* Here with a possibly matching instance,
     a possibly empty next instance,
     and the lock on the instance. */

  w->choice->x[X2_CHN] = X(X2_CHN) = PointerOrNullToTerm(x2_ins_h);
  w->choice->x[X5_CHN] = X(X5_CHN) = PointerOrNullToTerm(x5_ins_h);
  SET_EXECUTING(X(InvocationAttr));
  DEBUG__TRACEconc(debug_concchoicepoints,
                   "exiting, choice = %p, previous_choice = %p, conc. choice = %p\n",
                   w->choice, w->previous_choice, TopConcChpt);

#if defined(OPTIM_COMP)
  /* w->ins is not NULL here */
  CINSNP__GOTO(w->ins->emulcode);
#else
  CBOOL__PROCEED;
#endif
}

/* --------------------------------------------------------------------------- */
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
                                  WhichChain chain) {
  instance_handle_t *this_handle;

  /* Create the handle */
  this_handle = checkalloc_TYPE(instance_handle_t);

  /* Allow re-indexation */
  this_handle->head = head;
  link_handle(this_handle, inst, root, chain);
  DEBUG__TRACEconc(debug_conc, "made handle %p to instance %p\n", this_handle, inst);

  return this_handle;
}

/* Remove handle from list.  xi might be pointed to directly from the root,
   or from an instance record: need to update the pointer itself. */

void remove_handle(instance_handle_t *xi,
                   int_info_t *root,
                   WhichChain chain) {
  unlink_handle(xi, root, chain);
  checkdealloc_TYPE(instance_handle_t, xi);
  DEBUG__TRACEconc(debug_conc, "removed handle %p (to instance %p)\n", xi, xi->inst_ptr);
}

/* Make a handle to point to a new instance. */

static void change_handle_to_instance(instance_handle_t *handle,
                                      instance_t *new_inst,
                                      int_info_t *root,
                                      WhichChain chain) {
  if (handle->inst_ptr != new_inst) {      /* Do not move if not necessary */
    DEBUG__TRACEconc(debug_conc, "changes handle %p from instance %p to %p\n", handle, handle->inst_ptr, new_inst);
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
                        WhichChain chain) {
  RTCHECK_lockset(root);

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
                          WhichChain chain) {
  instance_t *inst;

  RTCHECK_lockset(root);

  /* A handle is enqueued in X2 (unindexed links) or X5 (indexed
     links) iff it has a non-null instance pointer; otherwise, it must
     be enqueued in the root queue. */

  if ((inst = xi->inst_ptr)) {
    if (chain == X2 && inst->pending_x2 == xi) inst->pending_x2 = xi->next_handle; /* First in queue */
    if (chain == X5 && inst->pending_x5 == xi) inst->pending_x5 = xi->next_handle;
  } else if (chain == X2) { /* xi->inst_ptr is a NULL pointer */
    if (root->x2_pending_on_instance == xi) root->x2_pending_on_instance = xi->next_handle;
  } else {
    if (root->x5_pending_on_instance == xi) root->x5_pending_on_instance = xi->next_handle;
  }

  if (xi->next_handle) xi->next_handle->previous_handle = xi->previous_handle;
  if (xi->previous_handle) xi->previous_handle->next_handle = xi->next_handle;
}

void expunge_instance(instance_t *i) {
  instance_t **loc;
  int_info_t *root = i->root;

#if defined(USE_THREADS) && defined(DEBUG_TRACE)
  if (root->behavior_on_failure != DYNAMIC) {
    DEBUG__TRACEconc(debug_conc, "deleting instance %p!\n", i);
  }
#endif
#if defined(USE_THREADS)
  RTCHECK({
    if (root->behavior_on_failure != DYNAMIC) RTCHECK_lockset(root);
  });
#endif
  RTCHECK_firstset(root);
  
  if (!i->forward) { /* last ? */
    root->first->backward = i->backward;
  } else {
    i->forward->backward = i->backward;
  }

  if (i == root->first) { /* first ? */
    root->first = i->forward;
  } else {
    i->backward->forward = i->forward;
  }
    
  loc = (i->key==ERRORTAG ? &root->varcase :
         i->key==functor_lst ? &root->lstcase :
         (instance_t **)&hashtab_get(root->indexer,i->key)->value.as_ptr);
  
  if (!i->next_forward) { /* last ? */
    (*loc)->next_backward = i->next_backward;
  } else {
    i->next_forward->next_backward = i->next_backward;
  }
    
  if (i == (*loc)) { /* first ? */
    (*loc) = i->next_forward;
  } else {
    i->next_backward->next_forward = i->next_forward;
  }
    
  i->rank = ERRORTAG;

#if defined(OPTIM_COMP)
  CHECKDEALLOC0_TAILED(instance_t, i);
#else
  checkdealloc_FLEXIBLE_S(instance_t, objsize, i);
#endif
}

/* Remove the linked chains which point to the calls to concurrent
   predicates which were suspended.  Start at topdynamic (topmost dynamic
   choicepoint) and go down the choicepoint stack until the next dynamic
   choicepoint to be considered is older than chpttoclear.  Then, return the
   value of that dynamic choicepoint in the variable topdynamic (it is the
   topmost dynamic choicepoint after the call!). */

void remove_link_chains(choice_t **topdynamic, choice_t *chpttoclear) {
  choice_t *movingtop = *topdynamic;
  DEBUG__TRACEconc(debug_conc, "removing from %p until %p\n", *topdynamic, chpttoclear);
  
  while (ChoiceYounger(movingtop, chpttoclear)) {
    DEBUG__TRACEconc(debug_conc, "removing handle at (dynamic) node %p\n", movingtop);

    Cond_Begin(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

    RTCHECK_concwarn(TaggedToInstHandle(movingtop->x[X2_CHN]) == NULL, "X2 handle is NULL!\n");
    RTCHECK_concwarn(TaggedToInstHandle(movingtop->x[X5_CHN]) == NULL, "X5 handle is NULL!\n");

    remove_handle(TaggedToInstHandle(movingtop->x[X2_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X2);
    remove_handle(TaggedToInstHandle(movingtop->x[X5_CHN]), 
                  TaggedToRoot(movingtop->x[RootArg]),
                  X5);

    Broadcast_Cond(TaggedToRoot(movingtop->x[RootArg])->clause_insertion_cond);

    movingtop=TermToPointerOrNull(choice_t, movingtop->x[PrevDynChpt]);
  }
  DEBUG__TRACEconc(debug_conc, "done at %p\n", movingtop);
  *topdynamic = movingtop;
}

/* called from make_undefined() */
CVOID__PROTO(erase_interpreted, definition_t *f) {
  /* erase as much as possible */
  instance_t *i, *j;

  for (i = f->code.intinfo->first; i; i=j) {
    j = i->forward;
    if (i->death==0xffff) {
      i->death = use_clock = def_clock;
      if (i->birth==i->death) { 
        /* make_undefined() is called from abolish() and
           define_predicate(), which in turn are called directly from
           Prolog and do not put any lock.  When reloading Prolog
           code, the existent clauses (including those of concurrent
           predicates) are erased, so we better put a lock on those
           predicates.  MCL.
        */
        Cond_Begin(f->code.intinfo->clause_insertion_cond);
        expunge_instance(i);
        Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
      }
    }
  }

  Cond_Begin(f->code.intinfo->clause_insertion_cond);
  (void)ACTIVE_INSTANCE(f->code.intinfo->first,use_clock,TRUE);
  Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
}

/* --------------------------------------------------------------------------- */

static void relocate_table_clocks(hashtab_t *sw, instance_clock_t *clocks) {
  hashtab_node_t *keyval;
  definition_t *d;
  intmach_t j = HASHTAB_SIZE(sw);
  
  for (--j; j>=0; --j) {
    keyval = &sw->node[j];
    if ((d = (definition_t *)keyval->value.as_ptr) &&
        d->predtyp==ENTER_INTERPRETED) {
      relocate_clocks(d->code.intinfo->first,clocks);
    }
  }
}

void relocate_clocks(instance_t *inst, instance_clock_t *clocks) {
  int i, j;
  instance_t *next;
  for (; inst; inst=next) {
    next = inst->forward;
    for (i=0; inst->birth>clocks[i]; i++) {}
    inst->birth = i;
    if (inst->death!=0xffff) {
      for (j=i; inst->death>clocks[j]; j++) {}
      inst->death = j;
      if (i==j) expunge_instance(inst);
    }
  }
}

/* MCL: make sure we have a lock before expunging instances; this might 
   be done concurrently */

CBOOL__PROTO(prolog_purge) {
  instance_t *inst;
  intmach_t current_mem = total_mem_count;
  
  DEREF(X(0),X(0));
  inst = TaggedToInstance(X(0));

  Cond_Begin(inst->root->clause_insertion_cond);
  expunge_instance(inst);
  Broadcast_Cond(inst->root->clause_insertion_cond);

  INC_MEM_PROG(total_mem_count - current_mem);

  CBOOL__PROCEED;
}

/* Move all elements of a queue to another queue, and make all of them to
   point to the instance destinst */
void move_queue(instance_handle_t **srcq,
                instance_handle_t **destq,
                instance_t *destinst) {
  instance_handle_t *last, *running = *srcq;

#if defined(DEBUG_TRACE)
  int counter = 0;
#endif
#if defined(USE_THREADS)
  RTCHECK({
    int_info_t *root = *srcq && (*srcq)->inst_ptr ? (*srcq)->inst_ptr->root : NULL;
    if (root != NULL) RTCHECK_lockset(root);
  });
#endif
  DEBUG__TRACEconc(debug_conc,
                   "moving queue from %p to %p (-> instance %p)\n",
                   srcq, destq, destinst);

  if (running) {
    while(running) {
#if defined(DEBUG_TRACE)
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
  DEBUG__TRACEconc(debug_conc, "after moving queue made %d steps\n", counter);
}

/* Erase an instance. In the case of instances from concurrent predicates
   which are being pointed at, move the handle to the next available
   instance.  For now, fail if no matching instances are available.  When
   called for a concurrent predicate, it must be protected by a clause lock
   set at Prolog level.  Memory accounting: delay until we are done with
   concurrency-related pointer juggling. */

#define InstOrNull(Handle) Handle ? Handle->inst_ptr : NULL

#if defined(OPTIM_COMP)
CVOID__PROTO(prolog_erase_ptr, instance_t *node)
#else
CBOOL__PROTO(prolog_erase)
#endif
{
#if !defined(OPTIM_COMP)
  instance_t *node;
#endif
  int_info_t *root;
  intmach_t current_mem;

#if defined(USE_THREADS)
  instance_handle_t *x2_ins_h, *x5_ins_h;
  instance_t *ipp, *x2_insp, *x5_insp;
#endif

#if !defined(OPTIM_COMP)
  DEREF(X(0),X(0));
  node = TaggedToInstance(X(0));
#endif
  root = node->root;

  DEBUG__TRACEconc(debug_conc, "entering\n");
  RTCHECK_firstset(root);

#if defined(USE_THREADS) /* MCL */
  /* An instance is about to be deleted.  If the predicate is
     concurrent, and there are calls pointing at that instance, move
     the queue of pending calls to the new available instance.  In
     order to choose which clause is to be pointed at, any handle is
     equally valid; we use the first one.  This call must not block if
     no next instance exists: blocking is performed by
     '$current_instance'/1 . */
  if (root->behavior_on_failure != DYNAMIC) {
    RTCHECK_lockset(root);
    x2_ins_h = node->pending_x2;
    x5_ins_h = node->pending_x5;
    if (x2_ins_h || x5_ins_h) {
#if defined(OPTIM_COMP)
      x2_insp = InstOrNull(x2_ins_h);
      x5_insp = InstOrNull(x5_ins_h);
      LOCATE_NEXT_INSTANCE(x2_insp, x5_insp, ipp);
#else
      jump_to_next_instance(InstOrNull(x2_ins_h), 
                            InstOrNull(x5_ins_h), 
                            &ipp, &x2_insp, &x5_insp);
#endif

      DEBUG__TRACEconc(debug_conc, "moving handles hanging from %p\n", node);

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
      node->birth == node->death) {
    expunge_instance(node);
  } else {
    (void)CFUN__EVAL(active_instance,node,use_clock,TRUE);
  }

  INC_MEM_PROG(total_mem_count - current_mem);

  DEBUG__TRACEconc(debug_conc, "exiting!\n");
#if !defined(OPTIM_COMP)
  CBOOL__PROCEED;
#endif
}

/*-----------------------------------------------------------------*/

/* Convert between internal and external instance ID's, i.e.
 * between integers and '$ref'(_,_).
 */
CBOOL__PROTO(instance_to_ref, instance_t *ptr, tagged_t t) {
  tagged_t *h = G->heap_top;
  HeapPush(h,functor_Dref);
  HeapPush(h,PointerToTerm(ptr));
  HeapPush(h,ptr->rank);
  G->heap_top = h;
  CBOOL__LASTUNIFY(Tagp(STR,HeapCharOffset(h,-3*sizeof(tagged_t))), t);
}
static CFUN__PROTO(ref_to_instance, instance_t *, tagged_t x2) {
  tagged_t x1;
  instance_t *n;

  DerefSw_HVAorCVAorSVA_Other(x2,{
    CFUN__PROCEED(NULL);
  },{
    if (!TaggedIsSTR(x2)) CFUN__PROCEED(NULL);
    if (TaggedToHeadfunctor(x2) != functor_Dref) CFUN__PROCEED(NULL);

    DerefArg(x1,x2,1);
    DerefArg(x2,x2,2);
    if (!TaggedIsSmall(x1)) CFUN__PROCEED(NULL);
    n = TaggedToInstance(x1);
    if (!(n != NULL && n->rank == x2 && n->death == 0xffff)) CFUN__PROCEED(NULL);
    CFUN__PROCEED(n);
  });
}
#if !defined(OPTIM_COMP)
CBOOL__PROTO(prolog_ptr_ref) {
  DEREF(X(0),X(0));
  if (TaggedIsSmall(X(0))) {
    instance_t *ptr = TaggedToInstance(X(0));
    CBOOL__LASTCALL(instance_to_ref, ptr, X(1));
  } else {
    tagged_t x2;
    x2=X(1); DerefSw_HVAorCVAorSVA_Other(x2,;,{});
    instance_t *n = CFUN__EVAL(ref_to_instance, x2);
    CBOOL__TEST(n != NULL);
    CBOOL__UnifyCons(PointerToTerm(n),X(0));
    CBOOL__PROCEED;
  }
}
#endif

/* If the predicate is concurrent, it is still open, it has no clauses
   (i.e., this first clause is also the last one) and there are invocations
   waiting for a clause, wake them up and make them point to the new clause.
   Unfortunately, this results in a lack of indexing. */
#if defined(OPTIM_COMP)
CBOOL__PROTO(inserta, int_info_t *root, instance_t *n)
#else
/* ASSERT: X(0) is a dereferenced integer. */
CBOOL__PROTO(inserta)
#endif
{
#if !defined(OPTIM_COMP)
  instance_t *n;
#endif
  instance_t **loc;
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(0));
#endif
  intmach_t current_mem = total_mem_count;
#if defined(USE_THREADS) /* MCL */
  bool_t move_insts_to_new_clause = FALSE;
#endif

  Cond_Begin(root->clause_insertion_cond);

  DEBUG__TRACEconc(debug_conc, "(root = %p, first = %p, &first = %p)\n", root, root->first, &(root->first));

#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_CLOSED) { /* MCL */
    Broadcast_Cond(root->clause_insertion_cond);
    USAGE_FAULT("$inserta in an already closed concurrent predicate");
  }
#endif

#if !defined(OPTIM_COMP)
  DEREF(X(1),X(1));
  n = TaggedToInstance(X(1));
#endif

  /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
  if (!root->first) {
    n->rank = TaggedZero;
    n->forward = NULL;
    n->backward = n;
#if defined(USE_THREADS)
    if (root->behavior_on_failure == CONC_OPEN) {
      move_insts_to_new_clause = TRUE;    /* 'n' will be the new clause */
    }
#endif
  } else if (root->first->rank == TaggedLow) {
#if defined(OPTIM_COMP)
    PANIC_FAULT("database node full in assert or record");
#else
    SERIOUS_FAULT("database node full in assert or record");
#endif
  } else {
    n->rank = SmallSub(root->first->rank,1);
    n->forward = root->first;
    n->backward = root->first->backward;
    root->first->backward = n;
  }
  root->first = n;    
    
  n->root = root;
  n->birth = use_clock = def_clock;
  n->death = 0xffff;

#if defined(USE_THREADS) /* MCL */
  n->pending_x5 = n->pending_x2 = NULL;
#endif
    
  loc = (n->key==ERRORTAG ? &root->varcase :
         n->key==functor_lst ? &root->lstcase :
         (instance_t **)&hashtab_lookup(&root->indexer,n->key)->value.as_ptr);
    
  if (!(*loc)) {
    n->next_forward = NULL;
    n->next_backward = n;
  } else {
    n->next_forward = (*loc);
    n->next_backward = (*loc)->next_backward;
    (*loc)->next_backward = n;
  }
  (*loc) = n;
    
#if defined(USE_THREADS)
  if (move_insts_to_new_clause) {
    if (root->x2_pending_on_instance)
      move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
    if (root->x5_pending_on_instance)
      move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
  }
#endif

  DEBUG__TRACEconc(debug_conc, "leaving (root = %p, first = %p, &first = %p)\n", root, root->first, &(root->first));

  Broadcast_Cond(root->clause_insertion_cond);
    
  INC_MEM_PROG(total_mem_count - current_mem);
  CBOOL__PROCEED;
}

/* ASSERT: X(0) is a (dereferenced) small integer */
#if defined(OPTIM_COMP)
CBOOL__PROTO(insertz, int_info_t *root, instance_t *n)
#else
CBOOL__PROTO(insertz)
#endif
{
#if !defined(OPTIM_COMP)
  instance_t *n;
#endif
  instance_t **loc;
#if !defined(OPTIM_COMP)
  int_info_t *root = TaggedToRoot(X(0));
#endif
  intmach_t current_mem = total_mem_count;

  Cond_Begin(root->clause_insertion_cond);

  DEBUG__TRACEconc(debug_conc, "(root = %p, first = %p, &first = %p)\n", root, root->first, &(root->first));

#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_CLOSED) { /* MCL */
    Broadcast_Cond(root->clause_insertion_cond);
    USAGE_FAULT("$insertz in an already closed concurrent predicate");
  }
#endif

#if !defined(OPTIM_COMP)
  DEREF(X(1),X(1));
  n = TaggedToInstance(X(1));
#endif

  /* (void)ACTIVE_INSTANCE(root->first,use_clock,TRUE); optional */
    
  if (!root->first) {
    n->rank = TaggedZero;
    n->backward = n;
    root->first = n;
  } else if (root->first->backward->rank == TaggedIntMax) {
#if defined(OPTIM_COMP)
    PANIC_FAULT("database node full in assert or record");
#else
    SERIOUS_FAULT("database node full in assert or record");
#endif
  } else {
    n->rank = SmallAdd(root->first->backward->rank,1);
    n->backward = root->first->backward;
    root->first->backward->forward = n;
    root->first->backward = n;
  }

  n->root = root;
  n->birth = use_clock = def_clock;
  n->death = 0xffff;
  n->forward = NULL;
  n->next_forward = NULL;

#if defined(USE_THREADS) /* MCL */
  n->pending_x5 = n->pending_x2 = NULL;
#endif

  loc = (n->key==ERRORTAG ? &root->varcase :
         n->key==functor_lst ? &root->lstcase :
         (instance_t **)&hashtab_lookup(&root->indexer,n->key)->value.as_ptr);
    
  if (!(*loc)) {
    n->next_backward = n;
    (*loc) = n;
  } else {
    n->next_backward = (*loc)->next_backward;
    (*loc)->next_backward->next_forward = n;
    (*loc)->next_backward = n;
  }

#if defined(DEBUG_TRACE) && defined(USE_THREADS)
  if (root->behavior_on_failure != DYNAMIC) {
    DEBUG__TRACEconc(debug_conc, "insertz'ed clause %p\n", n);
  }
#endif
    
#if defined(USE_THREADS)
  if (root->behavior_on_failure == CONC_OPEN) {
    if (root->x2_pending_on_instance)
      move_queue(&root->x2_pending_on_instance, &n->pending_x2, n);
    if (root->x5_pending_on_instance)
      move_queue(&root->x5_pending_on_instance, &n->pending_x5, n);
  }
#endif

  DEBUG__TRACEconc(debug_conc, "leaving (root = %p, first = %p, &first = %p)\n", root, root->first, &(root->first));

  Broadcast_Cond(root->clause_insertion_cond);

  INC_MEM_PROG(total_mem_count - current_mem);
  CBOOL__PROCEED;
}

#if !defined(OPTIM_COMP)
/* used from internals.c prolog_interpreted_clause() */
/* (like insertz() but without code for threads) */
bool_t insertz_aux(int_info_t *root, instance_t *n) {
  instance_t **loc;
  intmach_t current_mem = total_mem_count;

  if (!root->first) {
    n->rank = TaggedZero;
    n->backward = n;
    root->first = n;
  } else if (root->first->backward->rank == TaggedIntMax) {
#if defined(OPTIM_COMP)
    PANIC_FAULT("database node full in assert or record");
#else
    SERIOUS_FAULT("database node full in assert or record");
#endif
  } else {
    n->rank = SmallAdd(root->first->backward->rank,1);
    n->backward = root->first->backward;
    root->first->backward->forward = n;
    root->first->backward = n;
  }

  n->root = root;
  n->birth = use_clock = def_clock;
  n->death = 0xffff;
  n->forward = NULL;
  n->next_forward = NULL;

  loc = (n->key==ERRORTAG ? &root->varcase :
         n->key==functor_lst ? &root->lstcase :
         (instance_t **)&hashtab_lookup(&root->indexer,n->key)->value.as_ptr);
    
  if (!(*loc)) {
    n->next_backward = n;
    (*loc) = n;
  } else {
    n->next_backward = (*loc)->next_backward;
    (*loc)->next_backward->next_forward = n;
    (*loc)->next_backward = n;
  }
    
  INC_MEM_PROG(total_mem_count - current_mem);
  CBOOL__PROCEED;
}
#endif

/* --------------------------------------------------------------------------- */

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

/* if normal==TRUE follow first-chain else next_forward */
CFUN__PROTO(active_instance, instance_t *, instance_t *i, int/*instance_clock_t*/ itime, bool_t normal) {
  choice_t *b;
  instance_t *j;
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
    if (b->next_alt == address_nd_current_instance) {
      latest_static = b2;
      j = TaggedToInstance(b->x[X2_CHN]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[ClockSlot]);
        if (lorank>j->rank) lorank=j->rank;
      }
      j = TaggedToInstance(b->x[X5_CHN]);
      if (j && (j->root==i->root)) {
        lotime = GetSmall(b->x[ClockSlot]);
        if (lorank>j->rank) lorank=j->rank;
      }   
    }
  }
  
  /* Mark all chpt before latest_static as static */

  for (b=latest_static;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    ChoiceptMarkStatic(b);
  }
  
  if (normal) {
    /* Follow forward-chain ? */
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
  } else {
    /* follow next_forward-chain ! */
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
  CFUN__PROCEED(i);
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

CVOID__PROTO(clock_overflow) {
  instance_clock_t *clocks, *clockp;
  instance_clock_t t, current = 0xfffe;
  int count = 1;
  choice_t *b;

  DEBUG__TRACEconc(debug_conc, "clock overflow\n");

  /* count # distinct clock values existing in choicepoints */
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    if (b->next_alt == address_nd_current_instance) {
      t = GetSmall(b->x[ClockSlot]);
      if (current!=t) {
        current=t;
        count++;
      }
    }
  }

  /* grab space for temporary array of clock values */
#if defined(OPTIM_COMP)
  /* TODO: macro to align word size */
  TEST_HEAP_OVERFLOW(G->heap_top, ((count*sizeof(instance_clock_t)+sizeof(tagged_t)-1)/sizeof(tagged_t))*sizeof(tagged_t), DynamicPreserved);
#else
  TEST_HEAP_OVERFLOW(G->heap_top, count*sizeof(instance_clock_t), DynamicPreserved);
#endif
  clocks = (instance_clock_t *)G->heap_top;

  /* fill in distinct chpt clock values, relocating them as we go */
  clockp = clocks+count;
  *(--clockp) = 0xfffe;
  def_clock = count;
  use_clock = count-1;
  X(4) = MakeSmall(count-1);
  
  for (b=w->previous_choice;
       !ChoiceptTestStatic(b);
       b=ChoiceCont(b)) {
    if (b->next_alt == address_nd_current_instance) {
      t = GetSmall(b->x[ClockSlot]);
      if ((*clockp)!=t) *(--clockp)=t;
      b->x[ClockSlot] = MakeSmall(clockp-clocks);
    }
  }
  /* relocate all instance clocks */
#if defined(OPTIM_COMP)
  relocate_table_clocks(predicates_location,clocks);
#else
  relocate_table_clocks(prolog_predicates,clocks);
#endif
  relocate_gcdef_clocks(clocks);
}

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP) && defined(ABSMACH_OPT__regmod)
CBOOL__PROTO(interpreted_drain_marked, definition_t *f, tagged_t mark) {
  /* TODO: study if this code is correct --jf */
  /* TODO: what happens with concurrent predicates? */
  int_info_t *root;
  instance_t *i, *j;

  root = f->code.intinfo;
  for (i = root->first; i; i=j) {
    j = i->forward;
    if (i->mark == mark) {
      CVOID__CALL(prolog_erase_ptr, i);
    }
  }
  /* TODO: necessary? */
  CVOID__CALL(prolog_unlock_predicate, root);

  /* TODO: check if it has any problems... */
  /* Abolish if predicate is empty */
  if (root->first == NULL) {
    CBOOL__CALL(abolish, f);
  }
  CBOOL__PROCEED;
}
#endif

#if !defined(OPTIM_COMP) && defined(ABSMACH_OPT__regmod2)
CBOOL__PROTO(interpreted_drain_marked, definition_t *f, tagged_t mark) {
  /* TODO: study if this code is correct --jf */
  /* TODO: what happens with concurrent predicates? */
  int_info_t *root;
  instance_t *i, *j;

  root = f->code.intinfo;
  for (i = root->first; i; i=j) {
    j = i->forward;
    if (i->mark == mark) {
      CVOID__CALL(prolog_erase_ptr, i);
    }
  }
  /* TODO: necessary? */
  CVOID__CALL(prolog_unlock_predicate, root);

  /* TODO: check if it has any problems... */
  /* Abolish if predicate is empty */
  if (root->first == NULL) {
    CBOOL__CALL(abolish, f);
  }
  CBOOL__PROCEED;
}
#endif

/* --------------------------------------------------------------------------- */
/* Dynamic prolog database update */
/* (C coded) --jfran */

#if defined(OPTIM_COMP)

#define GET_ROOT(X) \
  DEREF(X,X); \
  int_info_t *root; \
  root = TaggedToRoot(X);

/* Fails if not interpreted */
#define HEAD_ROOT(X) \
  DEREF(X, X); \
  int_info_t *root; \
  root = find_int_info(X); \
  CBOOL__TEST(root != NULL);

/* Fails if not interpreted */
#define HEAD_ROOT_INSNP(X) \
  DEREF(X, X); \
  int_info_t *root; \
  root = find_int_info(X); \
  if (root == NULL) CINSNP__FAIL;

/* (from bc_aux.h) */
CFUN__PROTO(compile_term_aux, instance_t *, tagged_t head, tagged_t body, tagged_t mark);
#define COMPILE_TERM \
  instance_t *ptr; \
  ptr = CFUN__EVAL(compile_term_aux, X(0), X(1), ERRORTAG);

// '$asserta_root'/3
CBOOL__PROTO(prolog_asserta_root) {
  GET_ROOT(X(2));
  COMPILE_TERM;
  CBOOL__CALL(inserta, root, ptr);
  CBOOL__PROCEED;
}
// '$asserta'/2
CBOOL__PROTO(prolog_asserta) {
  HEAD_ROOT(X(0));
  COMPILE_TERM;
  CBOOL__CALL(inserta, root, ptr);
  CBOOL__PROCEED;
}
// '$asserta_ref'/3
CBOOL__PROTO(prolog_asserta_ref) {
  HEAD_ROOT(X(0));
  COMPILE_TERM;
  CBOOL__CALL(inserta, root, ptr);
  CBOOL__CALL(instance_to_ref, ptr, X(2));
  CBOOL__PROCEED;
}
// '$assertz_root'/3
CBOOL__PROTO(prolog_assertz_root) {
  GET_ROOT(X(2));
  COMPILE_TERM;
  CBOOL__CALL(insertz, root, ptr);
  CBOOL__PROCEED;
}
// '$assertz'/2
CBOOL__PROTO(prolog_assertz) {
  HEAD_ROOT(X(0));
  COMPILE_TERM;
  CBOOL__CALL(insertz, root, ptr);
  CBOOL__PROCEED;
}
// '$assertz_ref'/3
CBOOL__PROTO(prolog_assertz_ref) {
  HEAD_ROOT(X(0));
  COMPILE_TERM;
  CBOOL__CALL(insertz, root, ptr);
  CBOOL__CALL(instance_to_ref, ptr, X(2));
  CBOOL__PROCEED;
}

// '$erase'/2
CINSNP__PROTO(prolog_erase) {
  HEAD_ROOT_INSNP(X(0));
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL(current_instance, root, BLOCK);
}
// '$erase_nb_root'/3
CINSNP__PROTO(prolog_erase_nb_root) {
  GET_ROOT(X(2));
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL(current_instance, root, NO_BLOCK);
}
// '$erase_nb'/2
CINSNP__PROTO(prolog_erase_nb) {
  HEAD_ROOT_INSNP(X(0));
  X(3) = MakeSmall(0); /* erase */
  CINSNP__LASTCALL(current_instance, root, NO_BLOCK);
}
// '$erase_ref'/1
CBOOL__PROTO(prolog_erase_ref) {
  tagged_t Ref;
  instance_t *ptr;
  DEREF(Ref, X(0));
  ptr = CFUN__EVAL(ref_to_instance, Ref);
  CBOOL__TEST(ptr != NULL); 
  CVOID__CALL(prolog_erase_ptr, ptr);
  CBOOL__PROCEED;
}
// '$current'/2
CINSNP__PROTO(prolog_current) {
  HEAD_ROOT_INSNP(X(0));
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL(current_instance, root, BLOCK);
}
// '$current_nb_root'/3
CINSNP__PROTO(prolog_current_nb_root) {
  GET_ROOT(X(2));
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL(current_instance, root, NO_BLOCK);
}
// '$current_nb'/2
CINSNP__PROTO(prolog_current_nb) {
  HEAD_ROOT_INSNP(X(0));
  X(3) = MakeSmall(2); /* nothing */
  CINSNP__LASTCALL(current_instance, root, NO_BLOCK);
}
// '$current_nb_ref'/3
CINSNP__PROTO(prolog_current_nb_ref) {
  tagged_t Ref;
  DEREF(Ref, X(2));
  if (!IsVar(Ref)) {
    instance_t *ptr;
    ptr = CFUN__EVAL(ref_to_instance, Ref);
    if (ptr == NULL) CINSNP__FAIL; 
    /* Initialize dynpreserved (to ensure heapmargin correctness) */
    /* TODO: necessary? */
    /* X(0) must be the head */
    /* X(1) must be the body */
    X(X2_CHN) = TermNull;
    X(3) = MakeSmall(3); /* nothing, no unlock */
    X(ClockSlot) = MakeSmall(0);
    X(X5_CHN) = TermNull; 
    X(RootArg) = TermNull; /* TODO: hmmm unknown, so no unlock_predicate can be done... is this correct? */
    X(InvocationAttr) = MakeSmall(0);
    X(PrevDynChpt) = TermNull;
    w->ins = ptr;
    CINSNP__GOTO(w->ins->emulcode);
  } else {
    HEAD_ROOT_INSNP(X(0));
    X(3) = Ref; /* (variable) get ref */
    CINSNP__LASTCALL(current_instance, root, NO_BLOCK);
  }
}
// '$open_pred'/1
CBOOL__PROTO(prolog_open_pred) {
  HEAD_ROOT(X(0));
  CVOID__CALL(open_predicate, root);
  CBOOL__PROCEED;
}
// '$close_pred'/1
CBOOL__PROTO(prolog_close_pred) {
  HEAD_ROOT(X(0));
  CVOID__CALL(close_predicate, root);
  CBOOL__PROCEED;
}

CBOOL__PROTO(add_interpreted_clause, definition_t *d, tagged_t head, tagged_t body, tagged_t mark) {
  /* TODO: assert (d!=NULL) && (d->predtyp==ENTER_INTERPRETED) */
  instance_t *ptr;
  ptr = CFUN__EVAL(compile_term_aux, head, body, mark);
  CBOOL__LASTCALL(insertz, d->code.intinfo, ptr);
}

#endif
