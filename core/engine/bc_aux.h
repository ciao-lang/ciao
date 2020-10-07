/*
 *  bc_aux.h (NOTE: included from wam.c)
 *
 *  Auxiliary definitions for the Ciao bytecode interpreter.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/instrdefs.h>
#include <ciao/internals.h>
#include <ciao/io_basic.h>

/* ------------------------------------------------------------------------- */

/* Attributed variables support */
extern definition_t *address_pending_unifications;
extern definition_t *address_uvc;
extern definition_t *address_ucc;

/* ------------------------------------------------------------------------- */

#define SAVE_FIELD(Name) desc->wam_private_state.Name = Name

#define SAVE_WAM_STATE \
  SAVE_FIELD(p); \
  SAVE_FIELD(i);\
  SAVE_FIELD(pt1);\
  SAVE_FIELD(pt2);\
  SAVE_FIELD(t0);\
  SAVE_FIELD(t1);\
  SAVE_FIELD(t2);\
  SAVE_FIELD(t3);\
  SAVE_FIELD(ptemp);\
  SAVE_FIELD(wam_exit_code);\
  SAVE_FIELD(ins)

#define RECOVER_FIELD(Name) Name = desc->wam_private_state.Name

#define RECOVER_WAM_STATE \
  RECOVER_FIELD(p);\
  RECOVER_FIELD(i);\
  RECOVER_FIELD(pt1);\
  RECOVER_FIELD(pt2);\
  RECOVER_FIELD(t0);\
  RECOVER_FIELD(t1);\
  RECOVER_FIELD(t2);\
  RECOVER_FIELD(t3);\
  RECOVER_FIELD(ptemp);\
  RECOVER_FIELD(wam_exit_code);\
  RECOVER_FIELD(ins)

/* Macros for conditional code inside other macros */

#if defined(TABLING)
#define ON_TABLING(X) X
#else
#define ON_TABLING(X)
#endif

#if defined(ANDPARALLEL)
#define ON_ANDPARALLEL(X) X
#else
#define ON_ANDPARALLEL(X)
#endif

#if defined(DEBUG)
#define ON_DEBUG(X) X
#else
#define ON_DEBUG(X)
#endif

#if defined(DEBUG_NODE)
#define ON_DEBUG_NODE(X) X
#else
#define ON_DEBUG_NODE(X)
#endif

/* ------------------------------------------------------------------------- */

static try_node_t *get_null_alt(int arity);

bcp_t startgoalcode;                   /* WAM code to start a goal -- Shared */
bcp_t bootcode;                /* WAM bootstrap to run bootgoal -- Shared */
// #if defined(INTERNAL_CALLING)
// bcp_t internal_calling;                     /* WAM bootstrap to run bootgoal -- Shared */
// #endif
bcp_t contcode;/* continuations of FrameSize N after exceptions -- Shared */
bcp_t failcode;       /* continuation of FrameSize 0 that fails -- Shared */
bcp_t exitcode;   /* continuation of FrameSize 0 that exits wam -- Shared */

static bcp_t insnfail;                              /* Shared */
try_node_t *null_alt = NULL;/* linked list of null alternatives - Shared*/

try_node_t *termcode;/* "clause" of arity 1 that exits wam -- Shared */
try_node_t *fail_alt;         /* null alternative, arity=0 -- Shared */

/* Find a null alt. of 'arity', or push a new one. */
static try_node_t *get_null_alt(int arity)
{
  try_node_t *a;
  intmach_t current_mem = total_mem_count;

  for (a = null_alt; a; a = (a+1)->next)
    if (a->choice_offset == ArityToOffset(arity)) return a;

  a = (try_node_t *)checkalloc(sizeof(try_node_t)
                               + sizeof(try_node_t *)
#if defined(GAUGE)
                               + 2*sizeof(intmach_t)
#endif
                               );

  INC_MEM_PROG(total_mem_count - current_mem);

  a->choice_offset = ArityToOffset(arity);
  a->number = 0;
  a->emul_p = insnfail;
  a->emul_p2 = insnfail;
  (a+1)->next = null_alt;       /* tail of list */
#if defined(GAUGE)
  a->entry_counter = (intmach_t *)(a+1)+1;
  a->entry_counter[0] = 0;
  a->entry_counter[1] = 0;
#endif
  a->next = NULL;               /* no more alternatives */
  null_alt = a;
  return a;
}


/* return no. of bytes to skip if first alt. in read mode (skip work
   already done by indexing on first argument) */
int p2_offset(uintmach_t insn)
{
  switch(insn) {
  case GET_NIL_X0:
    return FTYPE_size(f_o);
  case GET_LIST_X0:
    return FTYPE_size(f_o);
  case GET_CONSTANT_X0:
    return FTYPE_size(f_o)+FTYPE_size(f_t);
  case GET_STRUCTURE_X0:
    return FTYPE_size(f_o)+FTYPE_size(f_f);
  case GET_CONSTANT_X0Q:
    return FTYPE_size(f_o)+FTYPE_size(f_Q)+FTYPE_size(f_t);
  case GET_STRUCTURE_X0Q:
    return FTYPE_size(f_o)+FTYPE_size(f_Q)+FTYPE_size(f_f);
  default:
    return 0;
  }
}

try_node_t *def_retry_c(cbool0_t proc, int arity)
{
  try_node_t *item;
  bcp_t P;

  item = (try_node_t *)
             checkalloc(sizeof(try_node_t)
                        // TODO check if this emul_info_t makes sense here
                        //+SIZEOF_FLEXIBLE_STRUCT(emul_info_t, char, FTYPE_size(f_o))
                        +FTYPE_size(f_o)
                        +FTYPE_size(f_Q)
                        +FTYPE_size(f_C)
#if defined(GAUGE)
                        +2*sizeof(intmach_t)
#endif
                        );
  item->choice_offset = ArityToOffset(arity);
  item->number = 0;
  P = item->emul_p = (bcp_t )(((char *)item)+sizeof(try_node_t));
  item->emul_p2 = P;
  item->next = item;
  EMIT_o(RETRY_CQ);
  EMIT_Q(0);
  EMIT_C((void *)proc);
#if defined(GAUGE)
  item->entry_counter = (intmach_t *)P;
  item->entry_counter[0] = 0;
  item->entry_counter[1] = 0;
#endif
  return item;
}

/* --------------------------------------------------------------------------- */

liveinfo_t prolog_format_print_integer__liveinfo;

void init_some_bytecode(void) {
  /*
    |CALLQ|0|address_call/1|...padding...|Initial Frame Size|EXIT_TOPLEVEL|
    ^                                                       ^
    bootcode                                                termcode?
  */
  {
    bcp_t P = (bcp_t)
      checkalloc_ARRAY(char,
                       (FTYPE_size(f_o)+
                        FTYPE_size(f_Q)+
                        FTYPE_size(f_E)+
                        FTYPE_size(f_e)+
                        FTYPE_size(f_o)+
                        2*FTYPE_size(f_i))); /* TODO: needed? */
    bootcode = P;
    EMIT_o(CALLQ);
    EMIT_Q(0);
    EMIT_E(address_call);
    EMIT_e(EToY0*sizeof(tagged_t)); /* initial FrameSize */
    exitcode = P;
    EMIT_o(EXIT_TOPLEVEL);

    termcode = def_retry_c(NULL,1);                 /* size of initial cpt. */
    P = termcode->emul_p;
    EMIT_o(EXIT_TOPLEVEL);

    address_nd_current_instance = def_retry_c(NULL,DynamicPreserved);
    P = address_nd_current_instance->emul_p;
    EMIT_o(RETRY_INSTANCE);
  }

// #if defined(INTERNAL_CALLING)
// {
//   bcp_t P = (bcp_t)
//     checkalloc_ARRAY(char, ...);
//   internal_calling = P;
//   EMIT_o(CALLQ);
//   EMIT_Q(0);
//   EMIT_E(address_internal_call);
//   EMIT_e(EToY0*sizeof(tagged_t)); /* initial FrameSize */
//   EMIT_o(EXIT_TOPLEVEL);
// }
// #endif

  /*
    |CALLQ|0|address_call/1|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
    ^                                                            ^
    bootcode                                                     termcode?
  */
  {
    bcp_t P = (bcp_t)
      checkalloc_ARRAY(char,
                       (FTYPE_size(f_o)+
                        FTYPE_size(f_Q)+
                        FTYPE_size(f_E)+
                        FTYPE_size(f_e)+
                        FTYPE_size(f_o)+
                        2*FTYPE_size(f_i))); /* TODO: needed? */
    startgoalcode = P;
    EMIT_o(CALLQ);
    EMIT_Q(0);
    EMIT_E(address_call);
    EMIT_e(EToY0*sizeof(tagged_t)); /* initial FrameSize */
    EMIT_o(EXIT_TOPLEVEL);
  }

  /* must be initialized before get_null_alt() is used! */
  {
    bcp_t P = (bcp_t)
      checkalloc_ARRAY(char, FTYPE_size(f_o));
    insnfail = P;
    EMIT_o(FAIL);
  }
  
  {
    bcp_t P = (bcp_t)
      checkalloc_ARRAY(char,
                       (FTYPE_size(f_e)+
                        FTYPE_size(f_o)+
                        FTYPE_size(f_E)+
                        2*FTYPE_size(f_i))); /* TODO: needed? */

    fail_alt = get_null_alt(0);

    failcode = BCoff(P, FTYPE_size(f_e));
    EMIT_e(EToY0*sizeof(tagged_t)); /* FrameSize */
    EMIT_o(EXECUTE);
    EMIT_E(address_fail);
  }

  /* CONTCODE(i+1)
     is a good continuation when interrupting 'foo/i'. */
  {
    bcp_t P = (bcp_t)
      checkalloc_ARRAY(char,
                       (FTYPE_size(f_e)+
                        FTYPE_size(f_o)+
                        FTYPE_size(f_i) /* TODO: needed? */
                        )*ARITYLIMIT);
    int i;
    contcode = BCoff(P, FTYPE_size(f_e));
    for (i=0; i<ARITYLIMIT; i++) {
      EMIT_e((EToY0+i)*sizeof(tagged_t)); /* FrameSize */
      EMIT_o(KONTINUE);
    }
  }

  LIVEINFO__INIT(prolog_format_print_integer__liveinfo, CONTPAD, 3);
}

/* --------------------------------------------------------------------------- */
#if defined(PROFILE)
extern bool_t profile;       /* profile execution -- Shared */
#endif

CBOOL__PROTO(set_trace_calls)
{
  tagged_t x;
  DEREF(x,X(0));
  if (!TaggedIsSmall(x))
    return FALSE;
  trace_calls = (bool_t)GetSmall(x);
#if defined(PROFILE)
  if (profile||trace_calls) stop_on_pred_calls = TRUE;
#else
  if (trace_calls) stop_on_pred_calls = TRUE;
#endif
  return TRUE;
}

/* run_determ_c(goal) runs the goal and returns TRUE if goal is
   defined as a c predicate, even if that predicate fails.  Otherwise
   it returns false.
   */

extern bcp_t startgoalcode;                                          /* Shared */

/* CALLQ|call/1|goal=X(0)|exit_toplevel */
CBOOL__PROTO(run_determ_c, tagged_t goal)
{
  definition_t *func;
  int i;
  tagged_t *s;

#if 0
  printf("run_determ_c: ");
  prolog_display(Arg); /* argument is in X(0) */
  printf("\n");
#endif

  DEREF(goal,goal);
  func = find_definition(predicates_location,goal,&w->structure,FALSE);

  if (func==NULL) return FALSE;

  if (func->enter_instr == ENTER_C) {
    for (i=func->arity, s=w->structure; --i>=0;)
      RefHeap(w->term[i],HeapOffset(s,i));
    return (*(cbool0_t)func->code.proc)(Arg);
  } else {
    if (func->enter_instr == BUILTIN_CALL) {
      w->next_insn = bootcode;          /* Should have been initialized */
      wam(w, NULL);
      return TRUE;
    }
  }
  return FALSE;
}

/* --------------------------------------------------------------------------- */
/* Term compiler for assert/record. */

static CBOOL__PROTO(c_term, 
                    tagged_t t,
                    int Xreg,
                    int FreeReg,
                    int x_variables,
                    tagged_t **trail_origo,
                    bcp_t *current_insn);
static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P);
static CVOID__PROTO(c_term_mark,
                    tagged_t t,
                    intmach_t temps,
                    intmach_t *hsize,
                    intmach_t *maxtemps,
                    intmach_t *bsize,
                    tagged_t **trail_origo);
static CVOID__PROTO(c_term_trail_push,
                    tagged_t t,
                    tagged_t **trail_origo);

/* NOTE: Aligned to the size of the pointers! */
#define ODDOP(Insn)        \
  if (((uintptr_t)P)&(sizeof(uintptr_t)-1))     \
    { EMIT(Insn-0); }   \
  else \
    { EMIT(Insn-1); EMIT_Q(0); }
#define EVENOP(Insn) \
  if (!(((uintptr_t)P)&(sizeof(uintptr_t)-1)))  \
    { EMIT(Insn-0); }                           \
  else \
    { EMIT(Insn-1); EMIT_Q(0); }
#define EMIT(I) { Last_Insn=P; EMIT_o((I)); }

/* TODO: Move as a compiler context structure -- JFMC */

/* I believe the following must be shared and locked, because they are
   related to code compilation */

/*static bcp_t */
 /* current_insn, */        /* Inside compile_term_aux and passed around */
 /* last_insn;    */                          /* Inside compile_term_aux */


/*static int*/
 /* x_variables, */       /* Now inside compile_term_aux and pased around */
 /* hsize,       */                         /* Inited in compile_term_aux */
 /* bsize,       */                         /* Inited in compile_term_aux */
 /* maxtemps;    */                         /* Inited in compile_term_aux */

/*static tagged_t*/
 /* *trail_origo;*/                         /* Inited in compile_term_aux */


static CVOID__PROTO(c_term_trail_push, tagged_t t, tagged_t **trail_origo);
static CVOID__PROTO(c_term_mark,
                    tagged_t t, intmach_t temps,
                    intmach_t *hsize, intmach_t *maxtemps, intmach_t *bsize, tagged_t **trail_origo);
static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P);
static CBOOL__PROTO(c_term, tagged_t t, int Xreg, int FreeReg,
                    int x_variables, tagged_t **trail_origo,
                    bcp_t *current_insn);
CBOOL__PROTO(compile_term, worker_t **new_worker);
CFUN__PROTO(compile_term_aux, instance_t *,
            tagged_t head, tagged_t body, worker_t **new_worker);

static CVOID__PROTO(c_term_trail_push, tagged_t t, tagged_t **trail_origo) {
  if (!ChoiceDifference(w->choice,w->trail_top)) {
    tagged_t *tr = w->trail_top;
    int reloc;

    choice_overflow(Arg,-CHOICEPAD);
    reloc = (char *)w->trail_top - (char *)tr;
    *trail_origo = (tagged_t *)((char *)*trail_origo + reloc);
    tr = w->trail_top;
    while (TrailGetTop(tr) & QMask) {
      TrailDec(tr);
      *tr += reloc; // (tr points to the popped element)
    }
    while (TrailYounger(tr,*trail_origo+DynamicPreserved)) {
      TrailDec(tr);
      *TagToPointer(*tr) += reloc; // (tr points to the popped element)
    }
  }
  TrailPush(w->trail_top,t);
}

//#define Tr(Str) fprintf(stderr, "%s\n", (Str));
#define Tr(Str)

static CVOID__PROTO(c_term_mark,
                    tagged_t t,
                    intmach_t temps,
                    intmach_t *hsize, intmach_t *maxtemps, intmach_t *bsize,
                    tagged_t **trail_origo) {
  CIAO_REG_2(tagged_t, t1);
  int i, arity;

 start:
  DerefHeapSwitch(t,t1,{goto var_size;});
  /* nonvar */
  if (!(t & TagBitComplex)) { /* NUM or ATM */
    if (t&QMask && t&TagBitFunctor) { /* ATM with QMask mark */
      if (!(t&3)) {
        /* unify_variable */
        Tr("m:x");
        *bsize += FTYPE_size(f_x);
        (*TagToPointer(*TagToPointer(t)))++;
      } else {
        /* unify_value */
        Tr("m:x");
        *bsize += FTYPE_size(f_x);
      }
    } else if (t!=atom_nil) {
      /* unify_ + pad + constant */
      Tr("m:(Q)+t");
      *bsize += FTYPE_size(f_Q)+FTYPE_size(f_t);
    }
    return;
  } else if (!(t & TagBitFunctor)) { /* LST */
    /* unify_ + xi + get_list + xj + 2*u */
    Tr("m:x+o (?)");
    Tr("m:x");
    *bsize += (FTYPE_size(f_x)+
               FTYPE_size(f_o)+
               FTYPE_size(f_x)+
               2*FTYPE_size(f_o));
    *hsize += 2*sizeof(tagged_t);
    if (*maxtemps < temps) {
      *maxtemps = temps;
    }
    RefCar(t1,t);
    Tr("m:o"); /* (from *bsize+=) */
    c_term_mark(Arg, t1, temps+1, hsize, maxtemps, bsize, trail_origo);
    RefCdr(t,t);
    Tr("m:o"); /* (from *bsize+=) */
    goto start;
  } else { /* STR or large NUM */
    if (STRIsLarge(t)) {
      arity = LargeSize(TagToHeadfunctor(t)); /* TODO: not 'arity'! */
      /* unify_ + xi + get_large + pad + xj + (functor + largeNum) */
      Tr("m:x+o (?)");
      Tr("m:(Q)+x+o(?)+bnlen");
      *bsize += (FTYPE_size(f_x)+
                 FTYPE_size(f_o)+
                 FTYPE_size(f_Q)+
                 FTYPE_size(f_x)+
                 FTYPE_size(f_o)+
                 arity);
      *hsize += sizeof(tagged_t)+arity;
      return;
    } else {
      arity = Arity(TagToHeadfunctor(t));
      /* unify_ + xi + get_structure + pad + functor + xj + arity*u */
      Tr("m:x+o (?)");
      Tr("m:(Q)+x+f");
      *bsize += (FTYPE_size(f_x)+
                 FTYPE_size(f_o)+
                 FTYPE_size(f_Q)+
                 FTYPE_size(f_x)+
                 FTYPE_size(f_f)+
                 arity*FTYPE_size(f_o));
      *hsize += (1+arity)*sizeof(tagged_t);
      if (*maxtemps < temps) {
        *maxtemps = temps;
      }
      for (i=1; i<arity; i++) {
        Tr("m:o"); /* (from *bsize+=) */
        t1 = *TaggedToArg(t,i);
        c_term_mark(Arg, t1, temps+arity-i, hsize, maxtemps, bsize, trail_origo);
      }
      Tr("m:o"); /* (from *bsize+=) */
      t = *TaggedToArg(t,arity);
      goto start;
    }
  }
 var_size:
  if (t & TagBitCVA) { /* CVA */
    *TagToPointer(t) = Tagp(ATM,w->trail_top)|QMask|2;
    c_term_trail_push(Arg,t,trail_origo);
    /* unify_variable + xi + get_constraint + xj + 2*u */
    Tr("m:x");
    Tr("m:o+x");
    Tr("m:o");
    Tr("m:o");
    *bsize += (FTYPE_size(f_x)+
               FTYPE_size(f_o)+
               FTYPE_size(f_x)+
               FTYPE_size(f_o)+
               FTYPE_size(f_o));
    if (*maxtemps < temps) {
      *maxtemps = temps;
    }
    t = Tagp(LST,TaggedToGoal(t));
    goto start;
  } else { /* HVA */
    *TagToPointer(t) = Tagp(ATM,w->trail_top)|QMask;
    c_term_trail_push(Arg,t,trail_origo);
    /* unify_variable + xi */
    Tr("m:x");
    *bsize += FTYPE_size(f_x);
  }
  return;
}

static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P) {
  switch (BCOp(Last_Insn, FTYPE_ctype(f_o), 0)) {
  case UNIFY_VOID_1:
  case UNIFY_VOID_2:
  case UNIFY_VOID_3:
    BCOp(Last_Insn, FTYPE_ctype(f_o), 0)++;
    break;
  case UNIFY_VOID_4:
    BCOp(Last_Insn, FTYPE_ctype(f_o), 0) = UNIFY_VOID;
    Tr("e:i");
    EMIT_i(5);
    break;
  case UNIFY_VOID:
    BCOp(P, FTYPE_ctype(f_i), -FTYPE_size(f_i))++;
    break;
  default:
    Tr("e:o");
    EMIT(UNIFY_VOID_1);
  }
  return P;
}

static CBOOL__PROTO(c_term, 
                    tagged_t t,
                    int Xreg, int FreeReg,
                    int x_variables,
                    tagged_t **trail_origo,
                    bcp_t *current_insn) {
  bcp_t psave;
  tagged_t *ssave;
  int i, ar = ~0, decr, Treg;
  bcp_t P = *current_insn;
  tagged_t *s = NULL;
  tagged_t t1;

  /* Step 1: Emit GET instruction for term's principal functor. */
  switch (TagOf(t)) { /* t is already dereferenced */
  case LST:
    ar = 2;
    s = TagpPtr(LST,t);
    Tr("e:o+x");
    EMIT(GET_LIST);
    EMITtok(f_x, Xop(Xreg));
    break;
  case STR:
    if (STRIsLarge(t)) {
      Tr("e:o(Q)+x+bnlen");
      EVENOP(GET_LARGE);
      EMITtok(f_x, Xop(Xreg));
      P = BCoff(P, compile_large(t, P));
      *current_insn = P;
      return TRUE;
    } else {
      Tr("e:o(Q)+x+f");
      ar = Arity(TagToHeadfunctor(t));
      s = TaggedToArg(t,1);
      EVENOP(GET_STRUCTURE);
      EMITtok(f_x, Xop(Xreg));
      EMIT_f(TagToHeadfunctor(t));
      break;
    }
  case ATM:
    if (t & QMask) {
      goto term_is_var;
    } else if (t==atom_nil) {
      Tr("e:o+x");
      EMIT(GET_NIL);
      EMITtok(f_x, Xop(Xreg));
      *current_insn = P;
      return TRUE;
    }
  case NUM:
    Tr("e:o(Q)+x+t");
    EVENOP(GET_CONSTANT);
    EMITtok(f_x, Xop(Xreg));
    EMIT_t(t);
    *current_insn = P;
    return TRUE;

  term_is_var:
    {
      if ((t&3)!=3) { /* get_variable */
        Tr("e:o+x+x");
        EMIT(GET_X_VARIABLE);
        EMITtok(f_x, Xop(Xreg));
        EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
        if (t&2) { /* enqueue constraint */
          c_term_trail_push(Arg,t,trail_origo);
        }
        *TagToPointer(*TagToPointer(t)) |= 3;
      } else { /* get_value */
        Tr("e:o+x+x");
        EMIT(GET_X_VALUE);
        EMITtok(f_x, Xop(Xreg));
        EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
      }
      *current_insn = P;
      return TRUE;
    }
  }

  /* Step 2: Emit tail-recursive UNIFY sequence for all subargs. */
  psave = P;
  ssave = s;
  Treg = reg_bank_size;

  for (i=1; i<=ar; i++) {
    RefHeapNext(t,s);
    DerefHeapSwitch(t,t1,{ goto arg_is_void; });
    switch (TagOf(t)) {
    case LST:
      if ((i==ar) && (Treg==reg_bank_size)) {
        Tr("e:o");
        EMIT(UNIFY_LIST);
        s = TagpPtr(LST,t);
        i=0, ar=2;
      } else {
        Tr("e:o+x");
        EMIT(UNIFY_X_VARIABLE);
        EMITtok(f_x, Xop(Treg++));
      }
      break;
    case STR:
      if (STRIsLarge(t)) {
        if ((i==ar) && (Treg==reg_bank_size)) {
          Tr("e:o(Q)+bnlen");
          ODDOP(UNIFY_LARGE);
          P = BCoff(P, compile_large(t, P));
        } else {
          Tr("e:o+x");
          EMIT(UNIFY_X_VARIABLE);
          EMITtok(f_x, Xop(Treg++));
        }
      } else if ((i==ar) && (Treg==reg_bank_size)) {
        Tr("e:o(Q)+f");
        ODDOP(UNIFY_STRUCTURE);
        EMIT_f(TagToHeadfunctor(t));
        s = TaggedToArg(t,1);
        i=0, ar=Arity(TagToHeadfunctor(t));
      } else {
        Tr("e:o+x");
        EMIT(UNIFY_X_VARIABLE);
        EMITtok(f_x, Xop(Treg++));
      }
      break;
    case ATM:
      if (t & QMask) {
        goto arg_is_var;
      } else if (t==atom_nil) {
        Tr("e:o");
        EMIT(UNIFY_NIL);
        break;
      }
    case NUM:
      Tr("e:o(Q)+t");
      ODDOP(UNIFY_CONSTANT);
      EMIT_t(t);
      break;

    arg_is_var:
      {
        if ((t&3)!=3) { /* unify_variable */
          Tr("e:o+x");
          EMIT(UNIFY_X_VARIABLE);
          EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
          if (t&2)      /* enqueue constraint */
            c_term_trail_push(Arg,t,trail_origo);
          *TagToPointer(*TagToPointer(t)) |= 3;
        } else { /* unify_value */
          Tr("e:o+x");
          EMIT(UNIFY_X_VALUE);
          EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
        }
        break;
      }

    arg_is_void:
      P = emit_unify_void(Arg, P);
      break;
    }
  }

  /* Step 3: Scan emitted code and recursively emit code for nested args. */
  *current_insn = P;
  if (FreeReg < x_variables)
    return FALSE;
  if (Treg==reg_bank_size)
    return TRUE;
  decr = Treg-1-FreeReg;
  s = ssave;
  P = psave;
  psave = *current_insn;

  while (P < psave) {
    switch (BcFetchOPCODE()) {
    case UNIFY_LIST:
      DerefHeap(t,s);
      s = TagpPtr(LST,t);
      break;
    case UNIFY_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_f));
      DerefHeap(t,s);
      s = TaggedToArg(t,1);
      break;
    case UNIFY_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_LARGE:
      P = BCoff(P, LargeSize(*(tagged_t *)P));
      (void)HeapNext(s);
      break;
    case UNIFY_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_CONSTANT:
      P = BCoff(P, FTYPE_size(f_t));
    case UNIFY_NIL:
      (void)HeapNext(s);
      break;
    case UNIFY_X_VARIABLE:
      i = Xinv(BCOp(P, FTYPE_ctype(f_x), 0));
      if (i>=reg_bank_size) {
        EMITtok(f_x, Xop(i-decr)); /* TODO: patch? */
        DerefHeapNext(t,s);
        if (!c_term(Arg,t,i-decr,i-decr,x_variables,
                    trail_origo,current_insn))
          return FALSE;
        break;
      }
    case UNIFY_X_VALUE:
      P = BCoff(P, FTYPE_size(f_x));
      (void)HeapNext(s);
      break;
    case UNIFY_VOID:
      s += BCOp(P, FTYPE_ctype(f_i), 0);
      P = BCoff(P, FTYPE_size(f_i));
      break;
    case UNIFY_VOID_4:
      (void)HeapNext(s);
    case UNIFY_VOID_3:
      (void)HeapNext(s);
    case UNIFY_VOID_2:
      (void)HeapNext(s);
    case UNIFY_VOID_1:
      (void)HeapNext(s);
      break;
    default:
      SERIOUS_FAULT("compile_term: internal error");
    }
  }
  return TRUE;
}

/* (used from absmach_def.pl) */

/* ASSERT: X(0) is always a dereferenced list. */
/* Also, returns in the second argument a pointer to a non-null worker
   pointer if the worker has changed, or to null if it has. */

CBOOL__PROTO(compile_term, 
             worker_t **new_worker) {
  tagged_t head, body;
  instance_t *object;

  RefCar(head,X(0));
  RefCdr(body,X(0));

  /*
#if defined(DEBUG)
  display_term(Arg, head, Output_Stream_Ptr, FALSE);
  putchar('\n');
#endif
  */

  *new_worker = NULL;                             /* may be changed after */

  object = compile_term_aux(Arg, head, body, new_worker);
  Arg = *new_worker == NULL ? Arg : *new_worker;

  CBOOL__UnifyCons(PointerToTerm(object),X(1));
  return TRUE;
}

/* Note on memory consumption: compile_term_aux may increase the size of the
   stacks and the size of the X register bank, so the simple approach "see
   how much the total memory has increased" does not work.  Instead, we try
   to find out exactly where we allocate and deallocate memory for program
   storage.  c_term_mark does not allocate program memory (instead, it may
   call choice_overflow, which expands stacks). */

CFUN__PROTO(compile_term_aux, instance_t *,
            tagged_t head, tagged_t body,
            worker_t **new_worker) {
  int truesize;
  //  tagged_t t0, *pt1, *pt2;
  CIAO_REG_1(tagged_t, t0);
  CIAO_REG_2(tagged_t *, pt1);
  CIAO_REG_3(tagged_t *, pt2);
  instance_t *object = NULL;
  
  intmach_t x_variables, hsize, bsize, maxtemps;
  tagged_t *trail_origo;
  bcp_t current_insn /*, *last_insn */ ;

  bsize = FTYPE_size(f_o); /* TODO: for DYNAMIC_NECK_PROCEED? */
  hsize=CONTPAD;
  maxtemps=0;
  trail_origo = Arg->trail_top-DynamicPreserved;

  Tr("c_term_mark1");
  DerefHeapSwitch(head,t0,{goto car_done;});
  Tr("m:o+x");
  bsize += FTYPE_size(f_o)+FTYPE_size(f_x); /* for "Step 1" of c_term (get + arg + ...) */
  c_term_mark(Arg, head, 0, &hsize, &maxtemps, &bsize, &trail_origo);
 car_done:
  Tr("c_term_mark2");
  DerefHeapSwitch(body,t0,{goto cdr_done;});
  Tr("m:o+x");
  bsize += FTYPE_size(f_o)+FTYPE_size(f_x); /* for "Step 1" of c_term (get + arg + ...) */
  c_term_mark(Arg, body, 0, &hsize, &maxtemps, &bsize, &trail_origo);
 cdr_done:

  /* allow for heapmargin_call insn */
  if (hsize>=STATIC_CALLPAD) { /* (was CALLPAD) */
    Tr("m:o(Q)+l+i");
    bsize += (FTYPE_size(f_o)+
              FTYPE_size(f_Q)+
              FTYPE_size(f_l)+
              FTYPE_size(f_i));
  }

  /* tidy out void vars */
  pt1 = pt2 = trail_origo+DynamicPreserved;
  while (pt1 < Arg->trail_top) {
    t0 = *pt1;
    pt1++;
    if (*TagToPointer(t0) & 3) {
      *TagToPointer(t0) -= (char *)(pt1-1)-(char *)pt2, TrailPush(pt2,t0);
    } else {
      *TagToPointer(t0) = t0;
    }
  }
  Arg->trail_top = pt2;
  x_variables = pt2-trail_origo;

                                /* ensure enough X registers */
  if (x_variables+maxtemps > reg_bank_size) {
    int size0 = reg_bank_size;
      
    if (x_variables+maxtemps > (1<<14) - WToX0 - maxtemps) /* TODO: ad-hoc size */
      goto sizebomb;

    reg_bank_size=x_variables+maxtemps;

    *new_worker = /* For local use */
      checkrealloc_FLEXIBLE(worker_t,
                            tagged_t,
                            size0,
                            reg_bank_size,
                            Arg);
#if defined(DEBUG)
    fprintf(stderr, "Reallocing WRB from %p to %p\n", Arg, *new_worker);
#endif
    Arg = *new_worker;
  }

  checkalloc_FLEXIBLE_S(instance_t,
                        objsize,
                        char,
                        bsize,
                        object);
  INC_MEM_PROG(object->objsize);
  current_insn = (bcp_t)object->emulcode;
  object->pending_x2 = NULL;
  object->pending_x5 = NULL;

  if (hsize>=STATIC_CALLPAD) { /* (was CALLPAD) */
    bcp_t P = current_insn;

    Tr("e:o(Q)+l+i");
    ODDOP(HEAPMARGIN_CALL);
    EMIT_l(hsize);
    EMIT_i(DynamicPreserved);
    current_insn = P;
  }

  if (!IsVar(head) &&
      !c_term(Arg,head,0,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;
  if (!IsVar(body) &&
      !c_term(Arg,body,1,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;

  while (TrailGetTop(Arg->trail_top) & QMask) {
    bcp_t P = current_insn;

    if (!TrailYounger(Arg->trail_top,Trail_Start))
      break;
    TrailDec(Arg->trail_top);
    t0 = *(Arg->trail_top); // (Arg->trail_top points to the popped element)
    if (!c_term(Arg,
                Tagp(LST,TaggedToGoal(*TagToPointer(t0))),
                TagToPointer(t0)-trail_origo,
                  reg_bank_size-1,
                x_variables,
                &trail_origo,
                &current_insn))
      goto sizebomb;
    BCOp(P, FTYPE_ctype(f_o), 0) = GET_CONSTRAINT;
  }

  {
    bcp_t P = current_insn;
    Tr("e:o");
    EMIT_o(DYNAMIC_NECK_PROCEED);
    current_insn = P;
  }
  truesize = SIZEOF_FLEXIBLE_STRUCT(instance_t, char, (char *)current_insn - (char *)object->emulcode);
  if (truesize > object->objsize) {
    DEC_MEM_PROG(object->objsize);
    checkdealloc_FLEXIBLE_S(instance_t, objsize, object);
    SERIOUS_FAULT("bug: memory overrun in assert or record");
  }

  if (IsVar(head)) {
    /* findall record---make it fast */
  } else {
    INC_MEM_PROG(truesize - object->objsize);
    //fprintf(stderr, "resize %x-%x\n", object->objsize, truesize);
    object=(instance_t *)checkrealloc((tagged_t *)object, object->objsize, truesize);
    object->objsize = truesize;
  }

  pt2 = Arg->trail_top;
  Arg->trail_top = trail_origo+DynamicPreserved;
  while (TrailYounger(pt2,Arg->trail_top)) {
    PlainUntrail(pt2,t0,{});
  }

  if (TaggedIsSTR(head))  {
    DerefArg(t0,head,1);
    if (TaggedIsSTR(t0)) {
      object->key = TagToHeadfunctor(t0);
    } else if (TaggedIsLST(t0)) {
      object->key = functor_list;
    } else if (!IsVar(t0)) {
      object->key = t0;
    } else {
      object->key = ERRORTAG;
    }
  } else {
    object->key = ERRORTAG;
  }

  Tr("c_term_end");
  return object;

 sizebomb:
  DEC_MEM_PROG(object->objsize);
  checkdealloc_FLEXIBLE_S(instance_t, objsize, object);
  SERIOUS_FAULT("term too large in assert or record");
}


#if defined(OLD_DATABASE)
/* Support for current_key/2: given a '$current instance'/2 instance,
   the key of which is a large number, decode the key. */
tagged_t decode_instance_key(instance_t *inst) {
  bcp_t P = inst->emulcode;
  int xreg = -1;

  for (;;) {
    switch (BcFetchOPCODE()) {
    case HEAPMARGIN_CALLQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case HEAPMARGIN_CALL:
      P = BCoff(P, FTYPE_size(f_l)+FTYPE_size(f_i));
      break;
    case GET_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_CONSTANT:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_t));
      break;
    case GET_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_f));
      break;
    case GET_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_LARGE:
      if (xreg == Xinv(BCOp(P, FTYPE_ctype(f_x), 0)))
        return MakeBlob((tagged_t *)BCoff(P, FTYPE_size(f_x)));
      P = BCoff(P, 
                FTYPE_size(f_x)+
                LargeSize((*(tagged_t *)(BCoff(P, FTYPE_size(f_x))))));
      break;
    case UNIFY_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_CONSTANT:
      P = BCoff(P, FTYPE_size(f_t));
      break;
    case UNIFY_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_f));
      break;
    case UNIFY_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_LARGE:
      P = BCoff(P, LargeSize(*(tagged_t *)P));
      break;
    case UNIFY_X_VARIABLE:
      if (xreg == -1)
        xreg = Xinv(BCOp(P, FTYPE_ctype(f_x), 0));
    case UNIFY_X_VALUE:
    case UNIFY_VOID:
    case GET_LIST:
    case GET_CONSTRAINT:
    case GET_NIL:
      P = BCoff(P, FTYPE_size(f_x));
      break;
    case GET_X_VARIABLE:
    case GET_X_VALUE:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_x));
    default:
      break;
    }
  }
}
#endif

/* --------------------------------------------------------------------------- */

/* Support for if/3 */
CBOOL__PROTO(bu1_if, tagged_t x0)
{
  DEREF(x0,x0);
  *TagToCar(x0) = atom_true;
  return TRUE;
}

CBOOL__PROTO(metachoice)
{
  CBOOL__UnifyCons(ChoiceToTagged(w->choice),X(0));
  return TRUE;
}

CBOOL__PROTO(metacut)
{
  DEREF(X(0),X(0));
  w->choice = ChoiceFromTagged(X(0));
  SetShadowregs(w->choice);
  /*  ConcChptCleanUp(TopConcChpt, w->choice);*/
  PROFILE__HOOK_METACUT;
  return TRUE;

}

/*------------------------------------------------------------*/

static CBOOL__PROTO(cunify_args_aux,
                    int arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2);
static CBOOL__PROTO(cunify_aux, tagged_t x1, tagged_t x2);

/* Unify the argument lists of two compund terms.
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
CBOOL__PROTO(cunify_args, 
             int arity,
             tagged_t *pt1,
             tagged_t *pt2)
{
  tagged_t x1, x2;
  bool_t result =
    (cunify_args_aux(Arg,arity,pt1,pt2,&x1,&x2) && cunify_aux(Arg,x1,x2));
  intmach_t i = w->value_trail;

  if (i<InitialValueTrail) {
    pt2 = (tagged_t *)w->choice;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = (intmach_t)InitialValueTrail;
  }

  return result;
}

static CBOOL__PROTO(cunify_args_aux, 
                    int arity,
                    tagged_t *pt1,
                    tagged_t *pt2,
                    tagged_t *x1,
                    tagged_t *x2)
{
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;
  tagged_t t3;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->choice,2*CHOICEPAD-w->value_trail),w->trail_top))
                                /* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (t1 != t2) {
      DerefHeapSwitch(t1,t3,goto noforward;);
      DerefHeapSwitch(t2,t3,goto noforward;);
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* replace smaller value by larger value,
           using choice stack as value trail */
        tagged_t *b = (tagged_t *)w->choice;
        intmach_t i = w->value_trail;

        if (t1>t2)
          b[--i] = *pt1,
            b[--i] = (tagged_t)pt1,
            *pt1 = t2;
        else
          b[--i] = *pt2,
            b[--i] = (tagged_t)pt2,
            *pt2 = t1;
        w->value_trail = i;
      noforward:
        if (arity>1 && !cunify_aux(Arg,t1,t2))
          return FALSE;
      } else if (t1 != t2)
        return FALSE;
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->choice,CHOICEPAD-w->value_trail),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

/* Unify two terms.
 * x1 - first term
 * x2 - second term
 */

/* NOTE: This is a recursive version of Robinson's 1965 unification
   algorithm without occurs check */

CBOOL__PROTO(cunify, tagged_t x1, tagged_t x2)
{
  bool_t result = cunify_aux(Arg,x1,x2);
  intmach_t i = w->value_trail;

  if (i<InitialValueTrail) {
    tagged_t *pt1, *pt2;

    pt2 = (tagged_t *)w->choice;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = (intmach_t)InitialValueTrail;
  }

  return result;
}

static CBOOL__PROTO(cunify_aux, tagged_t x1, tagged_t x2)
{
  tagged_t u, v, t1;

 in:
  u=x1, v=x2;

  SwitchOnVar(u,t1,
              {goto u_is_hva;},
              {goto u_is_cva;},
              {goto u_is_sva;},
              ;);

                                /* one non variable */
  SwitchOnVar(v,t1,
              { BindHVA(v,u); goto win; },
              { BindCVA(v,u); goto win; },
              { BindSVA(v,u); goto win; },
              ;);

                                /* two non variables */
  if (!(v ^= u))                /* are they equal? */
    goto win;
  else if (v>=QMask)            /* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;                   /* restore v */
      if (cunify_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2))
        goto in;
      else
        goto lose;
    }
  else                          /* structure. */
    {
      v ^= u;                   /* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
        goto lose;
      else if (t1&QMask)        /* large number */
        {
          intmach_t i;
        
          for (i = LargeArity(t1)-1; i>0; i--)
            if (*TaggedToArg(u,i) != *TaggedToArg(v,i)) goto lose;
          goto win;
        }
      if (cunify_args_aux(Arg,Arity(t1),TaggedToArg(u,1),TaggedToArg(v,1),&x1,&x2))
        goto in;
      else
        goto lose;
    }

 u_is_hva:
  SwitchOnVar(v,t1,
              { if (u==v)
                  ;
                else if (YoungerHeapVar(TagpPtr(HVA,v),TagpPtr(HVA,u)))
                  BindHVA(v,u)
                else
                  BindHVA(u,v); },
              { BindHVA(u,v); },
              { BindSVA(v,u); },
              { BindHVA(u,v); });
  goto win;

 u_is_cva:
  SwitchOnVar(v,t1,
              { BindHVA(v,u); },
              { if (u==v)
                  ;
                else if (YoungerHeapVar(TagpPtr(CVA,v),TagpPtr(CVA,u)))
                  { BindCVA(v,u); }
                else
                  { BindCVA(u,v); } },
              { BindSVA(v,u); },
              { BindCVA(u,v); });
  goto win;

 u_is_sva:
  for (; TaggedIsSVA(v); v = t1)
    {
      RefSVA(t1,v);
      if (v == t1)
        {
          if (u==v)
            ;
          else if (YoungerStackVar(TagpPtr(SVA,v),TagpPtr(SVA,u)))
            BindSVA(v,u)
          else
            BindSVA(u,v);
          goto win;
        }
    }
  BindSVA(u,v);

 win:
  return TRUE;

 lose:
  return FALSE;
}

/* --------------------------------------------------------------------------- */

/* TODO: currently unused */

/* Support function for dif/2.
   Fast cases have already been tested in wam().
   X(0) and X(1) are dereferenced.
   w->structure-1  points at an existing goal if non-NULL.
*/
CBOOL__PROTO(prolog_dif, definition_t *address_dif)
{
  choice_t *b;
  tagged_t t0, t1, t2, *pt1, *pt2;
  intmach_t i;
  tagged_t item, other;
                                /* avoid stack variables */
  if (!w->structure)
    {
      if (TaggedIsSVA(t0=X(0)))
        {
          LoadHVA(X(0),w->heap_top);
          BindSVA(t0,X(0));
        }
      if (TaggedIsSVA(t0=X(1)))
        {
          LoadHVA(X(1),w->heap_top);
          BindSVA(t0,X(1));
        }
    }
                                /* establish skeletal choicepoint */
  b = w->choice;
  w->next_alt = address_nd_repeat; /* arity=0 */
  ComputeA(w->local_top,b);
  w->choice = b = ChoiceCharOffset(b,ArityToOffset(0));
  b->next_alt = NULL;
  b->trail_top = w->trail_top;
  SaveGtop(b,w->heap_top);
  NewShadowregs(w->heap_top);
  
  if (cunify(Arg,X(0),X(1))) /* this could use AB, HB, TR, B. */
    item = atom_equal,
    other = Tagp(HVA,w->heap_top);
  else
    item = other = atom_lessthan;
  
  /* quasi failure */
  
  Heap_Warn_Soft = Int_Heap_Warn;
  b = w->choice;
  t2 = (tagged_t)TagToPointer(b->trail_top);
  pt1 = w->trail_top;
  if (TrailYounger(pt1, t2)) {
    do {
      if (IsVar(other)) {
        item = TrailGetTop(pt1);   /* variable */
        other = *TagToPointer(item);
      }
      PlainUntrail(pt1,t0,{});
    } while (TrailYounger(pt1, t2));
    w->trail_top = pt1;
  }
  
  RestoreGtop(b);
  w->choice = b = ChoiceCharOffset(b,-ArityToOffset(0));
  w->next_alt = NULL;
  SetShadowregs(b);

                                /* succeed, fail, or suspend */
  if (item==atom_lessthan)
    return TRUE;
  else if (item==atom_equal)
    return FALSE;
  

                                /* construct goal on the heap */
  pt2 = w->heap_top;
  if (w->structure)
    X(2) = Tagp(STR,w->structure-1);
  else
    {
      X(2) = Tagp(STR,pt2);
      HeapPush(pt2,SetArity(address_dif->printname,2));
      HeapPush(pt2,X(0));
      HeapPush(pt2,X(1));
    }


                                /* constrain pivot variable(s) */
  for (i=0, t1=item; i<2; i++, t1=other)
    {
      if (IsVar(t1))
          {
            if (TaggedIsHVA(t1))
              {
                LoadCVA(t0,pt2);
                if (CondHVA(t1)) {
                    TrailPush(pt1,t1);
                    *TagpPtr(HVA,t1) = t0;
                } else {
                  *TagpPtr(HVA,t1) = t0;
                }
                goto check_trail;
              }
            else if (!CondCVA(t1))
              {
                HeapPush(pt2,*TaggedToGoal(t1));
                HeapPush(pt2,*TaggedToDef(t1));
                *TaggedToGoal(t1) = Tagp(LST,HeapOffset(pt2,-2));
                *TaggedToDef(t1) = Tagp(LST,pt2);
              }
            else
              {
                LoadCVA(t0,pt2);
                HeapPush(pt2,Tagp(LST,TaggedToGoal(t1)));
                HeapPush(pt2,Tagp(LST,HeapOffset(pt2,1)));
                TrailPush(pt1,t1);
                *TagpPtr(CVA,t1) = t0;
              check_trail:
                if (ChoiceYounger(w->choice,TrailOffset(pt1,CHOICEPAD)))
                  w->trail_top = pt1,
                  choice_overflow(Arg,CHOICEPAD),
                  pt1 = w->trail_top;
              }
            HeapPush(pt2,X(2));
            HeapPush(pt2,PointerToTerm(address_dif));
          }
    }
  w->heap_top = pt2;
  w->trail_top = pt1;
  
  return TRUE;
}

