/*
 *  wamsupport.c
 *
 *  Basic emulator support.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/initial.h>
#include <ciao/instrdefs.h>
#include <ciao/wamsupport.h>
#include <ciao/wam.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/indexing.h>
#include <ciao/io_basic.h>

static try_node_t *get_null_alt(int arity);

bcp_t startgoalcode;		       /* WAM code to start a goal -- Shared */
bcp_t bootcode;		       /* WAM bootstrap to run bootgoal -- Shared */
// #if defined(INTERNAL_CALLING)
// bcp_t internal_calling;		       /* WAM bootstrap to run bootgoal -- Shared */
// #endif
bcp_t startgoalcode_cont;    /* Specify cont. on success and failure -- Shared */
bcp_t contcode;/* continuations of FrameSize N after exceptions -- Shared */
bcp_t failcode;	      /* continuation of FrameSize 0 that fails -- Shared */
bcp_t exitcode;	  /* continuation of FrameSize 0 that exits wam -- Shared */

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
    if (a->node_offset == ArityToOffset(arity)) return a;

  a = (try_node_t *)checkalloc(sizeof(try_node_t)
			       + sizeof(try_node_t *)
#if defined(GAUGE)
			       + 2*sizeof(intmach_t)
#endif
			       );

  INC_MEM_PROG(total_mem_count - current_mem);

  a->node_offset = ArityToOffset(arity);
  a->number = 0;
  a->emul_p = insnfail;
  a->emul_p2 = insnfail;
  (a+1)->next = null_alt;	/* tail of list */
#if defined(GAUGE)
  a->entry_counter = (intmach_t *)(a+1)+1;
  a->entry_counter[0] = 0;
  a->entry_counter[1] = 0;
#endif
  a->next = NULL;		/* no more alternatives */
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

try_node_t *def_retry_c(cbool_pred_t proc, int arity)
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
  item->node_offset = ArityToOffset(arity);
  item->number = 0;
  P = item->emul_p = (bcp_t )(((char *)item)+sizeof(try_node_t));
  item->emul_p2 = P;
  item->next = item;
  EMIT_o(RETRY_CQ);
  EMIT_Q(0);
  EMIT_C((char *)proc);
#if defined(GAUGE)
  item->entry_counter = (intmach_t *)P;
  item->entry_counter[0] = 0;
  item->entry_counter[1] = 0;
#endif
  return item;
}

/* --------------------------------------------------------------------------- */

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

  /*
    |CALLQ|0|address_call_with_cont/2|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
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
    startgoalcode_cont = P;
    EMIT_o(CALLQ);
    EMIT_Q(0);
    EMIT_E(address_call_with_cont);
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
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(set_trace_calls)
{
  tagged_t x;
  DEREF(x,X(0));
  if (!TagIsSmall(x))
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
extern bcp_t startgoalcode_cont;                                     /* Shared */

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
#if 0                                                        /* was GAUGE */
    return (*func->code.cinfo->procedure)(Arg);
#else
    return (*func->code.cinfo)(Arg);
#endif
  } else {
    if (func->enter_instr == BUILTIN_CALL) {
      w->next_insn = bootcode;          /* Should have been initialized */
      wam(w, NULL);
      return TRUE;
    }
  }
  return FALSE;
}

