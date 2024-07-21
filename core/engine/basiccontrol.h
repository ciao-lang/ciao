/*
 *  basiccontrol.h
 *
 *  Basic definitions for the bytecode emulator.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_BASICCONTROL_H
#define _CIAO_BASICCONTROL_H

#include <ciao/eng_registry.h>

CVOID__PROTO(wam, goal_descriptor_t *);

/* ------------------------------------------------------------------------- */
/* Support bytecode */

extern bcp_t bootcode;
// #if defined(INTERNAL_CALLING)
// extern bcp_t internal_calling;
// #endif
extern bcp_t startgoalcode;

#define CONTCODE(Arity) BCoff(contcode, (Arity) * (FTYPE_size(f_e) + FTYPE_size(f_o)))
extern bcp_t contcode;

extern bcp_t failcode;
extern bcp_t exitcode;
extern try_node_t *termcode;
extern try_node_t *fail_alt;

void init_some_bytecode(void);

int p2_offset(uintmach_t insn);
try_node_t *def_retry_c(cbool0_t proc, int arity);

CBOOL__PROTO(run_determ_c, tagged_t goal);

/* ------------------------------------------------------------------------- */

#if defined(DEBUG_NODE)
#define AssignNodeFunctor(Y) { w->choice->functor = Y; }
#else
#define AssignNodeFunctor(Y)
#endif

#if defined(DEBUG_TRACE)
void wr_functor(char *s, definition_t *func);
CVOID__PROTO(wr_call, char *s, definition_t *func);
#endif

/* Predicate tracer */
#if defined(DEBUG_TRACE)
#define PRED_TRACE(X,Y) if (debug_predtrace) { wr_functor(X,Y); }
#else
#define PRED_TRACE(X,Y)
#endif

/* Predicate profiler */
#if defined(ABSMACH_OPT__profile_calls)
#define PRED_PROFILE(X,Y) if (profile_flags != 0) add_to_profiling(Y);
#elif defined(ABSMACH_OPT__profilecc)
#define PRED_PROFILE(X,Y) if (profile_flags != 0) { PROFILE__HOOK_CALL(Y); }
#else
#define PRED_PROFILE(X,Y)
#endif

#define PRED_HOOK(X,Y) { \
  AssignNodeFunctor(Y); \
  PRED_TRACE(X,Y); \
  PRED_PROFILE(X,Y); \
}

#if defined(DEBUG_TRACE)
#define TRACE_CHPT_CLEANUP(TopCChpt, TopNode) \
 if(debug_concchoicepoints) \
    fprintf(stderr, "cut: removing chains (%p to %p)\n", \
                    TopCChpt, TopNode);

#define TRACE_CHPT_CUT(NewNode) \
 if (debug_choicepoints)  \
   fprintf(stderr, "Cutting: new chpt = %p\n", NewNode);
#else
#define TRACE_CHPT_CUT(NewNode)
#define TRACE_CHPT_CLEANUP(TopCChpt, TopNode)
#endif

/* TODO: can we reuse this to implement actions on cuts? (JFMC) */
#if defined(USE_THREADS)
#define ConcChptCleanUp(TopCChpt, TopNode) \
    if (ChoiceYounger(TopCChpt, TopNode)) { \
       TRACE_CHPT_CLEANUP(TopCChpt, TopNode); \
       remove_link_chains(&TopCChpt, TopNode); \
    } 
#else
#define ConcChptCleanUp(TopCChpt, TopNode)
#endif

/* ------------------------------------------------------------------------- */
/* ConcChpt relocation (support for GC) */

/* Relocate the concurrent topmost choicepoint */
/* The chain of concurrent dynamic choicepoints has to be
   relocated as well.  The initial TopConcChpt was set to be
   the initial choice node.  MCL. */
#define RelocateConcChptChain(choice_reloc_factor) do { \
  choice_t *concchpt = TopConcChpt = RelocPtr(TopConcChpt, choice_reloc_factor); \
  while(concchpt != InitialChoice) { \
    DEBUG__TRACE(debug_concchoicepoints || debug_gc, \
                 "*** %" PRIdm "(%" PRIdm ") Changing dynamic chpt@%p\n", \
                 (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, \
                 concchpt); \
    /* TODO: wrong if it is Zero (null)? (JFMC) */ \
    choice_t *prev = TermToPointerOrNull(choice_t, concchpt->x[PrevDynChpt]); \
    AssignRelocPtr(prev, choice_reloc_factor); \
    concchpt->x[PrevDynChpt] = PointerToTermOrZero(prev); \
    concchpt = prev; \
  } \
} while(0);

/* ------------------------------------------------------------------------- */

#define P               p
#define B               b
#define E               e
#define SetE(X)         (e = (X))
#define H               cached_r_h
#define S               r_s
#define Func            ((definition_t *)P)

/* ------------------------------------------------------------------------- */

#define Setfunc(X)      { P = (bcp_t)(X); }

#define SETUP_PENDING_CALL(E, ADDR) { \
  intmach_t i; \
  CODE_ALLOC(E);                                      \
  Y(0) = PointerToTerm(Func);                         \
  for (i=0; i<Func->arity; i++) Y(i+1) = X(i);        \
  E->next_insn = w->next_insn;                        \
  E->frame = w->frame;                                \
  w->frame = E;                                       \
  w->next_insn = CONTCODE(i+1);                       \
  w->local_top = (frame_t *)Offset(E,EToY0+i+1);      \
  Setfunc(ADDR);                                      \
}

/* ------------------------------------------------------------------------- */

#if defined(__clang__)
/* NOTE: this seems to generate better code than ({ ... }) macros, at
   least for clang */
static C_AINLINE FTYPE_ctype(f_o) fetch_opcode(bcp_t *p) {
  FTYPE_ctype(f_o) op = BCOp(*p, FTYPE_ctype(f_o), 0);
  *p = BCoff(*p, FTYPE_size(f_o));
  return op;
}
#define BcFetchOPCODE_() fetch_opcode(&P)
#else
#define BcFetchOPCODE_() ({ \
  FTYPE_ctype(f_o) op = BCOp(P, FTYPE_ctype(f_o), 0); \
  P = BCoff(P, FTYPE_size(f_o)); \
  op; \
})
#endif

#if defined(DEBUG_TRACE)
#define TRACE_INSTR 1
#endif

#if defined(TRACE_INSTR)
CVOID__PROTO(dump_instr, bcp_t p);
#define BcFetchOPCODE() (({ if (debug_instrace) { dump_instr(Arg, P); } }), BcFetchOPCODE_())
#else
#define BcFetchOPCODE() BcFetchOPCODE_()
#endif

#endif /* _CIAO_BASICCONTROL_H */
