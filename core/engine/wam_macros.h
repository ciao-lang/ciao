/*
 *  wam_macros.h
 *
 *  Emulator macros.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_WAM_MACROS_H
#define _CIAO_WAM_MACROS_H

#include <ciao/wamsupport.h>
#include <ciao/initial.h>

/* ------------------------------------------------------------------------- */

#if defined(DEBUG_NODE)
# define AssignNodeFunctor(F, Y) \
  {F = Y;}
#else
# define AssignNodeFunctor(F, Y)
#endif

#if defined(DEBUG)
   /* stop_on_pred_calls = trace_calls | profile */
# if defined(PROFILE)
#  define PredTrace(X,Y) \
{ \
  AssignNodeFunctor(w->node->functor, Y); \
  if (stop_on_pred_calls) \
  { \
    if (trace_calls) wr_functor(X,Y); \
    else { \
      PROFILE__HOOK_CALL(w,Y); \
    } \
  } \
}

# else /* DEBUG & !PROFILE */
#  define PredTrace(X,Y) \
{ \
  AssignNodeFunctor(w->node->functor, Y); \
  if (trace_calls) \
    wr_call(Arg,X,Y); \
}
# endif
#else
# if defined(DEBUG_NODE)
#  define PredTrace(X,Y) \
  AssignNodeFunctor(w->node->functor, Y)
# else
#  define PredTrace(X,Y)
# endif
#endif

#if defined(DEBUG)
#define TRACE_CHPT_CLEANUP(TopCChpt, TopNode) \
 if(debug_concchoicepoints) \
    fprintf(stderr, "cut: removing chains (%x to %x)\n", \
                    (int)TopCChpt, (int)TopNode);

#define TRACE_CHPT_CUT(NewNode) \
 if (debug_choicepoints)  \
   fprintf(stderr, "Cutting: new chpt = %x\n", (int)NewNode);
#else
#define TRACE_CHPT_CUT(NewNode)
#define TRACE_CHPT_CLEANUP(TopCChpt, TopNode)
#endif

/* TODO: can we reuse this to implement actions on cuts? (JFMC) */
#if defined(THREADS)
#define ConcChptCleanUp(TopCChpt, TopNode) \
    if (ChoiceYounger(TopCChpt, TopNode)) { \
       TRACE_CHPT_CLEANUP(TopCChpt, TopNode); \
       remove_link_chains(&TopCChpt, TopNode); \
    } 
#else
#define ConcChptCleanUp(TopCChpt, TopNode)
#endif

/* ------------------------------------------------------------------------- */

#define P		p
#define B		((node_t *)pt1)
#define E		((frame_t *)pt1)
#define SetE(X)         (pt1 = (tagged_t *)(X))
#define H		pt2
#define S		pt2
#define Func		((definition_t *)P)

/* ------------------------------------------------------------------------- */

#define Setfunc(X)	{ P = (bcp_t)(X); }
#define SetA(Frame,Ptr) { \
  w->local_top = (frame_t *)(Ptr); \
}

#define SETUP_PENDING_CALL(ADDR) { \
  ComputeE;			      \
  Y(0) = PointerToTerm(Func);			      \
  for(i=0; i<Func->arity; i++) Y(i+1) = X(i);	      \
  E->next_insn = w->next_insn;			      \
  E->frame = w->frame;				      \
  w->frame = E;					       \
  w->next_insn = CONTCODE(i+1);			       \
  SetA(E,Offset(E,EToY0+i+1));			       \
  Setfunc(ADDR);				       \
}

/* Do not edit this defn - it's a special case of ComputeA. */
#define ComputeE { \
  if (w->local_top) {                                        \
    SetE(w->local_top);                                     \
  } else {                                                   \
    SetE(NodeLocalTop(w->node));			      \
    if (!StackYounger(E,w->frame))				\
      SetE(StackCharOffset(w->frame,FrameSize(w->next_insn))); \
  } 							\
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

#if defined(DEBUG)
#define TRACE_INSTR 1
#endif

#if defined(TRACE_INSTR)
CVOID__PROTO(dump_instr, bcp_t p);
#define BcFetchOPCODE() (({ if (trace_instr) { dump_instr(Arg, P); } }), BcFetchOPCODE_())
#else
#define BcFetchOPCODE() BcFetchOPCODE_()
#endif

#endif /* _CIAO_WAM_MACROS_H */
