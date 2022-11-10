/*
 *  eng_gc.h
 *
 *  Garbage collector and code for growing areas when full.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_GC_H
#define _CIAO_ENG_GC_H

#include <ciao/eng.h>

extern bool_t current_gcmode;
#define GCTRACE__OFF 0
#define GCTRACE__TERSE 1
#define GCTRACE__VERBOSE 2
extern intmach_t current_gctrace;
extern intmach_t current_gcmargin;
#if defined(OPTIM_COMP)
#define GCMARGIN_CHARS ((intmach_t)(current_gcmargin*1024*(sizeof(tagged_t)/4)))
#else
#define GCMARGIN_CHARS ((intmach_t)(current_gcmargin*1024))
#endif

void init_gc(void);

CVOID__PROTO(trail__compress, bool_t from_gc);

CVOID__PROTO(choice_overflow, intmach_t pad, bool_t remove_trail_uncond);
CVOID__PROTO(stack_overflow);
CBOOL__PROTO(gc_start);
CVOID__PROTO(heap_overflow, intmach_t pad);
CVOID__PROTO(collect_goals_from_trail, intmach_t wake_count);
CVOID__PROTO(explicit_heap_overflow, intmach_t pad, intmach_t arity);

CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor);

/* --------------------------------------------------------------------------- */

/* Make sure that there is enough heap to allocate AMOUNT bytes
 * Arity - number of live X regs
 */
#define TEST_HEAP_OVERFLOW(H, AMOUNT, ARITY) ({ \
  if (HeapCharAvailable((H)) < (AMOUNT)) { \
    G->heap_top = (H); \
    CVOID__CALL(explicit_heap_overflow, (AMOUNT), (ARITY)); \
    (H) = G->heap_top; \
  } \
})

/* Make sure that there is enough heap to construct a list spine.
 * N - length of the list
 * Arity - number of live X regs
 */
#define ENSURE_HEAP_LST(N, Arity) TEST_HEAP_OVERFLOW(G->heap_top, (N)*LSTCELLS*sizeof(tagged_t)+CONTPAD, (Arity))

/* --------------------------------------------------------------------------- */
/* Some handy C preprocessor macros */

/* ensure that 'b' is evaluated in ID concatenation */
#define CONCAT(a, b) CONCAT_(a, b)
#define CONCAT_(a, b) a ## b

/* Count number of arguments (max 10 args) */
#define VA_NARGS(...) VA_NARGS_(_, ## __VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define VA_NARGS_(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...) N

/* --------------------------------------------------------------------------- */

/* Execute Code, where additional arguments are GC roots (if a heap
   overflow or GC is needed) */
#define PROT_GC(Code, ...) PROT_GC_(Code, VA_NARGS(__VA_ARGS__), (__VA_ARGS__)) 
#define PROT_GC_(Code, GCLen, GCRegs) ({ \
  CONCAT(PUSH_GC,GCLen) GCRegs; \
  Code; \
  CONCAT(POP_GC,GCLen) GCRegs; \
})

/* Call explicit_heap_overflow, where additional arguments are GC roots. */
#define HeapOverflow_GC(REQ, ...) HeapOverflow_GC_(REQ, VA_NARGS(__VA_ARGS__), (__VA_ARGS__)) 
#define HeapOverflow_GC_(REQ, GCLen, GCRegs) ({ \
  intmach_t idx_ MAYBE_UNUSED = LIVEINFO__ARITY(w->liveinfo); \
  CONCAT(SAVE_XS,GCLen) GCRegs; \
  CVOID__CALL(explicit_heap_overflow, (REQ)+LIVEINFO__HEAP(w->liveinfo), LIVEINFO__ARITY(w->liveinfo) + GCLen); \
  CONCAT(RESTORE_XS,GCLen) GCRegs; \
})

/* TODO: improve LIVEINFO support? */
/* TODO: document why a->x[0] is reserved for TaggedZero (see eng_gc.c) */
/* TODO: use a frame instead of temporary X beyond liveinfo arity? */

#define SAVE_XS0() {}
#define SAVE_XS1(V0) { X(idx_)=V0; }
#define SAVE_XS2(V0,V1) { X(idx_)=V0; X(idx_+1)=V1; }
#define RESTORE_XS0() {}
#define RESTORE_XS1(V0) { V0=X(idx_); }
#define RESTORE_XS2(V0,V1) { V0=X(idx_); V1=X(idx_+1); }

#define SAVE_YS1(V0) { frame_t *a=G->frame; a->x[0]=TaggedZero; a->x[1]=V0; }
#define SAVE_YS2(V0,V1) { frame_t *a=G->frame; a->x[0]=TaggedZero; a->x[1]=V0; a->x[2]=V1; }
#define RESTORE_YS1(V0) { frame_t *a=G->frame; V0=a->x[1]; }
#define RESTORE_YS2(V0,V1) { frame_t *a=G->frame; V0=a->x[1]; V1=a->x[2]; }

#if defined(CONTCODE) /* TODO: always? */
static inline CVOID__PROTO(push_gc_frame, intmach_t i) {
  choice_t *b = w->choice;
  frame_t *a;
  GetFrameTop(a,b,G->frame);
  a->frame = w->frame;
  a->next_insn = w->next_insn;
  w->frame = a;
  w->next_insn = CONTCODE(i+1);
  w->local_top = (frame_t *)Offset(a,EToY0+i+1);
}
static inline CVOID__PROTO(pop_gc_frame) {
  frame_t *a = w->frame;
  w->local_top = a;
  w->frame = a->frame;
  w->next_insn = a->next_insn;
}
#endif

#define PUSH_GC0() {}
#define PUSH_GC1(V0) ({ CVOID__CALL(push_gc_frame,1); SAVE_YS1(V0); })
#define PUSH_GC2(V0,V1) ({ CVOID__CALL(push_gc_frame,2); SAVE_YS2(V0,V1); })
#define POP_GC0() {}
#define POP_GC1(V0) ({ RESTORE_YS1(V0); CVOID__CALL(pop_gc_frame); })
#define POP_GC2(V0,V1) ({ RESTORE_YS2(V0,V1); CVOID__CALL(pop_gc_frame); })

/* --------------------------------------------------------------------------- */

#endif /* _CIAO_ENG_GC_H */
