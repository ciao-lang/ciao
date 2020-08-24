/*
 *  eng_gc.h
 *
 *  Garbage collector and code for growing areas when full.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_ENG_GC_H
#define _CIAO_ENG_GC_H

CBOOL__PROTO(gc_usage);
CBOOL__PROTO(gc_mode);
CBOOL__PROTO(gc_trace);
CBOOL__PROTO(gc_margin);
CVOID__PROTO(compressTrail, bool_t from_gc);
CVOID__PROTO(GarbageCollect);

CVOID__PROTO(choice_overflow, intmach_t pad);
CVOID__PROTO(stack_overflow);
CBOOL__PROTO(gc_start);
CVOID__PROTO(heap_overflow, intmach_t pad);
CVOID__PROTO(collect_goals_from_trail, intmach_t wake_count);
CVOID__PROTO(trail_gc);
CBOOL__PROTO(stack_shift_usage);
CBOOL__PROTO(termheap_usage);
CBOOL__PROTO(envstack_usage);
CBOOL__PROTO(choice_usage);
CBOOL__PROTO(trail_usage);
CVOID__PROTO(explicit_heap_overflow, intmach_t pad, intmach_t arity);
CBOOL__PROTO(heap_limit);

#if defined(ANDPARALLEL)
//bool_t is_rem_Hterm(tagged_t term, worker_t *w, worker_t *remote_w);
CVOID__PROTO(heap_overflow_adjust_wam,
             intmach_t reloc_factor, tagged_t *newh, bool_t remote_reloc, worker_t *remote_worker);
#else
CVOID__PROTO(heap_overflow_adjust_wam, intmach_t reloc_factor, tagged_t *newh);
#endif
CVOID__PROTO(stack_overflow_adjust_wam, intmach_t reloc_factor);

#if defined(USE_OVERFLOW_EXCEPTION)
CBOOL__PROTO(undo_heap_overflow_excep);
#endif

/* Make sure that there is enough heap to allocate N cells (plus CONTPAD)
 * N - number of cells
 * Arity - number of live X regs
 */
#define ENSURE_HEAP(N, Arity) { \
  if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(N)) { \
    CVOID__CALL(explicit_heap_overflow,CONTPAD+(N),(Arity)); \
  } \
} 

/* Make sure that there is enough heap to allocate N bytes */
#define ENSURE_HEAP_BYTES(N, Arity) { \
  if (HeapCharDifference(w->global_top,Heap_End)<(N)) { \
    CVOID__CALL(explicit_heap_overflow,((N)+sizeof(tagged_t)-1)/sizeof(tagged_t),(Arity)); \
  } \
}

/* Make sure that there is enough heap to construct a list spine.
 * N - length of the list
 * Arity - number of live X regs
 */
#define ENSURE_HEAP_LST(N, Arity) ENSURE_HEAP((N)*LSTCELLS, (Arity))

#endif /* _CIAO_ENG_GC_H */
