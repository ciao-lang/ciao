/*
 *  stacks.h
 *
 *  Code for growing areas when full.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_STACKS_H
#define _CIAO_STACKS_H

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
#define ENSURE_HEAP(N, Arity) {					\
    if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(N)) {		\
      explicit_heap_overflow(Arg,CONTPAD+(N),(Arity));			\
    }									\
  }

/* Make sure that there is enough heap to allocate N bytes */
#define ENSURE_HEAP_BYTES(N, Arity) {					\
    if (HeapCharDifference(w->global_top,Heap_End)<(N)) {		\
      explicit_heap_overflow(Arg,((N)+sizeof(tagged_t)-1)/sizeof(tagged_t),(Arity)); \
    }									\
  }

/* Make sure that there is enough heap to construct a list spine.
 * N - length of the list
 * Arity - number of live X regs
 */
#define ENSURE_HEAP_LST(N, Arity) ENSURE_HEAP((N)*LSTCELLS, (Arity))

#endif /* _CIAO_STACKS_H */
