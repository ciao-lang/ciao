/*
 *  heapgc.h
 *
 *  Term heap garbage collector.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_HEAPGC_H
#define _CIAO_HEAPGC_H

/*
  static CVOID__PROTO(shuntVariables);
  static CVOID__PROTO(markTrail);
  static void markFrames(frame_t *frame, bcp_t l);
  static CVOID__PROTO(markChoicepoints);
  static void markVariable(tagged_t *start);
  static void updateRelocationChain(tagged_t *curr, tagged_t *dest);
  static CVOID__PROTO(sweepTrail);
  static void sweepFrames(frame_t *frame, bcp_t l);
  static CVOID__PROTO(sweepChoicepoints);
  static CVOID__PROTO(compressHeap);
 */

CBOOL__PROTO(gc_usage);
CBOOL__PROTO(gc_mode);
CBOOL__PROTO(gc_trace);
CBOOL__PROTO(gc_margin);
CVOID__PROTO(compressTrail, bool_t from_gc);
CVOID__PROTO(GarbageCollect);

#endif /* _CIAO_HEAPGC_H */
