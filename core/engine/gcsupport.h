/*
 *  gcsupport.h
 *
 *  Utility macros for heap GC.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_GCSUPPORT_H
#define _CIAO_GCSUPPORT_H

#define gc_Reverse(curr,next) \
{  tagged_t *temp; \
   temp= TagToPointer(*next); \
   *next= gc_PutValue((tagged_t)curr,*next); \
   curr= next; \
   next= temp; }

#define gc_Undo(curr,next)  gc_Reverse(next,curr)

#define gc_Advance(curr,next) \
{  tagged_t *temp; \
   temp= TagToPointer(*curr); \
   *curr= gc_PutValue((tagged_t)next,*curr); \
   HeapDecr(curr); \
   next= TagToPointer(*curr); \
   *curr= gc_PutValue((tagged_t)temp,*curr); }

#define gc_TrailStart		TagToPointer(w->segment_node->trail_top)
#define gc_HeapStart		(NodeGlobalTop(w->segment_node))
#define gc_StackStart		(NodeLocalTop(w->segment_node))
#define gc_ChoiceStart		(w->segment_node)

#define gc_ReverseChoice(cp,prevcp,alt) \
{ \
  try_node_t *m_alt = alt; \
  cp = prevcp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  prevcp = ChoiceCharOffset(cp,-alt->node_offset); \
}

#define gc_UndoChoice(cp,prevcp,alt) \
{ \
  try_node_t *m_alt = alt; \
  prevcp = cp; \
  alt = cp->next_alt; \
  cp->next_alt = m_alt; \
  cp = ChoiceCharOffset(cp,alt->node_offset); \
}

#endif /* _CIAO_GCSUPPORT_H */
