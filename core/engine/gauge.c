/*
 *  gauge.c
 *
 *  Support for profiling predicates.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#if defined(GAUGE)

#include <ciao/eng.h>
#include <ciao/basiccontrol.h>

/* declarations for global functions accessed here */

#include <ciao/gauge.h>

/* local declarations */

#define TRY_NODE_IS_NULL(T) (((T)==NULL) || ((T)==fail_alt))

/* Prolog predicates. */

CBOOL__PROTO(counter_values)
{
  tagged_t *h;
  intmach_t *current_counter, *max_counter, counter;
  int count;
  tagged_t values;

  DEREF(X(0),X(0));
  current_counter = TermToPointer(intmach_t, X(0));

  DEREF(X(1),X(1));
  count = TaggedToIntmach(X(1));
  if (count > 255) {
    USAGE_FAULT("$clause_counters/2: Too many counters per clause");
  }

  max_counter = current_counter + count;
  
  h = w->heap_top;
  values = Tagp(STR,h);
  HeapPush(h,SetArity(atom_counter,count));
  
  while (current_counter < max_counter) {
    counter = *current_counter++;
    if (!IsInSmiValRange(counter)) {
      USAGE_FAULT("$clause_counters/2: Counter value exceeds smallint");
    } else {
      HeapPush(h,MakeSmall(counter));
    }
  }
  w->heap_top = h;
  
  CBOOL__LASTUNIFY(values,X(2));
}

CBOOL__PROTO(reset_counters)
{
  intmach_t *current_counter, *max_counter;

  DEREF(X(0),X(0));
  current_counter = TermToPointer(intmach_t, X(0));
  DEREF(X(1),X(1));
  max_counter = current_counter + TaggedToIntmach(X(1));

  while (current_counter < max_counter)
    *current_counter++ = 0;

  return TRUE;
}

CBOOL__PROTO(emulated_clause_counters)
{
  definition_t *d;
  emul_info_t *cl;
  int i, count;
  
  DEREF(X(0),X(0));
  d = parse_definition(X(0));
  if (!d)
    return FALSE;
  DEREF(X(1),X(1));
  for (i=GetSmall(X(1)), cl=d->code.incoreinfo->clauses.ptr; i>1; --i)
    cl = cl->next.ptr;
  CBOOL__UnifyCons(PointerToTerm(cl->counters),X(2));
  count = NumberOfCounters(cl);
  CBOOL__LASTUNIFY(IntvalToTagged(count),X(3));
}

#else

int gauge__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif /* GAUGE */
