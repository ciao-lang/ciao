/*
 *  gauge.c
 *
 *  Support for profiling predicates.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#if defined(GAUGE)

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/wamsupport.h>

/* declarations for global functions accessed here */

#include <ciao/gauge.h>

/* local declarations */

#define ISNOTRY(T)		(((T)==NULL) || ((T)==fail_alt))

/* Prolog predicates. */

CBOOL__PROTO(counter_values)
{
  tagged_t *h;
  intmach_t *current_counter, *max_counter, counter;
  int count;
  tagged_t values;

  DEREF(X(0),X(0));
  current_counter = (intmach_t *)TermToPointer(X(0));

  DEREF(X(1),X(1));
  count = GetInteger(X(1));
  if (count > 255)
    {
      USAGE_FAULT("$clause_counters/2: Too many counters per clause");
    }

  max_counter = current_counter + count;
  
  h = w->global_top;
  values = Tag(STR,h);
  HeapPush(h,SetArity(atom_counter,count));
  
  while (current_counter < max_counter)
    {
      counter = *current_counter++;
      if (!IntIsSmall(counter))
	{
	  USAGE_FAULT("$clause_counters/2: Counter value exceeds smallint");
	}
      else
	HeapPush(h,MakeSmall(counter));
    }
  w->global_top = h;
  
  return cunify(Arg,values,X(2));
}

CBOOL__PROTO(reset_counters)
{
  intmach_t *current_counter, *max_counter;

  DEREF(X(0),X(0));
  current_counter = (intmach_t *)TermToPointer(X(0));
  DEREF(X(1),X(1));
  max_counter = current_counter + GetInteger(X(1));

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
  Unify_constant(PointerToTerm(cl->counters),X(2));
  count = NumberOfCounters(cl);
  return cunify(Arg,MakeInteger(count),X(3));
}

#else

int gauge__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif /* GAUGE */
