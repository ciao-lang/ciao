/*
 *  rt_exp.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <string.h>

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/internals.h>
#include <ciao/dynamic_rt.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_profile.h>
#include <ciao/basiccontrol.h>

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_PREDICATE/2
   -----------------------------------------------------------------------*/

CBOOL__PROTO(current_predicate)
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (!IsVar(X(1))) {
    tagged_t *junk;
    definition_t *d =
      find_definition(predicates_location,X(1),&junk,FALSE);

    if (d==NULL || d->predtyp==ENTER_UNDEFINED /*  || d->properties.public */)
      return FALSE;
    CBOOL__UnifyCons(d->printname,X(0));
    return TRUE;
  }

  X(2) = MakeSmall(SwitchSize(*predicates_location));
  X(3) = PointerToTerm(*predicates_location);
  push_choicept(Arg,address_nd_current_predicate);
  return nd_current_predicate(Arg);
}

CBOOL__PROTO(nd_current_predicate)
{
  sw_on_key_t *table = TermToPointer(sw_on_key_t, X(3));
  sw_on_key_node_t *keyval;
  intmach_t j = GetSmall(X(2));
  definition_t *d;
  tagged_t mask = (IsVar(X(0)) ? 0 : INDEXMASK);

  for (--j; j>=0; --j) {
    keyval = &table->node[j];
    if ((keyval->key & mask) == (X(0) & mask) &&
        (d = keyval->value.def) &&
        d->predtyp != ENTER_UNDEFINED /*  && !d->properties.public */ ){
      if (j==0)
        pop_choicept(Arg);
      else
        w->choice->x[2] = MakeSmall(j);
      CBOOL__UnifyCons(d->printname,X(0));
      CBOOL__LASTUNIFY(make_structure(Arg,SetArity(d->printname,d->arity)), X(1));
    }
  }
  pop_choicept(Arg);
  return FALSE;
}



/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       PREDICATE_PROPERTY/2
   -----------------------------------------------------------------------*/

 /* See dummy.pl (MCL) */

static unsigned int predicate_property_bits(definition_t *d) {
  return
    /* (d->properties.public     ?  0x1 : 0) | */
      (d->properties.concurrent ?  0x1 : 0)
    | (d->properties.dynamic    ?  0x2 : 0)
    | (d->properties.wait       ?  0x4 : 0)
    | (d->properties.multifile  ?  0x8 : 0)
    ;
}

CBOOL__PROTO(predicate_property)
{
  DEREF(X(0),X(0));
  if (! IsVar(X(0))) {
    tagged_t *junk;
    definition_t *d =
      find_definition(predicates_location,X(0),&junk,FALSE);

    if (d==NULL || d->predtyp==ENTER_UNDEFINED)
      return FALSE;
    CBOOL__UnifyCons(MakeSmall(d->predtyp),X(1));
    CBOOL__UnifyCons(MakeSmall(predicate_property_bits(d)),X(2));
    return TRUE;
  }

  X(3) = MakeSmall(SwitchSize(*predicates_location));
  X(4) = PointerToTerm(*predicates_location);
  push_choicept(Arg,address_nd_predicate_property);
  return nd_predicate_property(Arg);
}

CBOOL__PROTO(nd_predicate_property)
{
  sw_on_key_t *table = TermToPointer(sw_on_key_t, X(4));
  intmach_t j = GetSmall(X(3));
  sw_on_key_node_t *keyval;
  definition_t *d;

  for (--j; j>=0; --j)
    {
      keyval = &table->node[j];
      if ((d = keyval->value.def) &&
          d->predtyp != ENTER_UNDEFINED)
        {
          if (j==0)
            pop_choicept(Arg);
          else
            w->choice->x[3] = MakeSmall(j);
          CBOOL__UnifyCons(MakeSmall(d->predtyp),X(1));
          CBOOL__UnifyCons(MakeSmall(predicate_property_bits(d)),X(2));
          CBOOL__LASTUNIFY(make_structure(Arg,SetArity(d->printname,d->arity)), X(0));
        }
    }
  pop_choicept(Arg);
  return FALSE;
}
