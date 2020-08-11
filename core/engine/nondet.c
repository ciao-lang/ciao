/*
 *  nondet.c
 *
 *  Non-deterministic predicates.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 */

#if defined(DEBUG)
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>                                    /* For getpid() */
#endif

#include <string.h>

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/internals.h>
#include <ciao/dynamic_rt.h>
#include <ciao/nondet.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_profile.h>
#include <ciao/basiccontrol.h>

/* -------------------------------------------------------------------
   CHOICEPT MANIPULATIONS
   ----------------------------------------------------------------------*/

CVOID__PROTO(pop_choicept)
{
  node_t *b = w->node;

  w->node = b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  SetShadowregs(b);
}

CVOID__PROTO(push_choicept, try_node_t *alt)
{
  intmach_t n = alt->node_offset;
  tagged_t *b0 = (tagged_t *)w->node;
  node_t *b = ChoiceCharOffset(b0,n);

  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  n = OffsetToArity(n);
  while (n>0)
    ChoicePush(b0,X(--n));
  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
}

#if defined(PARBACK)
CBOOL__PROTO(nd_suspension_point)
{
  //Reinstall next_insn and Frame (Done by fail).
  //Reinstal argument register
  tagged_t t = w->node->term[0];
  definition_t * func = (definition_t *) *TagToPointer(t);
  int i;
  for (i = 0; i < func->arity; i++) DEREF(X(i),*TagToPointer(TagToPointer(t)+i+1));
  //Take choice point out
  Arg->next_insn = restart_point_insn;

  return TRUE;
}
#endif

/*
   Support for the builtin C-predicate atom_concat/3 (see term_support.c)
*/

CBOOL__PROTO(nd_atom_concat)
{
  intmach_t i = GetSmall(X(3));
  char *s, *s1, *s2;

  w->node->term[3] += MakeSmallDiff(1);

  s2 = GetString(X(2));

  s = Atom_Buffer;

  s1 = s2 + i;
  strcpy(s, s1);
  CBOOL__UnifyCons(init_atom_check(Atom_Buffer),X(1));

  strcpy(s, s2);
  *(s+i) = '\0';
  CBOOL__UnifyCons(init_atom_check(Atom_Buffer),X(0));

  if (i == strlen(s2))
    pop_choicept(Arg);
  
  return TRUE;
}

#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept)
{
  pop_choicept(Arg);
  return FALSE;
}
#endif


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_ATOM/1
   -----------------------------------------------------------------------*/

CBOOL__PROTO(current_atom)
{
  DEREF(X(0),X(0));
  if (TaggedIsATM(X(0)))
    return TRUE;
  if (! IsVar(X(0)))
    MINOR_FAULT("current_atom/1: incorrect 1st arg");
#if defined(ATOMGC)
  { 
    /* There is an (improbable) case: the 0-th table entry is empty.
       Take it into account. */
    intmach_t size = SwitchSize(ciao_atoms) >> 1;
    intmach_t i = 0;
    while (i < size && (atmtab[i] == NULL))
      i++;
    X(1) = MakeSmall(i);  /* Yes, I am not considering an empty symbol table */
  }
#else
  X(1) = TaggedZero;
#endif
  push_choicept(Arg,address_nd_current_atom);
  return nd_current_atom(Arg);
}

CBOOL__PROTO(nd_current_atom)
{
  intmach_t i = GetSmall(X(1));

#if defined(ATOMGC)
 /* 

    Atom GC may leave holes in the atom table.  Therefore: 
    
    1- The "following" valid entry is not necessarily the next one; 
    we may have to skip a number of empty atom entries.

    2- Stopping when the number of indices read reaches the current number of
    atoms is not right.  We have to use instead the size of the table. 

  */

  intmach_t size = SwitchSize(ciao_atoms) >> 1;

  /* Invariant: at entry, the current i points to a nonempty atom */
  CBOOL__UnifyCons(TagIndex(ATM,i),X(0));
  /* Go forward until the next non-empty atom; final stop when the end of
     the table has been reached.  */
  i++;
  while(i < size && (atmtab[i] == NULL))
    i++;
  
  if (i < size)                                  /* We got the next index */
    w->node->term[1] = MakeSmall(i);
  else 
    pop_choicept(Arg);
#else
  w->node->term[1] += MakeSmallDiff(1);
  CBOOL__UnifyCons(TagIndex(ATM,i),X(0));
    
  if (i+1 == ciao_atoms->count)
    pop_choicept(Arg);
#endif

  return TRUE;
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       $CURRENT_CLAUSES/2
   -----------------------------------------------------------------------*/

/* This used to be nondeterministic. */
/* ASSERT: X1 is always unbound, unconditional. */
CBOOL__PROTO(current_clauses)
{
  tagged_t *junk;
  definition_t *d;

  DEREF(X(0),X(0));
  if (! IsVar(X(0))) {
    d = find_definition(predicates_location,X(0),&junk,FALSE);
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED)) {
      return (*TagToPointer(X(1))=PointerToTerm(d->code.intinfo), TRUE);
    } else {
      return FALSE;
    }
  }
  else {
    MINOR_FAULT("$current_clauses/2: incorrect 1st arg");
  }
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       REPEAT/0
   -----------------------------------------------------------------------*/

CBOOL__PROTO(prolog_repeat)
{
  push_choicept(Arg,address_nd_repeat);
  return TRUE;
}

CBOOL__PROTO(nd_repeat)
{
  return TRUE;
}


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
  sw_on_key_t *table = (sw_on_key_t *)TermToPointer(X(3));
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
        w->node->term[2] = MakeSmall(j);
      CBOOL__UnifyCons(d->printname,X(0));
      return cunify(Arg,
                    make_structure(Arg,SetArity(d->printname,d->arity)),
                    X(1));
    }
  }
  pop_choicept(Arg);
  return FALSE;
}



/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       PREDICATE_PROPERTY/2
   -----------------------------------------------------------------------*/

 /* See dummy.pl (MCL) */

static unsigned int predicate_property_bits(definition_t *d)
{
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
  sw_on_key_t *table = (sw_on_key_t *)TermToPointer(X(4));
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
            w->node->term[3] = MakeSmall(j);
          CBOOL__UnifyCons(MakeSmall(d->predtyp),X(1));
          CBOOL__UnifyCons(MakeSmall(predicate_property_bits(d)),X(2));
          return cunify(Arg,make_structure(Arg,SetArity(d->printname,d->arity)),
                        X(0));
        }
    }
  pop_choicept(Arg);
  return FALSE;
}

/* ------------------------------------------------------------------ */

CBOOL__PROTO(module_is_static)
{
  DEREF(X(0),X(0));
  if (!TaggedIsATM(X(0)))
    return FALSE;

  module_t *d = insert_module(modules_location,X(0),FALSE);
  if (d==NULL) return FALSE;

  return d->properties.is_static;
}

