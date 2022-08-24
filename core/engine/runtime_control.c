/*
 *  runtime_control.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
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
#include <ciao/runtime_control.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_profile.h>
#include <ciao/basiccontrol.h>

/* -------------------------------------------------------------------
   CHOICEPT MANIPULATIONS
   ----------------------------------------------------------------------*/

CVOID__PROTO(pop_choicept) {
  choice_t *b = w->choice;
  w->choice = b = ChoiceCont(b);
  SetShadowregs(b);
}

CVOID__PROTO(push_choicept, try_node_t *alt)
{
  intmach_t n = alt->choice_offset;
  tagged_t *b0 = (tagged_t *)w->choice;
  choice_t *b = ChoiceCharOffset(b0,n);

  ComputeA(w->local_top,w->choice);
  w->choice = b;
  NewShadowregs(w->heap_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->heap_top);
  b->next_alt = alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  n = OffsetToArity(n);
  while (n>0)
    ChoicePush(b0,X(--n));
  if (ChoiceYounger(ChoiceOffset(w->choice,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
}

#if defined(PARBACK)
CBOOL__PROTO(nd_suspension_point)
{
  //Reinstall next_insn and Frame (Done by fail).
  //Reinstal argument register
  tagged_t t = w->choice->x[0];
  definition_t * func = (definition_t *) *TaggedToPointer(t);
  int i;
  for (i = 0; i < func->arity; i++) DEREF(X(i),*TaggedToPointer(TaggedToPointer(t)+i+1));
  //Take choice point out
  Arg->next_insn = restart_point_insn;

  return TRUE;
}
#endif

#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept)
{
  pop_choicept(Arg);
  return FALSE;
}
#endif

/* internals:'$yield'/0: force exit of the wam function (continue with ciao_query_resume()) */
CBOOL__PROTO(prolog_yield) {
  // TODO: try a more direct way, do not create choice points */
  push_choicept(w,address_nd_yield);
  Stop_This_Goal(w) = TRUE;
  SetEvent(); // TODO: see concurrency.c using "SetWakeCount(1);" instead
  goal_descriptor_t *ctx = w->misc->goal_desc_ptr;
  SetSuspendedGoal(w, TRUE);
  ctx->action = BACKTRACKING | KEEP_STACKS; // continue on alternative
  CBOOL__PROCEED;
}

CBOOL__PROTO(nd_yield) {
  pop_choicept(w);
  CBOOL__PROCEED;
}

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
    w->choice->x[1] = MakeSmall(i);
  else 
    pop_choicept(Arg);
#else
  w->choice->x[1] += MakeSmallDiff(1);
  CBOOL__UnifyCons(TagIndex(ATM,i),X(0));
    
  if (i+1 == ciao_atoms->count)
    pop_choicept(Arg);
#endif

  return TRUE;
}

/* --------------------------------------------------------------------------- */

/* TODO: alternatively: use the mem address, if it is stable (JF) */

/* 
   Support for generating new atoms with "funny names", always different.
   Make sure that the generation works OK with concurrency.  */

/* This seems to be the right size: one character less, and time (at large)
   doubles; one character more, and comparison in the symbol table takes
   longer. */
#define NEW_ATOM_LEN 13
#define NUM_OF_CHARS 62
static char allowed_char_table[NUM_OF_CHARS + 1] =
"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char new_atom_str[] = "!!!!!!!!!!!!!";
#define FIRST_CHAR 0
#define LAST_CHAR  (NUM_OF_CHARS-1)

unsigned int x = 13*17;

CBOOL__PROTO(prolog_new_atom)
{
  ERR__FUNCTOR("runtime_control:new_atom", 1);
  int i;
  int previous_atoms_count;
  tagged_t new_atom;

  DEREF(X(0), X(0));
  if (!IsVar(X(0))) {
    BUILTIN_ERROR(UNINSTANTIATION_ERROR,X(0),1);
  }

  Wait_Acquire_slock(atom_id_l);

  previous_atoms_count = ciao_atoms->count;
  do {
    for (i = 0; i < NEW_ATOM_LEN; i++) {
      x = (((new_atom_str[i] + x - FIRST_CHAR) * 13) + 300031);
      new_atom_str[i] = allowed_char_table[(x % NUM_OF_CHARS) + FIRST_CHAR];
      x = x / NUM_OF_CHARS;
    }
    new_atom = GET_ATOM(new_atom_str);
    /* Make sure no smart guy already inserted the atom we have in mind */
  } while(ciao_atoms->count == previous_atoms_count);

  Release_slock(atom_id_l);
  CBOOL__LASTUNIFY(X(0), new_atom);
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
      return (*TaggedToPointer(X(1))=PointerToTerm(d->code.intinfo), TRUE);
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

