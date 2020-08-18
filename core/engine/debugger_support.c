/*
 *  debugger_support.c
 *
 *  Debugger support
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/eng_registry.h>
#include <ciao/basiccontrol.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/timing.h>
#include <ciao/eng_profile.h>

CBOOL__PROTO(retry_cut)
{
  tagged_t number;
  node_t *nd;

  DEREF(X(0),X(0));
  if (!TaggedIsSmall(X(0)))
    return FALSE;
  for (nd = w->node;
       ChoiceYounger(nd,Choice_Start);
       nd = ChoiceCharOffset(nd,-nd->next_alt->node_offset))
    {
      DEREF(number,nd->term[0]);
      if (nd->term[3]==atom_retry_hook && number<=X(0))
        {
          nd->term[1] = X(1);   /* always dereferenced */
          w->node = nd;
          SetShadowregs(nd);
          break;
        }
    }
  return ChoiceYounger(nd,Choice_Start);
}

CBOOL__PROTO(spypoint)
{
  tagged_t *junk;
  definition_t *func;
  
  DEREF(X(0),X(0));
  func = find_definition(predicates_location,X(0),&junk,FALSE);
  if (!func  /* || func->properties.public */)
    return FALSE;
  if (func->properties.spy)
    CBOOL__UnifyCons(atom_on,X(1))
  else
    CBOOL__UnifyCons(atom_off,X(1));

  DEREF(X(2),X(2));
  func->properties.spy = (X(2)==atom_on);

  SetEnterInstr(func,func->predtyp);
  return TRUE;
}

CBOOL__PROTO(debugger_state)
{
#if defined(DEBUG)
  if (debug_gc)
    fprintf(stderr,
            "Thread %d is in debbuger_state\n", (int)Thread_Id);

  /*
  DEREF(X(0), X(0));
  if (IsVar(X(0)))
    fprintf(stderr, "First Arg. is Var\n");
  else 
    wr_functor("First Arg. is ", X(0));

  DEREF(X(1), X(1));
  if (IsVar(X(1)))
    fprintf(stderr, "Second Arg. is Var\n");
  else 
    wr_functor("Second Arg. is ", X(1));
  */
#endif

  if (!cunify(Arg,Current_Debugger_State,X(0)))
    return FALSE;
  DEREF(Current_Debugger_State,X(1));
  return TRUE;
}

CBOOL__PROTO(debugger_mode)
{
#if defined(DEBUG)
  if (debug_gc)
    fprintf(stderr, "Thread %d is changing debbuger mode\n", (int)Thread_Id);
#endif

  if (TagIsSTR(Current_Debugger_State)) {
#if defined(DEBUG)
  if (debug_gc)  fprintf(stderr, "Current_Debugger_State is structure\n");
#endif
    RefArg(Current_Debugger_Mode,Current_Debugger_State,2);
    if (Current_Debugger_Mode != atom_off)
      address_interpret_c_goal = address_interpret_compiled_goal;
    else
      address_interpret_c_goal = address_interpret_goal;
  } else {
    Current_Debugger_Mode = atom_off;
    address_interpret_c_goal = address_interpret_goal;
  }
  return TRUE;
}

/****
void debugger_trap(void)
{
  if (TagIsSTR(current_debugger_state))
  {
      *TagToArg(current_debugger_state,2) = atom_trace;
      *TagToArg(current_debugger_state,3) = MakeSmall(1000000);
      current_debugger_mode = atom_trace;
      address_apply = address_slow_apply;
      address_interpret_c_goal = address_interpret_compiled_goal;
  }
}
****/



/*
CBOOL__PROTO(leash_mode)
{
  CBOOL__UnifyCons(current_leash_mode,X(0));
  DEREF(current_leash_mode,X(1)); 
  return TRUE;
}

CBOOL__PROTO(maxdepth)
{
  CBOOL__UnifyCons(current_maxdepth,X(0));
  DEREF(current_maxdepth,X(1)); 
  return TRUE;
}

CBOOL__PROTO(printdepth)
{
  CBOOL__UnifyCons(current_printdepth,X(0));
  DEREF(current_printdepth,X(1)); 
  return TRUE;
}

CBOOL__PROTO(breaklevel)
{
  CBOOL__UnifyCons(current_breaklevel,X(0));
  DEREF(X(1),X(1));
  current_breaklevel += X(1)-TaggedZero;
  return TRUE;
}
*/

