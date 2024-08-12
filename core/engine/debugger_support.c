/*
 *  debugger_support.c
 *
 *  Debugger support
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <ciao/eng.h>
#if defined(OPTIM_COMP)
// #include <ciao/io_basic.h> /* display_term */
#else
#include <ciao/eng_registry.h>
#include <ciao/basiccontrol.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/rt_exp.h>
#include <ciao/timing.h>
#include <ciao/eng_profile.h>
#endif

CBOOL__PROTO(retry_cut) {
  tagged_t number;
  choice_t *nd;

  DEREF(X(0),X(0));
  CBOOL__TEST(TaggedIsSmall(X(0)));
  for (nd = w->choice;
       ChoiceYounger(nd,Choice_Start);
       nd = ChoiceCont(nd)) {
    DEREF(number,nd->x[0]);
    if (nd->x[3]==atom_retry_hook && number<=X(0)) {
      nd->x[1] = X(1); /* always dereferenced */
      SetChoice(nd);
      break;
    }
  }
  CBOOL__LASTTEST(ChoiceYounger(nd,Choice_Start));
}

CBOOL__PROTO(spypoint) {
  tagged_t *junk;
  definition_t *func;
  
  DEREF(X(0),X(0));
#if defined(OPTIM_COMP)
  func = find_definition(X(0),&junk,FALSE);
#else
  func = find_definition(predicates_location,X(0),&junk,FALSE);
#endif
  CBOOL__TEST(func != NULL);

  CBOOL__UnifyCons((func->properties.spy ? atom_on : atom_off), X(1));

  DEREF(X(2),X(2));
  func->properties.spy = (X(2)==atom_on);

#if defined(OPTIM_COMP)
  /* TODO: disabled! fix it; patch enterop and save previous value */
  /* SetEnterInstr(func,func->predtyp); */
#else
  SetEnterInstr(func,func->predtyp);
#endif
  CBOOL__PROCEED;
}

#if !defined(OPTIM_COMP)
CBOOL__PROTO(debugger_state) {
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is in debugger_state\n", (intmach_t)Thread_Id);
  CBOOL__UNIFY(Current_Debugger_State,X(0));
  DEREF(Current_Debugger_State,X(1));
  CBOOL__PROCEED;
}
#endif

#if defined(OPTIM_COMP)
CBOOL__PROTO(set_debugger_mode) {
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is changing to debugger mode\n", (intmach_t)Thread_Id);

  DEREF(Current_Debugger_Mode,X(0));
  if (Current_Debugger_Mode != atom_off) {
    //SetCIntEvent();
    debug_mode = TRUE;
    debug_status = 2;
  } else {
    /* TODO: does it interfere with C-c? */
    UnsetCIntEvent();
    debug_mode = FALSE;
    debug_status = 0;
  }
  CBOOL__PROCEED;
}
#else
CBOOL__PROTO(debugger_mode) {
  DEBUG__TRACE(debug_gc, "Thread %" PRIdm " is changing to debugger mode\n", (intmach_t)Thread_Id);

  if (TaggedIsSTR(Current_Debugger_State)) {
    DEBUG__TRACE(debug_gc, "Current_Debugger_State is structure\n");
    Current_Debugger_Mode = *TaggedToArg(Current_Debugger_State,2);
    if (Current_Debugger_Mode != atom_off)
      address_interpret_c_goal = address_interpret_compiled_goal;
    else
      address_interpret_c_goal = address_interpret_goal;
  } else {
    Current_Debugger_Mode = atom_off;
    address_interpret_c_goal = address_interpret_goal;
  }
  CBOOL__PROCEED;
}
#endif

#if defined(OPTIM_COMP)
CINSNP__PROTO(code_call1);

CINSNP__PROTO(code_notracecall1) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    //TRACE_PRINTF("dc_(");
    //CVOID__CALL(display_term, X(0), stream_trace, TRUE);
    //TRACE_PRINTF(")\n");
    /* enable tracing for next goal */
    debug_status = 1;
    SetCIntEvent();
  }
#endif
  CINSNP__LASTCALL(code_call1);
}

CBOOL__PROTO(code_start_trace) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    SetCIntEvent();
    debug_status = 2;
  }
#endif
  CBOOL__PROCEED;
}

CBOOL__PROTO(code_stop_trace) {
#if defined(USE_PROLOG_DEBUGGER)
  if (debug_mode) {
    UnsetCIntEvent();
    debug_status = 0;
  }
#endif
  CBOOL__PROCEED;
}
#endif

/****
void debugger_trap(void) {
  if (TaggedIsSTR(current_debugger_state)) {
      *TaggedToArg(current_debugger_state,2) = atom_trace;
      *TaggedToArg(current_debugger_state,3) = MakeSmall(1000000);
      current_debugger_mode = atom_trace;
      address_apply = address_slow_apply;
      address_interpret_c_goal = address_interpret_compiled_goal;
  }
}
****/

/*
CBOOL__PROTO(leash_mode) {
  CBOOL__UnifyCons(current_leash_mode,X(0));
  DEREF(current_leash_mode,X(1)); 
  return TRUE;
}

CBOOL__PROTO(maxdepth) {
  CBOOL__UnifyCons(current_maxdepth,X(0));
  DEREF(current_maxdepth,X(1)); 
  return TRUE;
}

CBOOL__PROTO(printdepth) {
  CBOOL__UnifyCons(current_printdepth,X(0));
  DEREF(current_printdepth,X(1)); 
  return TRUE;
}

CBOOL__PROTO(breaklevel) {
  CBOOL__UnifyCons(current_breaklevel,X(0));
  DEREF(X(1),X(1));
  current_breaklevel += X(1)-TaggedZero;
  return TRUE;
}
*/
