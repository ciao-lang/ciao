/*
 *  debug.c
 *
 *  Code for debuging tabling.
 *
 *  XS
 */

#include <stdio.h> 
#include <string.h> 

#include <ciao/ciao_prolog.h>
#include <ciao/termdefs.h>
#include <ciao/tabling.h> 

#include "debug.h"
#include "engine.h" 


//#if defined(DEBUG_ALL)
tagged_t tabling_debug;
tagged_t tabling_debug_tries;
tagged_t tabling_trace;
tagged_t tabling_print;
tagged_t tabling_bypass;
//#endif

#if defined(ANS_COUNTER)
intmach_t ans_no_saved, ans_removed, ans_saved, ans_aggregated;

CBOOL__PROTO(print_counters_c) 
{
  printf("answers saved      = %10ld \n", (long)ans_saved);
  printf("answers discarded  = %10ld \n", (long)ans_no_saved);
  printf("answers removed    = %10ld \n", (long)ans_removed);
  printf("answers aggregated = %10ld \n", (long)ans_aggregated);
  return TRUE;
}
#endif

CBOOL__PROTO(set_tabling_flag_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  if (tabling_debug == atom_on)
    printf("Set_tabling_flag\n"); //XS
#endif

  char * flag, * value;
  tagged_t newvalue;

  /* check value is on or off */

  DEREF(ARG2,ARG2);
  value = (char *)TagToAtom(ARG2)->name;
  
  if (strcmp(value, "off")==0) newvalue = atom_off;
  else if (strcmp(value, "on")==0) newvalue = atom_on;
  else  return FALSE;

  /* check flag is correct */

  DEREF(ARG1,ARG1);
  flag = (char *)TagToAtom(ARG1)->name;

  //#if defined(DEBUG_ALL)
  if (strcmp(flag, "debug")==0) tabling_debug = newvalue;
  else if (strcmp(flag, "debug_tries")==0) tabling_debug_tries = newvalue;
  else if (strcmp(flag, "trace")==0) tabling_trace = newvalue;
  else if (strcmp(flag, "print")==0) tabling_print = newvalue;
  else if (strcmp(flag, "bypass")==0) tabling_bypass = newvalue;
  else return FALSE; 
  //#endif

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

// TODO XS: current_tabling_flag can NOT be used to request flags as current_prolog_flag(X,Y).
CBOOL__PROTO(current_tabling_flag_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  if (tabling_debug == atom_on)
    printf("Set_tabling_flag\n"); //XS
#endif

  char * flag, * value;
  tagged_t newvalue;

  /* check flag is correct */

  DEREF(ARG1,ARG1);
  if (TaggedIsATM(ARG1)) {
    flag = (char *)TagToAtom(ARG1)->name;
  } else {
    printf("First argument it is not instanciated\n"); 
    return FALSE;
  }

  //#if defined(DEBUG_ALL)
  if (strcmp(flag, "debug")==0) return Unify(ARG2,tabling_debug);
  else if (strcmp(flag, "debug_tries")==0) return Unify(ARG2,tabling_debug_tries);
  else if (strcmp(flag, "trace")==0) return Unify(ARG2,tabling_trace);
  else if (strcmp(flag, "print")==0) return Unify(ARG2,tabling_print);
  else if (strcmp(flag, "bypass")==0) return Unify(ARG2,tabling_bypass);
  //#endif

  printf("The flag '%s' does not exists\n", flag);
  return FALSE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


CBOOL__PROTO(tabling_stats_c){

  frame_t *newa;

  /* printf("Heap   (%p-%p)  global_top  = (%p) %d \n", */
  /*     Heap_Start, Heap_End, w->global_top, (int)HeapDifference(Heap_Start, w->global_top)); */
  /* ComputeA(newa,w->node); */
  /* printf("Stack  (%p-%p)  local_top   = (%p) %d \n", */
  /*     Stack_Start, Stack_End, newa, (int)StackDifference(Stack_Start, newa)); */
  /* printf("Trail  (%p-%p)  trail_top   = (%p) %d \n", */
  /*     Trail_Start, Trail_End, w->trail_top, (int)TrailDifference(Trail_Start, w->trail_top)); */
  /* printf("Choice (%p-%p)  choice_top  = (%p) %d \n", */
  /*     Choice_Start, Choice_End, w->node+w->value_trail, (int)ChoiceDifference(Choice_Start, w->node+w->value_trail)); */

  printf("TABLING_GLOBAL_TABLE (%p-%p) size = %lu\n\n",
         global_table, global_table_end, (global_table_free-global_table));

  printf("TABLING_STACK_TABLE  (%p-%p) size = %lu\n\n",
         tabling_stack, tabling_stack_end, (tabling_stack_free-tabling_stack));

  return TRUE;

}
