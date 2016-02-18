/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <stdio.h>
#include <stdlib.h>

#include <ciao_prolog.h>

#include <ciao/support.h>
#include <ciao/termdefs.h>
#include <ciao/datadefs.h>
#include <ciao/access.h>
#include <ciao/tasks.h>
#include <ciao/task_areas.h>
#include <ciao/stacks.h>
#include <ciao/wam.h>
#include <ciao/attr.h>

#include <ciao/tabling.h>
#include "engine.h"
#include "terms.h"
#include "difference_constraints.h"
#include "space.h"

/* -------------------------- */
/*          Code              */
/* -------------------------- */

/* TODO (JFMC): add more foreign files instead */
#include "terms.c"
#include "space.c"

/* -------------------------- */
/*     Difference_Constraints Interface       */
/* -------------------------- */

CBOOL__PROTO(incr_dc_num_vars_c)
{
#if defined(DEBUG_ALL)
  printf("\nincr_dc_num_vars START\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif
  space->size++;
#if defined(DEBUG_ALL)
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nincr_dc_num_vars END\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(decr_dc_num_vars_c)
{
#if defined(DEBUG_ALL)
  printf("\ndecr_dc_num_vars START\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif
  space->size--;
#if defined(DEBUG_ALL)
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\ndecr_dc_num_vars END\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(put_dc_value_c)
{
#if defined(DEBUG_ALL)
  printf("\nput_dc_value START\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  space->edges[GetInteger(X(0))][GetInteger(X(1))] = GetInteger(X(2));
#if defined(DEBUG_ALL)
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nput_dc_value END\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(put_dc_pi_c)
{
#if defined(DEBUG_ALL)
  printf("\nput_dc_pi START\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif
  DEREF(X(0),X(0));

  int i = 0;
  while (X(0) != EMPTY_LIST)
  {
    tagged_t term = HeadOfTerm(X(0));
    DEREF(term,term);
    space->pi[i++] = IntOfTerm(term);
    X(0) = TailOfTerm(X(0));
    DEREF(X(0),X(0));      
  }
#if defined(DEBUG_ALL)
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nput_dc_pi END\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(put_dc_attr_c)
{
#if defined(DEBUG_ALL)
  printf("\nput_dc_attr START\n"); fflush(stdout);
#endif
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  args[0] = X(1);						
  args[1] = X(0);							
  tmp_term1 = MkApplTerm(functor_dbm_id,2,args);			

  bu2_update_attribute(w,X(0),tmp_term1);

#if defined(DEBUG_ALL)
  printf("\nput_dc_attr END\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(put_dc_space_c)
{
#if defined(DEBUG_ALL)
  printf("\nput_dc_space %p START\n",space); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif
  DEREF(X(0),X(0));

  space = (struct space*) IntOfTerm(X(0));
#if defined(DEBUG_ALL)
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nput_dc_space %p END\n",space); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(difference_constraints_print_c)
{
  print_space(space);

  return TRUE;
}

CBOOL__PROTO(difference_constraints_print_variable_c)
{
#ifdef DEBUG_ALL
  printf("\nEntering print_variable\n"); fflush(stdout);
#endif
  DEREF(X(0),X(0));
  
  int id = GetInteger(X(0));

  print_variable_space(space,id);

#ifdef DEBUG_ALL
  printf("\nExiting print_variable\n"); fflush(stdout);
#endif
  return TRUE;
}

//CBOOL__PROTO(difference_constraints_get_space_c)
//{
//#ifdef DEBUG_ALL
//  printf("\nEntering get_space\n"); fflush(stdout);
//#endif
//  struct space* prev_space = space;
//  if (last_cp != w->node)
//    space = clone_space(space);
//
//  Unify(X(0),MkInt((int)prev_space));
//  Unify(X(1),MkInt((int)last_cp));
//
//  last_cp = w->node;
//
//  return TRUE;
//}
//
//CBOOL__PROTO(difference_constraints_backtracking_space_c)
//{
//#ifdef DEBUG_ALL
//  printf("\nEntering backtracking_space\n"); fflush(stdout);
//#endif
//  DEREF(X(0),X(0));
//  DEREF(X(1),X(1));
//  struct space *prev_space = (struct space*)GetInteger(X(0));
//  last_cp = (node_t*)GetInteger(X(1));
//
////  if (prev_space != space) delete_space(space);
//
//  space = prev_space;
//  last_cp = NULL;
//
//  return TRUE;
//}

CBOOL__PROTO(difference_constraints_var_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering new_diff_var\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif

  int id = new_diff_var_space(w,space);

#ifdef DEBUG_ALL
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nExiting new_diff_var\n"); fflush(stdout);
#endif
  return Unify(X(0),MkIntTerm(id));
}

CBOOL__PROTO(difference_constraints_LB_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering diff_LB %p\n",space); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif

  DEREF(X(0),X(0));
  int id = GetInteger(X(0));
  DEREF(X(1),X(1));
  int lb = GetInteger(X(1));

  int res = add_diff_const_space(w,space,0,id,lb*(-1));
#ifdef DEBUG_ALL
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nExiting diff_LB %d\n",res); fflush(stdout);
#endif
  return res;
}

CBOOL__PROTO(difference_constraints_UB_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering diff_UB\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  int id = GetInteger(X(0));
  int ub = GetInteger(X(1));

  int resul = add_diff_const_space(w,space,id,0,ub);
#ifdef DEBUG_ALL
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nExiting diff_UB %d\n",resul); fflush(stdout);
#endif
  return resul;
}

CBOOL__PROTO(difference_constraints_const_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering diff_const\n"); fflush(stdout);
  printf("\nPRE SPACE\n");
  difference_constraints_print_c(w);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));
  int d = GetInteger(X(2));

  int resul = add_diff_const_space(w,space,id1,id2,d);

#ifdef DEBUG_ALL
  printf("\nPOST SPACE\n");
  difference_constraints_print_c(w);
  printf("\nExiting diff_const %d\n",resul); fflush(stdout);
#endif
  return resul;
}

CBOOL__PROTO(difference_constraints_get_value_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering get_value\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  if (space->edges[id1][id2] != MAX)
    return Unify(X(2),MkIntTerm(space->edges[id1][id2]));
  
  return FALSE;
}

CBOOL__PROTO(difference_constraints_do_canonical_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering do_canonical\n"); fflush(stdout);
#endif

  int *vars = (int*) malloc (space->size * sizeof(int));
  int i;
  int *sp;
  for (i = 0; i < space->size; i++) 
    vars[i] = i;
  for (i = 0; i < space->size; i++) 
    dijkstra_space(w,space,i);
  free(vars);
  
#ifdef DEBUG_ALL
  printf("\nExiting do_canonical\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(difference_constraints_full_abstraction_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering full_abstraction\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  tagged_t l1, l2;

  for (l1 = X(0); l1 != EMPTY_LIST; l1 = TailOfTerm(l1))
    {
      for (l2 = TailOfTerm(l1); l2 != EMPTY_LIST; l2 = TailOfTerm(l2))
	{
	  int id1 = GetInteger(HeadOfTerm(l1));
	  int id2 = GetInteger(HeadOfTerm(l2));
	  full_abstraction_space(w,space,id1,id2);
	}
    }

#ifdef DEBUG_ALL
  printf("\nExiting full_abstraction\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(difference_constraints_normalize_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering normalize\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  int max_lb = GetInteger(X(1));
  int max_ub = GetInteger(X(2));

  tagged_t l1, l2;

  for (l1 = X(0); l1 != EMPTY_LIST; l1 = TailOfTerm(l1))
    {
      for (l2 = TailOfTerm(l1); l2 != EMPTY_LIST; l2 = TailOfTerm(l2))
	{
	  int id1 = GetInteger(HeadOfTerm(l1));
	  int id2 = GetInteger(HeadOfTerm(l2));
	  normalize_space(w,space,id1,id2,max_lb,max_ub);
	}
    }

#ifdef DEBUG_ALL
  printf("\nExiting normalize\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(difference_constraints_delay_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering delay\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  tagged_t list;

  for (list = X(0); list != EMPTY_LIST; list = TailOfTerm(list))
    {
      int id = GetInteger(HeadOfTerm(list));
      delay_space(w,space,id);
    }

#ifdef DEBUG_ALL
  printf("\nExiting delay\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(difference_constraints_reset_c)
{  
#ifdef DEBUG_ALL
  printf("\nEntering reset\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  int id1 = GetInteger(X(0));
  int id2 = GetInteger(X(1));

  tagged_t list;
  for (list = X(2); list != EMPTY_LIST; list = TailOfTerm(list))
    {
      int v = GetInteger(HeadOfTerm(list));
      reset_space(w,space,id1,id2,v);
    }

#ifdef DEBUG_ALL
  printf("\nExiting reset\n"); fflush(stdout);
#endif
  return TRUE;
}

CBOOL__PROTO(initial_space_c)
{
#if defined(DEBUG_ALL)
  printf("\nInitial Space\n"); fflush(stdout);
#endif
  space = create_space();
  
  atom_incr_dc_num_vars = 
    MakeString("difference_constraints_rt_ll:$incr_dc_num_vars");
  atom_decr_dc_num_vars = 
    MakeString("difference_constraints_rt_ll:$decr_dc_num_vars");
  functor_forward_trail = 
    SetArity(MakeString("forward_trail:$forward_trail"), 2);
  functor_put_dc_value = 
    SetArity(MakeString("difference_constraints_rt_ll:$put_dc_value"), 3);
  functor_put_dc_pi = 
    SetArity(MakeString("difference_constraints_rt_ll:$put_dc_pi"), 1);
  functor_dbm_id = SetArity(MakeString("$dbm_id"), 2);
  functor_put_dc_space = 
    SetArity(MakeString("difference_constraints_rt_ll:$put_dc_space"), 1);

  return TRUE;
}
