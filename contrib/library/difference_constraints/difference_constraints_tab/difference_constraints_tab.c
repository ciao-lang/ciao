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

#if defined(TABLING)
#include <ciao/tabling.h>
#include "../engine.h"
#include "../space.h"
#include "../difference_constraints.h"
#include "difference_constraints_tab.h"

//#include "../space.c"

int *get_attr_values(int size, tagged_t *vars)
{
  int *res = (int*) checkalloc (size * sizeof(int));
  int i;
  for (i = 0; i < size; i++)
    {
      tagged_t attr = fu1_get_attribute(NULL,vars[i]);
      DEREF(attr,ArgOfTerm(1, attr));
      res[i] = IntOfTerm(attr);
    }
  return res;
}
#endif

CBOOL__PROTO(lookup_attr_call_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nlookup_attr_call START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  TrNode node =  (TrNode) IntOfTerm(X(0));
  struct l_gen *l_gen = (struct l_gen*) node->child;
  struct sf *sf = (struct sf*) IntOfTerm(X(1));
  tagged_t l_prune;
  int *attr_values;

  switch(sf->attr_size)
    {
    case 0:
      if (l_gen == NULL)
	{
	  ALLOC_GLOBAL_TABLE(l_gen, struct l_gen*, sizeof(struct l_gen));
	  node->child = (TrNode) l_gen;
	  l_gen->space = create_space();
	  l_gen->node = NULL;
	  l_gen->orig_space = space;
	  l_gen->clone_space = clone_space(l_gen->space);
	  CHANGE_SPACE(w,l_gen->clone_space);
	  l_gen->next = NULL;
	}
      l_prune = (tagged_t)NULL;
      break;
    default:
      attr_values = get_attr_values(sf->attr_size,sf->attrs);
      get_shortest_path_space(w,space,sf->attr_size, attr_values);
//      struct timeval t_ini, t_fin;
//      gettimeofday(&t_ini,NULL);
      for ( ; l_gen != NULL; l_gen = l_gen->next)
	{
	  //Check for entailmentg
	  if (is_more_general_space(l_gen->space, sf->attr_size,
				    space, attr_values)) break;
	}
//      gettimeofday(&t_fin,NULL);
//      trail_time = trail_time + timeval_diff(&t_fin, &t_ini);
#if defined(DEBUG_ALL)
      printf("\nLGEN = %p\n",l_gen);
#endif
      if (l_gen == NULL)
	{
	  ALLOC_GLOBAL_TABLE(l_gen, struct l_gen*, sizeof(struct l_gen));
	  l_gen->node = NULL;
	  //attr_values needs to be save into global_table
	  ALLOC_GLOBAL_TABLE(l_gen->orig_attrs, 
			     int*, 
			     sf->attr_size * sizeof(int));
	  int i;
	  for (i = 0; i < sf->attr_size; i++)
	    l_gen->orig_attrs[i] = attr_values[i];
//	  l_gen->orig_attrs = attr_values;
	  l_gen->space = proy_space(Arg,sf->attr_size,sf->attrs,
				    l_gen->orig_attrs,1);
	  l_gen->orig_space = space;
	  l_gen->clone_space = clone_space(l_gen->space);
#if defined(DEBUG_ALL)
	  printf("\nPROYEC\n");
	  print_space(l_gen->space);
	  printf("\nORIG\n");
	  print_space(l_gen->orig_space);
#endif
	  CHANGE_SPACE(w,l_gen->clone_space);
	  l_gen->next = (struct l_gen*) node->child;
	  node->child = (TrNode) l_gen;
	}
      checkdealloc(attr_values, sf->attr_size * sizeof(int));
      l_prune = (tagged_t)NULL;
      //l_prune = EMPTY_LIST; subsumed generators
    }

#if defined(DEBUG_ALL)
  printf("\nlookup_attr_call END\n"); fflush(stdout);
#endif

  return Unify(X(2),MkIntTerm((int)(&(l_gen->node)))) &&
    Unify(X(3),MkIntTerm((int)l_gen)) &&
    Unify(X(4),MkIntTerm((int)l_prune));

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(lookup_attr_answer_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

//  struct timeval t_ini, t_fin;
  TrNode node = (TrNode) IntOfTerm(X(0));
  struct l_ans *l_ans = (struct l_ans*) node->child;
  struct attrs *attrs = (struct attrs*) IntOfTerm(X(1));
  tagged_t l_prune;
  int *attr_values = NULL;

  switch(attrs->size)
    {
    case 0:
      if (l_ans != NULL)
	{
#if defined(DEBUG_ALL)
	  printf("\nlookup_attr_answer END I\n"); fflush(stdout);
#endif
	  //Tabling_stk is used as tmp memory (this is safe)
	  DEALLOC_TABLING_STK(attrs);
	  return FALSE;
	}
      break;
    default:
      attr_values = get_attr_values(attrs->size,attrs->attrs);
      get_shortest_path_space(w,space,attrs->size, attr_values);
    look_more_general:
//      gettimeofday(&t_ini,NULL);
      for ( ; l_ans != NULL; l_ans = l_ans->next)
	{
	  //Check for entailmentg
	  if (is_more_general_space(l_ans->space, attrs->size,
			      space,attr_values)) break;
	}
//      gettimeofday(&t_fin,NULL);
//      trail_time = trail_time + timeval_diff(&t_fin, &t_ini);
      if (l_ans != NULL)
	{
	  //Tabling_stk is used as tmp memory (this is safe)
	  DEALLOC_TABLING_STK(attrs);
#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer END II\n"); fflush(stdout);
#endif
	  return FALSE;
	}
    }

  ALLOC_GLOBAL_TABLE(l_ans, struct l_ans*, sizeof(struct l_ans));
  l_ans->space = proy_space(Arg,attrs->size,attrs->attrs,attr_values,0);
#if defined(DEBUG_ALL)
  print_space(l_ans->space);
#endif
  l_ans->next = (struct l_ans*) node->child;
  node->child = (TrNode)l_ans;
  l_prune = (tagged_t)NULL;
  //l_prune = EMPTY_LIST; subsumed generators

  //Tabling_stk is used as tmp memory (this is safe)
  DEALLOC_TABLING_STK(attrs);

#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer END III\n"); fflush(stdout);
#endif


  return Unify(X(2),MkIntTerm(l_ans)) &&
    Unify(X(3),MkIntTerm(l_prune));

 //   if (node->child == NULL && node != call->last_ans)
 //     { 
 //       struct answer_list *answer_list = 
 //    	      (struct answer_list*) malloc (sizeof(struct answer_list));
 //       answer_list->answer = answer;
 //       answer_list->sig = NULL;
 //       
 //       //updating generator answer list
 //       if (call->first_answer == NULL) call->first_answer = answer_list;    
 //       if (call->last_answer != NULL) call->last_answer->sig = answer_list;
 //       call->last_answer = answer_list;
 //       if (call->first_ans == NULL) call->first_ans = node;      
 //       if (call->last_ans != NULL) call->last_ans->child = node;
 //       call->last_ans = node;
 // 
 //       //looking for merging
 //       struct space *convex_hull;
 //       printf("\nCalling merging\n"); fflush(stdout);
 //       struct difference_constraints_list *difference_constraints_list = answer;
 //       while (convex_hull = make_answer_merging_c(difference_constraints_list))
 // 	 {
 // 	  printf("\nMerging!!\n"); fflush(stdout);
 // 	  // initialazing a new difference_constraints_list structure 
 //       // with the new answer == convex hull
 // 	  difference_constraints_list = (struct difference_constraints_list*) 
 //                         malloc (sizeof(struct difference_constraints_list));
 // 	  int i;
 // 	  int *new_attr = (int*) malloc (answer->size * sizeof(int));
 // 	  for (i = 0; i < answer->size; i++) new_attr[i] = i;
 // 	  INIT_DIFFERENCE_CONSTRAINTS_LIST(difference_constraints_list,convex_hull,answer->answer,
 //                            answer->not_new_size,answer->size,new_attr);
 // 	  //deleting entailed answers.
 // 	  printf("\nCalling cancelling\n"); fflush(stdout);
 // 	  CANCELLING_ENTAILED_ANSWERS(difference_constraints_list);
 // 	  printf("\nEnding cancelling %p\n",difference_constraints_list->sig); 
 //       fflush(stdout);
 // 
 // 	  struct answer_list *answer_list = 
 // 	    (struct answer_list*) malloc (sizeof(struct answer_list));
 // 	  answer_list->answer = difference_constraints_list;
 // 	  answer_list->sig = NULL;
 //       
 // 	  //updating generator answer list
 // 	  call->last_answer->sig = answer_list;
 // 	  call->last_answer = answer_list;	  
 // 	  printf("\nCalling merging\n"); fflush(stdout);
 // 	}
 //       printf("\nEnding merging\n"); fflush(stdout);
 //     }  

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(reinstall_gen_space_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nreinstall_gen_space START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  struct sf *sf = (struct sf*) IntOfTerm(X(0));
  struct l_gen *l_gen = (struct l_gen*) IntOfTerm(X(1));

  if (sf->isGen) 
    {
#if defined(DEBUG_ALL)
      printf("\nPROY STATE\n");
      print_space(space);
#endif
      CHANGE_SPACE(w,l_gen->orig_space);
#if defined(DEBUG_ALL)
      printf("\nORIG STATE\n");
      print_space(space);
#endif
      int i;
      for (i = 0; i < sf->attr_size; i++)
	{
	  //Update attributtes 
	  MAKE_UNDO_ATTR(w, sf->attrs[i], l_gen->orig_attrs[i]);
	}
    }

#if defined(DEBUG_ALL)
  printf("\nreinstall_gen_space END\n"); fflush(stdout);
#endif

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(consume_attr_answer_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nconsume_attr_answer START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  struct l_ans *answ = (struct l_ans*) IntOfTerm(X(0));
  struct attrs *attrs = (struct attrs*) IntOfTerm(X(1));
  int *attr_values = (int*) checkalloc ((attrs->size+1) * sizeof(int));
  
  int i;
#if defined(DEBUG_ALL)
  printf("\nattrs->size %d %p-%p\n",attrs->size,attrs,answ);
#endif
  
  attr_values[0] = 0;
  for (i = 0; i < attrs->size; i++)
    {
      if (!TagIsCVA(attrs->attrs[i]))
	{
#if defined(DEBUG_ALL)
	  printf("\nCreating aux CVA %lx\n",attrs->attrs[i]);
#endif
	  tagged_t term = attrs->attrs[i];
	  DEREF(term,term);
	  int id = new_diff_var_space(w,space);
	  args[0] = MkIntTerm(id);
	  args[1] = MkIntTerm(term);
	  bu2_attach_attribute(w,term,
			       MkApplTerm(functor_dbm_id,2,args));
	  attr_values[i+1] = id;
#if defined(DEBUG_ALL)
	  printf("\nEND Creating aux CVA\n");
#endif
	}
      else
	{
	  tagged_t attr = fu1_get_attribute(NULL,attrs->attrs[i]);
	  DEREF(attr,ArgOfTerm(1, attr));
	  attr_values[i+1] = IntOfTerm(attr);
	}
#if defined(DEBUG_ALL)
      tagged_t term = fu1_get_attribute(NULL,attrs->attrs[i]);
      DEREF(term,ArgOfTerm(1, term));
      printf("\nATTR ANSWER %d = %li(%d)\n",i,IntOfTerm(term),attr_values[i+1]);
#endif      
    }

#if defined(DEBUG_ALL)
  printf("\nInserting answer in\n");
  print_space(space);
  printf("\nTHE ANSWER %p\n",answ->space); fflush(stdout);
  print_space(answ->space); fflush(stdout);
#endif
  int j;
  for (i = 0; i <= attrs->size; i++)
    {
      for (j = 0; j <= attrs->size; j++)
	{
#if defined(DEBUG_ALL)
	  printf("\nAnswer [%d,%d] = %d\n",attr_values[i],attr_values[j],
		 answ->space->edges[i][j]);
#endif
	  if (!add_diff_const_space(w,space,attr_values[i],attr_values[j],
				    ((struct space*)answ->space)->edges[i][j]))
	    break;
	}
    }

  checkdealloc(attr_values, attrs->size * sizeof(int));

#if defined(DEBUG_ALL)
  printf("\nconsume_attr_answer END\n"); fflush(stdout);
#endif

  int value = ((i-1) == attrs->size) && ((j-1) == attrs->size);
  checkdealloc(attrs->attrs, attrs->size * sizeof(tagged_t));
  checkdealloc(attrs, sizeof(struct attrs));
  return value;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}
