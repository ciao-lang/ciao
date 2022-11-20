/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ciao_prolog.h>

#include <ciao/eng.h>
#include <ciao/internals.h>
#include <ciao/eng_gc.h>
#include <ciao/instrdefs.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/eng_start.h>
#include <ciao/basiccontrol.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_registry.h>
#include <ciao/io_basic.h>


#include <ciao/tabling.h>

#include "engine.h"
#include "debug.h"
#include "tabling_terms.h"
#include "tries.h"
#include "chat_tabling.h"

//#include "call_prolog_functions.h"

/* ----------- */
/* Global Vars */
/* ----------- */

#if defined(TABLING)
//parent tabled call stack
struct gen **ptcp_stk;     
intmach_t iptcp_stk;
tagged_t args[2];
tagged_t tmp_term;
FTYPE_ctype(f_o) dummy_frame_op = LASTCALL; /* TODO: (JFMC) */

//first trie node
TrNode trie_node_top;
TrNode auxiliar_trie;
//last generator
struct gen *last_gen_list;   

node_tr_t *initial_node_tr;             //NodeTR to check nested consumers.

//address for non determinist predicates
try_node_t *address_nd_consume_answer_c;
try_node_t *address_nd_consume_answer_attr_c;
try_node_t *address_nd_resume_cons_c;
try_node_t *address_nd_back_answer_c;

//int total_memory;

tagged_t functor_comma;
tagged_t functor_copy_term;
tagged_t functor_push_ptcp;
tagged_t atom_pop_ptcp;
tagged_t atom_gen_tree_backtracking;


/* -------------------------- */
/*            Code            */
/* -------------------------- */

#include "memo_management.c"
#include "print_st.c"
#include "terms.c"
#include "tries.c"
#include "exec_prolog_functions.c"
#include "chat_tabling_tab.c"


//#include <time.h>
//
//double sch_time;
//double timeval_diff(struct timeval *finishtime, struct timeval *starttime)
//{
//  double msec;
//  msec=(((double)finishtime->tv_sec-starttime->tv_sec)*1000)/CLOCKS_PER_SEC;
//  return msec;
//}

struct gen *get_leader(struct gen *gen)
{
  while (gen != gen->leader) gen = gen->leader;
  return gen;  
}                            

void set_leader(struct gen* gen, struct gen *leader)
{
  while (gen->leader != leader)
    {
      struct gen *aux = gen->leader;
      gen->leader = leader;
      gen = aux;
    }
}                            

intmach_t is_executing(struct gen *gen)
{
  tagged_t term;
  DEREF(term,gen->on_exec);
  return IsVarTerm(term);  
}                            

CFUN__PROTO(get_cons,
            struct cons_list*,
            struct sf *sf, struct gen *gen, struct gen *last_gen) {
  struct cons_list* res;
  ALLOC_TABLING_STK(res,struct cons_list*,sizeof(struct cons_list));
  ALLOC_TABLING_STK(res->cons,struct cons*,sizeof(struct cons));

  res->cons->sf = sf;

#if defined(DEBUG_ALL)
  printf("\nFreezing state\n");
#endif

  INIT_NODE_TR(res->cons->node_tr);
  if (last_gen == MODE_CONSUMER)
    res->type = CONSUMER;
  else
    {
      res->type = GENERATOR;
      last_gen->cons_node_tr->next = res->cons->node_tr;
    }

#if defined(DEBUG_ALL)
  printf("\nConsumer %p con node_tr %p\n",res,res->cons->node_tr);
#endif

  res->cons->frame = Arg->frame;
  res->cons->next_insn = Arg->next_insn; //continuation
  res->cons->last_ans = NULL;
  res->cons->ptcp = PTCP;
  res->cons->gen = gen;
  res->next = NULL;

  freeze_stacks(Arg,LastNodeTR,res->cons->node_tr);
#if defined(DEBUG_ALL)
  printf("\nEnd freezing - get_consumer\n");
#endif

  return res;
}

#if defined(SWAPPING)
CVOID__PROTO(swapping, struct gen *oldGen) {
  //index variables for looping
  choice_t *inode;
  tagged_t *itrail;
  struct gen *igen;
  //pointers for auxiliary memory
  tagged_t *aux_stack;
  int aux_stack_tam;
  //memory segment sizes
  int oldgen_cp_tam, younger_cp_tam, oldgen_tr_tam, younger_tr_tam;
  //frontier pointers (last of choice/trail moved down)
  tagged_t *frontier_tr;
  choice_t *frontier_cp;
  //temporary information
  struct sf *newGenSF;
  
#if defined(DEBUG_SWAPPING)
  printf("\nSWAPPING OPERATION\n");
  printf("\nGENERATOR %p: choice %p trail %p\n",
         oldGen, oldGen->choice, oldGen->choice->trail_top);
  printf("\nANSWER: choice %p trail %p\n", oldGen->answer_cp, oldGen->answer_tr);
#endif
  
  newGenSF = (struct sf*) Arg->choice->x[0];
  //Remove choice_pt of consumer (I will generate a generator cp on stack top
  Arg->choice = PREV_CP(Arg->choice);

  //Computing local_top
  GetFrameTop(Arg->local_top,Arg->choice,G->frame);
  
  //Check if last execution path has to be swapped.
  if (oldGen->answer_cp != NULL) 
    {
#if defined(DEBUG_SWAPPING)
      printf("\nSWAPPING OPERATION\n");
      printf("\nGENERATOR %p: choice %p trail %p\n",
             oldGen, oldGen->choice, oldGen->choice->trail_top);
      printf("\nANSWER: choice %p trail %p\n", 
             oldGen->answer_cp, oldGen->answer_tr);

      for(inode = Arg->choice; !ChoiceYounger(oldGen->choice,inode); 
          inode = PREV_CP(inode))
        {
          printf("\nNode %p=%p trail value %p=%p\n",
                 inode,inode->next_alt,inode->trail_top,
                 (void*)*TrailTopUnmark(inode->trail_top));
        }
      for (itrail = Arg->trail_top; 
           !TrailYounger(oldGen->choice->trail_top,itrail); itrail--)
        printf("\nitrail %p=%x\n",itrail,*TaggedToPointer(itrail));
#endif

      // STEP ZERO: Calculate sizes of auxiliary data structures.
      // Stack and trail grow in different directions
      oldgen_cp_tam = ChoiceCharDifference(oldGen, oldGen->answer_cp);
      younger_cp_tam = ChoiceCharDifference(oldGen->answer_cp,Arg->choice);
      oldgen_tr_tam = TrailCharDifference(oldGen->choice->trail_top,
                                          oldGen->answer_tr);
      younger_tr_tam = TrailCharDifference(oldGen->answer_tr,Arg->trail_top);
          
      // STEP ONE: save memory for auxiliary data structures
      // Behavior of memcpy when overlapping is undetermined.
      // Therefore, we need to move younger section of stack to another
      // auxiliary place in memory before moving it down on the stack

      //Getting the biggest memory section
      if (oldgen_tr_tam > younger_tr_tam) aux_stack_tam = oldgen_tr_tam;
      else aux_stack_tam = younger_tr_tam;
      if (oldgen_cp_tam > aux_stack_tam) aux_stack_tam = oldgen_cp_tam;
      if (younger_cp_tam > aux_stack_tam) aux_stack_tam = younger_cp_tam;

      ALLOC_TABLING_STK(aux_stack, tagged_t*, aux_stack_tam);

      frontier_tr = TrailCharOffset(Arg->trail_top, -oldgen_tr_tam);
      if (oldgen_tr_tam > younger_tr_tam)
        {
          // STEP ONE: Copy trail cells of oldgen execution to
          // auxiliary data structures
          memcpy(aux_stack, oldGen->choice->trail_top, oldgen_tr_tam);
          // STEP TWO: Move younger trail cells down
          memcpy(oldGen->choice->trail_top, oldGen->answer_tr, younger_tr_tam);
          // STEP THREE: Move oldgen trail cells to top of stack
          memcpy(frontier_tr, aux_stack, oldgen_tr_tam);
        }
      else
        {
          memcpy(aux_stack, oldGen->answer_tr, younger_tr_tam);
          memcpy(frontier_tr, oldGen->choice->trail_top, oldgen_tr_tam);
          memcpy(oldGen->choice->trail_top, aux_stack, younger_tr_tam);
        }

      frontier_cp = ChoiceCharOffset(Arg->choice, -oldgen_cp_tam);
      if (oldgen_cp_tam > younger_cp_tam)
        {
          // STEP ONE: Copy choice points of goal execution to
          // auxiliary data structures
          memcpy(aux_stack, oldGen->answer_cp, oldgen_cp_tam);
          // STEP TWO: Move younger choice points down
          memcpy(frontier_cp, Arg->choice, younger_cp_tam);
          // STEP THREE: Move pargoal_node to top of stack
          memcpy(Arg->choice, aux_stack, oldgen_cp_tam);
        }
      else
        {
          memcpy(aux_stack, Arg->choice, younger_cp_tam);
          memcpy(Arg->choice, oldGen->answer_cp, oldgen_cp_tam);
          memcpy(frontier_cp, aux_stack, younger_cp_tam);
        }
      DEALLOC_TABLING_STK(aux_stack);

      // STEP SIX: Update trail pointers of choice points that are
      // moved down the stack
      for (inode = frontier_cp; ChoiceYounger(inode,oldGen->choice);
           inode = PREV_CP(inode))
        inode->trail_top = TrailCharOffset(inode->trail_top, -oldgen_tr_tam);

      // STEP SEVEN: Update trail pointers of choice points that are
      // moved on top of the stack (and nullify fake trails)
      itrail = Arg->trail_top;
      for (inode = Arg->choice; ChoiceYounger(inode, frontier_cp); 
           inode = PREV_CP(inode))
        {
          //nullifying fake trail cells
          if (inode->heap_top != (tagged_t*)(&(HeapFReg)))
            {
              for (; !TrailYounger(inode->trail_top,itrail); itrail--)
                {
                  if (TaggedIsHVA(*TaggedToPointer(itrail)))
                    {
                      if (!HeapYounger(inode->heap_top,*TaggedToPointer(itrail)))
                        NullifyTrailEntry(itrail);
                    }
                  else if (TaggedIsSVA(*TaggedToPointer(itrail)))
                    {
                      if (!StackYounger(inode->local_top,*TaggedToPointer(itrail)))
                        NullifyTrailEntry(itrail);
                    }
                }       
              //Protect current memory if node is not frozen
              inode->heap_top = Arg->heap_top;
              inode->local_top = Arg->local_top;
            }
          //Update trail pointer
          inode->trail_top = TrailCharOffset(inode->trail_top, younger_tr_tam);
        }
          
      // STEP EIGHT: answer_cp and answer_tr pointers of generators
      for (igen = last_gen_list; igen != NULL; igen = igen->prev)
        {
          if (ChoiceYounger(igen->choice,oldGen->choice))
            {
              if (ChoiceYounger(igen->answer_cp,oldGen->answer_cp))
                igen->answer_cp = 
                  ChoiceCharOffset(igen->answer_cp,-oldgen_cp_tam);
              else
                igen->answer_cp = 
                  ChoiceCharOffset(igen->answer_cp, younger_cp_tam);
            }

          if (igen->answer_cp == NULL) continue;

          if (ChoiceYounger(igen->answer_cp,oldGen->choice))
            {
              if (ChoiceYounger(igen->answer_cp,oldGen->answer_cp))
                igen->answer_cp = 
                  ChoiceCharOffset(igen->answer_cp,-oldgen_cp_tam);
              else
                igen->answer_cp = 
                  ChoiceCharOffset(igen->answer_cp, younger_cp_tam);
            }

          if (TrailYounger(igen->answer_tr,oldGen->choice->trail_top))
            {
              if (TrailYounger(igen->answer_tr,oldGen->answer_tr))
                igen->answer_tr = 
                  TrailCharOffset(igen->answer_tr,-oldgen_tr_tam);
              else
                igen->answer_tr = 
                  TrailCharOffset(igen->answer_tr, younger_tr_tam);
            }
        }

      //Reordering generator list executions
      if (oldGen != last_gen_list)
        {
          igen = oldGen->post;
          oldGen->post = NULL;
          while ((igen->ptcp->post == NULL) && (igen->post != NULL))
            {
              igen = igen->post;
              igen->prev->post = NULL;
            }
          //Not need to reorder - just reinstall post fields.
          if (igen->post != NULL) 
            {
              //Moving subchain
              struct gen *aux = igen->prev;
              aux->post = NULL;
              last_gen_list->post = oldGen;
              igen->prev = oldGen->prev;
              oldGen->prev = last_gen_list;
              int last_id = last_gen_list->id + 1;
              last_gen_list = aux;
              igen = aux;
              //Reinstall post fields
              for ( ; aux != oldGen; aux = aux->prev)
                aux->prev->post = aux;
              for ( ; aux != NULL; aux = aux->post)
                aux->id = last_id++;
            }
          for ( ; igen != oldGen; igen = igen->prev)
            {
              igen->leader = igen;
              igen->prev->post = igen;
            }
          igen->leader = igen;
        }
      
#if defined(DEBUG_SWAPPING)
      printf("\nPOST SWAPPING\n");
      for(inode = Arg->choice; !ChoiceYounger(oldGen->choice,inode); 
          inode = PREV_CP(inode))
        {
          printf("\nNode %p=%p trail value %p=%p\n",
                 inode,inode->next_alt,inode->trail_top,
                 (void*)*TrailTopUnmark(inode->trail_top));
        }
      for (itrail = Arg->trail_top; 
           !TrailYounger(oldGen->choice->trail_top,itrail); itrail--)
        printf("\nitrail %p=%p\n",itrail,(void*)*TaggedToPointer(itrail));
#endif
    }

  //Exhausted old generator
  if (oldGen->cons != NULL)
    {
      oldGen->cons->type = CONSUMER;
      oldGen->cons->cons->last_ans = oldGen->last_ans;
    }
  //Old generator still has a choice point (it is not exhausted)
  else
    {
      //do I have to freeze oldGen->newCons - check if previous cp is frozen
      //next cp is back_answer -> arity = 1
      inode = ChoiceNext0(oldGen->choice,1);
      if (inode->heap_top == (tagged_t*)(&(HeapFReg)))
        {
          oldGen->choice->heap_top = (tagged_t*)(&(HeapFReg));
          oldGen->choice->local_top = inode->local_top;
        }
    }

  SetChoice(w->choice);
  //Push new gen!
  push_choicept(Arg, address_nd_resume_cons_c);
  inode = Arg->choice;
  //take arguments from oldGen
  if (oldGen->answer_cp != NULL) 
    {
      inode->x[0] = oldGen->choice->x[0];
      inode->x[1] = oldGen->choice->x[1];
    }
  else
    {
      inode->x[0] = (tagged_t) NULL;
      inode->x[1] = oldGen->choice->x[1];
    }
  //update arguments of oldGen
  if (oldGen->cons == NULL)
    {
      //SF for consuming answers
      oldGen->choice->x[0] = (tagged_t) oldGen->sf;
      //last consumed answer
      oldGen->choice->x[1] = (tagged_t) oldGen->last_ans;
      //generator of the consumer
      oldGen->choice->x[2] = (tagged_t) oldGen;
      //update next_alt of oldGen -> it is now a consumer
      oldGen->choice->next_alt = address_nd_consume_answer_c;
      //TODO - update FReg of first not moved generator with the ones of oldGen
    }
  //New initial FRegs
  oldGen->heap_freg = HeapFReg;
  oldGen->stack_freg = StackFReg;
  //Protect memory (there could be frozen choice_pts)
  HeapFReg = Arg->heap_top;
  StackFReg = Arg->local_top;
  //Update tricky frame
  oldGen->first_frame->frame = inode->frame;
  //Update oldGen->sf
  oldGen->sf = newGenSF;
  oldGen->leader = oldGen;
  oldGen->ptcp = PTCP;
  oldGen->last_node_tr = LastNodeTR;
  oldGen->tabl_stk_top = TABLING_STK_TOP;
  //Update leader using consumers
  struct cons_list *icons;
  struct gen *aux_gen;
  for (igen = last_gen_list; igen != oldGen->prev; igen = igen->prev)
    {
      for (icons = igen->first_cons; icons != NULL; icons = icons->next)
        {
          if ((icons->type == CONSUMER) && is_executing(icons->cons->gen))
            {        
              for  (aux_gen = icons->cons->ptcp;
                    aux_gen->leader->id > icons->cons->gen->id;
                    aux_gen = aux_gen->ptcp)
                aux_gen->leader = icons->cons->gen;
            }       
        }
    }
}
#endif

CBOOL__PROTO(nd_back_answer_c) {
#if defined(TABLING)

  pop_choicept(Arg);
  return FALSE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


CFUN__PROTO(complete, int,
            struct gen *call, struct gen *parentcall) {
  if (call == call->leader) 
    {
      HeapFReg = call->heap_freg;
      StackFReg = call->stack_freg;
      Arg->heap_top = call->heap_top;
      Arg->local_top = call->local_top;
      DEALLOC_TABLING_STK(call->tabl_stk_top);
      LastNodeTR = call->last_node_tr;

#if defined(SWAPPING)
      struct gen *igen = last_gen_list;
      while (igen != NULL)
        {
          if (igen->leader == call)
            {
              igen->state = COMPLETE;
              igen->on_exec = NOEXECUTING;
              if (igen == last_gen_list)
                {
                  last_gen_list = igen->prev;
                  last_gen_list->post = NULL;
                }
            }
          igen = igen->prev;
        } 
      igen = last_gen_list;
      while (igen != NULL)
        {
          if ((igen->prev != NULL) && (igen->prev->state == COMPLETE))
            {
              igen->prev = igen->prev->prev;
              igen->prev->post = igen;
            }
          else igen = igen->prev;
        } 
#else
      while (last_gen_list != call->prev)
        {
          last_gen_list->state = COMPLETE;
          last_gen_list->on_exec = NOEXECUTING;
          last_gen_list = last_gen_list->prev;
        } 
      if (last_gen_list != NULL) last_gen_list->post = NULL;
#endif
    }
  return TRUE;
}
#endif


/* -------------------------- */
/*            API             */     
/* -------------------------- */

CBOOL__PROTO(abolish_all_tables_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nabolish START\n"); fflush(stdout);
#endif

//  printf("\nTOTAL trail time %g\n",trail_time);
//  printf("\nTOTAL sch time %g\n",sch_time);
//  trail_time = 0;
//  sch_time = 0;

  DEALLOC_GLOBAL_TABLE;

//  printf("\nTOTAL MEMORY %g\n",(total_memory-24816)/(double)1024);
//  GetFrameTop(Arg->local_top,Arg->choice,G->frame);
//  printf("\nINIT %d %d %d %d %d\n",
//       HeapCharDifference(Arg->heap_start,Arg->heap_top),
//       StackCharDifference(Arg->stack_start,Arg->local_top),
//       TrailCharDifference(Arg->trail_start,Arg->trail_top),
//       ChoiceCharDifference(Arg->choice_start,Arg->choice),
//       (tabling_stack_free - tabling_stack) * sizeof(tagged_t));
//  total_memory = 0;
  init_tries_module();
  trie_node_top = NULL;
  auxiliar_trie = NULL;


#if defined(ANS_COUNTER)
  ans_no_saved=0;
  ans_removed=0;
  ans_saved=0;
  ans_aggregated=0;
#endif


#if defined(DEBUG_ALL)
  printf("\nabolish END\n"); fflush(stdout);
#endif

  return TRUE;
#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

/* -------------------------- */
/*            tabled_call     */     
/* -------------------------- */
//tabled_call version for non constraint mode
CBOOL__PROTO(tabled_call_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\ntabled_call START\n"); fflush(stdout);
#endif

  //I cannot use stacks here because a consumer can read
  //all its answers (from a complete generator) and this not
  //chronological
  struct sf *sf = (struct sf*) checkalloc(sizeof(struct sf));
  struct gen *callid;
  TrNode node;
  TABLED_CALL(Arg, X(0), node, sf);
  callid = (struct gen*) node->child;

  if (callid == NULL) //it is a generator
    {
      tagged_t on_exec = MkVarTerm(Arg);
      //TODO - replicate for constraints
      struct sf *sf_priv;
#if defined(SWAPPING)
      sf_priv = (struct sf*) checkalloc(sizeof(struct sf));
      //copy_term X(0) -> private_term
      tagged_t private_term = MkVarTerm(Arg);
      args[0] = X(0);
      args[1] = private_term;
      tagged_t copy_term_call = MkApplTerm(functor_copy_term,2,args);
      ciao_frame_re_begin(Arg->misc->goal_desc_ptr);
      ciao_commit_call_term(ciao_refer(copy_term_call));
      ciao_frame_re_end();
      //end copy
      TABLED_CALL(Arg, private_term, node, sf_priv);
#endif

      push_choicept(Arg, address_nd_resume_cons_c);
      //TODO: store this information in subgoal frame!! 
      Arg->choice->x[0] = (tagged_t)NULL;
      Arg->choice->x[1] = (tagged_t)NO_ATTR;

      //TODO - replicate for constraints
#if defined(SWAPPING)
      //tricky frame for swapping
      GetFrameTop(Arg->local_top,Arg->choice,G->frame);
      Arg->local_top->next_insn = &dummy_frame_op;
      Arg->local_top->frame = Arg->frame;
      Arg->frame = Arg->local_top;
      Arg->local_top = (frame_t *)Offset(Arg->local_top,EToY0);
#endif

      INIT_CALLID(Arg, &callid, sf, on_exec, sf_priv);

      //#if defined(DEBUG_ALL)
      if (tabling_trace == atom_on) {
        callid->realcall = X(0);
        printf("Call no constraints id = %ld \t\t ", (long)callid->id);
        PRINT_TERM(Arg, " ", X(0)); }
      //#endif

      node->child = (TrNode) callid;
      PUSH_PTCP(callid);
#if defined(DEBUG_ALL)
      printf("\nPUSH_PTCP %p\n",callid);
#endif
      MAKE_UNDO_PUSH_PTCP(Arg,callid);

      int res;
      EXECUTE_CALL(res,Arg, X(0), callid);
      return res;
    }

  //it is a consumer
  //  if (callid->state != COMPLETE)
  struct gen *leader = get_leader(callid);
  set_leader(callid,leader);
  if (is_executing(leader))
    {
      //new dependency - update leader field
      intmach_t i;
      for (i = iptcp_stk - 1; i > 0; i--)
        {
          if (ptcp_stk[i]->leader->id <= leader->id) break;
          ptcp_stk[i]->leader = leader;
        }
    }                                                                   
  
  struct cons_list* cons;
  CONSUME_ANSWER(Arg, callid, sf, NO_ATTR);
  MAKE_CONSUMER(Arg, cons, callid, sf, MODE_CONSUMER);
#if defined(DEBUG_ALL)
  printf("\nConsumer %p\n", cons);
#endif
  
#if defined(DEBUG_ALL)
  printf("\ntabled_call END\n"); fflush(stdout);
#endif

  return FALSE;
#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}






CBOOL__PROTO(nd_consume_answer_c)
{
#if defined (TABLING)
#if defined(DEBUG_ALL)
  printf("\nnd_consume_answer START\n"); fflush(stdout);
#endif

  //TODO: take this info from data structures
  struct sf *sf = (struct sf*) X(0);
  struct l_ans *l_ans = (struct l_ans*) X(1);
  
  if(l_ans->next == NULL) 
    {
      checkdealloc(sf->vars,sf->size * sizeof(tagged_t));               
      checkdealloc(sf->attrs,sf->attr_size * sizeof(tagged_t)); 
      checkdealloc((tagged_t *)sf,sizeof(struct sf));
      //check for swapping
#if defined(SWAPPING)
      struct gen *callid = (struct gen*) X(2);
      if (callid->state != COMPLETE)
        {
          struct gen *leader = get_leader(callid);
          set_leader(callid,leader);
          if (!is_executing(leader)) swapping(Arg,callid);
          else
            {
              pop_choicept(Arg);
              struct cons_list* cons;
              MAKE_CONSUMER(Arg, cons, callid, sf, CONSUMER);
            }
        }
      else
#endif
        pop_choicept(Arg);
      return FALSE;
    }

  l_ans = l_ans->next;
  //TODO: storing this info in data structures
  Arg->choice->x[1] = (tagged_t)l_ans;

  get_trie_answer(Arg, l_ans->node, sf);

#if defined(DEBUG_ALL)
  printf("\nnd_consume_answer END\n"); fflush(stdout);
#endif

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


/**                                 **/

//tabled_call version for TCLP mode 
// + execute_call + consume_answer
CBOOL__PROTO(lookup_trie_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nlookup_trie START\n"); fflush(stdout);
#endif

  //I cannot use stacks here because a consumer can read
  //all its answers (from a complete generator) and this not
  //chronological
  struct sf *sf = (struct sf*) checkalloc(sizeof(struct sf));
  struct gen *callid;
  TrNode node;
  TABLED_CALL(Arg, X(0), node, sf);

#if defined(DEBUG_ALL)
  printf("\nlookup_trie END\n"); fflush(stdout);
#endif

  return Unify(X(1),MkIntTerm((intmach_t)node)) && 
    Unify(X(2),MkIntTerm((intmach_t)sf));

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


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
  tagged_t list_of_attrs, attributes;
  int i;
  
  // Maybe create this list previously and pass it as argument 
  // (it is also used in loopup_attr_answer_c)

  array_to_list(Arg, sf->attr_size, sf->attrs, &list_of_attrs); //if size == 0 -> list_of_attrs = atom_nil

  exec_call_domain_projection(Arg, list_of_attrs, &attributes); //it should works for list_of_attrs == atom_nil

  // Only mandatory to give the correct values to l_gen...
  if (sf->attr_size == 0)
    {
      if (l_gen == NULL)
        {         
          ALLOC_GLOBAL_TABLE(l_gen, struct l_gen*, sizeof(struct l_gen));
          l_gen->node = NULL;
          l_gen->next = NULL;
          node->child = (TrNode) l_gen;

          exec_current_store(Arg, &l_gen->orig_space);
          exec_call_store_projection(Arg, list_of_attrs, attributes, &l_gen->space);
          l_gen->orig_attrs = save_term(Arg, attributes);
        }
    }
  else // sf->attr_size > 0
    {
      for ( ; l_gen != NULL; l_gen = l_gen->next)
        {
          if (exec_call_entail(Arg, list_of_attrs, attributes, l_gen->orig_attrs, l_gen->space))
            {       
              break;
            }
        }

#if defined(DEBUG_ALL)
      printf("\nLGEN = %p\n",l_gen);
#endif
      if (l_gen == NULL)
        {
          ALLOC_GLOBAL_TABLE(l_gen, struct l_gen*, sizeof(struct l_gen));
          l_gen->node = NULL;
          l_gen->next = (struct l_gen*) node->child;
          node->child = (TrNode) l_gen;

          exec_current_store(Arg, &l_gen->orig_space);
          exec_call_store_projection(Arg, list_of_attrs, attributes, &l_gen->space);
          l_gen->orig_attrs = save_term(Arg, attributes);
        }       
    } // end if (sf->size == 0)

#if defined(DEBUG_ALL)
  printf("\nlookup_attr_call END\n"); fflush(stdout);
#endif

  tagged_t l_prune = (tagged_t)NULL;

  //  PRINT_TERM(Arg, "CallSpace in lookup_trie ", MkIntTerm((int)l_gen));
  
  int check = Unify(X(2),MkIntTerm((intmach_t)(&(l_gen->node)))) &&
    Unify(X(3),MkIntTerm((intmach_t)l_gen)) &&
    Unify(X(4),MkIntTerm((intmach_t)l_prune));
  return check;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


CBOOL__PROTO(execute_call_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nexecute_call START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  //TODO: pruning of LPrune (4th argument)
  struct sf *sf = (struct sf*) IntOfTerm(X(1));
  struct gen **callid = (struct gen**) IntOfTerm(X(2));
  if (*callid == NULL)
    {
      sf->isGen = 1;

      tagged_t on_exec = MkVarTerm(Arg);
      struct sf *sf_priv;

      push_choicept(Arg, address_nd_resume_cons_c);
      //TODO: storing this info in subgoal frame!
      Arg->choice->x[0] = (tagged_t)NULL;
      Arg->choice->x[1] = (tagged_t)ATTR;

#if defined(SWAPPING)
      //tricky frame for swapping
      GetFrameTop(Arg->local_top,Arg->choice,G->frame);
      Arg->local_top->next_insn = &dummy_frame_op;
      Arg->local_top->frame = Arg->frame;
      Arg->frame = Arg->local_top;
      Arg->local_top = (frame_t *)Offset(Arg->local_top,EToY0);
#endif

      INIT_CALLID(Arg, callid, sf, on_exec, sf_priv);

      //#if defined(DEBUG_ALL)
      if (tabling_trace == atom_on) {
        (*callid)->realcall = X(0);
        printf("Call id = %ld \t\t ", (long)(*callid)->id);
        PRINT_TERM(Arg, " ", X(0)); 
      }
      //#endif

      PUSH_PTCP(*callid);
#if defined(DEBUG_ALL)
      printf("\nPUSH_PTCP %p\n",*callid);
#endif
      MAKE_UNDO_PUSH_PTCP(Arg,*callid);

      int res;
      EXECUTE_CALL(res,Arg, X(0), *callid);
#if defined(DEBUG_ALL)
      printf("\nFIN tabled_call %d\n",res);
#endif
      return res;
    }
  else
    sf->isGen = 0;

  //  if ((*callid)->state != COMPLETE)         
  struct gen *leader = get_leader(*callid);
  set_leader(*callid,leader);
  if (is_executing(leader))             
    {                                                                   
      intmach_t i;
      for (i = iptcp_stk - 1; i > 0; i--)
        {
          if (ptcp_stk[i]->leader->id <= leader->id) break;
          ptcp_stk[i]->leader = leader;
        }
    }           
  
#if defined(DEBUG_ALL)
  printf("\nexecute_call END\n"); fflush(stdout);
#endif

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(consume_answer_c) {
#if defined (TABLING)
#if defined(DEBUG_ALL)
  printf("\nconsume_answer START\n"); fflush(stdout);
#endif

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  struct sf *sf = (struct sf*) IntOfTerm(X(0));
  struct gen *gen = *((struct gen**)IntOfTerm(X(1)));
  struct cons_list* cons;

  if (sf->isGen) {LastNodeTR = gen->last_node_tr;}

  CONSUME_ANSWER(Arg, gen, sf, ATTR); // push_choicept(nd_consume_answer_attr_c)

  if (sf->isGen) {MAKE_CONSUMER(Arg, cons, gen, sf, gen);}
  else {MAKE_CONSUMER(Arg, cons, gen, sf, MODE_CONSUMER);}

  //TODO - I do not like this!
  cons->cons->ans_space = X(2);
  
  PRINT_DEREF(Arg, "ans_space = X(2) = ", X(2));
  cons->cons->attr_vars = X(3);
  PRINT_DEREF(Arg, "attr_vars = X(3) = ", X(3));

#if defined(DEBUG_ALL)
  printf("\nconsume_answer END\n"); fflush(stdout);
#endif
  
  return FALSE;

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

  /* printf("reinstall enter size %d Is gen? %d\n", sf->attr_size, sf->isGen); */
  /* PRINT_TERM(Arg, "CallSpace in reisntall_gen_space ", X(1)); */


  if (sf->isGen)
    {
      struct l_gen *l_gen = (struct l_gen*) IntOfTerm(X(1));
      tagged_t list_of_attrs;

      array_to_list(Arg, sf->attr_size, sf->attrs, &list_of_attrs);

      exec_reinstall_store(Arg, list_of_attrs, l_gen->orig_attrs, l_gen->orig_space);
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

  PRINT_DEREF(Arg, "consume_attr_answer ", X(0));
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  //  PRINT_TERM(Arg, "Attrs in consume_attr_answer ", X(1));

  struct attrs *attrs = (struct attrs*) IntOfTerm(X(1));
  int result;

  if (attrs->size > 0) 
    {
      struct l_ans *answ = (struct l_ans*) IntOfTerm(X(0));
    
      if (answ->valid) {
        tagged_t list_of_attrs;
        array_to_list(Arg, attrs->size, attrs->attrs, &list_of_attrs);
        result = exec_apply_answer(Arg, list_of_attrs, answ->ans_attrs, answ->space);
      }
      else  {
        result = FALSE;   // Do not apply NO valid ansers
      }
      
      checkdealloc(attrs->attrs, attrs->size * sizeof(tagged_t));
    }
  else // attrs->size == 0 
    { 
      result = TRUE;
    }

  //  printf("Dealloc attrs %d\n", (int)attrs);
  checkdealloc((tagged_t *)attrs, sizeof(struct attrs));

  return result;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}



CBOOL__PROTO(nd_consume_answer_attr_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nnd_consume_answer_attr START\n"); fflush(stdout);
#endif

  //TODO: take this info from data structures
  struct sf *sf = (struct sf*) X(0);
  struct l_ans *l_ans = (struct l_ans*) X(1);

  if(l_ans->next == NULL) 
    {
      checkdealloc(sf->vars,sf->size * sizeof(tagged_t));
      checkdealloc(sf->attrs,sf->attr_size * sizeof(tagged_t)); 
      checkdealloc((tagged_t *)sf,sizeof(struct sf));   
      pop_choicept(Arg);

      return FALSE;
    }

  l_ans = l_ans->next;
  // there is always an active answer at the end.
  //  while (!l_ans->answer->active) l_ans = l_ans->sig; 
  //TODO: storing this info in data structures
  Arg->choice->x[1] = (tagged_t)l_ans;

  get_trie_answer(Arg, l_ans->node, sf);

  struct attrs *attrs;
  GET_ATTRS_ANSW(X(3),l_ans->space,attrs,X(4));

#if defined(DEBUG_ALL)
  printf("\nnd_consume_answer_attr END\n"); fflush(stdout);
#endif

  return TRUE;
  //  return separation_add_answer_constraint_c
  //(l_ans->answer->space, l_ans->answer->size, 
  //                      sf->attrs, l_ans->answer->attrs, 
  //                      l_ans->answer->not_new_size);

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}




//TODO: separate constraint and standard versions
//TODO: optimize switch between consumers!
CBOOL__PROTO(nd_resume_cons_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nnd_resume_icons START\n"); fflush(stdout);
#endif

//  clock_t ts_ini, ts_fin;
//

  struct cons_list *icons_l_init = (struct cons_list*) X(0);
  struct cons_list *icons_l = icons_l_init;
  intmach_t is_attr = (intmach_t) X(1);

  struct l_ans *l_ans;
  node_tr_t *inode_tr, *inode_tr_cons, *inode_tr_prev;
  struct attrs *attrs;  
  intmach_t i, iTrail;

#if defined(DEBUG_ALL)
  printf("\nCONS %p is attr = %ld\n", icons_l, (long)is_attr); fflush(stdout);
#endif

//  ts_ini = clock();
  //check if nd_resume_cons has already been executed before 
  if (icons_l != NULL) 
    {
      //Link shared trail of children consumers
      if (LastNodeTR != icons_l->cons->node_tr)
        {
          LastNodeTR->next = icons_l->cons->node_tr;
          //it was linked to previous LastNodeTR
          icons_l->cons->node_tr->chain = NULL; 
        }
      CHECK_NEXT_ANSWER;  // goto consume_answer
#if defined(SWAPPING)
      struct gen *leader = get_leader(icons_l->cons->gen);
      set_leader(icons_l->cons->gen,leader);
      if (!is_executing(leader))
        {
          swapping(Arg,icons_l->cons->gen);
          return FALSE;
        }
#endif
      //Untrailing of previous consumer
//      clock_t t_ini, t_fin;
//      t_ini = clock();
#if defined(DEBUG_ALL)
      printf("\nUntrailing\n");
#endif
      UNTRAILING(Arg,inode_tr,icons_l->cons->node_tr);
//      t_fin = clock();
//      trail_time += (((double) (t_fin - t_ini))*1000) / CLOCKS_PER_SEC;

      struct cons_list *prev_gen = icons_l->cons->ptcp->cons;
      //check icons_l - it could be a generator! (better before untrailing)
      CHECK_CONSUMERS(icons_l,NULL);
    }
  else
    {
      LAST_PTCP->cons_node_tr = LastNodeTR;
#if defined(SWAPPING)
      //Updating gen->choice pointers
      struct gen *gen = last_gen_list;
      while (gen != LAST_PTCP)
        gen->choice = Arg->choice;
#endif
    }
  
  struct cons_list *prev_gen = NULL;
  CHECK_CONSUMERS(LAST_PTCP->first_cons, icons_l_init);

#if defined(DEBUG_ALL)
  printf("\nRESUME CALLS FINISHING\n"); fflush(stdout);
#endif
  complete(Arg, LAST_PTCP, LAST_PTCP);
  pop_choicept(Arg);

#if defined(DEBUG_ALL)
    printf("\nCOMPLETE Generator\n");
#endif

  if (!is_attr)
    {
      struct cons_list* cons;
      LastNodeTR = LAST_PTCP->last_node_tr;
      CONSUME_ANSWER(Arg, LAST_PTCP, LAST_PTCP->sf, NO_ATTR);

      //To force freezing with the original LastNodeTR
      MAKE_CONSUMER(Arg, cons, LAST_PTCP, LAST_PTCP->sf, LAST_PTCP);
#if defined(SWAPPING)
      LAST_PTCP->choice = PTCP->choice;
#endif
      LAST_PTCP->cons = cons;

#if defined(DEBUG_ALL)
      printf("\nGENERATOR %p becomes a consumer %p\n",LAST_PTCP,cons);
#endif

#if defined(DEBUG_ALL)
      printf("\nnd_resume_cons END e\n"); fflush(stdout);
#endif
      return FALSE;
    }

#if defined(DEBUG_ALL)
  printf("\nnd_resume_cons END\n"); fflush(stdout);
#endif

  return TRUE;

 consume_answer:
  // while (!l_ans->answer->active) l_ans = l_ans->sig;
  // there is always an active answer at the end.
#if defined(DEBUG_ALL)
  printf("\nConsuming from consumer %p\n", icons_l);
#endif
  icons_l->cons->last_ans = l_ans;
  if (icons_l != icons_l_init)
    {
#if defined(DEBUG_ALL)
      printf("\nForward trail\n");
#endif
      Arg->choice->x[0] = (tagged_t)icons_l;
      //Reinstalling the trail
//      struct timeval t_ini, t_fin;
//      gettimeofday(&t_ini,NULL);
      FORWARD_TRAIL(Arg,icons_l->cons,inode_tr,iTrail,
                    inode_tr_cons,inode_tr_prev);      // XA check here
//      gettimeofday(&t_fin,NULL);
//      trail_time = trail_time + timeval_diff(&t_fin, &t_ini);
    }

  LastNodeTR = icons_l->cons->node_tr;
  //Reinstalling subtitution factor from answer
  get_trie_answer(Arg, l_ans->node, icons_l->cons->sf);
  
  Arg->next_insn = icons_l->cons->next_insn;
  Arg->frame = icons_l->cons->frame;
  
  //TODO - it should be a consumer ATTR, right?
  if (is_attr)
    {
      GET_ATTRS_ANSW(icons_l->cons->ans_space,
                         l_ans->space,
                         attrs,
                         icons_l->cons->attr_vars);
    }

#if defined(DEBUG_ALL)
  printf("\nnd_resume_cons CONSUMING END \n"); fflush(stdout);
#endif
//  ts_fin = clock();
//  sch_time = sch_time + (((double) (ts_fin - ts_ini))*1000) / CLOCKS_PER_SEC;
  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

#if defined(TABLING)
TrNode set_diff_answer_trie(struct attrs *attrs)
{
  struct gen *call = PTCP;
  TrNode node;

#if defined(SWAPPING)
  return put_trie_answer(call->trie_ans, call->sf_priv, attrs);
#else
  return put_trie_answer(call->trie_ans, call->sf, attrs);
#endif
}
#endif



/* -------------------------- */
/*            new_answer      */     
/* -------------------------- */
//new_answer version for non constraint mode
CBOOL__PROTO(new_answer_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nnew_answer START\n"); fflush(stdout);
#endif

//  GetFrameTop(Arg->local_top,Arg->choice,G->frame);
//  int current_memory = HeapCharDifference(Arg->heap_start,Arg->heap_top);
//  current_memory += StackCharDifference(Arg->stack_start,Arg->local_top);
//  current_memory += TrailCharDifference(Arg->trail_start,Arg->trail_top);
//  current_memory += ChoiceCharDifference(Arg->choice_start,Arg->choice);
//  current_memory += (tabling_stack_free - tabling_stack) * sizeof(tagged_t);
//  
//  if (current_memory > total_memory)
//    {
////      printf("\nNew TOTAL %d %d %d %d %d\n",
////         HeapCharDifference(Arg->heap_start,Arg->heap_top),
////         StackCharDifference(Arg->stack_start,Arg->local_top),
////         TrailCharDifference(Arg->trail_start,Arg->trail_top),
////         ChoiceCharDifference(Arg->choice_start,Arg->choice),
////         (tabling_stack_free - tabling_stack) * sizeof(tagged_t));
//      total_memory = current_memory;
//    }

  TrNode node = set_diff_answer_trie(NULL);

  if (node->child == NULL)
    { 
      //#if defined(DEBUG_ALL)
      if (tabling_trace == atom_on) {
        printf("New no constraints answer id = %ld \t ", (long)PTCP->id);
        PRINT_TERM(Arg, " ", PTCP->realcall); }
      //#endif

      struct l_ans *answ;
      ALLOC_GLOBAL_TABLE(answ, struct l_ans*, sizeof(struct l_ans));
      answ->node = node;
      answ->space = (TrNode)NULL;
      answ->ans_attrs = (TrNode)NULL;
      answ->valid = TRUE;
      answ->next = NULL;

      if (PTCP->first_ans == NULL) PTCP->first_ans = answ;      
      if (PTCP->last_ans != NULL) PTCP->last_ans->next = answ;
      PTCP->last_ans = answ;
      node->child = (TrNode)1;

      //TODO - replicate for constraints
#if defined(SWAPPING)
      Unify(PTCP->on_exec,NOEXECUTING);
      //MAKE_UNDO has to be before answer_tr updating
      MAKE_UNDO_GEN_TREE_BACKTRACKING(Arg); 
      //insert UNDO POP_PTCP (undo before POP -> PTCP is the right value)
      MAKE_UNDO_POP_PTCP(Arg,PTCP);
      PTCP->answer_cp = Arg->choice;
      PTCP->answer_tr = Arg->trail_top;
      PTCP->answer_node_tr = LastNodeTR;
      push_choicept(Arg, address_nd_back_answer_c);
      Arg->choice->x[0] = (tagged_t)PTCP;
      //TODO - Consume current answer
      get_trie_answer(Arg, node, PTCP->sf);
      POP_PTCP;
#if defined(DEBUG_ALL)
  printf("\nNEW new_answer END\n"); fflush(stdout);
#endif
      return TRUE;
#endif
    }

#if defined(DEBUG_ALL)
  printf("\nnew_answer END\n"); fflush(stdout);
#endif

  return FALSE;  

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


//new_answer version for TCLP mode
CBOOL__PROTO(new_answer_attr_c)
{
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nnew_answer_attr START\n"); fflush(stdout);
  printf("\nlookup_answer START\n"); fflush(stdout);
#endif

  struct attrs *attrs;
  ALLOC_TABLING_STK(attrs, struct attrs*, sizeof(struct attrs));
//  attrs = (struct attrs*) checkalloc(sizeof(struct attrs));
  TrNode node = set_diff_answer_trie(attrs);

#if defined(DEBUG_ALL)
  printf("\nlookup_answer END\n"); fflush(stdout);
#endif


#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer START\n"); fflush(stdout);
#endif

  struct l_ans *l_ans = (struct l_ans*) node->child;

  tagged_t list_of_attrs, attributes, newattributes;
  int i, comparation;

  if (attrs->size ==  0)
    {
      if (l_ans != NULL)
        {
#if defined(DEBUG_ALL)
          printf("\nlookup_attr_answer END I\n"); fflush(stdout);
#endif
          //Tabling_stk is used as tmp memory (this is safe)
          DEALLOC_TABLING_STK(attrs);
          return FALSE;
        }
    }
  else // attrs->size > 0
    {
      array_to_list(Arg, attrs->size, attrs->attrs, &list_of_attrs);

      exec_answer_domain_projection(Arg, list_of_attrs, &attributes);

      //    look_more_general:
      for ( ; l_ans != NULL; l_ans = l_ans->next)
        {
          if (l_ans->valid) {
            comparation = exec_answer_check_entail(Arg, list_of_attrs, attributes, l_ans->ans_attrs, l_ans->space, &newattributes);
            if (comparation == 1) {
#if defined(ANS_COUNTER)
              ans_no_saved++;
#endif
              break;
            }
            else if (comparation == -1) { 
#if defined(ANS_COUNTER)
              ans_removed++;
#endif
              l_ans->valid = FALSE;
            }
            else if (comparation == 2) {
#if defined(ANS_COUNTER)
              ans_aggregated++;
#endif
              attributes = newattributes;
              l_ans->valid = FALSE;
            }
          } // check only the valid answers              
        }

      if (l_ans != NULL)
        {
          //Tabling_stk is used as tmp memory (this is safe)
          DEALLOC_TABLING_STK(attrs);
#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer END II\n"); fflush(stdout);
#endif
          return FALSE;
        }
    } // end (attrs->size > 0)

  ALLOC_GLOBAL_TABLE(l_ans, struct l_ans*, sizeof(struct l_ans));

#if defined(ANS_COUNTER)
  ans_saved++;
  if (ans_saved  > 1140000)
    print_counters_c(Arg);
#endif

  if (attrs->size == 0)
    {
      /* l_ans->space = (TrNode)NULL; */
      /* l_ans->ans_attrs = (TrNode)NULL; */
    }
  else
    {
      exec_answer_store_projection(Arg, list_of_attrs, attributes, &l_ans->space);
      
      l_ans->ans_attrs = save_term(Arg, attributes);
    }

  l_ans->next = (struct l_ans*) node->child;
  l_ans->valid = TRUE;
  node->child = (TrNode)l_ans;

  //Tabling_stk is used as tmp memory (this is safe)
  DEALLOC_TABLING_STK(attrs);

#if defined(DEBUG_ALL)
  printf("\nlookup_attr_answer END III\n"); fflush(stdout);
#endif

  tagged_t l_prune = (tagged_t)NULL;

#if defined(DEBUG_ALL)
  printf("l_ans = %p\n", (void*)l_ans->space);
#endif

#if defined(DEBUG_ALL)
  printf("\nnew_attr_answer START\n"); fflush(stdout);
#endif


  //#if defined(DEBUG_ALL)
  if (tabling_trace == atom_on) {
    printf("New answer id = %ld \t ", (long)PTCP->id);
    PRINT_TERM(Arg, " ", PTCP->realcall); }
  //#endif

  struct l_ans *answ;
  ALLOC_GLOBAL_TABLE(answ, struct l_ans*, sizeof(struct l_ans));
  answ->node = node;                   //(TrNode) IntOfTerm(X(0));
  answ->space = (TrNode)l_ans;       // IntOfTerm(X(1));
  answ->next = NULL;

  struct gen *call = PTCP;
  if (call->first_ans == NULL) call->first_ans = answ;      
  if (call->last_ans != NULL) call->last_ans->next = answ;
  call->last_ans = answ;



#if defined(DEBUG_ALL)
  printf("\nnew_attr_answer END\n"); fflush(stdout);
#endif

  return FALSE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}


CBOOL__PROTO(push_ptcp_c)
{
#if defined (TABLING)

  DEREF(X(0),X(0));
  struct gen *gen = (struct gen*) IntOfTerm(X(0));
  PUSH_PTCP(gen);
#if defined(DEBUG_ALL)
  printf("\nPUSH_PTCP %p\n",gen);
#endif
  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(pop_ptcp_c) {
#if defined (TABLING)

#if defined(DEBUG_ALL)
  printf("\nPOP_PTCP %p\n",PTCP);
#endif
  POP_PTCP;
  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(gen_tree_backtracking_c) {
#if defined (TABLING)

#if defined(DEBUG_ALL)
  printf("\nGEN_TREE_BACTRACKING %p\n",PTCP);
#endif

  PTCP->answer_cp = NULL;
  PTCP->answer_tr = NULL;

  if (LastNodeTR != PTCP->answer_node_tr)
    {
      PTCP->heap_freg = HeapFReg;
      PTCP->stack_freg = StackFReg;
      PTCP->tabl_stk_top = TABLING_STK_TOP;
      LastNodeTR = PTCP->answer_node_tr;
    }

  PTCP->answer_node_tr = NULL; 

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}

CBOOL__PROTO(initial_tabling_c) {
#if defined(TABLING)
#if defined(DEBUG_ALL)
  printf("\nInitial Tabling\n"); fflush(stdout);
#endif

  functor_comma = SetArity(GET_ATOM(","), 2);
  functor_copy_term = SetArity(GET_ATOM("term_basic:copy_term"), 2);
  functor_forward_trail =
    SetArity(GET_ATOM("forward_trail:$forward_trail"), 2);
  functor_push_ptcp =
    SetArity(GET_ATOM("tabling_rt:$push_ptcp"), 1);
  atom_pop_ptcp = GET_ATOM("tabling_rt:$pop_ptcp");
  atom_gen_tree_backtracking = GET_ATOM("tabling_rt:$gen_tree_backtracking");

  INIT_GLOBAL_TABLE;
  INIT_TABLING_STACK;

//  trail_time = 0;
//  sch_time = 0;
  iptcp_stk = 0;
  ptcp_stk = (struct gen**) checkalloc (PTCP_STKSIZE * sizeof(struct gen*)); 
  PUSH_PTCP(NULL);

//  total_memory = 0;

  trie_node_top = NULL;
  auxiliar_trie = NULL;
  last_gen_list = NULL;
  init_tries_module();
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  address_nd_consume_answer_attr_c = def_retry_c(nd_consume_answer_attr_c,5);
  //arity 3 for compatibility with consume answer
  address_nd_resume_cons_c = def_retry_c(nd_resume_cons_c,3); 
  address_nd_back_answer_c = def_retry_c(nd_back_answer_c,1);

  //Initialize LastNodeTR
  INIT_NODE_TR(initial_node_tr);
  INIT_NODE_TR(LastNodeTR);

  if (Heap_End != HeapCharOffset(Heap_Start, TABLING_GLOBALSTKSIZE*sizeof(tagged_t)))
    {
      intmach_t size = (TABLING_GLOBALSTKSIZE*sizeof(tagged_t) -
                  HeapCharDifference(Heap_Start, w->heap_top))/2;
      heap_overflow(Arg,2*size); // TODO:[oc-merge] pad was multiplied inside heap_overflow
    }

  if (Stack_End != StackOffset(Stack_Start, TABLING_LOCALSTKSIZE))
    {
      tagged_t *new_Stack_Start;
      intmach_t reloc_factor;

      new_Stack_Start = checkrealloc
        (Stack_Start, StackDifference(Stack_Start,Stack_End)*sizeof(tagged_t),
         TABLING_LOCALSTKSIZE*sizeof(tagged_t));

      reloc_factor = (char *)new_Stack_Start - (char *)Stack_Start;
      stack_overflow_adjust_wam(w, reloc_factor);

      /* Final adjustments */
      Stack_Start = new_Stack_Start;            /* new bounds */
      Stack_End = StackOffset(new_Stack_Start,TABLING_LOCALSTKSIZE);
    }

  if (Trail_End != TrailOffset(Trail_Start, TABLING_CHOICESTKSIZE +
                               TABLING_TRAILSTKSIZE))
    {
      tagged_t *choice_top = ChoiceTopFromChoice(w->choice);
      intmach_t size = (TABLING_CHOICESTKSIZE + TABLING_TRAILSTKSIZE -
                  ChoiceDifference(Choice_Start, choice_top) -
                  TrailDifference(Trail_Start, w->trail_top)) / 2;
      CVOID__CALL(choice_overflow,2*size*sizeof(tagged_t),TRUE);
    }  


#if defined(ANS_COUNTER)
  ans_no_saved=0;
  ans_removed=0;
  ans_saved=0;
  ans_aggregated=0;
#endif


  /* printf("Heap (%p-%p) stack (%p-%p)\n\n", */
  /*     Heap_Start, Heap_End, */
  /*     Stack_Start, Stack_End); */
  /* printf("GLOBAL_TABLE (%p-%p)\n\n", */
  /*     global_table, global_table_end); */

  return TRUE;

#else
  printf("\nTABLING Flag must be activated\n");
  return FALSE;
#endif
}
