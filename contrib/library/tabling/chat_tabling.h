#ifndef _CIAO_CHAT_TABLING_H
#define _CIAO_CHAT_TABLING_H

#if defined(TABLING)
/* --------------------------- */
/*           Defines           */
/* --------------------------- */

tagged_t functor_comma;
tagged_t functor_copy_term;
tagged_t functor_push_ptcp;
tagged_t atom_pop_ptcp;
tagged_t atom_gen_tree_backtracking;

#define	TRUE		1
#define	FALSE		0
#define	ATTR		1
#define	NO_ATTR  	0
#define READY           0
#define EVALUATING      1
#define COMPLETE        2
#define MEMSIZE         512*2*2*2*4*4*4


#define MODE_CONSUMER   NULL

#define TABLING_GLOBALSTKSIZE  (4800*kCells-1) 
#define TABLING_LOCALSTKSIZE   (3000*kCells-1)
#define TABLING_CHOICESTKSIZE  (3000*kCells-1)
#define TABLING_TRAILSTKSIZE   (3000*kCells-1)
#define PTCP_STKSIZE          2048

/* --------------------------- */
/*           Macros            */
/* --------------------------- */

#define EMPTY_PTCP (iptcp_stk == 1)
#define PTCP (ptcp_stk[iptcp_stk - 1])
#define LAST_PTCP (ptcp_stk[iptcp_stk])
#define PUSH_PTCP(GEN) (ptcp_stk[iptcp_stk++] = GEN)
#define POP_PTCP (iptcp_stk--)
#define PREV_CP(NODE) ChoiceCharOffset(NODE,-(NODE)->next_alt->node_offset)

#define SETMIN(x, y) if (y < x) x = y;

#define TABLED_CALL(ARG, CALL, NODE, SF)				\
  {									\
    if (trie_node_top == NULL) trie_node_top = open_trie();		\
    NODE = put_trie_entry(trie_node_top, (CALL), (SF));			\
  }

#define INIT_CALLID(ARG, CALLID, SF, ON_EXEC, SF_PRIV)			\
  {									\
    ALLOC_GLOBAL_TABLE(*(CALLID), struct gen*, sizeof(struct gen));	\
    (*(CALLID))->ptcp = PTCP;						\
    (*(CALLID))->on_exec = (ON_EXEC);					\
    (*(CALLID))->sf = (SF);						\
    (*(CALLID))->sf_priv = (SF_PRIV);					\
    (*(CALLID))->trie_ans = open_trie();				\
    (*(CALLID))->first_ans = NULL;					\
    (*(CALLID))->last_ans = NULL;					\
    (*(CALLID))->first_cons = NULL;					\
    (*(CALLID))->last_cons = NULL;					\
    ComputeA(Arg->local_top,Arg->node);					\
    (*(CALLID))->local_top = Arg->local_top;				\
    (*(CALLID))->global_top = Arg->global_top;				\
    (*(CALLID))->stack_freg = StackFReg;				\
    (*(CALLID))->heap_freg = HeapFReg;					\
    (*(CALLID))->tabl_stk_top = TABLING_STK_TOP;			\
    (*(CALLID))->last_node_tr = LastNodeTR;				\
    (*(CALLID))->node = Arg->node;					\
    (*(CALLID))->cons = NULL;						\
    (*(CALLID))->answer_cp = NULL;					\
    (*(CALLID))->answer_tr = NULL;					\
									\
    if (last_gen_list == NULL) (*(CALLID))->id = 0;			\
    else (*(CALLID))->id = ((struct gen *)last_gen_list)->id + 1;	\
									\
    (*(CALLID))->leader = *(CALLID);					\
    (*(CALLID))->prev = last_gen_list;					\
    (*(CALLID))->post = NULL;						\
    if (last_gen_list != NULL) last_gen_list->post = (*(CALLID));	\
    last_gen_list = (*(CALLID));					\
    (*(CALLID))->state = READY;						\
  }

#define EXECUTE_CALL(BOOL, ARG, CALL, CALLID)				\
  {									\
    (CALLID)->state = EVALUATING;					\
    									\
    init_state_from_WAM((ARG)->misc->goal_desc_ptr);			\
    (BOOL) = ciao_commit_call_term(ciao_refer(CALL));			\
    re_ciao_frame_end();						\
  }

//TODO: take if (TYPE) out by defining two different CONSUME_ANSWER macros
#define CONSUME_ANSWER(ARG, CALLID, SF, TYPE)				\
  {									\
    struct gen *leader = get_leader(CALLID);				\
    set_leader((CALLID),leader);					\
    if (!is_executing(leader))						\
      {									\
	if ((CALLID)->first_ans != NULL)				\
	  {								\
	    /* there is always an active answer at the end */		\
	    /* - I should release memory here. */			\
	    /* Next line should be manage with constraints */		\
	    /* while (!(CALLID)->first_answer->answer->active)*/	\
	    /* (CALLID)->first_answer = (CALLID)->first_answer->sig; */	\
	    if (TYPE)							\
	      {								\
		push_choicept((ARG),address_nd_consume_answer_attr_c);	\
		(ARG)->node->term[3] = ARG3;				\
		(ARG)->node->term[4] = ARG4;				\
	      }								\
	    else							\
	      push_choicept((ARG),address_nd_consume_answer_c);		\
	    (ARG)->node->term[0] = (tagged_t)(SF);			\
	    (ARG)->node->term[1] = (tagged_t)(CALLID)->first_ans;	\
	    (ARG)->node->term[2] = (tagged_t)(CALLID);			\
	    /*(ARG)->node->term[2] = (tagged_t)clone_space(space);*/	\
	    /*struct subs_factor *answer = get_trie_answer((ARG),*/	\
	    /*(CALLID)->first_answer->answer,(SF)->attr_vars); */	\
	    get_trie_answer((ARG),(CALLID)->first_ans->node, (SF));	\
									\
	    if (TYPE)							\
	      {								\
		struct attrs *attrs;					\
		GET_ATTRS_ANSW(ARG3,(CALLID)->first_ans->space,attrs,ARG4); \
	      }								\
	    return TRUE;						\
	  }								\
	checkdealloc((SF)->vars,(SF)->size * sizeof(tagged_t));		\
	checkdealloc((SF)->attrs,(SF)->attr_size * sizeof(tagged_t));	\
	checkdealloc((SF),sizeof(struct sf));				\
	return FALSE;							\
      }									\
  }

#define INSERT_CONSUMER(GEN,CONS)					\
  {									\
    if ((GEN)->last_cons == NULL)					\
      {									\
	(GEN)->first_cons = (CONS);					\
	(GEN)->last_cons = (CONS);					\
      }									\
    else								\
      {									\
	(GEN)->last_cons->next = (CONS);				\
	(GEN)->last_cons = (CONS);					\
      }									\
  }


#define MAKE_CONSUMER(ARG, CONS, CALLID, SF, MODE)			\
  {									\
    (CONS) = get_cons((ARG), (SF), (CALLID), (MODE));			\
    INSERT_CONSUMER((PTCP),(CONS));					\
  }

#define SHOW_TRAIL(NodeTR)						\
  {									\
    printf("\nShowing trail\n");					\
    for ( ;								\
	  (NodeTR) != NULL;						\
	  (NodeTR) = (NodeTR)->next)					\
      {									\
	for ((iTrail) = 0;						\
	     (iTrail) < (NodeTR)->size;					\
	     (iTrail) = (iTrail) + 2)					\
	  {								\
	    printf("\nVar %lx Value %lx\n",				\
		   (NodeTR)->trail_sg[(iTrail)],			\
		   (NodeTR)->trail_sg[(iTrail+1)]);			\
	  }								\
      }									\
  }

#define SHOW_VALUE(NodeTR)						\
  {									\
    printf("\nShowing value\n");					\
    for ( ;								\
	  (NodeTR) != NULL;						\
	  (NodeTR) = (NodeTR)->next)					\
      {									\
	for ((iTrail) = 0;						\
	     (iTrail) < (NodeTR)->size;					\
	     (iTrail) = (iTrail) + 2)					\
	  {								\
	    printf("\nState %lx\n",					\
		   CTagToPointer((NodeTR)->trail_sg[(iTrail)]));	\
	  }								\
      }									\
  }

#define SHOW_CHAIN(NodeTR)						\
  {									\
    printf("\nShowing chain: ");					\
    for ( ;								\
	  (NodeTR) != NULL;						\
	  (NodeTR) = (NodeTR)->next)					\
      {									\
	printf("%p ",(NodeTR));						\
      }									\
  }

#define REVERSE_NODE_TR(NodeTR,Ini,Aux,Prev)				\
  {									\
    (Prev) = NULL;							\
    for ((NodeTR) = (Ini);						\
	 (NodeTR) != NULL;						\
	 (NodeTR) = (Aux))						\
      {									\
	(Aux) = (NodeTR)->next;						\
	(NodeTR)->next = (Prev);					\
	(Prev) = (NodeTR);						\
      }									\
    (NodeTR) = (Prev);							\
  }									\

#define FORWARD_TRAIL(ARG,C,NodeTR,iTrail,Aux,Prev)			\
  {									\
    REVERSE_NODE_TR((NodeTR),(C)->node_tr,(Aux),(Prev));		\
    (Aux) = (NodeTR);							\
    /* NodeTr is initialized by REVERSE_NODE_TR */			\
    for ( ;								\
	  (NodeTR) != NULL;						\
	  (NodeTR) = (NodeTR)->next)					\
      {									\
	for ((iTrail) = (NodeTR)->size - 2;				\
	     (iTrail) >= 0 ;						\
	     (iTrail) = (iTrail) - 2)					\
	  {								\
	    if (IsVar((NodeTR)->trail_sg[(iTrail)]))			\
	      {								\
		CTagToPointer((NodeTR)->trail_sg[(iTrail)]) =		\
		  (NodeTR)->trail_sg[(iTrail) + 1];			\
	      }								\
	    else							\
	      {								\
		/*Execute forward trail*/				\
		init_state_from_WAM((ARG)->misc->goal_desc_ptr);	\
		tagged_t term = ArgOfTerm(1, (NodeTR)->trail_sg[(iTrail)]); \
		ciao_commit_call_term(ciao_refer(term));		\
		re_ciao_frame_end();					\
	      }								\
	  }								\
      }									\
    REVERSE_NODE_TR((NodeTR),(Aux),(Aux),(Prev));			\
  }

#define UNTRAILING(ARG,NodeTR,Ini)					\
  {									\
    for ((NodeTR)=(Ini); (NodeTR) != NULL; (NodeTR)=(NodeTR)->next)	\
      {									\
	for ((iTrail) = 0;						\
	     (iTrail) < (NodeTR)->size;					\
	     (iTrail) = (iTrail) + 2)					\
	  {								\
	    if (IsVar((NodeTR)->trail_sg[(iTrail)]))			\
	      {								\
		CTagToPointer((NodeTR)->trail_sg[(iTrail)]) =		\
		  (NodeTR)->trail_sg[(iTrail)];				\
	      }								\
	    else							\
	      {								\
		/*Execute forward trail*/				\
		init_state_from_WAM((ARG)->misc->goal_desc_ptr);	\
		tagged_t term = ArgOfTerm(2, (NodeTR)->trail_sg[(iTrail)]); \
		ciao_commit_call_term(ciao_refer(term));		\
		re_ciao_frame_end();					\
	      }								\
	  }								\
      }									\
  }

#define CHECK_NEXT_ANSWER						\
  {									\
    l_ans = icons_l->cons->last_ans;					\
									\
    if (l_ans != icons_l->cons->gen->last_ans)				\
      {									\
	l_ans = l_ans->next;						\
	/* there is always an active answer at the end.*/		\
	/* while (!l_ans->answer->active) l_ans = l_ans->sig;*/		\
	goto consume_answer;						\
      }									\
  }

#define CHECK_FIRST_ANSWER						\
  {									\
    l_ans = icons_l->cons->last_ans;					\
									\
    if (l_ans != icons_l->cons->gen->last_ans)				\
      {									\
	if (l_ans == NULL) l_ans = icons_l->cons->gen->first_ans;	\
	else l_ans = l_ans->next;					\
	goto consume_answer;						\
      }									\
  }

//Before calling CHECK_CONSUMERS, prev_GEN has to be updated!
#if defined(SWAPPING)
#define CHECK_CONSUMERS(INIT,END)					\
  {									\
    for (icons_l = (INIT); ; )						\
      {									\
	if ((icons_l == (END)) || (icons_l == NULL))			\
	  {								\
	    if ((END) != NULL) break;					\
	    else							\
	      {								\
		if (prev_gen == NULL) break;				\
		icons_l = prev_gen->next;				\
		prev_gen = prev_gen->cons->ptcp->cons;			\
		continue;						\
	      }								\
	  }								\
	if (icons_l->type == GENERATOR)					\
	  {								\
	    prev_gen = icons_l;						\
	    icons_l = prev_gen->cons->gen->first_cons;			\
	    continue;							\
	  }								\
	CHECK_FIRST_ANSWER;						\
	icons_l = icons_l->next;					\
      }									\
  }
#else
#define CHECK_CONSUMERS(INIT,END)					\
  {									\
    for (icons_l = (INIT); ; )						\
      {									\
	if ((icons_l == (END)) || (icons_l == NULL))			\
	  {								\
	    if ((END) != NULL) break;					\
	    else							\
	      {								\
		if (prev_gen == NULL) break;				\
		icons_l = prev_gen->next;				\
		prev_gen = prev_gen->cons->ptcp->cons;			\
		continue;						\
	      }								\
	  }								\
	CHECK_FIRST_ANSWER;						\
	if (icons_l->type == GENERATOR)					\
	  {								\
	    prev_gen = icons_l;						\
	    icons_l = prev_gen->cons->gen->first_cons;			\
	    continue;							\
	  }								\
	icons_l = icons_l->next;					\
      }									\
  }
#endif

#define GET_ATTRS_ANSW(SPACE,ANSW_SPACE,ATTRS,ATTR_VARS)		\
  {									\
    Unify((SPACE),MkIntTerm((int)(ANSW_SPACE)));			\
    (ATTRS) = (struct attrs*) checkalloc (sizeof(struct attrs));	\
    (ATTRS)->size = (stack_attrs - stack_attrs_base);			\
    (ATTRS)->attrs = (tagged_t*) checkalloc				\
      ((ATTRS)->size * sizeof(tagged_t));				\
    int i;								\
    for (i = 0; i < (ATTRS)->size; i++)					\
      {									\
	(ATTRS)->attrs[i] = stack_attrs_base[i];			\
      }									\
    Unify((ATTR_VARS),MkIntTerm((int)(ATTRS)));				\
  }

#define MAKE_UNDO_PUSH_PTCP(ARG,GEN)					\
  {									\
    args[0] = MkIntTerm((tagged_t)GEN);					\
    tmp_term = MkApplTerm(functor_push_ptcp,1,args);			\
    args[1] = atom_pop_ptcp;						\
    args[0] = tmp_term;							\
    tmp_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPush((ARG)->trail_top,tmp_term);				\
  }

#define MAKE_UNDO_POP_PTCP(ARG,GEN)					\
  {									\
    args[0] = MkIntTerm((tagged_t)GEN);					\
    args[1] = MkApplTerm(functor_push_ptcp,1,args);			\
    args[0] = atom_pop_ptcp;						\
    tmp_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPush((ARG)->trail_top,tmp_term);				\
  }

#define MAKE_UNDO_GEN_TREE_BACKTRACKING(ARG)				\
  {									\
    TrailPush((ARG)->trail_top,atom_gen_tree_backtracking);		\
  }

#endif

#endif /* _CIAO_CHAT_TABLING_H */
