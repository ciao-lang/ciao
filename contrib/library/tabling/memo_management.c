#if defined(TABLING)
CVOID__PROTO(freeze_stacks, node_tr_t *orig_node_tr, node_tr_t *last_node_tr) {
  //Updating new values
  HeapFReg = Arg->global_top;
  if (StackYounger(NodeLocalTop(Arg->node), 
		   StackCharOffset(Arg->frame,FrameSize(Arg->next_insn)))) 
    {
      if (!StackYounger(StackFReg,NodeLocalTop(Arg->node))) 
	StackFReg = NodeLocalTop(Arg->node); 
    }
  else 
    {
      if (!StackYounger(StackFReg,
			StackCharOffset(Arg->frame,FrameSize(Arg->next_insn))))
	StackFReg = StackCharOffset(Arg->frame,FrameSize(Arg->next_insn));
    }

  //Updating pointers for generator.
  if (PTCP->node->global_top != (tagged_t*)&(HeapFReg))
    {
      PTCP->node->global_top = (tagged_t*)&(HeapFReg); 
      PTCP->node->local_top = (frame_t*)orig_node_tr;
    }
  if (PTCP->node->local_top == NULL) 
      PTCP->node->local_top = (frame_t*)orig_node_tr;

  //Updating pointers from not frozen choice points.
  node_t *ind;
  tagged_t *itrail = Arg->trail_top;
  for (ind = Arg->node; 
       ind->global_top != (tagged_t *)&(HeapFReg);
       ind = ChoiceCharOffset(ind,-ind->next_alt->node_offset))
    {
      for (; !TrailYounger(ind->trail_top,itrail); itrail--)
	{
	  if (TagIsHVA(*TagToPointer(itrail)))
	    {
	      if (!HeapYounger(ind->global_top,*TagToPointer(itrail)))
		NullifyTrailEntry(itrail);
	    }
	  else if (TagIsSVA(*TagToPointer(itrail)))
	    {
	      if (!StackYounger(ind->local_top,*TagToPointer(itrail)))
		NullifyTrailEntry(itrail);
	    }
	}	
      ind->global_top = (tagged_t *)&(HeapFReg);
      ind->local_top = (frame_t *)orig_node_tr;
      
#if defined(SWAPPING)
      //is this a back_answer_cp?
      if (ind->next_alt == address_nd_back_answer_c)
	{
	  printf("\nQue carajo\n");
	  //is the generator in its original place? or was it swapped?
	  if (((struct gen*)ind->term[0])->answer_cp == PREV_CP(ind))
	    {
	      ((struct gen*)ind->term[0])->last_node_tr = orig_node_tr;
	      ind = ((struct gen*)ind->term[0])->node;
	    }
	}
#endif
    }

  //Frozen choice_point which has been previously backtraked.
  if (ind->local_top == NULL)
    ind->local_top = (frame_t *)orig_node_tr;

//  printf("\nFrozen Mark LastNodeTR %p\n",orig_node_tr); fflush(stdout);

  //Updating node_tr list
  // printf("\nLinking %p with %p\n",LastNodeTR,last_node_tr);
  // fflush(stdout);
  LastNodeTR->chain = last_node_tr;
  LastNodeTR = last_node_tr;  
  // printf("\nNew LASTNodeTR %p\n",LastNodeTR); fflush(stdout);
}

#endif
