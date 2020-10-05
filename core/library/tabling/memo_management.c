#if defined(TABLING)
CVOID__PROTO(freeze_stacks, node_tr_t *orig_node_tr, node_tr_t *last_node_tr) {
  //Updating new values
  HeapFReg = Arg->heap_top;
  if (StackYounger(NodeLocalTop(Arg->choice), 
                   StackCharOffset(Arg->frame,FrameSize(Arg->next_insn)))) 
    {
      if (!StackYounger(StackFReg,NodeLocalTop(Arg->choice))) 
        StackFReg = NodeLocalTop(Arg->choice); 
    }
  else 
    {
      if (!StackYounger(StackFReg,
                        StackCharOffset(Arg->frame,FrameSize(Arg->next_insn))))
        StackFReg = StackCharOffset(Arg->frame,FrameSize(Arg->next_insn));
    }

  //Updating pointers for generator.
  if (PTCP->choice->heap_top != (tagged_t*)&(HeapFReg))
    {
      PTCP->choice->heap_top = (tagged_t*)&(HeapFReg); 
      PTCP->choice->local_top = (frame_t*)orig_node_tr;
    }
  if (PTCP->choice->local_top == NULL) 
      PTCP->choice->local_top = (frame_t*)orig_node_tr;

  //Updating pointers from not frozen choice points.
  choice_t *ind;
  tagged_t *itrail = Arg->trail_top;
  for (ind = Arg->choice; 
       ind->heap_top != (tagged_t *)&(HeapFReg);
       ind = ChoiceCont(ind))
    {
      for (; !TrailYounger(ind->trail_top,itrail); itrail--)
        {
          if (TaggedIsHVA(*TagToPointer(itrail)))
            {
              if (!HeapYounger(ind->heap_top,*TagToPointer(itrail))) {
                //              printf("\nNullifyTrailEntry !HeapYounger\n");
                //              NullifyTrailEntry(itrail);
              }
            }
          else if (TaggedIsSVA(*TagToPointer(itrail)))
            {
              if (!StackYounger(Tagp(SVA,ind->local_top),*TagToPointer(itrail))) {
                //              printf("\nNullifyTrailEntry !StackYounger\n");
                //              NullifyTrailEntry(itrail);
              }
            }
        }       
      ind->heap_top = (tagged_t *)&(HeapFReg);
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
              ind = ((struct gen*)ind->term[0])->choice;
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
