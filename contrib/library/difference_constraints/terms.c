tagged_t sep_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  tagged_t resul = TagHVA(GTOP);
  HeapPush(GTOP, resul);
  return resul;
}			      

tagged_t sep_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);  //change to in my_heap
  return MakeInteger(REGISTERS, i);
}

tagged_t sep_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  return MakeFloat(REGISTERS, f);
}

tagged_t sep_make_list(goal_descriptor_t *state, tagged_t head, tagged_t tail) 
{
  tagged_t list;
  ciao_ensure_heap(state, 3);
  worker_t * w = REGISTERS;
  MakeLST(list, head, tail);
  return list;
}

tagged_t sep_make_functor(goal_descriptor_t *state, tagged_t atom, 
			  int arity, tagged_t *args) 
{
  worker_t *w = REGISTERS;
  if (arity == 0) return atom;
  else 
    {
      int i;
      ciao_ensure_heap(state, 2 + arity);
      HeapPush(P_GTOP, atom);
      for (i = 0; i < arity; i++) HeapPush(P_GTOP, args[i]);
      return Tag(STR, HeapOffset(P_GTOP, -(arity+1)));
    }
}
