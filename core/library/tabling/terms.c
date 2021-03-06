#if defined(TABLING)
tagged_t chat_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  tagged_t resul = Tagp(HVA,GTOP);
  HeapPush(GTOP, resul);
  return resul;
}                             

tagged_t chat_make_integer(goal_descriptor_t *state, intmach_t i) 
{
  worker_t *w = REGISTERS;
  ciao_ensure_heap(state, 4);  //change to in my_heap
  return IntvalToTagged(i);
}

// // TODO: wrong, use PointerToTerm, etc. (JFMC)
// tagged_t chat_make_pointer(goal_descriptor_t *state, intmach_t i) 
// {
//   worker_t *w = REGISTERS;
//   ciao_ensure_heap(state, 4);
//   return MakeBlob(i);
// }

tagged_t chat_make_float(goal_descriptor_t *state, double f) 
{
  worker_t *w = REGISTERS;
  ciao_ensure_heap(state, 4);
  return BoxFloat(f);
}

tagged_t chat_make_list(goal_descriptor_t *state, tagged_t head, tagged_t tail) 
{
  tagged_t list;
  ciao_ensure_heap(state, 3);
  worker_t *w = REGISTERS;
  MakeLST(list, head, tail);
  return list;
}

tagged_t chat_make_functor(goal_descriptor_t *state, tagged_t atom, intmach_t arity, tagged_t *args) 
{
  worker_t *w = REGISTERS;
  if (arity == 0) return atom;
  else 
    {
      intmach_t i;
      ciao_ensure_heap(state, 2 + arity);
      HeapPush(P_GTOP, atom);
      for (i = 0; i < arity; i++) HeapPush(P_GTOP, args[i]);
      return Tagp(STR, HeapOffset(P_GTOP, -(arity+1)));
    }
}
#endif
