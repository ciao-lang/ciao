

CBOOL__PROTO(array_to_list, intmach_t size, tagged_t *array, tagged_t*list)
{
  *list = atom_nil;
  for ( ; size > 0; size--)
    {
      *list = MkPairTerm(array[size - 1], *list);
    }
  return TRUE;
}

CFUN__PROTO(save_term, TrNode, tagged_t term)
{
  TrNode copy;

  if (auxiliar_trie == NULL) auxiliar_trie = open_trie();
  copy = put_trie_term(auxiliar_trie, term);

  return copy;
}

CFUN__PROTO(load_term, tagged_t, TrNode copy)
{
  tagged_t term;

  term = get_trie_term(Arg, copy);

  return term;
}


