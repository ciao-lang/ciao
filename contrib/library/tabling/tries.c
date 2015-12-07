#if defined(TABLING)
static TrNode put_trie(TrNode node, tagged_t entry);
static CFUN__PROTO(get_trie, tagged_t,
		   TrNode node, tagged_t *stack_list, TrNode *cur_node);
//static void free_child_nodes(TrNode node);
//static void traverse_trie_usage(TrNode node, int depth);



/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

//static struct global_trie_stats GLOBAL_STATS;
//static struct local_trie_stats LOCAL_STATS;
TrNode TRIES;
TrHash HASHES;
static tagged_t TERM_STACK[TERM_STACK_SIZE];
static tagged_t ATTR_STACK[ATTR_STACK_SIZE];
static tagged_t *stack_args, *stack_args_base;
static tagged_t *stack_vars, *stack_vars_base;
static tagged_t *stack_attrs, *stack_attrs_base;
static int max_index;
static int max_index_attr;



/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

static inline
TrNode trie_node_check_insert(TrNode parent, tagged_t t) {
  TrNode child;

  child = TrNode_child(parent);
  if (child == NULL) 
    {
      new_trie_node(child, t, parent, NULL, NULL, NULL);
      TrNode_child(parent) = child;
      return child;
    } else if (! IS_TRIE_HASH(child)) 
      {
	int count = 0;
	do 
	  {
	    if (TrNode_entry(child) == t) return child;
	    count++;
	    child = TrNode_next(child);
	  } 
	while (child);
	new_trie_node(child, t, parent, NULL, TrNode_child(parent), NULL);
	if (++count > MAX_NODES_PER_TRIE_LEVEL) 
	  {
	    // alloc a new trie hash
	    TrHash hash;
	    TrNode chain, next, *bucket;
	    new_trie_hash(hash, count);
	    chain = child;
	    do 
	      {
		bucket = TrHash_bucket(hash, HASH_TERM(TrNode_entry(chain), 
						       BASE_HASH_BUCKETS - 1));
		next = TrNode_next(chain);
		TrNode_next(chain) = *bucket;
		*bucket = chain;
		chain = next;
	      } 
	    while (chain);
	    TrNode_child(parent) = (TrNode) hash;
	  } else 
	    {
	      TrNode_child(parent) = child;
	    }
	return child;
      } else 
	{
	  // is trie hash 
	  TrHash hash;
	  TrNode *bucket;
	  int count;
	  hash = (TrHash) child;
	  bucket = TrHash_bucket(hash, HASH_TERM(t, TrHash_seed(hash)));
	  child = *bucket;
	  count = 0;
	  while (child) 
	    {
	      if (TrNode_entry(child) == t) return child;
	      count++;
	      child = TrNode_next(child);
	    } 
	  while (child);
	  TrHash_num_nodes(hash)++;
	  new_trie_node(child, t, parent, NULL, *bucket, 
			AS_TR_NODE_NEXT(bucket));
	  *bucket = child;
	  if (count > MAX_NODES_PER_BUCKET && 
	      TrHash_num_nodes(hash) > TrHash_num_buckets(hash)) 
	    {
	      // expand trie hash 
	      TrNode chain, next, *first_bucket, *new_bucket;
	      int seed;
	      first_bucket = TrHash_buckets(hash);
	      bucket = first_bucket + TrHash_num_buckets(hash);
	      TrHash_num_buckets(hash) *= 2;
	      new_hash_buckets(hash, TrHash_num_buckets(hash)); 
	      seed = TrHash_num_buckets(hash) - 1;
	      do 
		{
		  if (*--bucket) 
		    {
		      chain = *bucket;
		      do 
			{
			  new_bucket = TrHash_bucket
			    (hash, HASH_TERM(TrNode_entry(chain), seed));
			  next = TrNode_next(chain);
			  TrNode_next(chain) = *new_bucket;
			  *new_bucket = chain;
			  chain = next;
			} 
		      while (chain);
		    }
		} 
	      while (bucket != first_bucket);
	    }
	  return child;
	}
}

/* -------------------------- */
/*            API             */     
/* -------------------------- */

void init_tries_module(void) 
{
  TRIES = NULL;
  HASHES = NULL;

//  MEMORY_IN_USE = 0;
//  MEMORY_MAX_USED = 0;
//  NODES_IN_USE = 0;
//  NODES_MAX_USED = 0;
//  HASHES_IN_USE = 0;
//  HASHES_MAX_USED = 0;
//  BUCKETS_IN_USE = 0;
//  BUCKETS_MAX_USED = 0;

  return;
}


TrNode open_trie(void) 
{
  TrNode new_node;

  new_trie_node(new_node, 0, NULL, NULL, TRIES, AS_TR_NODE_NEXT(&TRIES));
  TRIES = new_node;
  return new_node;
}

TrNode put_trie_entry(TrNode node, tagged_t entry, struct sf* sf) 
{
  stack_args_base = stack_args = TERM_STACK;
  stack_vars_base = stack_vars = TERM_STACK + TERM_STACK_SIZE - 1;
  stack_attrs_base = stack_attrs = ATTR_STACK + ATTR_STACK_SIZE - 1;

  node = put_trie(node, entry);

  //I cannot use stacks here because a consumer can read
  //all its answers (from a complete generator) and this not
  //chronological
  sf->size = (stack_vars_base - stack_vars) / 2;
  sf->vars = (tagged_t*) checkalloc(sf->size * sizeof(tagged_t));

  sf->attr_size = (stack_attrs_base - stack_attrs) / 2;
  sf->attrs = (tagged_t*) checkalloc(sf->attr_size * sizeof(tagged_t));
 
  int index = sf->size - 1;
  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) 
    {
      POP_DOWN(stack_vars);
      *TagToPointer(*stack_vars) = *stack_vars;
      sf->vars[index--] = *stack_vars;
    }

  index = sf->attr_size - 1;
  while (STACK_NOT_EMPTY(stack_attrs++, stack_attrs_base)) 
    {
      POP_DOWN(stack_attrs);
      *TagToPointer(*stack_attrs) = *stack_attrs;
#if defined(DEBUG_ALL)
      tagged_t term;
      DEREF(term,*stack_attrs);
      printf("\nTRIES %lx %lx\n",term,*stack_attrs);
#endif
      sf->attrs[index--] = *stack_attrs;
    }

  return node;
}

//struct separation_list* put_trie_answer(TrNode node, struct subs_factor* answer)
//{
//  stack_args_base = stack_args = TERM_STACK;
//  stack_vars_base = stack_vars = TERM_STACK + TERM_STACK_SIZE - 1;
//  int attr_size = answer->attr_size;
//  int *new_attr = (int*) malloc (attr_size * sizeof(int));
//
//  int index;
//
//  //FROM index = 1 because attr_vars[0] is the dummy variable for different constraints.
//  for (index = 1; index < attr_size; index++)
//    {
//      tagged_t t = answer->attr_vars[index];
//      *TagToPointer(t) = AttrTrie | ((stack_vars_base - stack_vars) << 2);
//      PUSH_UP(stack_vars, t, stack_args);
//      PUSH_UP(stack_vars, stack_vars, stack_args);
//    }
//
//  for (index = 0; index < answer->size; index++)
//    node = put_trie(node, answer->vars[index],&attr_size);
//
//  //Recovering original values
//  int index_attr = attr_size - 1;
//  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) 
//    {
//      POP_DOWN(stack_vars);
//      *TagToPointer(*stack_vars) = *stack_vars;
//      if (TagIsCVA(*stack_vars))
//	{
//	  tagged_t attr = fu1_get_attribute(NULL,*stack_vars);
//	  DEREF(attr,ArgOfTerm(1, attr));
//	  new_attr[index_attr--] = IntOfTerm(attr);
//	}
//    }
//  new_attr[0] = 0;
//
//  struct separation_list *separation_list = (struct separation_list*) node->child;
//  separation_list = (struct separation_list*) get_more_general_answer_c(separation_list,attr_size,new_attr);
//
//  if (separation_list == NULL)
//    {
//      separation_list = (struct separation_list*) malloc (sizeof(struct separation_list));
//      INIT_SEPARATION_LIST(separation_list,NULL,node,answer->attr_size,attr_size,new_attr);
//
//      //Cancelling entailed answers
////      CANCELLING_ENTAILED_ANSWERS(separation_list);
//      return separation_list;
//    }
//  free(new_attr);
//  return NULL;
//}

TrNode put_trie_answer(TrNode node, struct sf* ans, struct attrs* new_attrs)
{
  stack_args_base = stack_args = TERM_STACK;
  stack_vars_base = stack_vars = TERM_STACK + TERM_STACK_SIZE - 1;
  stack_attrs_base = stack_attrs = ATTR_STACK + ATTR_STACK_SIZE - 1;

  int index;
  tagged_t t;
  tagged_t attr;
  for (index = 0; index < ans->attr_size; index++)
    {
      t = ans->attrs[index];
      DEREF(t,t);
#if defined(DEBUG_ALL)
      printf("\nT = %lx value %lx\n",t,*TagToPointer(t));
      attr = fu1_get_attribute(NULL,t);
      DEREF(attr,ArgOfTerm(1, attr));
      printf("\nIN put_trie_answer %lx = %li\n",t,IntOfTerm(attr));
#endif
      *TagToPointer(t) = AttrTrie | ((stack_attrs_base - stack_attrs) << 2);
      PUSH_UP(stack_attrs, t, ATTR_STACK);
      PUSH_UP(stack_attrs, stack_attrs, ATTR_STACK);
    }

  for (index = 0; index < ans->size; index++)
    {
      node = put_trie(node, ans->vars[index]);
    }

  while (STACK_NOT_EMPTY(stack_vars++, stack_vars_base)) 
    {
      POP_DOWN(stack_vars);
      *TagToPointer(*stack_vars) = *stack_vars;
    }

  if (new_attrs != NULL)
    {
      new_attrs->size = (stack_attrs_base - stack_attrs) / 2;
      ALLOC_TABLING_STK(new_attrs->attrs, tagged_t*, 
			new_attrs->size * sizeof(tagged_t));
//     new_attrs->attrs = (tagged_t*) checkalloc
//	(new_attrs->size * sizeof(tagged_t));
      index = new_attrs->size - 1;
    }

  while (STACK_NOT_EMPTY(stack_attrs++, stack_attrs_base)) 
    {
      POP_DOWN(stack_attrs);
      *TagToPointer(*stack_attrs) = *stack_attrs;
      new_attrs->attrs[index--] = *stack_attrs;
#if defined(DEBUG_ALL)
      tagged_t term;
      DEREF(term,*stack_attrs);
      printf("\nTRIES 2 %lx %lx\n",term,*stack_attrs);
      attr = fu1_get_attribute(NULL,new_attrs->attrs[index+1]);
      DEREF(attr,ArgOfTerm(1, attr));
      printf("\nOUT %d put_trie_answer %lx = %li\n",
	     index+1,new_attrs->attrs[index+1],IntOfTerm(attr));
#endif
    }

  return node;
//  new_attr[0] = 0;

//  struct separation_list *separation_list = (struct separation_list*) node->child;
//  separation_list = (struct separation_list*) get_more_general_answer_c(separation_list,attr_size,new_attr);
//
//  if (separation_list == NULL)
//    {
//      separation_list = (struct separation_list*) malloc (sizeof(struct separation_list));
//      INIT_SEPARATION_LIST(separation_list,NULL,node,answer->attr_size,attr_size,new_attr);
//      //Cancelling entailed answers
//      CANCELLING_ENTAILED_ANSWERS(separation_list);
//
//      return separation_list;
//    }
//  free(new_attr);
//  return NULL;
}

static
TrNode put_trie(TrNode node, tagged_t entry) 
{
  tagged_t t;
  DEREF(t,entry);
  switch(TagOf(t))
    {
    case STR:
      if (IsTrieVar(t) || IsTrieAttr(t)) return trie_node_check_insert(node, t);
      if (IsIntTerm(t)) 
	{
	  node = trie_node_check_insert(node, LargeInitTag);
	  node = trie_node_check_insert(node, *(TagToPointer(t) + 1));
	  return trie_node_check_insert(node, LargeEndTag);
	} 
      if (IsFloatTerm(t)) 
	{
	  node = trie_node_check_insert(node, FloatInitTag);
	  node = trie_node_check_insert(node, *(TagToPointer(t) + 1));
	  node = trie_node_check_insert(node, *(TagToPointer(t) + 2));
	  return trie_node_check_insert(node, FloatEndTag);
	}
      if (!strcmp(NameOfFunctor(t),",") && ArityOfFunctor(t)  == 2) 
	{
	  node = trie_node_check_insert(node, CommaInitTag);
	  do 
	    {
	      node = put_trie(node, ArgOfTerm(1, t));
	      DEREF(t,ArgOfTerm(2, t));
	    } 
	  while (IsApplTerm(t) && 
		 !strcmp(NameOfFunctor(t),",") && 
		 ArityOfFunctor(t)  == 2);
	  node = put_trie(node, t);
	  return trie_node_check_insert(node, CommaEndTag);	    
	}

      int i;
      node = trie_node_check_insert(node, *TagToPointer(t));
      for (i = 1; i <= ArityOfFunctor(t); i++)
	{
	  node = put_trie(node, ArgOfTerm(i, t));
	}
      return node;
    case ATM:
    case NUM:
      return trie_node_check_insert(node, t);
    case CVA:
      node = trie_node_check_insert
	(node, AttrTrie | ((stack_attrs_base - stack_attrs) << 2));
      *TagToPointer(t) = AttrTrie | ((stack_attrs_base - stack_attrs) << 2);
      PUSH_UP(stack_attrs, t, ATTR_STACK);
      PUSH_UP(stack_attrs, stack_attrs, ATTR_STACK);
      return node;
    case HVA:
    case SVA:
    case UBV:
      node = trie_node_check_insert
	(node, VarTrie | ((stack_vars_base - stack_vars) << 2));
      *TagToPointer(t) = VarTrie | ((stack_vars_base - stack_vars) << 2);
      PUSH_UP(stack_vars, t, stack_args);
      PUSH_UP(stack_vars, stack_vars, stack_args);
      return node;
    case LST:
      node = trie_node_check_insert(node, PairInitTag);
      do 
	{
	  node = put_trie(node, HeadOfTerm(t));
	  DEREF(t,TailOfTerm(t));
	} 
      while (IsPairTerm(t));
      node = put_trie(node, t);
      return trie_node_check_insert(node, PairEndTag);
    default:
      fprintf(stderr, "\nTries module: unknown type tag I\n");
    }
  return node;
}

// CFUN__PROTO(get_trie_answer, struct subs_factor*, struct separation_list *answerG, tagged_t *attr_vars)
CVOID__PROTO(get_trie_answer, TrNode node, struct sf *sf) {
  stack_vars_base = stack_vars = TERM_STACK;
  stack_args_base = stack_args = TERM_STACK + TERM_STACK_SIZE - 1;
  stack_attrs_base = stack_attrs = ATTR_STACK;
  max_index = -1;
  max_index_attr = sf->attr_size - 1;


  int i;
  for (i = 0; i < sf->attr_size; i++) 
    {
      stack_attrs_base[i] = sf->attrs[i];
    }
  stack_attrs = stack_attrs_base + i;

  get_trie(Arg, node, stack_args, &node);

//  for (i = answerG->not_new_size - 1; i <= max_index; i++)
//    {
//      if ((stack_vars_base[i]) && TagIsCVA(*TagToPointer(stack_vars_base[i])))
//	{	
//	  *TagToPointer(stack_vars_base[i]) = stack_vars_base[i];
//	  tagged_t id[2];
//	  id[0] = MkIntTerm(new_diff_var_space(space));
//	  id[1] = stack_vars_base[i];
//	  id[0] = MkApplTerm("$separation_id", 2, id);
//	  bu2_attach_attribute(Arg,stack_vars_base[i],id[0]);
//	}
//    }

  int index;
  stack_args++;
  for (index = 0; index < sf->size; index++)
    {
      fflush(stdout);
      Unify(sf->vars[index], *stack_args++);
    }
}

static
CFUN__PROTO(get_trie, tagged_t,
	    TrNode node, tagged_t *stack_mark, TrNode *cur_node) {
  tagged_t t;

  while (TrNode_parent(node)) 
    {
      t = TrNode_entry(node);
//      if (IsTrieAttr(t)) 
//	{
//	  int index = TrieVarIndex(t);
//	  if (index > max_index) 
//	    {
//	      int i;
//	      stack_vars = stack_vars_base + index + 1;
//	      if (stack_vars > stack_args + 1)
//		fprintf(stderr, "\nTries module: TERM_STACK full");
//	      for (i = index; i > max_index; i--)
//		stack_vars_base[i] = 0;
//	      max_index = index;
//	    }
//	  if (stack_vars_base[index]) 
//	    {
//	      t = stack_vars_base[index];
//	    } 
//	  else 
//	    {
//	      t = MkVarTerm();
//	      *TagToPointer(t) = Tag(CVA,TagToPointer(MkVarTerm()));	      
//	      stack_vars_base[index] = t;
//	    }
//	  PUSH_UP(stack_args, t, stack_vars);
//	}
//      else if (IsTrieVar(t)) 
      if (IsTrieVar(t)) 
	{
	  int index = TrieVarIndex(t);
	  if (index > max_index) 
	    {
	      int i;
	      stack_vars = &stack_vars_base[index + 1];
	      if (stack_vars > stack_args + 1)
		fprintf(stderr, "\nTries module: TERM_STACK full");
	      for (i = index; i > max_index; i--)
		stack_vars_base[i] = 0;
	      max_index = index;
	    }
	  if (stack_vars_base[index]) 
	    {
	      t = stack_vars_base[index];
	    } 
	  else 
	    {
	      t = MkVarTerm(Arg);
	      stack_vars_base[index] = t;
	    }
	  PUSH_UP(stack_args, t, stack_vars);
	} 
      else if (IsTrieAttr(t)) 
	{
	  int index = TrieVarIndex(t);
	  if (index > max_index_attr) 
	    {
	      int i;
	      stack_attrs = stack_attrs_base + index + 1;
	      for (i = index; i > max_index_attr; i--)
		stack_attrs_base[i] = 0;
	      max_index_attr = index;
	    }
	  if (stack_attrs_base[index]) 
	    {
	      t = stack_attrs_base[index];
	    } 
	  else 
	    {
	      t = MkVarTerm(Arg);
	      stack_attrs_base[index] = t;
	    }
	  PUSH_UP(stack_args, t, stack_vars);
	} 
      else
	{
	  DEREF(t,t); 
	  int end = 1;
	  tagged_t t2;
	  tagged_t *stack_aux;
	  double f;
	  tagged_t *p;
	  switch(t)
	    {
	    case FloatInitTag:
	    case LargeInitTag:
	      break;
	    case FloatEndTag:
	      p = (tagged_t *)((void *) &f); // to avoid gcc warning 
	      node = TrNode_parent(node);
	      *(p + 1) = TrNode_entry(node);
	      node = TrNode_parent(node);
	      *p = TrNode_entry(node);
	      node = TrNode_parent(node); 
	      t = MkFloatTerm(f);
	      PUSH_UP(stack_args, t, stack_vars);
	      break;
	    case CommaEndTag:
	      node = TrNode_parent(node);
	      t = get_trie(Arg, node, stack_args, &node);
	      PUSH_UP(stack_args, t, stack_vars);
	      break;
	    case CommaInitTag: 
	      stack_aux = stack_mark;
	      stack_aux--;
	      while (STACK_NOT_EMPTY(stack_aux, stack_args)) 
		{
		  t = MkApplTerm(functor_comma, 2, stack_aux);
		  *stack_aux = t;
		  stack_aux--;
		}
	      stack_args = stack_mark;
	      *cur_node = node;
	      return t;
	    case PairEndTag:
	      node = TrNode_parent(node);
	      t = get_trie(Arg, node, stack_args, &node);
	      PUSH_UP(stack_args, t, stack_vars);
	      break;
	    case PairInitTag:
	      stack_aux = stack_mark;
	      t = *stack_aux--;
	      while (STACK_NOT_EMPTY(stack_aux, stack_args)) 
		{
		  t2 = *stack_aux--;
		  t = MkPairTerm(t2, t);
		}
	      stack_args = stack_mark;
	      *cur_node = node;
	      return t;
	    default:
	      end = 0;
	    }
	  if (!end)
	    {
	      int arity;
	      switch(TagOf(t))
		{
		case ATM:
		  arity = Arity(t);
		  if (arity == 0)
		    {
		      PUSH_UP(stack_args, t, stack_vars);
		    }
		  else
		    {
		      t = MkApplTerm(t, arity, stack_args + 1);
		      stack_args += arity;
		      PUSH_UP(stack_args, t, stack_vars);
		    }
		  break;
		case NUM:
		  PUSH_UP(stack_args, t, stack_vars);
		  break;
		default:
		  break;
		}
	    } 
	}
      node = TrNode_parent(node);
    }
  *cur_node = node;

  return t;
}
#endif
