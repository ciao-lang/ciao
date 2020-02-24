

#if defined(DEBUG_EXEC)
#define TRACE_EXEC 1
#else
#define TRACE_EXEC 0
#endif


CBOOL__PROTO(exec_call_domain_projection, tagged_t list_of_attrs, tagged_t *attributes)
{
  int result;

  SAVE_REG;
  
  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  tagged_t exec_functor, exec_args[2];

  exec_args[0] = list_of_attrs;
  exec_args[1] = MkVarTerm(Arg);
 
  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$call_domain_projection"),2)
                            , 2, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_call_domain_projection = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  
  RESTORE_REG;

  if (result) 
    {
      *attributes = exec_args[1];
      if (TRACE_EXEC)
        PRINT_TERM(Arg, "\t\t EXIT - exec_call_domain_projection = ", exec_functor);
    }  
  else 
    {
      printf("ERROR in exec_call_domain_projection[ \n");
    }
  
  return result;
}

CBOOL__PROTO(exec_answer_domain_projection, tagged_t list_of_attrs, tagged_t *attributes)
{
  tagged_t exec_functor, exec_args[2];
  int result;

  SAVE_REG;
  
  exec_args[0] = list_of_attrs;
  exec_args[1] = MkVarTerm(Arg);
 
  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$answer_domain_projection"),2)
                            , 2, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_answer_domain_projection = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();
  
  RESTORE_REG;

  if (result) 
    {
      *attributes = exec_args[1];
      if (TRACE_EXEC) 
        PRINT_TERM(Arg, "\t\t EXIT - exec_answer_domain_projection = ", exec_functor);

    }  
  else 
    {
      printf("ERROR in exec_answer_domain_projection[ \n");
    }
  
  return exec_args[1];
}


CBOOL__PROTO(exec_call_store_projection, tagged_t list_of_attrs, tagged_t attributes, TrNode *project)
{
  tagged_t exec_functor, exec_args[3];
  int result;

  SAVE_REG;

  exec_args[0] = list_of_attrs;
  exec_args[1] = attributes;
  exec_args[2] = MkVarTerm(Arg);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$call_store_projection"),3)
                            , 3, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_call_store_projection = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (result) 
    {
      if (TRACE_EXEC || tabling_trace == atom_on) PRINT_TERM(Arg, "\t\t ", exec_functor);

      *project = save_term(Arg, exec_args[2]);
    }
  else
    {
      printf("ERROR in exec_call_store_projection \n");
    }

  RESTORE_REG;
 
  return TRUE;
}


CBOOL__PROTO(exec_answer_store_projection, tagged_t list_of_attrs, tagged_t attributes, TrNode* ans_store)
{
  tagged_t exec_functor, exec_args[3];
  int result;

  SAVE_REG;

  exec_args[0] = list_of_attrs;
  exec_args[1] = attributes;
  exec_args[2] = MkVarTerm(Arg);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$answer_store_projection"),3)
                            , 3, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_answer_store_projection = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (result) 
    {
      if (TRACE_EXEC || tabling_trace == atom_on) PRINT_TERM(Arg, "\t\t ", exec_functor);

      *ans_store = save_term(Arg, exec_args[2]);
    }
  else
    {
      printf("ERROR in exec_answer_store_projection\n");
    }

  RESTORE_REG;
 
  return TRUE;
}


CBOOL__PROTO(exec_call_entail, tagged_t list_of_attrs, tagged_t attributesA, TrNode attributesB, TrNode storeB)
{
  tagged_t exec_functor, exec_args[4];
  int result;

  SAVE_REG;
  
  /* DEREF(exec_args[0], list_of_attrs); */
  /* DEREF(exec_args[1], attributesA); */
  /* DEREF(exec_args[2], load_term(Arg, attributesB)); */
  /* DEREF(exec_args[3], load_term(Arg, storeB)); */

  exec_args[0] = list_of_attrs;
  exec_args[1] = attributesA;
  exec_args[2] = load_term(Arg, attributesB);
  exec_args[3] = load_term(Arg, storeB);


  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$call_entail"),4)
                            , 4, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_call_entail = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  RESTORE_REG;
 
  return result;
}


CBOOL__PROTO(exec_answer_check_entail, tagged_t list_of_attrs, tagged_t attributesA, TrNode attributesB, TrNode storeB, tagged_t *newattributes)
{
  tagged_t exec_functor, exec_args[6];
  int result;

  SAVE_REG;
  
  exec_args[0] = list_of_attrs;
  exec_args[1] = attributesA;
  exec_args[2] = load_term(Arg, attributesB);
  exec_args[3] = load_term(Arg, storeB);
  exec_args[4] = MkVarTerm(Arg);
  exec_args[5] = MkVarTerm(Arg);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$answer_check_entail"),6)
                            , 6, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_answer_check_entail = ", exec_functor) ;

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (result != 0) {
    DEREF(exec_args[4], exec_args[4]);
    result = IntOfTerm(exec_args[4]);
    if (result == 2)
      *newattributes = exec_args[5];
  }

  RESTORE_REG;
 
  return result;
}



CBOOL__PROTO(exec_apply_answer, tagged_t list_of_attrs, TrNode ans_attrs, TrNode ans_store)
{
  tagged_t exec_functor, exec_args[3];
  int result;

  SAVE_REG;  

  /* DEREF(exec_args[0], list_of_attrs); */
  /* DEREF(exec_args[1], load_term(Arg, ans_attrs)); */
  /* DEREF(exec_args[2], load_term(Arg, ans_store)); */

  exec_args[0] = list_of_attrs;
  exec_args[1] = load_term(Arg, ans_attrs);
  exec_args[2] = load_term(Arg, ans_store);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$apply_answer"),3)
                            , 3, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_apply_answer = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (TRACE_EXEC) printf("\t\t result = %d\n", result);
  
  RESTORE_REG;

  return result;
}





CBOOL__PROTO(exec_current_store, TrNode *orig)
{
  tagged_t exec_functor, exec_args[1];
  int result;

  SAVE_REG;

  exec_args[0] = MkVarTerm(Arg);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$current_store"),1)
                            , 1, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_current_store = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (result) 
    {
      *orig = save_term(Arg, exec_args[0]);
    }
  else
    {
      printf("ERROR in exec_current_store \n");
    }

  RESTORE_REG;
 
  return TRUE;
}



CBOOL__PROTO(exec_reinstall_store, tagged_t list_of_attrs, TrNode attributes, TrNode store)
{
  tagged_t exec_functor, exec_args[3];
  int result;

  SAVE_REG;
  
  /* DEREF(exec_args[0], list_of_attrs); */
  /* DEREF(exec_args[1], load_term(Arg, attributes)); */
  /* DEREF(exec_args[2], load_term(Arg, store)); */

  exec_args[0] = list_of_attrs;
  exec_args[1] = load_term(Arg, attributes);
  exec_args[2] = load_term(Arg, store);

  ciao_frame_re_begin(Arg->misc->goal_desc_ptr);

  PRINT_REG("\n");

  exec_functor = MkApplTerm(SetArity(GET_ATOM("tabling_rt:$reinstall_store"),3)
                            , 3, exec_args);

  if (TRACE_EXEC) PRINT_TERM(Arg, "\t\t exec_reinstall_store = ", exec_functor);

  result = ciao_commit_call_term(ciao_refer(exec_functor));
  ciao_frame_re_end();

  if (!result) 
    {
      printf("ERROR in exec_reinstall_store \n");
    }

  RESTORE_REG;

  return TRUE;
}


