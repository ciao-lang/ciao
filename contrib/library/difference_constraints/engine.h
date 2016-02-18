/*
 *  engine.h
 *
 *  Adaptor for engine macros
 */

/* TODO (JFMC): depends on particular 32-bit tagging scheme */

#ifndef _CIAO_DIFFERENCE_CONSTRAINTS_ENGINE_H
#define _CIAO_DIFFERENCE_CONSTRAINTS_ENGINE_H

#define EMPTY_LIST 0xA00000E0
#define INTEGER_MARK 0xB0000008
#define FLOAT_MARK 0x9000000C
#define GTOP state->worker_registers->global_top
#define P_GTOP  w->global_top
#define REGISTERS state->worker_registers

/* ------------------------- */
/*      Construct Terms      */
/* ------------------------- */
#define MkVarTerm() (sep_make_var(Arg->misc->goal_desc_ptr))
#define MkIntTerm(INT) (sep_make_integer(Arg->misc->goal_desc_ptr,(INT)))
#define MkFloatTerm(FLOAT) (sep_make_float(Arg->misc->goal_desc_ptr,(FLOAT)))
#define MkAtomTerm(ATOM) (MakeString(ATOM))
#define MkPairTerm(HEAD,TAIL) (sep_make_list(Arg->misc->goal_desc_ptr,(HEAD),(TAIL)))
#define MkApplTerm(FUNCTOR,ARITY,ARGS) (sep_make_functor(Arg->misc->goal_desc_ptr,(FUNCTOR),(ARITY),(ARGS))) 
          

/* ------------------------ */
/*      Destruct Terms      */
/* ------------------------ */
#define IntOfTerm(TERM) (GetInteger(TERM)) 
#define FloatOfTerm(TERM) (GetFloat(TERM)) 
#define AtomName(ATOM) (((atom_t *)TagToAtom(ATOM))->name)
#define HeadOfTerm(TERM) (*TagToPointer(TERM))
#define TailOfTerm(TERM) (*(TagToPointer(TERM) + 1))
#define ArgOfTerm(A,TERM) (*TagToArg(TERM,A))
#define NameOfFunctor(FUNCTOR) (((atom_t *)TagToAtom(SetArity(TagToHeadfunctor(FUNCTOR),0)))->name)
#define ArityOfFunctor(FUNCTOR)(Arity(TagToHeadfunctor(FUNCTOR)))


/* -------------------- */
/*      Test Terms      */
/* -------------------- */
#define IsIntTerm(TERM) (IsInteger(TERM))
#define IsFloatTerm(TERM) (IsFloat(TERM))
#define IsAtomTerm(TERM) (IsAtom(TERM)) 
#define IsPairTerm(TERM) (TagIsLST(TERM)) 
#define IsApplTerm(TERM) (TagIsSTR(TERM) && !IsNumber(TERM))
#define IsVarTerm(TERM) (IsVar(TERM))
#define IsNonVarTerm(TERM) (!IsVar(TERM))
#define IsFreeVar(X) (IsVar(X) && ((X) == *TagToPointer(X)))

/* -------------------- */
/*      Unification     */
/* -------------------- */
#define Unify(TERM1,TERM2) (cunify(Arg,(TERM1),(TERM2)))

/* -------------------- */
/*         Undo         */
/* -------------------- */

#ifdef TABLING
#ifdef DEBUG_ALL
#define MAKE_UNDO_VAR(ARG)						\
  {									\
    args[0] = atom_incr_dc_num_vars;					\
    args[1] = atom_decr_dc_num_vars;					\
    printf("\nUNDO(incr,decr)\n");					\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_DC(ARG,X,Y,OLD,NEW)					\
  {									\
    args[0] = MkIntTerm(X);						\
    args[1] = MkIntTerm(Y);						\
    args[2] = MkIntTerm(NEW);						\
    tmp_term1 = MkApplTerm(functor_put_dc_value,3,args);		\
    args[2] = MkIntTerm(OLD);						\
    tmp_term2 = MkApplTerm(functor_put_dc_value,3,args);		\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    printf("\nUNDO(put_value(%d,%d,%d),put_value(%d,%d,%d))\n",		\
	   (X),(Y),(NEW),(X),(Y),(OLD));				\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_PI(ARG,OLD,NEW)					\
  {									\
    args[0] = EMPTY_LIST;						\
    index_macro = space->size - 1;					\
    for (; index_macro >= 0; index_macro--)				\
      args[0] = MkPairTerm(MkIntTerm((NEW)[index_macro]),args[0]);	\
    tmp_term1 = MkApplTerm(functor_put_dc_pi,1,args);			\
    args[0] = EMPTY_LIST;						\
    index_macro = space->size - 1;					\
    for (; index_macro >= 0; index_macro--)				\
      args[0] = MkPairTerm(MkIntTerm((OLD)[index_macro]),args[0]);	\
    tmp_term2 = MkApplTerm(functor_put_dc_pi,1,args);			\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    printf("\nUNDO(put_pi,put_pi)\n");					\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_ATTR(ARG,VAR,ATTR)					\
  {									\
    /* Modify the current attribute */					\
    args[0] = MkIntTerm(ATTR);						\
    args[1] = VAR;							\
    tmp_term1 = MkApplTerm(functor_dbm_id,2,args);			\
    bu2_update_attribute((ARG),(VAR),tmp_term1);			\
  }

#define MAKE_UNDO_CHANGE_SPACE(ARG,SPACE)				\
  {									\
    args[0] = MkIntTerm(SPACE);						\
    tmp_term1 = MkApplTerm(functor_put_dc_space,1,args);		\
    args[0] = MkIntTerm(space);						\
    tmp_term2 = MkApplTerm(functor_put_dc_space,1,args);		\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    printf("\nUNDO(put_space(%p),put_space(%p))\n",(SPACE),space);	\
    printf("\nOLD STATE\n");						\
    print_space(space);							\
    printf("\nNEW STATE\n");						\
    print_space(SPACE);							\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }
#else
#define MAKE_UNDO_VAR(ARG)						\
  {									\
    args[0] = atom_incr_dc_num_vars;					\
    args[1] = atom_decr_dc_num_vars;					\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_DC(ARG,X,Y,OLD,NEW)					\
  {									\
    args[0] = MkIntTerm(X);						\
    args[1] = MkIntTerm(Y);						\
    args[2] = MkIntTerm(NEW);						\
    tmp_term1 = MkApplTerm(functor_put_dc_value,3,args);		\
    args[2] = MkIntTerm(OLD);						\
    tmp_term2 = MkApplTerm(functor_put_dc_value,3,args);		\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_PI(ARG,OLD,NEW)					\
  {									\
    args[0] = EMPTY_LIST;						\
    index_macro = space->size - 1;					\
    for (; index_macro >= 0; index_macro--)				\
      args[0] = MkPairTerm(MkIntTerm((NEW)[index_macro]),args[0]);	\
    tmp_term1 = MkApplTerm(functor_put_dc_pi,1,args);			\
    args[0] = EMPTY_LIST;						\
    index_macro = space->size - 1;					\
    for (; index_macro >= 0; index_macro--)				\
      args[0] = MkPairTerm(MkIntTerm((OLD)[index_macro]),args[0]);	\
    tmp_term2 = MkApplTerm(functor_put_dc_pi,1,args);			\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_ATTR(ARG,VAR,ATTR)					\
  {									\
    /* Modify the current attribute */					\
    args[0] = MkIntTerm(ATTR);						\
    args[1] = VAR;							\
    tmp_term1 = MkApplTerm(functor_dbm_id,2,args);			\
    bu2_update_attribute((ARG),(VAR),tmp_term1);			\
  }

#define MAKE_UNDO_CHANGE_SPACE(ARG,SPACE)				\
  {									\
    args[0] = MkIntTerm(SPACE);						\
    tmp_term1 = MkApplTerm(functor_put_dc_space,1,args);		\
    args[0] = MkIntTerm(space);						\
    tmp_term2 = MkApplTerm(functor_put_dc_space,1,args);		\
    args[0] = tmp_term1;						\
    args[1] = tmp_term2;						\
    undo_term = MkApplTerm(functor_forward_trail,2,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }
#endif
#else
#define MAKE_UNDO_VAR(ARG)						\
  {									\
    undo_term =	atom_decr_dc_num_vars;					\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_DC(ARG,X,Y,OLD,NEW)					\
  {									\
    args[0] = MkIntTerm(X);						\
    args[1] = MkIntTerm(Y);						\
    args[2] = MkIntTerm(OLD);						\
    undo_term = MkApplTerm(functor_put_dc_value,3,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_PI(ARG,OLD,NEW)					\
  {									\
    args[0] = EMPTY_LIST;						\
    index_macro = space->size - 1;					\
    for (; index_macro >= 0; index_macro--)				\
      args[0] = MkPairTerm(MkIntTerm((OLD)[index_macro]),args[0]);	\
    undo_term = MkApplTerm(functor_put_dc_pi,1,args);			\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }

#define MAKE_UNDO_ATTR(ARG,VAR,ATTR)					\
  {									\
    /* Modify the current attribute */					\
    args[0] = MkIntTerm(ATTR);						\
    args[1] = VAR;							\
    tmp_term1 = MkApplTerm(functor_dbm_id,2,args);			\
    bu2_update_attribute((ARG),(VAR),tmp_term1);			\
  }

#define MAKE_UNDO_CHANGE_SPACE(ARG,SPACE)				\
  {									\
    args[0] = MkIntTerm(space);						\
    undo_term = MkApplTerm(functor_put_dc_space,1,args);		\
    TrailPushCheck((ARG)->trail_top,undo_term);				\
  }
#endif

#endif /* _CIAO_DIFFERENCE_CONSTRAINTS_ENGINE_H */
