#ifndef _CIAO_TABLING_ENGINE_H
#define _CIAO_TABLING_ENGINE_H

#if defined(TABLING)
/* -------------------------- */
/*          Includes          */
/* -------------------------- */


/* -------------- */
/*      Tags      */
/* -------------- */
/* WARNING: these macros need Ciao tag scheme */

#define PairInitTag  0xC0000001  //to mark the begining of a list in trie.c
#define PairEndTag   0xD0000001  //to mark the end of a list in trie.c

#define CommaInitTag 0xB0000001  //to mark the begining of comma functor
#define CommaEndTag  0xF0000001  //to mark the end of comma functor in trie.c

#define LargeInitTag 0xB0000002  //to mark the begining of a large integer
#define LargeEndTag  0xF0000002  //to mark the end of a large integer in trie.c

#define FloatInitTag 0xB0000003  //to mark the begining of a float in trie.c
#define FloatEndTag  0xF0000003  //to mark the end of a float in trie.c

#define VarTrie      0xE0000001  //ID of trie vars
#define AttrTrie     0xE0000002  //ID of trie attrs
#define TrieVarIndex(TERM)  (((TERM) & 0x0FFFFFFF) >> 3)
#define NOEXECUTING  0x88000001

//---------------------------//

#define EMPTY_LIST 0xA00000E0
#define INTEGER_MARK 0xB0000008
#define FLOAT_MARK 0x9000000C
#define GTOP state->worker_registers->global_top
#define P_GTOP  w->global_top
#define REGISTERS state->worker_registers
//---------------------------//

/* ------------------------- */
/*      Construct Terms      */
/* ------------------------- */
#define MkVarTerm(ARG) (chat_make_var((ARG)->misc->goal_desc_ptr))
#define MkIntTerm(INT) (chat_make_integer(Arg->misc->goal_desc_ptr,(INT)))
#define MkFloatTerm(FLOAT) (chat_make_float(Arg->misc->goal_desc_ptr,(FLOAT)))
#define MkAtomTerm(ATOM) (MakeString(ATOM))
#define MkPairTerm(HEAD,TAIL) (chat_make_list(Arg->misc->goal_desc_ptr,(HEAD),(TAIL)))
#define MkApplTerm(FUNCTOR,ARITY,ARGS) (chat_make_functor(Arg->misc->goal_desc_ptr,(FUNCTOR),(ARITY),(ARGS))) 
          

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
#define IsTrieVar(TERM)  (((TERM) & 0xF0000003) == VarTrie)
#define IsTrieAttr(TERM)  (((TERM) & 0xF0000003) == AttrTrie)

/* -------------------- */
/*      Unification     */
/* -------------------- */
#define Unify(TERM1,TERM2) (cunify(Arg,(TERM1),(TERM2)))


/* ---------------------------- */
/*      Predicate Arguments     */
/* ---------------------------- */
#define ARG1 X(0)
#define ARG2 X(1)
#define ARG3 X(2)
#define ARG4 X(3)
#define ARG5 X(4)
#define ARG6 X(5)
#define ARG7 X(6)
#define ARG8 X(7)
#define ARG9 X(8)
#define ARG10 X(9)
#endif

#endif /* _CIAO_TABLING_ENGINE_H */
