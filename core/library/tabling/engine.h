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

#if tagged__size == 64
#define PairInitTag  0xC000000000000001  //to mark the begining of a list in trie.c
#define PairEndTag   0xD000000000000001  //to mark the end of a list in trie.c

#define CommaInitTag 0xB000000000000001  //to mark the begining of comma functor
#define CommaEndTag  0xF000000000000001  //to mark the end of comma functor in trie.c

#define LargeInitTag 0xB000000000000002  //to mark the begining of a large integer
#define LargeEndTag  0xF000000000000002  //to mark the end of a large integer in trie.c

#define FloatInitTag 0xB000000000000003  //to mark the begining of a float in trie.c
#define FloatEndTag  0xF000000000000003  //to mark the end of a float in trie.c

#define VarTrie      0xE000000000000001  //ID of trie vars
#define AttrTrie     0xE000000000000002  //ID of trie attrs
#define TrieVarIndex(TERM)  (((TERM) & 0x0FFFFFFFFFFFFFFF) >> 3)

#define EMPTY_LIST atom_list
//#define EMPTY_LIST   0xA0000000000000E0
#define INTEGER_MARK 0xB000000000000008
#define FLOAT_MARK   0x900000000000000C

#elif tagged__size == 32

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

#define EMPTY_LIST atom_list
//#define EMPTY_LIST   0xA00000E0
#define INTEGER_MARK 0xB0000008
#define FLOAT_MARK   0x9000000C
#else
#error "undefined tagged size in tabling/engine.h"
#endif

#define NOEXECUTING  atom_off

//---------------------------//

#define GTOP state->worker_registers->heap_top
#define P_GTOP  w->heap_top
#define REGISTERS state->worker_registers
//---------------------------//

/* ------------------------- */
/*      Construct Terms      */
/* ------------------------- */
#define MkVarTerm(ARG) (chat_make_var((ARG)->misc->goal_desc_ptr))
#define MkIntTerm(INT) (chat_make_integer(Arg->misc->goal_desc_ptr,(INT)))
#define MkFloatTerm(FLOAT) (chat_make_float(Arg->misc->goal_desc_ptr,(FLOAT)))
#define MkAtomTerm(ATOM) (GET_ATOM(ATOM))
#define MkPairTerm(HEAD,TAIL) (chat_make_list(Arg->misc->goal_desc_ptr,(HEAD),(TAIL)))
#define MkApplTerm(FUNCTOR,ARITY,ARGS) (chat_make_functor(Arg->misc->goal_desc_ptr,(FUNCTOR),(ARITY),(ARGS))) 
/* From eng.h PointerToTerm */
#define MkPtrTerm(INT) ((tagged_t)(INT) ^ (TaggedZero^MallocBase))
          

/* ------------------------ */
/*      Destruct Terms      */
/* ------------------------ */
#define IntOfTerm(TERM) (TaggedToIntmach(TERM)) 
#define FloatOfTerm(TERM) (TaggedToFloat(TERM)) 
#define AtomName(ATOM) (((atom_t *)TaggedToAtom(ATOM))->name)
#define HeadOfTerm(TERM) (*TaggedToPointer(TERM))
#define TailOfTerm(TERM) (*(TaggedToPointer(TERM) + 1))
#define ArgOfTerm(A,TERM) (*TaggedToArg(TERM,A))
#define NameOfFunctor(FUNCTOR) (((atom_t *)TaggedToAtom(SetArity(TaggedToHeadfunctor(FUNCTOR),0)))->name)
#define ArityOfFunctor(FUNCTOR)(Arity(TaggedToHeadfunctor(FUNCTOR)))
/* From eng.h TermToPointer */
#define PtrOfTerm(TERM) ((tagged_t *)((TERM) ^ (TaggedZero^MallocBase)))

/* -------------------- */
/*      Test Terms      */
/* -------------------- */
#define IsIntTerm(TERM) (IsInteger(TERM))
#define IsFloatTerm(TERM) (IsFloat(TERM))
#define IsAtomTerm(TERM) (TaggedIsATM(TERM)) 
#define IsPairTerm(TERM) (TaggedIsLST(TERM)) 
#define IsApplTerm(TERM) (TaggedIsSTR(TERM) && !IsNumber(TERM))
#define IsVarTerm(TERM) (IsVar(TERM))
#define IsNonVarTerm(TERM) (!IsVar(TERM))
#define IsFreeVar(X) (IsVar(X) && ((X) == *TaggedToPointer(X)))

#if tagged__size == 64
#define IsTrieVar(TERM)   (((TERM) & 0xF000000000000003) == VarTrie)
#define IsTrieAttr(TERM)  (((TERM) & 0xF000000000000003) == AttrTrie)
#elif tagged__size == 32
#define IsTrieVar(TERM)   (((TERM) & 0xF0000003) == VarTrie)
#define IsTrieAttr(TERM)  (((TERM) & 0xF0000003) == AttrTrie)
#error "undefined tagged size in tabling/engine.h"
#endif

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
