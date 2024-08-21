/*
 *  eng_terms.h
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_TERMS_H
#define _CIAO_ENG_TERMS_H

#if defined(OPTIM_COMP)
#error "not valid for OPTIM_COMP"
#endif

/* ------------------------------------------------------------------------- */
/* Some definitions for integer types */

/* TODO:[oc-merge] move closer to eng_predefs.pl/eng_predefs.h */
/* Sizes */
#if tagged__size == 64
#define INTMACH_MAX INT64_MAX
#define INTMACH_MIN INT64_MIN
#elif tagged__size == 32
#define INTMACH_MAX INT32_MAX
#define INTMACH_MIN INT32_MIN
#endif

/* --------------------------------------------------------------------------- */
/* Bit manipulation operations */

/* TODO:[JF] use generic __builtin_ctzg, etc.? (simpler code) */

/* operations for uint32_t ('unsigned int') */
#define LSB32(x) (__builtin_ctz(x))
#define MSB32(x) (31 - __builtin_clz(x))
#define POPCOUNT32(x) (__builtin_popcount(x))
#if tagged__size == 64 /* assume LP64 */
/* operations for uint64_t ('unsigned long' or 'unsigned long long') */
#define LSB64(x) (__builtin_ctzl(x))
#define MSB64(x) (63 - __builtin_clzl(x))
#define POPCOUNT64(x) (__builtin_popcountl(x))
#elif tagged__size == 32 /* assume ILP32 */
/* operations for uint64_t ('unsigned long long') */
#define LSB64(x) (__builtin_ctzll(x))
#define MSB64(x) (63 - __builtin_clzll(x))
#define POPCOUNT64(x) (__builtin_popcountll(x))
#endif

#if tagged__size == 64 /* 64 bit intval_t */
#define intval_LSB LSB64
#define intval_MSB MSB64
#define intval_POPCOUNT POPCOUNT64
#elif tagged__size == 32 /* 32 bit intval_t */
#define intval_LSB LSB32
#define intval_MSB MSB32
#define intval_POPCOUNT POPCOUNT32
#endif

/* ------------------------------------------------------------------------- */
/* Macros for formatting integers */

#include <inttypes.h> /* for PRI* macros */
#if tagged__size == 64
#define PRIum PRIu64 /* intmach_t using %u */
#define PRIdm PRId64 /* intmach_t using %d */
#define PRIxm PRIx64 /* intmach_t using %x */
#elif tagged__size == 32
#define PRIum PRIu32 /* intmach_t using %u */
#define PRIdm PRId32 /* intmach_t using %d */
#define PRIxm PRIx32 /* intmach_t using %x */
#endif

/* ------------------------------------------------------------------------- */
/* Term data definitions and macros */

/*
  The macros here involve casting,tagging, detagging and the like.
  Term to pointer conversion must know where object are in virtual memory.
  Some macros requires a heap and a trail.
*/

/* tagged_t scheme:

   TAGMASK:     E000...0000
   QTAGMASK:    1000...0000
   POINTERMASK: 0FFF...FFFC
   INDEXMASK:   000F...FFFF
   ZMASK:       0800...0000
 */

#define tagged__tag_size 3
#define tagged__tag_offset (tagged__size-tagged__tag_size)
#define tagged__qtag_size 1
#define tagged__qtag_offset (tagged__tag_offset-1)

#define tagged__gc_marked_size 1
#define tagged__gc_marked_offset 1
#define tagged__gc_reversed_size 1
#define tagged__gc_reversed_offset 0

#define tagged__num_offset SMALLPTR_LOWERBITS 
#define tagged__atm_offset SMALLPTR_LOWERBITS 
#define tagged__ptr_offset SMALLPTR_LOWERBITS 
#define ARITYSIZE 8
#define ARITYOFFSET (tagged__size-tagged__tag_size-1-ARITYSIZE)
#define ARITYLIMIT (1<<ARITYSIZE) /* 256 */

#define tagged__num_size (tagged__size - tagged__tag_size - 1 - tagged__num_offset)

#define TAGMASK MakeMask(tagged_t, tagged__tag_size, tagged__tag_offset)
#define QTAGMASK MakeMask(tagged_t, tagged__qtag_size, tagged__qtag_offset)
 
#define GC_MARKMASK MakeMask(tagged_t, tagged__gc_marked_size, tagged__gc_marked_offset)
#define GC_REVERSEDMASK MakeMask(tagged_t, tagged__gc_reversed_size, tagged__gc_reversed_offset)
#define GC_ANYMASK (GC_MARKMASK|GC_REVERSEDMASK)

#define INDEXMASK       (((tagged_t)1<<ARITYOFFSET)-1)
#define TagIndex(T,P)   (Tagt((T))+((tagged_t)((P)<<tagged__atm_offset)))
#define TagIndexDiff(P) ((tagged_t)((P)<<tagged__atm_offset))
#define IndexPart(T)    (((T)&INDEXMASK)>>tagged__atm_offset)

#define POINTERMASK     (QTAGMASK-(1<<tagged__ptr_offset))
#define PointerPart(T)  ((intmach_t)((T)&POINTERMASK))  
#if SMALLPTR_BASE
#define TaggedToPointer(T) ((tagged_t *)(((tagged_t)(T)&POINTERMASK)+SMALLPTR_BASE))
#else
#define TaggedToPointer(T) ((tagged_t *)((tagged_t)(T)&POINTERMASK))
#endif

#define Tagt(T) (((tagged_t)(T)<<tagged__tag_offset))

/* Tagp(T,P) creates tagged_t from tag T and pointer P */
#if SMALLPTR_BASE
#define Tagp(T,P) (Tagt((T))+((tagged_t)(P) & POINTERMASK))
#else
#define Tagp(T,P) (Tagt((T))+((tagged_t)(P)))
#endif

/* todo[ts]: add a macro that uses tagged__qval_offset, for atoms */
#define Tagn(T,P) (Tagt((T))+(((tagged_t)(P)<<tagged__num_offset)))

// #define MaxAtomCount (INDEXMASK>>tagged__atm_offset)
#define MaxAtomCount (((tagged_t)1<<(tagged__size-tagged__tag_size-1-ARITYSIZE-tagged__atm_offset))-1)

// TODO:[oc-merge] this one seems a bit faster?
// #define HasTag(X,T) (TagOf((X)) == (T))
#define HasTag(X,T)     (((X) & TAGMASK) == Tagt((T)))
#define TagOf(P)        ((P)>>tagged__tag_offset)  /* collects tag */
#define CT(T1,T2)       ((T1)<<tagged__tag_size|(T2)) /* for concatenating tags     */

#define TaggedSameTag(U,V) (((U)^(V)) < QTAGMASK)

/* ------------------------------------------------------------------------- */
/* Pointers to tagged words with GC marks */

/* Get a tagged ensuring that it does not have gc marks */
#define GC_UNMARKED(X) ((X)&(~(GC_ANYMASK)))
#define GC_UNMARKED_M(X) ((X)&(~(GC_MARKMASK)))

#define gc_IsMarked(x)  ((x)&GC_MARKMASK)
#define gc_IsFirst(x)   ((x)&GC_REVERSEDMASK)
#define gc_IsForM(x)   ((x)&(GC_REVERSEDMASK|GC_MARKMASK))
#define gc_MarkM(x)  ((x)|= GC_MARKMASK)
#define gc_MarkF(x)  ((x)|= GC_REVERSEDMASK)
#define gc_UnmarkM(x)  ((x)&=(~GC_MARKMASK))
#define gc_UnmarkF(x)  ((x)&=(~GC_REVERSEDMASK))
#define gc_PutValue(p,x) Deposit(p,POINTERMASK,x)
#define gc_PutValueFirst(p,x) Deposit(p,POINTERMASK|GC_REVERSEDMASK,x)

#if !defined(OPTIM_COMP)
#define TG_Let(X, Ptr) tagged_t *X=(Ptr); tagged_t X##val
#define TG_Val(X) X##val
#define TG_Fetch(X) ({ TG_Val(X) = *(X); })
//
#define TG_Put(V,X) ({ *(X) = (V); })
#define TG_PutPtr(p,dest) TG_Put(gc_PutValue((tagged_t)p,TG_Val(dest)), dest)
#define TG_PutPtr_SetR(curr,j) TG_Put(gc_PutValueFirst((tagged_t)curr|GC_REVERSEDMASK,TG_Val(j)), j)
#define TG_PutPtr_UnsetR(dest,j) TG_Put(Deposit((tagged_t)dest,POINTERMASK|GC_REVERSEDMASK,TG_Val(j)), j)

#define TG_SetR(X) gc_MarkF(*(X))
#define TG_UnsetR(X) gc_UnmarkF(*(X))

#define TG_SetM(X) gc_MarkM(*(X))
#define TG_IsM(X) gc_IsMarked(TG_Val(X))
#define TG_IsR(X) gc_IsFirst(TG_Val(X))
#define TG_IsROrM(X) gc_IsForM(TG_Val(X))
#define TG_UnsetM(X) gc_UnmarkM(*(X))
#define TG_SetAll_SetM(T, X) do { gc_MarkM(T); *(X) = (T); } while(0)

#define TG_MoveUNMARKED_M_UnsetM(src,dest) ({ \
  TG_Put(src, dest); \
})
#define TG_MoveValue_MoveR(val,curr) TG_Put(gc_PutValueFirst(val,TG_Val(curr)), curr)
#define TG_MoveValue_UnsetR(c1,A) TG_Put(Deposit(c1,POINTERMASK|GC_REVERSEDMASK,TG_Val(A)), A)
#endif

/* ------------------------------------------------------------------------- */

#define RelocPtr(P,Offset) ((typeof(P))((char *)(P)+(Offset)))
#define AssignRelocPtr(P,Offset) (P) = RelocPtr((P), (Offset))

// TODO: compare with OC version
#define RelocateTagged(X, FACTOR) *(X) += (FACTOR)

/* --------------------------------------------------------------------------- */
/* TagNested is a special atom used temporarily only during term
   compilation.
   
   Free variables are unified with a TagNested. It contains:
   - an index, that identifies both the assigned X register
   - a mark
*/
#define IsATM_TagNested(F) TaggedATMIsATMQ((F))
#if defined(ABSMACH_OPT__qtag)
#define IsTagNested(F) ((F) & QTAGMASK)
#else
#define IsTagNested(F) TaggedIsATMQ((F))
#endif

#define NestedGetMark(V) ((V) & NESTEDMARK__MASK)
/* Pre: NestedGetMark(V) == NESTEDMARK__SINGLEVAR */
#define NestedSetMark__USEDVARCVA(V) ((V) | NESTEDMARK__USEDVARCVA)
/* Pre: NestedGetMark(V) == NESTEDMARK__SINGLEVAR */
#define NestedSetMark__USEDVAR(X) ((X) | NESTEDMARK__USEDVAR)
#define NestedSetMark__VAL(V) ((V) | NESTEDMARK__VAL)

#define NESTEDMARK__SINGLEVAR ((intval_t)0<<tagged__nestedmark_offset)
#define NESTEDMARK__USEDVARCVA  ((intval_t)2<<tagged__nestedmark_offset)
#define NESTEDMARK__USEDVAR ((intval_t)1<<tagged__nestedmark_offset)
#define NESTEDMARK__VAL ((intval_t)3<<tagged__nestedmark_offset)
#define NESTEDMARK__MASK ((intval_t)3<<tagged__nestedmark_offset)

/* Get the value of a TagNested */
#define NestedValue(T) (((intval_t)((T)&MakeMask(tagged_t, tagged__nestedval_size, tagged__nestedval_offset)))>>tagged__nestedval_offset)
/* Add I to the value of a TagNested */
#define NestedAdd(X,I) ((X)+((I)<<tagged__nestedval_offset))
/* Special atom with value X, and mark NESTEDMARK__SINGLEVAR */
#define TagNested(X) (Tagn(ATM,(X))|QTAGMASK)

/* --------------------------------------------------------------------------- */

/* If this ordering ever changes, must update other macros */

#define HVA ((tagged_t)0)               /* heap variable */
#define CVA ((tagged_t)1)               /* constrained variable */
#define SVA ((tagged_t)2)               /* stack variable */
#define UBV ((tagged_t)3)               /* Unbound -- low bits are array index */

#define NUM ((tagged_t)4)               /* number: small integer */
#define ATM ((tagged_t)5)               /* atom: low part is atmtab index */
#define LST ((tagged_t)6)               /* list */
#define STR ((tagged_t)7)               /* structure */

/* ------------------------------------------------------------------------- */

#define IsVar(A)        ((stagged_t)(A)>=0)        /* variable tags begin with 0 */

#define TaggedIsHVA(X)     ((X) < Tagt(CVA))
#define TaggedIsCVA(X)     HasTag(X,CVA)
#define TaggedIsSVA(X)     ((stagged_t)(X) >= (stagged_t)Tagt(SVA))
/* TODO:[oc-merge] <0x900...000 vs <0xA00...000 TaggedIsNUM? */
#define TaggedIsSmall(X)   ((stagged_t)(X) < (stagged_t)(TaggedLow+QTAGMASK))
#define TaggedIsLarge(X)   (TaggedIsSTR(X) && STRIsLarge(X))
#define TaggedIsNUM(X)     ((stagged_t)(X) < (stagged_t)Tagt(ATM)) 
#define TaggedIsATM(X)     HasTag(X,ATM)
#define TaggedIsLST(X)     HasTag(X,LST)
#define TaggedIsSTR(X)     ((X) >= Tagt(STR))

#define TaggedIsStructure(X) (TaggedIsSTR(X) && !STRIsLarge(X))
#define STRIsLarge(X)   (FunctorIsBlob(TaggedToHeadfunctor(X)))

/* Assuming IsVar(X): */
#define VarIsCVA(X)     ((stagged_t)(X<<1) >= (stagged_t)(CVA<<1<<tagged__tag_offset))

/* Assuming !IsVar(X): */
#define IsNonvarAtom(X)    ((stagged_t)(X<<1) >= (stagged_t)(ATM<<1<<tagged__tag_offset))
#define TermIsLST(X)    ((stagged_t)(X<<1) < (stagged_t)(STR<<1<<tagged__tag_offset))

#define IsNonvarAtomic(X) (!(x0 & TagBitComplex) || TaggedIsLarge(x0))

/* Test for HVA, CVA, LST, STR i.e. 0, 1, 6, 7 (and LNUM)*/
/* This works for some machines, but not for others...
   #define IsHeapPtr(A)        ((stagged_t)(A)+(SVA<<tagged__tag_offset)>=0)
*/
#define IsHeapPtr(A)   ((tagged_t)(A)+Tagt(SVA) < Tagt(NUM))

#define IsHeapVar(X)    ((X) < Tagt(SVA))
#define IsAtomic(X)     ((stagged_t)(X) < (stagged_t)Tagt(LST))
#define IsComplex(X)    ((X) >= Tagt(LST))

#define TermIsAtomic(X) (IsAtomic(X) || TaggedIsLarge(X))
#define TermIsComplex(X) (IsComplex(X) && !TaggedIsLarge(X))

#define TagBitFunctor  ((tagged_t)1<<tagged__tag_offset)       /* ATM or STR or large NUM */
#define TagBitComplex  ((tagged_t)2<<tagged__tag_offset)       /* LST or STR or large NUM */

#define TagBitCVA ((tagged_t)1<<tagged__tag_offset) /* CVA (or UBV) */
#define TagBitSVA ((tagged_t)2<<tagged__tag_offset) /* SVA (or UBV) */

/* ------------------------------------------------------------------------- */
/* Term<->pointer conversion (as NUM) */

/* NOTE: pointers must be in the SMALLPTR_BASE range and they must be
   aligned to 1<<tagged__num_offset (32-bits) */
#if SMALLPTR_BASE
#define TermToPointer(T, X) ((T *)((X) ^ (TaggedZero^SMALLPTR_BASE)))
#define TermToPointerOrNull(T, X) ((T *)((X)==TaggedZero ? 0 : (X) ^ (TaggedZero^SMALLPTR_BASE)))
#define PointerToTerm(X) ((tagged_t)(X) ^ (TaggedZero^SMALLPTR_BASE))
#define PointerToTermOrZero(X)  (!(X) ? TaggedZero : (tagged_t)(X) ^ (TaggedZero^SMALLPTR_BASE))
#else
#define TermToPointer(T, X) ((T *)((X) ^ TaggedZero))
#define TermToPointerOrNull(T, X) ((T *)((X) ^ TaggedZero))
#define PointerToTerm(X) ((tagged_t)(X) ^ TaggedZero)
#define PointerToTermOrZero(X)  ((tagged_t)(X) ^ TaggedZero)
#endif

/* ------------------------------------------------------------------------- */

/* Tags + one more bit: 
   Funny objects are represented as small ints.

   Floats and integers > 26 bits are represented as structures with
   special functors. The functors have the subtag bit = 1.
   Float=NUM, integer=ATM.

   ATM = atom as index in atmtab.
*/

/* ------------------------------------------------------------------------- */

/* ERRORTAG is a tagged_t pointer guaranteed to be different from all
   tagged_t objects */
/* todo[ts]: check 0 is never used as a synonym of ERRORTAG (so that
   HVA can change) */
#define ERRORTAG Tagn(HVA,0)

/* ------------------------------------------------------------------------- */
/* SMall Integer Values */

/* Ranges for SMall Integer Value */
#define SmiValMax ((intval_t)(((uintval_t)1<<(tagged__num_size-1))-1))
#define SmiValMin ((intval_t)((uintval_t)(-1)<<(tagged__num_size-1)))
#define IsInSmiValRange(X) ((X) >= SmiValMin && (X) <= SmiValMax)
#if tagged__num_size == 32 /* TODO: assumes intval_t == int32_t in this case */
/* casting to int64_t because intval_t is not large enough */
#define SmiValMaxPlus1 ((int64_t)SmiValMax+1)
#define SmiValMinMinus1 ((int64_t)SmiValMin-1)
#else
#define SmiValMaxPlus1 (SmiValMax+1)
#define SmiValMinMinus1 (SmiValMin-1)
#endif

#define IntvalIsInSmiValRange(X) IsInSmiValRange(X)

/* ------------------------------------------------------------------------- */
/* Small integers (NUM) */

#define TaggedLow Tagp(NUM,0)
#define ZMask ((tagged_t)1<<(tagged__num_offset+tagged__num_size-1))
#define TaggedZero (TaggedLow+ZMask)
#define TaggedIntMax MakeSmall(SmiValMax)

/* A small integer */
#define MakeSmall(X) (((tagged_t)((intmach_t)(X)<<tagged__num_offset))+TaggedZero)
/* Get integer from small integer */
#define GetSmall(X) ((intmach_t)(((X)>>tagged__num_offset)-(TaggedZero>>tagged__num_offset)))
/* Difference between integer and TaggedZero */  
#define MakeSmallDiff(X) ((intmach_t)(X)<<tagged__num_offset)

#define SmallAdd(U,I) ((U)+MakeSmallDiff((I)))
#define SmallSub(U,I) ((U)-MakeSmallDiff((I)))

/* --------------------------------------------------------------------------- */

/* Get string of an atom */
#define GetString(X)    (TaggedToAtom(X)->name)

#define ABSMACH_OPT__atom_len 1

#if defined(ABSMACH_OPT__atom_len)
#define GetAtomLen(X)   (TaggedToAtom(X)->atom_len)
#else
#define GetAtomLen(X)   (strlen(GetString((X))))
#endif

/* 1 + no. untyped words */
#define LargeArity(X)   (PointerPart(X)>>tagged__atm_offset)
/* LargeArity() in bytes */
#define LargeSize(X)    ((PointerPart(X)>>tagged__atm_offset)*sizeof(tagged_t))

/* Pre: a functor; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define FunctorIsBlob(F) ((F) & QTAGMASK)
/* Pre: any tagged; Post: a functor for STR(blob(bignum)) or STR(blob(float)) */
#define BlobHF(F) ((F) & QTAGMASK)

#define BlobFunctorBignum(L) ((bignum_t)(TagIndexDiff((L))+TagIndex(ATM,1)+QTAGMASK))
#define FunctorBignumValue(T) (((T) - BlobFunctorBignum(0))>>tagged__atm_offset)

#define MakeFunctorFix   BlobFunctorBignum(1)
#define BlobFunctorFlt64 (TagIndex(NUM,3) + QTAGMASK)
#define LargeIsFloat(X)  FunctorIsFloat(TaggedToHeadfunctor(X))
#define FunctorIsFloat(X) (!((X)&TagBitFunctor))

#define MakeBlob(Ptr) make_blob(Arg,(tagged_t *)(Ptr))
#define IntmachToTagged(X) (IsInSmiValRange(X) ? MakeSmall(X) : make_integer(Arg,X))
#define IntvalToTagged(X) (IsInSmiValRange(X) ? MakeSmall(X) : make_integer(Arg,X))
#define BoxFloat(X) make_float(Arg,(X))
#define MakeAtom(X) TagIndex(ATM,X)
#define GET_ATOM(X) MakeAtom(lookup_atom_idx(X))

#define TaggedToIntmach(X) (TaggedIsSmall(X) ? GetSmall(X) : get_integer(X))
#define TaggedToFloat(X) (TaggedIsSmall(X) ? (flt64_t)GetSmall(X) : blob_to_flt64(X))

#define IsInteger(X)    (TaggedIsSmall(X) || (TaggedIsLarge(X) && !LargeIsFloat(X)))
#define IsFloat(X)      (TaggedIsLarge(X) && LargeIsFloat(X))
#define IsNumber(X)     (TaggedIsSmall(X) || TaggedIsLarge(X))
/* TODO:[oc-merge] remove IsString */
#define IsString(X)     TaggedIsATM(X)

/* (variables or numbers are not callable in Prolog) */
#define TermIsCallable(X) (!(IsVar((X)) || IsNumber((X))))

/* TODO:[oc-merge] merge */
#define FuncName(Func) (SetArity((Func)->printname, 0))
//#define FuncArity(Func) (Arity((Func)->printname))
#define FuncArity(Func) ((Func)->arity)

#if BC_SCALE==2
/* SmiVal for BC32 (for BC_SCALE==2) */
#define tagged__size_BC32 32
#define tagged__num_offset_BC32 2
#define tagged__num_size_BC32 (tagged__size_BC32 - tagged__tag_size - 1 - tagged__num_offset_BC32)
#define SmiValMax_BC32 ((intval_t)(((uintval_t)1<<(tagged__num_size_BC32-1))-1))
#define SmiValMin_BC32 ((intval_t)((uintval_t)(-1)<<(tagged__num_size_BC32-1)))
#define IsInSmiValRange_BC32(X) ((X) >= SmiValMin_BC32 && (X) <= SmiValMax_BC32)
#endif

/* internals.c */
/* TODO:[oc-merge] create versions with a generic heap ptr; not worker_t ? */
flt64_t blob_to_flt64(tagged_t t);
intmach_t get_integer(tagged_t t);
CFUN__PROTO(make_float, tagged_t, flt64_t i);
/* TODO: rename to IntmachToTagged, etc. */
CFUN__PROTO(make_integer, tagged_t, intmach_t i);
CFUN__PROTO(make_blob, tagged_t, tagged_t *ptr);
CFUN__PROTO(make_structure, tagged_t, tagged_t functor);

/* X is an Integer that fits in an intmach_t.
   This is the postcondition of IntmachToTagged.
*/ 
#define IsIntegerFix(X) (TaggedIsSmall(X) || (TaggedIsSTR(X) && TaggedToHeadfunctor(X)==MakeFunctorFix))

/* TODO: backport from optim_comp */
/* size of blob, aligned to ensure correct alignment of tagged words
   in memory */
#if defined(ALIGN_BLOB_64)
#define BlobFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), BlobFunctorSize((X)))
#define FloatFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), FloatFunctorSize((X)))
#define BignumFunctorSizeAligned(X) ALIGN_TO(sizeof(int64_t), BignumFunctorSize((X)))
#else
/* no alignment is necessary (when disabled or when blob granularity
   matchs tagged word size */
#define BlobFunctorSizeAligned(X) BlobFunctorSize((X))
#define FloatFunctorSizeAligned(X) FloatFunctorSize((X))
#define BignumFunctorSizeAligned(X) BignumFunctorSize((X))
#endif

/* Length in bytes of unboxed data */
/* TODO:[oc-merge] LargeArity is 1+no of untyped words, must substract 1 */
#define BlobFunctorSize(X) (LargeArity((X)) * sizeof(tagged_t) - sizeof(tagged_t))

/* Create and compare large numbers from bytecode.

   In BC_SCALE==2, bignums may not be normalized, which make things a
   bit more complex. */
#if BC_SCALE == 2
CFUN__PROTO(bc_make_blob, tagged_t, tagged_t *ptr);
CBOOL__PROTO(bc_eq_blob, tagged_t t, tagged_t *ptr);
#define BC_MakeBlob(ARG, Ptr) bc_make_blob(ARG,(tagged_t *)(Ptr))
#define BC_EqBlob(T, Ptr, FailCode) {                                  \
    if (!bc_eq_blob(Arg, (T), (tagged_t *)(Ptr))) FailCode;             \
  }
#else
#define BC_MakeBlob(ARG, Ptr) make_blob(ARG,(tagged_t *)(Ptr))
#define BC_EqBlob(T, Ptr, FailCode) {                                  \
    if (!TaggedIsSTR((T))) FailCode;                                    \
    for (intmach_t i=LargeArity(*(tagged_t *)(Ptr)); i>0; i--) {                  \
      if (((tagged_t *)(Ptr))[i-1] != *TaggedToArg((T),i-1)) FailCode;  \
    }                                                                   \
  }
#endif

/* tagged_t TO POINTER and related -----------------------------------*/
/* manipulating tagged_t objects removing tag and getting correct type of
   pointer  */

#define TagpPtr(T,X) ((tagged_t*)((X)-(T<<tagged__tag_offset)+SMALLPTR_BASE))

/* Functor hackery. --MC */
/*-----------------------*/

#define Arity(X)        (PointerPart(X)>>ARITYOFFSET)
#define SetArity(X,A)   ((tagged_t)(((X) & (TAGMASK | INDEXMASK)) | ((tagged_t)A<<ARITYOFFSET)))

#define TaggedToAtom(X)    (atmtab[IndexPart(X)]->value.atomp)

/* Access operations for complex tagged data */
/* finding the principal functor of a structure */
#define TaggedToHeadfunctor(X) (*TagpPtr(STR,X))
/* finding the arguments of a structure, first argument is 1 */
#define TaggedToArg(X,N) HeapOffset(TagpPtr(STR,X),N)
/* finding the car & cdr of a list. */
#define TaggedToCar(X) TagpPtr(LST,X)
#define TaggedToCdr(X) HeapOffset(TagpPtr(LST,X),1)
/* finding the constraints of a CVA. */
#define TaggedToGoal(X) HeapOffset(TagpPtr(CVA,X),1)
#define TaggedToDef(X) HeapOffset(TagpPtr(CVA,X),2)

/* --------------------------------------------------------------------------- */
/* Term deref and switch */

#define DerefHeap(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  RefHeap(m_i,Ptr); \
  DerefSw_HVAorCVAorSVA_Other(m_i,{break;},{}); \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  RefCar(m_i,Ptr); \
  DerefSw_HVAorCVAorSVA_Other(m_i,{break;},{}); \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  RefCdr(m_i,Ptr); \
  DerefSw_HVAorCVAorSVA_Other(m_i,{break;},{}); \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  tagged_t m_i; \
  m_i = *TaggedToArg(Ptr,I); \
  DerefSw_HVAorCVAorSVA_Other(m_i,{break;},{}); \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  RefHeapNext(m_i,Ptr); \
  DerefSw_HVAorCVAorSVA_Other(m_i,{break;},{}); \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  tagged_t m_i; \
  m_i = X; \
  DerefSw_HVAorCVAorSVA_Other(m_i,;,{}); \
  Xderef = m_i; \
}

#define SwStruct(F, V, CODE_STRBlob, CODE_STRStruct) ({ \
  tagged_t F = TaggedToHeadfunctor((V)); \
  if (FunctorIsBlob(F)) { \
    CODE_STRBlob; \
  } else { \
    CODE_STRStruct; \
  } \
})

#define DerefSw_HVAorCVAorSVA_Other(Reg,CODE_HVAorCVAorSVA,CODE_Other) do { \
  __label__ labelend; \
  if (IsVar(Reg)) { \
    for(;;) { \
      tagged_t Aux; \
      Aux = *TaggedToPointer(Reg); \
      if (Reg == Aux) { \
        CODE_HVAorCVAorSVA; \
        goto labelend; \
      } \
      Reg = Aux; \
      if (!IsVar(Reg)) break; \
    } \
  } \
  CODE_Other; \
labelend: {} \
} while(0)

#define DerefSw_CVA_Other(X, CODE_CVA, CODE_Other) { \
  __label__ derefsw_cva; \
  __label__ derefsw_other; \
  __label__ derefsw_end; \
  DerefSw_HVAorCVAorSVA_Other((X),{ if (VarIsCVA((X))) goto derefsw_cva; },{}); \
  goto derefsw_other; \
 derefsw_cva: \
  CODE_CVA; \
  goto derefsw_end; \
 derefsw_other: \
  CODE_Other; \
  goto derefsw_end; \
 derefsw_end: {} \
}

//TODO:[merge-oc] DerefSw_HVA_CVA_SVA_Other?
#define DerefSw_HVA_CVA_SVA_Other(Reg,HVACode,CVACode,SVACode,OtherCode) do { \
  __label__ derefsw_hva; \
  __label__ derefsw_cva; \
  __label__ derefsw_sva; \
  __label__ derefsw_other; \
  __label__ derefsw_end; \
  for (;;) { \
    Sw_HVA_CVA_SVA_Other(Reg, { \
      tagged_t aux_ = *TagpPtr(HVA,Reg); \
      if (Reg!=aux_) { Reg=aux_; } else { goto derefsw_hva; } \
    },{ \
      tagged_t aux_ = *TagpPtr(CVA,Reg); \
      if (Reg!=aux_) { Reg=aux_; } else { goto derefsw_cva; } \
    },{ \
      tagged_t aux_ = *TagpPtr(SVA,Reg); \
      if (Reg!=aux_) { Reg=aux_; } else { goto derefsw_sva; } \
    },{ \
      goto derefsw_other; \
    }); \
  } \
derefsw_hva: HVACode; goto derefsw_end; \
derefsw_cva: CVACode; goto derefsw_end; \
derefsw_sva: SVACode; goto derefsw_end; \
derefsw_other: OtherCode; goto derefsw_end; \
derefsw_end: {} \
} while(0)

#define DerefSw_HVA_CVA_Other(Reg,HVACode,CVACode,OtherCode) do { \
  __label__ derefsw_hva; \
  __label__ derefsw_cva; \
  __label__ derefsw_other; \
  __label__ derefsw_end; \
  for (;;) { \
    Sw_HVA_CVA_Other(Reg, { \
      tagged_t aux_ = *TagpPtr(HVA,Reg); \
      if (Reg!=aux_) { Reg=aux_; } else { goto derefsw_hva; } \
    },{ \
      tagged_t aux_ = *TagpPtr(CVA,Reg); \
      if (Reg!=aux_) { Reg=aux_; } else { goto derefsw_cva; } \
    },{ \
      goto derefsw_other; \
    }); \
  } \
derefsw_hva: HVACode; goto derefsw_end; \
derefsw_cva: CVACode; goto derefsw_end; \
derefsw_other: OtherCode; goto derefsw_end; \
derefsw_end: {} \
} while(0)

#define Sw_HVA_CVA_SVA_Other(T, HVACode, CVACode, SVACode, NONVARCode) do { \
  if (!IsVar((T))) { \
    NONVARCode; \
  } else { \
    if ((T) & TagBitSVA) { \
      SVACode; \
    } else if (!((T) & TagBitCVA)) { \
      HVACode; \
    } else { \
      CVACode; \
    } \
  } \
} while(0);

#define Sw_HVA_CVA_Other(T, HVACode, CVACode, NONVARCode) do { \
  if (!IsVar((T))) { \
    NONVARCode; \
  } else { \
    if (!((T) & TagBitCVA)) { \
      HVACode; \
    } else { \
      CVACode; \
    } \
  } \
} while(0);

#define Sw_HVA_SVA_CVA_NUM_ATM_LST_STR(Reg, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STR) ({ \
  switch (TagOf(Reg)) { \
  case UBV: case SVA: CODE_SVA; break; \
  case HVA: CODE_HVA; break; \
  case CVA: CODE_CVA; break; \
  case STR: CODE_STR; break; \
  case ATM: CODE_ATM; break; \
  case LST: CODE_LST; break; \
  case NUM: CODE_NUM; break; \
  } \
})

#define DerefSw_any(Reg, HeadFunctor, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRFloat, CODE_STRBignum, CODE_STRStruct) ({ \
  DEREF(Reg, Reg); \
  Sw_HVA_SVA_CVA_NUM_ATM_LST_STR(Reg, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, { \
    SwStruct(HeadFunctor, Reg, { \
      if (FunctorIsFloat(HeadFunctor)) { \
        CODE_STRFloat; \
        break; \
      } else { \
        CODE_STRBignum; \
        break; \
      } \
    }, { \
      CODE_STRStruct; \
      break; \
    }); \
  }); \
})

#define DerefSw_HVAorCVAorSVA_NUMorATM_LST_STR(Reg, CODE_HVAorCVAorSVA, CODE_NUMorATM, CODE_LST, CODE_STR) ({ \
  DerefSw_HVAorCVAorSVA_Other(Reg, { /* HVA CVA SVA */ \
    CODE_HVAorCVAorSVA; \
  }, { /* Other */ \
    Sw_NUMorATM_LST_STR(Reg, CODE_NUMorATM, CODE_LST, CODE_STR); \
  }); \
})

/* Pre: NUM ATM LST STR(blob) STR(struct) */
#define Sw_NUMorATM_LST_STR(Reg, CODE_NUMorATM, CODE_LST, CODE_STR) ({ \
  if (!(u & TagBitComplex)) { /* -LST & -STR == NUM | ATM (atomic & not large) */ \
    CODE_NUMorATM; \
  } else if (!(u & TagBitFunctor)) { /* (LST|STR) & -ATM & -STR == LST */ \
    CODE_LST; \
  } else { \
    CODE_STR; \
  } \
})

#define Sw_NUM_Large_Other(X, CODE_NUM, CODE_Large, CODE_Other) ({ \
  __label__ sw_num; \
  __label__ sw_large; \
  __label__ sw_other; \
  __label__ sw_end; \
  if (TaggedIsSmall((X))) { goto sw_num; \
  } else if (TaggedIsSTR((X))) { \
    SwStruct(f, (X), { if (!FunctorIsFloat(f)) { goto sw_large; } }, {}); \
  } \
  goto sw_other; \
 sw_num: CODE_NUM; goto sw_end; \
 sw_large: CODE_Large; goto sw_end; \
 sw_other: CODE_Other; goto sw_end; \
 sw_end: {} \
})

#define SwOnAnyTagB(Reg, HeadFunctor, CODE_HVA, CODE_SVA, CODE_CVA, CODE_NUM, CODE_ATM, CODE_LST, CODE_STRBlob, CODE_STRStruct) ({ \
  switch (TagOf(Reg)) { \
  case HVA: \
    CODE_HVA; \
    break; \
  case SVA: \
    CODE_SVA; \
    break; \
  case CVA: \
    CODE_CVA; \
    break; \
  case NUM: \
    CODE_NUM; \
    break; \
  case ATM: \
    CODE_ATM; \
    break; \
  case LST: \
    CODE_LST; \
    break; \
  case STR: \
    { \
      SwStruct(HeadFunctor, Reg, { \
        CODE_STRBlob; \
      }, { \
        CODE_STRStruct; \
      }); \
      break; \
    } \
  } \
})

// TODO:[oc-merge] use SwStruct
#define SwEval(V, HeadFunctor, NUMCode, LSTCode, BlobCode, STRCode, OtherCode) ({ \
  switch (TagOf((V))) { \
  case NUM: NUMCode; break; \
  case LST: LSTCode; break; \
  case STR: \
    if (STRIsLarge((V))) { \
      BlobCode; \
    } else { \
      tagged_t HeadFunctor = TaggedToHeadfunctor((V)); \
      STRCode; \
    } \
    break; \
  default: \
    OtherCode; break; \
  } \
})

#endif /* _CIAO_ENG_TERMS_H */
