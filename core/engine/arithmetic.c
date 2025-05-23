/*
 *  arithmetic.c
 *
 *  Arithmetic builtins
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/internals.h>
#include <ciao/eng_registry.h> /* switch_on_function */
#include <ciao/basiccontrol.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#endif

#include <math.h>
#include <limits.h>

/* Compatibility with non-clang compilers */
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

/* Signed integer multiplication with overflow checking */
#if __has_builtin(__builtin_mul_overflow)
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  if (__builtin_mul_overflow((A), (B), &P)) { OVERFLOW; } \
})
#else
#if tagged__num_size <= 32
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  int64_t R = (int64_t)(A) * (int64_t)(B); \
  if (R > INT_MAX || R < INT_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#else
#include <stdint.h>
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  __int128_t R = (__int128_t)(A) * (__int128_t)(B); \
  if (R > INT64_MAX || R < INT64_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#endif
#endif

#if tagged__num_size == 32 /* Only needed for this case */
#define SADD_OVERFLOW(A, B, P, OVERFLOW) ({ \
  int64_t R = (int64_t)(A) + (int64_t)(B); \
  if (R > INT_MAX || R < INT_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#define SSUB_OVERFLOW(A, B, P, OVERFLOW) ({ \
  int64_t R = (int64_t)(A) - (int64_t)(B); \
  if (R > INT_MAX || R < INT_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#endif

#if defined(Solaris)||defined(LINUX)||defined(EMSCRIPTEN)||defined(DARWIN)||defined(Win32)||defined(BSD)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

static inline bool_t float_is_finite(flt64_t f) {
  /* Assume IEEE comparison for floats */
  f = f - f;
  return (f == f);
}

#define GetFiniteFloat(F, U, ArgNo) ({ \
  F = blob_to_flt64(U); \
  if (!float_is_finite(F)) { \
    BUILTIN_ERROR(ERR_representation_error(nan_or_inf_to_integer), U, ArgNo); \
  } \
})

/* --------------------------------------------------------------------------- */

/* Evaluate an arithmetic expression 
   Note: evaluate return v in case of error; */
static CFUN__PROTO(evaluate, tagged_t, tagged_t v) {
 restart:
  SwEval(v, t_head_functor, { /* NUM */
    CFUN__PROCEED(v);
  }, { /* LST */
    tagged_t t;
    DerefCdr(t,v);
    if (t != atom_nil) goto err;
    DerefCar(v,v);
    goto restart;
  }, { /* STR(blob) */
    CFUN__PROCEED(v);
  }, { /* STR(struct) */
#if defined(OPTIM_COMP)
    void *proc = hashtab_get(switch_on_function,t_head_functor)->value.as_ptr;
#else
    void *proc = hashtab_get(switch_on_function,t_head_functor)->value.proc;
#endif
    if (proc==NULL) goto err;
    switch (Arity(t_head_functor)) {
    case 1:
      {
        tagged_t t = *TaggedToArg(v,1);
        CFUN__LASTCALL(((ctagged1_t)proc),t);
      }
    case 2:
      {
        tagged_t t = *TaggedToArg(v,1);
        tagged_t u = *TaggedToArg(v,2);
        CFUN__LASTCALL(((ctagged2_t)proc),t,u);
      }
    }
    goto err;
  }, { /* other */
    goto err;
  });
 err:
  CFUN__PROCEED(v);
}

/* --------------------------------------------------------------------------- */

#define DerefCheckNonvar(U,ArgNo) \
  DerefSw_HVAorCVAorSVA_Other((U), {BUILTIN_ERROR(ERR_instantiation_error, (U), (ArgNo));}, {})

#define CheckNumber(U,ArgNo) ({ \
  if (!IsNumber(U)) { \
    BUILTIN_ERROR(ERR_type_error(evaluable), (U), (ArgNo)); \
  } \
})

// TODO:[oc-merge] unused?
#define CheckInteger(U,ArgNo) ({ \
  if (!TaggedIsSmall(U)) { \
    __label__ noteval_; \
    if (!TaggedIsSTR(U)) goto noteval_; \
    tagged_t hf = TaggedToHeadfunctor(U); \
    if (!FunctorIsBlob(hf)) { \
     noteval_: /* not(NUM STR) or STRStruct */ \
      BUILTIN_ERROR(ERR_type_error(evaluable), (U), (ArgNo)); \
    } \
    if (FunctorIsFloat(hf)) { /* STR(blob(float)) */ \
      BUILTIN_ERROR(ERR_type_error(integer), (U), (ArgNo)); \
    } \
  } \
})

/* Evaluate U (if needed) and check that it has type DOM.
   Additional arguments are GC roots (if a heap overflow or GC is needed) */
#define EvalArith_GC(DOM, U, ArgNo, ...) ({ \
  if (!Is##DOM(U)) { \
    PROT_GC(U = CFUN__EVAL(evaluate, U), __VA_ARGS__); \
    Check##DOM(U, ArgNo); \
  } \
})

/* Dereference and evaluate U (assume no more GC roots) */
#define EvalArith1(DOM, U) { \
  DerefCheckNonvar(U, 1); \
  EvalArith_GC(DOM, U, 1); \
}
/* Same as above, jump to LabSmall or LabNonsmall (early if possible) */
#define EvalArith1Sw(DOM, U, LabSmall, LabNonsmall) { \
  DerefCheckNonvar(U, 1); \
  if (TaggedIsSmall(U)) goto LabSmall; \
  EvalArith_GC(DOM, U, 1); \
  if (TaggedIsSmall(U)) goto LabSmall; else goto LabNonsmall; \
}

/* Dereference and evaluate U and V (using them as GC roots) */
#define EvalArith2(DOM, U, V) { \
  DerefCheckNonvar(U, 1); \
  DerefCheckNonvar(V, 2); \
  EvalArith_GC(DOM, U, 1, V); \
  EvalArith_GC(DOM, V, 2, U); \
}
/* Same as above, jump to LabSmall or LabNonsmall (early if possible) */
#define EvalArith2Sw(DOM, U, V, LabSmall, LabNonsmall) { \
  DerefCheckNonvar(U, 1); \
  DerefCheckNonvar(V, 2); \
  if (TaggedIsSmall(U)&&TaggedIsSmall(V)) goto LabSmall; \
  EvalArith_GC(DOM, U, 1, V); \
  EvalArith_GC(DOM, V, 2, U); \
  if (TaggedIsSmall(U)&&TaggedIsSmall(V)) goto LabSmall; else goto LabNonsmall; \
}

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP)
#define ENSURE_LIVEINFO \
  RTCHECK({if (w->liveinfo == NULL) { PANIC_FAULT("null liveinfo"); }})
#else
#if defined(DEBUG_TRACE)
#define ENSURE_LIVEINFO ({ \
  if (w->liveinfo == NULL) { PANIC_FAULT("null liveinfo"); } \
})
#else
#define ENSURE_LIVEINFO
#endif
#endif

#if defined(OPTIM_COMP)
#if IntvalRequires64
/* intval_t = int64_t */
#define BIGNUM_FOR_INTVAL_SIZE sizeof(functor_t) + sizeof(int64_t)
#else
/* intval_t = int32_t */
#if defined(ALIGN_BLOB_64)
#define BIGNUM_FOR_INTVAL_SIZE sizeof(functor_t) + sizeof(int32_t) + sizeof(int32_t)
#else
#define BIGNUM_FOR_INTVAL_SIZE sizeof(functor_t) + sizeof(int32_t)
#endif
#endif
#endif

/* declare a local bignum for ENSURE_BIGNUM */
#if defined(OPTIM_COMP)
#define DECL_BIGNUM_FOR_INTVAL(XB) char XB[BIGNUM_FOR_INTVAL_SIZE]
#else
#define DECL_BIGNUM_FOR_INTVAL(XB) tagged_t XB[2]
#endif

/* pre: BN_X0 is STR(blob(bignum)) or NUM */
#if defined(OPTIM_COMP)
#define ENSURE_BIGNUM(BN_X, BN_XB, BN_XP) ({ \
  if (TaggedIsSTR((BN_X))) { \
    BN_XP = (bignum_t *)TagpPtr(STR,(BN_X)); \
  } else { /* TaggedIsSmall((BN_X)) */ \
    intval_t i; \
    char *ptr = &BN_XB[0]; \
    tagged_t *h = (tagged_t *)ptr; \
    i = GetSmall((BN_X)); \
    IntvalToBlobnm(h, i); \
    BN_XP = (bignum_t *)ptr; \
  } \
})
#else
#define ENSURE_BIGNUM(BN_X, BN_XB, BN_XP) ({ \
  if (TaggedIsSTR((BN_X))) { \
    BN_XP = TaggedToBignum((BN_X)); \
  } else { /* TaggedIsSmall((BN_X)) */ \
    BN_XB[0] = MakeFunctorFix; \
    BN_XB[1] = GetSmall((BN_X)); \
    BN_XP = (bignum_t *)BN_XB; \
  } \
})
#endif

CFUN__PROTO(bn_call2, tagged_t, bn_fun2_t f, tagged_t x0, tagged_t y0) {
  bignum_size_t req;
  DECL_BIGNUM_FOR_INTVAL(xb);
  DECL_BIGNUM_FOR_INTVAL(yb);
  bignum_t *x;
  bignum_t *y;

  if (IsFloat(x0) || IsFloat(y0)) { // TODO: only in DEBUG?
    SERIOUS_FAULT("bn_call2: called with floats");
  }

  ENSURE_BIGNUM(x0, xb, x);
  ENSURE_BIGNUM(y0, yb, y);

  ENSURE_LIVEINFO;
  req = (*f)(x, y, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC);
  if (req != 0) {
    // if (req < 0) PANIC_FAULT("infinite bignum!"); /* TODO: not needed now (captured elsewhere) */
    HeapOverflow_GC(req*sizeof(bignum_t), x0, y0);
    ENSURE_BIGNUM(x0, xb, x);
    ENSURE_BIGNUM(y0, yb, y);
    if ((*f)(x, y, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC)) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(bn_finish);
}

CFUN__PROTO(bn_call1, tagged_t, bn_fun1_t f, tagged_t x0) {
  bignum_size_t req;
  DECL_BIGNUM_FOR_INTVAL(xb);
  bignum_t *x;

  if (IsFloat(x0)) {
    SERIOUS_FAULT("bn_call1: called with floats");
  }

  ENSURE_BIGNUM(x0, xb, x);

  ENSURE_LIVEINFO;
  req = (*f)(x, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC);
  if (req != 0) {
    // if (req < 0) PANIC_FAULT("infinite bignum!"); /* TODO: not needed now (captured elsewhere) */
    HeapOverflow_GC(req*sizeof(bignum_t), x0);
    ENSURE_BIGNUM(x0, xb, x);
    if ((*f)(x, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC)) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(bn_finish);
}

CFUN__PROTO(bn_from_float_GC, tagged_t, flt64_t f) {
  bignum_size_t req;

  ENSURE_LIVEINFO;
  req = bn_from_float(f, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC);
  if (req != 0) { /* expand heap and try again */
    // if (req < 0) PANIC_FAULT("infinite bignum!"); /* TODO: not needed now (captured elsewhere) */
    HeapOverflow_GC(req*sizeof(bignum_t));
    if (bn_from_float(f, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn_GC)) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(bn_finish);
}

/* NOTE: possible heap overflow (make sure that all GC roots are visible) */
CFUN__PROTO(flt64_to_blob_GC, tagged_t, flt64_t i) {
  tagged_t *h;

  ENSURE_LIVEINFO;
  h = G->heap_top;
  TEST_HEAP_OVERFLOW(h,
                     LIVEINFO__HEAP(w->liveinfo)+FLT64_ALIGNED_BLOB_SIZE,
                     LIVEINFO__ARITY(w->liveinfo));
  HeapPush(h, BlobFunctorFlt64);
  HeapPushFlt64(h, i);
#if (!defined(OPTIM_COMP)) && LOG2_bignum_size==6 && BC_SCALE==2
  HeapPush(h, 0); /* TODO: avoid dummy word for BC_SCALE==2? (fix length?) */
#endif
  HeapPush(h, BlobFunctorFlt64);
  G->heap_top = h;
  CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -FLT64_ALIGNED_BLOB_SIZE)));
}

// // GC protected versions of int32/int64 to blob, not needed now
//
// #if defined(OPTIM_COMP)
//
// #define BLOB_CHECK_HEAP(AMOUNT) ({
//   ENSURE_LIVEINFO;
//   TEST_HEAP_OVERFLOW(h,
//                      LIVEINFO__HEAP(w->liveinfo)+(AMOUNT),
//                      LIVEINFO__ARITY(w->liveinfo));
// })
// 
// /* Create a blob from an integer type that can contain at least 1 bit
//    more than a SmiVal */
// #if tagged__num_size < 32
// /* int32_t is large enough to store SmiValMaxPlus1 or SmiValMinMinus1 */
// #define SmiVal1ToTagged_GC int32_to_blob_GC
// #else
// #define SmiVal1ToTagged_GC int64_to_blob_GC
// #endif
//
// #if IntvalRequires64 /* intval_t = int64_t */
// #define IntvalToTagged_GC int64_to_blob_GC
// #else /* intval_t = int32_t */
// #if (!Int32FitsInSmi)
// #define IntvalToTagged_GC int32_to_blob_GC
// #endif
// #endif
// 
// #if tagged__num_size < 32
// /* this code is only necessary when some int32_t are not small integers */
// CFUN__PROTO(int32_to_blob_GC, tagged_t, int32_t i) {
//   tagged_t *h;
//   h = G->heap_top;
//   BLOB_CHECK_HEAP(INT32_ALIGNED_BLOB_SIZE);
//   HeapPush(h, BlobFunctorFix32);
//   BlobPushInt32(h, i);
//   HeapPush(h, BlobFunctorFix32);
//   G->heap_top = h;
//   CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
// }
// #endif
// 
// CFUN__PROTO(int64_to_blob_GC, tagged_t, int64_t i) {
//   if (Int64FitsInInt32(i)) {
//     tagged_t *h;
//     h = G->heap_top;
//     BLOB_CHECK_HEAP(INT32_ALIGNED_BLOB_SIZE);
//     HeapPush(h, BlobFunctorFix32);
//     BlobPushInt32(h, (int32_t)i);
//     HeapPush(h, BlobFunctorFix32);
//     G->heap_top = h;
//     CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT32_ALIGNED_BLOB_SIZE)));
//   } else {
//     tagged_t *h;
//     h = G->heap_top;
//     BLOB_CHECK_HEAP(INT64_ALIGNED_BLOB_SIZE);
//     HeapPush(h, BlobFunctorFix64);
//     BlobPushInt64(h, i);
//     HeapPush(h, BlobFunctorFix64);
//     G->heap_top = h;
//     CFUN__PROCEED(Tagp(STR, HeapCharOffset(h, -INT64_ALIGNED_BLOB_SIZE)));
//   }
// }
//
// #else
//
// /* Tagged from SmiVal1 (SmiVal plus at least 1 bit) */
// #define SmiVal1ToTagged_GC intmach_to_blob_GC
//
// /* NOTE: possible heap overflow (make sure that all GC roots are visible) */
// CFUN__PROTO(intmach_to_blob_GC, tagged_t, intmach_t i) {
//   tagged_t *h;
// 
//   if (IsInSmiValRange(i)) return MakeSmall(i);
// 
//   ENSURE_LIVEINFO;
//   h = G->heap_top;
//   if (HeapCharDifference(h, Heap_End) < (intmach_t)LIVEINFO__HEAP(w->liveinfo)+3*sizeof(tagged_t)) {
//     explicit_heap_overflow(Arg, 2*(LIVEINFO__HEAP(w->liveinfo)+3*sizeof(tagged_t)), (short)LIVEINFO__ARITY(w->liveinfo));
//     h = G->heap_top;
//   }
//   G->heap_top = h+3;
//   HeapPush(h, MakeFunctorFix);
//   HeapPush(h, (tagged_t)i);
//   HeapPush(h, MakeFunctorFix);
//   return Tagp(STR, h-3);
// }
// #endif

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=:=", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t==u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)==TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__FAIL;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))==0);
  }
}

CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=\\=", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t!=u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)!=TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__PROCEED;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))!=0);
  }
}

CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:<", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t<u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)<TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))<0);
  }
}

CBOOL__PROTO(bu2_numle, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=<", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t<=u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)<=TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))<=0);
  }
}

CBOOL__PROTO(bu2_numgt, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:>", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t>u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)>TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))>0);
  }
}

CBOOL__PROTO(bu2_numge, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:>=", 2);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  CBOOL__LASTTEST(t>=u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)>=TaggedToFloat(u));
  } else if (TaggedIsSmall(t)) {
    CBOOL__LASTTEST(!bn_positive(TaggedToBignum(u)));
  } else if (TaggedIsSmall(u)) {
    CBOOL__LASTTEST(bn_positive(TaggedToBignum(t)));
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))>=0);
  }
}

/* --------------------------------------------------------------------------- */

/* Code blocks to negate the number U (CFUN__LASTCALL version) */
#define CFUN__LASTCALL_NumberNegate(LabSmall, LabNonsmall, U) { \
 __label__ big_;  \
 LabSmall: \
  if (U==TaggedLow) goto big_;/*CFUN__LASTCALL(SmiVal1ToTagged_GC, SmiValMaxPlus1);*/ \
  CFUN__PROCEED(TaggedZero-(U-TaggedZero)); \
 big_: \
  CFUN__LASTCALL(bn_call1,bn_minus,U); \
 LabNonsmall: \
  if (IsFloat(U)) { \
    CFUN__LASTCALL(flt64_to_blob_GC, -TaggedToFloat(U)); \
  } else { \
    goto big_; \
  } \
}

/* Code blocks to negate the integer U */
#define IntegerNegate_GC(LabSmall, LabNonsmall, U, ...) { \
  __label__ big_; \
  __label__ end_; \
LabSmall: \
  if (U==TaggedLow) goto big_;/*PROT_GC(U = CFUN__EVAL(SmiVal1ToTagged_GC, SmiValMaxPlus1), __VA_ARGS__);*/ \
  U = TaggedZero-(U-TaggedZero); \
  goto end_; \
LabNonsmall: \
big_: \
  PROT_GC(U = CFUN__EVAL(bn_call1,bn_minus,U), __VA_ARGS__); \
  goto end_; \
end_: {} \
}

CFUN__PROTO(fu1_minus, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$-", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);
  CFUN__LASTCALL_NumberNegate(small, nonsmall, t);
}

CFUN__PROTO(fu1_plus, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$+", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__PROCEED(t);
}

CFUN__PROTO(fu1_integer, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$integer", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  CFUN__PROCEED(t);

 nonsmall:
  if (IsFloat(t)) {
    flt64_t f;
    GetFiniteFloat(f, t, 1);
    CFUN__LASTCALL(bn_from_float_GC, f);
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_float, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$float", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  if (IsFloat(t)) {
    CFUN__PROCEED(t);
  } else {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t));
  }
}

/* --------------------------------------------------------------------------- */

CFUN__PROTO(fu1_add1, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$++", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  if (t==TaggedIntMax) goto big;//CFUN__LASTCALL(SmiVal1ToTagged_GC, SmiValMaxPlus1);
  CFUN__PROCEED(SmallAdd(t, 1));

 big:
  CFUN__LASTCALL(bn_call1,bn_incr,t);

 nonsmall:
  if (IsFloat(t)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) + 1.0);
  } else {
    goto big;
  }
}

CFUN__PROTO(fu1_sub1, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$--", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  if (t==TaggedLow) goto big;//CFUN__LASTCALL(SmiVal1ToTagged_GC, SmiValMinMinus1);
  CFUN__PROCEED(SmallSub(t, 1));

 big:
  CFUN__LASTCALL(bn_call1,bn_decr,t);

 nonsmall:
  if (IsFloat(t)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) - 1.0);
  } else {
    goto big;
  }
}

/* R=U+V on tagged small integers, jump to LabOverflow if overflow */
#if (!defined(OPTIM_COMP))||defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
/* val bits overflow into the tag bits */
/* Note: with qtag, ensure that QTAGMASK is 0 */
#define SmiPlus(U, V, R, LabOverflow) ({ \
  R = U+(V-TaggedZero); /* optimistically add */ \
  if (!TaggedIsSmall(R)) goto LabOverflow; \
})
#elif tagged__num_size == 32
/* int64_t required to detect overflows (intval_t==int32_t) */
#define SmiPlus(U, V, R, LabOverflow) ({ \
  intmach_t r_; \
  SADD_OVERFLOW(GetSmall(U), GetSmall(V), r_, { goto LabOverflow; }); \
  R = MakeSmall(r_); \
})
#else
#define SmiPlus(U, V, R, LabOverflow) ({ \
  intval_t r_ = GetSmall(U) + GetSmall(V); \
  if (!IntvalIsInSmiValRange(r_)) goto LabOverflow; \
  R = MakeSmall(r_); \
})
#endif

CFUN__PROTO(fu2_plus, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$+", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  {
    tagged_t t1;
    SmiPlus(t, u, t1, big);
    CFUN__PROCEED(t1);
  }

 big:
  CFUN__LASTCALL(bn_call2,bn_add,t,u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) + TaggedToFloat(u));
  } else {
    goto big;
  }
}

/* R=U-V on tagged small integers, jump to LabOverflow if overflow */
#if (!defined(OPTIM_COMP))||defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
/* val bits overflow into the tag bits */
/* Note: with qtag, ensure that QTAGMASK is 0 */
#define SmiMinus(U, V, R, LabOverflow) ({ \
  R = U-(V-TaggedZero); /* optimistically add */ \
  if (!TaggedIsSmall(R)) goto LabOverflow; \
})
#elif tagged__num_size == 32
/* int64_t required to detect overflows (intval_t==int32_t) */
#define SmiMinus(U, V, R, LabOverflow) ({ \
  intmach_t r_; \
  SSUB_OVERFLOW(GetSmall(U), GetSmall(V), r_, { goto LabOverflow; }); \
  R = MakeSmall(r_); \
})
#else
#define SmiMinus(U, V, R, LabOverflow) ({ \
  intval_t r_ = GetSmall(U) - GetSmall(V); \
  if (!IntvalIsInSmiValRange(r_)) goto LabOverflow; \
  R = MakeSmall(r_); \
})
#endif

CFUN__PROTO(fu2_minus, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$-", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  {
    tagged_t t1;
    SmiMinus(t, u, t1, big);
    CFUN__PROCEED(t1);
  }

 big:
  CFUN__LASTCALL(bn_call2,bn_subtract,t,u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) - TaggedToFloat(u));
  } else {
    goto big;
  }
}

#if (!defined(OPTIM_COMP))||defined(VAL_OVERFLOWS_INTO_TAG_OR_QTAG)
/* val bits overflow into the tag bits */
#define SmiTimes(U, V, R, LabOverflow) ({ \
  intval_t u_ = GetSmall(U); \
  intval_t v_ = (intval_t)(V-TaggedZero); \
  intval_t r_; \
  SMUL_OVERFLOW(u_, v_, r_, { goto LabOverflow; }); \
  R = ((tagged_t)r_)+TaggedZero; \
  if (!TaggedIsSmall(R)) goto LabOverflow; \
})
#else
#define SmiTimes(U, V, R, LabOverflow) ({ \
  intval_t u_ = GetSmall(U); \
  intval_t v_ = GetSmall(V); \
  intval_t r_; \
  SMUL_OVERFLOW(u_, v_, r_, { goto LabOverflow; }); \
  if (!IntvalIsInSmiValRange(r_)) goto big;/*CFUN__LASTCALL(SmiVal1ToTagged_GC, r_);*/ \
  R = MakeTagged(r_); \
})
#endif

CFUN__PROTO(fu2_times, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$*", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Number, t, u, small, nonsmall);

 small:
  {
    tagged_t tu;
    SmiTimes(t, u, tu, big);
    CFUN__PROCEED(tu);
  }

 big:
  CFUN__LASTCALL(bn_call2,bn_multiply,t,u);

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) * TaggedToFloat(u));
  } else {
    goto big;
  }
}

CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$/", 3);
  tagged_t t=x0,u=x1;
  EvalArith2(Number, t, u);
  CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t)/TaggedToFloat(u));
}

CFUN__PROTO(fu2_idivide, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$//", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  {
#if tagged__num_size == 32 /* Only needed for this case */
    if (GetSmall(u) == MakeSmall(-1) && GetSmall(t) == INT_MIN) goto nonsmall;
#endif
    if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);
    intval_t r = (intval_t)(t-TaggedZero)/(intval_t)(u-TaggedZero);
    if (!IntvalIsInSmiValRange(r)) goto nonsmall;//CFUN__LASTCALL(SmiVal1ToTagged_GC, r);
    CFUN__PROCEED(MakeSmall(r));
  }

 nonsmall:
  if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);
  /*bn_quotient_wanted = TRUE;*/
  CFUN__LASTCALL(bn_call2,bn_quotient_remainder_quot_wanted,t,u);
}

CFUN__PROTO(fu2_rem, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$rem", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);  
  CFUN__PROCEED((intval_t)(t-TaggedZero)%(intval_t)(u-TaggedZero)+TaggedZero);

 nonsmall:
  if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);
  /*bn_quotient_wanted = FALSE;*/
  CFUN__LASTCALL(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u);
}

CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0);

CFUN__PROTO(fu2_mod, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$mod", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  {
    intval_t rem, denom;
    if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);
    denom = (intval_t)(u-TaggedZero);
    rem = (intval_t)(t-TaggedZero)%denom;
    if ((denom > 0 && rem < 0) || (denom < 0 && rem > 0)) {
      CFUN__PROCEED(rem+denom+TaggedZero);
    } else {
      CFUN__PROCEED(rem+TaggedZero);
    }
  }

 nonsmall:
  {
    tagged_t T_rem;
    if (u == TaggedZero) BUILTIN_ERROR(ERR_evaluation_error(zero_divisor), u, 1);
    /*bn_quotient_wanted = FALSE;*/
    PROT_GC(T_rem = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u), u);
    if (T_rem != TaggedZero && CFUN__EVAL(fu1_sign,u) != CFUN__EVAL(fu1_sign,T_rem)) {
      /* Note: GC safe, fu1_sign above always return small ints! */
      CFUN__LASTCALL(bn_call2,bn_add,T_rem,u);
    } else {
      CFUN__PROCEED(T_rem);
    }
  }
}

/* Jump to NegSmall/NegNonsmall if number U is negative */
#define BranchNumberIsNeg(U, NegSmall, NegNonsmall) ({ \
  if (TaggedIsSmall(U)) { \
    if (U < TaggedZero) goto NegSmall; \
  } else { \
    if (IsFloat(U)) { \
      if (TaggedToFloat(U) < 0.0) goto NegNonsmall; \
    } else { \
      if (!bn_positive(TaggedToBignum(U))) goto NegNonsmall; \
    } \
  } \
})

/* Jump to NegSmall/NegNonsmall if integer U is negative */
#define BranchIntegerIsNeg(U, NegSmall, NegNonsmall) ({ \
  if (TaggedIsSmall(U)) { \
    if (U < TaggedZero) goto NegSmall; \
  } else { \
    if (!bn_positive(TaggedToBignum(U))) goto NegNonsmall; \
  } \
})

CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sign", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  if (t==TaggedZero) {
    CFUN__PROCEED(TaggedZero);
  } else if (t < TaggedZero) {
    CFUN__PROCEED(SmallSub(TaggedZero,1));
  } else {
    CFUN__PROCEED(SmallAdd(TaggedZero,1));
  }

 nonsmall:
  if (IsFloat(t)) {
    flt64_t f = TaggedToFloat(t);
    if (f == 0.0) {
      CFUN__PROCEED(t);
    } else if (f < 0.0) {
      CFUN__LASTCALL(flt64_to_blob_GC, -1.0);
    } else {
      CFUN__LASTCALL(flt64_to_blob_GC, 1.0);
    }
  } else {
    if (!bn_positive(TaggedToBignum(t))) {
      CFUN__PROCEED(SmallSub(TaggedZero,1));
    } else {
      CFUN__PROCEED(SmallAdd(TaggedZero,1));
    }
  }
}

/* Absolute value of an integer (smi or bignum) */
#define IntegerAbs_GC(U, ...) ({ \
  __label__ neg_small_; \
  __label__ neg_nonsmall_; \
  __label__ end_; \
  BranchIntegerIsNeg(U, neg_small_, neg_nonsmall_); \
  goto end_; \
  IntegerNegate_GC(neg_small_, neg_nonsmall_, U, __VA_ARGS__); \
end_: {} \
})

CFUN__PROTO(fu1_abs, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$abs", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);

  BranchNumberIsNeg(t, neg_small, neg_nonsmall);
  CFUN__PROCEED(t);
  /* (declares labels) */
  CFUN__LASTCALL_NumberNegate(neg_small, neg_nonsmall, t);
}

CFUN__PROTO(fu1_not, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$\\", 2);
  tagged_t t=x0;
  EvalArith1Sw(Integer, t, small, nonsmall);

 small:
  CFUN__PROCEED(t^(TaggedIntMax-TaggedLow));

 nonsmall:
  CFUN__LASTCALL(bn_call1,bn_not,t);
}

CFUN__PROTO(fu2_xor, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$#", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  CFUN__PROCEED(t^u^TaggedZero);

 nonsmall:
  CFUN__LASTCALL(bn_call2,bn_xor,t,u);
}

CFUN__PROTO(fu2_and, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$/\\", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  CFUN__PROCEED(((t^ZMask)&(u^ZMask))^ZMask);

 nonsmall:
  CFUN__LASTCALL(bn_call2,bn_and,t,u);
}

CFUN__PROTO(fu2_or, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$\\/", 3);
  tagged_t t=x0,u=x1;
  EvalArith2Sw(Integer, t, u, small, nonsmall);

 small:
  CFUN__PROCEED(((t^ZMask)|(u^ZMask))^ZMask);

 nonsmall:
  CFUN__LASTCALL(bn_call2,bn_or,t,u);
}

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  if (TaggedIsSmall(t)) goto small; else goto nonsmall;

 small:
  {
#if (!defined(OPTIM_COMP))||defined(ABSMACH_OPT__qtag)
    /* note: this code have strong dependencies to the traditional tag
       scheme in Ciao */
    tagged_t u;
    switch (dist) {
    case 0:
      CFUN__PROCEED(t);
    case 1:
      u = (t<<1) + TaggedZero - (TaggedZero<<1); //0x78000000
      break;
    case 2:
      u = (t<<2) + TaggedZero - (TaggedZero<<2); //0x68000000
      break;
    case 3:
      u = (t<<3) + TaggedZero - (TaggedZero<<3); //0x48000000
      break;
    case 4:
      u = (t<<4) + TaggedZero - (TaggedZero<<4); //0x08000000
      break;
    default:
      goto nonsmall;
    }
    if (!TaggedIsSmall(u)) goto nonsmall;
    CFUN__PROCEED(u);
#else
    intval_t value = GetSmall(t);

    if (!(dist<tagged__num_size &&
          ((value>=0 && value < (((uintval_t)1)<<(tagged__num_size-1-dist))) ||
           (value<0 && value >= (((intval_t)-1)<<(tagged__num_size-1-dist)))))) goto nonsmall;
    /* ok, no overflow (for a intval_t type) after shift */
    intval_t r = value<<dist;
    if (!IntvalIsInSmiValRange(r)) goto nonsmall;//CFUN__LASTCALL(SmiVal1ToTagged_GC, r);
    CFUN__PROCEED(MakeSmall(r));
#endif
  }

 nonsmall:
  CFUN__LASTCALL(bn_call2, bn_lshift, t, IntmachToTagged(dist)); /* TODO: avoid IntmachToTagged, add a new bn_call2? */
}

static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  if (TaggedIsSmall(t)) goto small; else goto nonsmall;

 small:  
  if (dist>=tagged__num_size) {
    CFUN__PROCEED(MakeSmall((t>=TaggedZero)-1));
  } else {
#if (!defined(OPTIM_COMP))
    CFUN__PROCEED(((intmach_t)((t>>dist)-(TaggedZero>>dist)) & (-MakeSmallDiff(1))) + TaggedZero);
#elif defined(ABSMACH_OPT__qtag)
    CFUN__PROCEED(((intval_t)((t>>dist)-(TaggedZero>>dist)) & (-SmallAdd(0,1))) + TaggedZero);
#else
    CFUN__PROCEED(MakeSmall(GetSmall(t)>>dist));
#endif
  }

 nonsmall:
  CFUN__LASTCALL(bn_call2, bn_rshift, t, IntmachToTagged(dist)); /* TODO: avoid IntmachToTagged, add a new bn_call2? */
}

/* TaggedToIntmach but saturates to INTMACH_MIN or INTMACH_MAX if not
   representable into an intmach_t */
static inline intmach_t tagged_to_intmach_saturated(tagged_t u) {
  if (IsIntegerFix(u)) {
    return TaggedToIntmach(u);
  } else { /* assume STR(blob(bignum)) */
    if (bn_positive(TaggedToBignum(u))) {
      return INTMACH_MAX;
    } else {
      return INTMACH_MIN;
    }
  }
}

/*
   For shifting operations we assume it is not possible to allocate
   enough memory to represent number whose absolute value is larger
   than 2^(INTMACH_MAX). Therefore shifting to the left any number by
   more than INTMACH_MAX throws a memory overflow exception and
   shifting to the right any number by more than INTMACH_MAX returns
   0. Note that INTMACH_MIN = -INTMACH_MAX-1.
*/
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$<<", 3);
  tagged_t t=x0,u=x1;
  EvalArith2(Integer, t, u);

  intmach_t dist = tagged_to_intmach_saturated(u); /* saturate shifts to integer max/min */
  if (dist<0) {
    dist = (dist == INTMACH_MIN) ? INTMACH_MAX : (-dist); /* saturated negation */
    CFUN__LASTCALL(rsh_internal,t,dist);
  } else {
    if (dist == INTMACH_MAX) {
      if (t == TaggedZero) CFUN__PROCEED(TaggedZero);
      BUILTIN_ERROR(ERR_resource_error(r_stack), u, 2); 
    }
    CFUN__LASTCALL(lsh_internal,t,dist);
  }
}

CFUN__PROTO(fu2_rsh, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$>>", 3);
  tagged_t t=x0,u=x1;
  EvalArith2(Integer, t, u);

  intmach_t dist = tagged_to_intmach_saturated(u); /* saturate shifts to integer max/min */
  if (dist<0) {
    dist = (dist == INTMACH_MIN) ? INTMACH_MAX : (-dist); /* saturated negation */
    if (dist == INTMACH_MAX) {
      if (t == TaggedZero) CFUN__PROCEED(TaggedZero);
      BUILTIN_ERROR(ERR_resource_error(r_stack), u, 2); 
    }
    CFUN__LASTCALL(lsh_internal,t,dist);
  } else {
    CFUN__LASTCALL(rsh_internal,t,dist);
  }
}

/* GCD for rat arithm., ch feb 92 */
CFUN__PROTO(fu2_gcd, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  tagged_t u=x0,v=x1;
  EvalArith2(Integer, u, v);

  IntegerAbs_GC(u, v);
  IntegerAbs_GC(v, u);

  int type = 0;
  if (!TaggedIsSmall(u)) type += 2; /* u is bignum */
  if (!TaggedIsSmall(v)) type += 1; /* v is bignum */

  if (u==TaggedZero) CFUN__PROCEED(v);
  if (v==TaggedZero) CFUN__PROCEED(u);
  /*bn_quotient_wanted = FALSE;*/

 again:
  switch (type) { /* u x v */
  case 0: /* small x small */
  small_x_small: {
      uintval_t x = GetSmall(u);
      uintval_t y = GetSmall(v);
      for (;;) {
        x = x % y; if (x==0) CFUN__PROCEED(MakeSmall(y));
        y = y % x; if (y==0) CFUN__PROCEED(MakeSmall(x));
      }
    }
  case 1: /* small x big */
    PROT_GC(v = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,v,u), u);
    if (v==TaggedZero) CFUN__PROCEED(u);
    goto small_x_small;
  case 2: /* big x small */
    PROT_GC(u = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,u,v), v);
    if (u==TaggedZero) CFUN__PROCEED(v);
    goto small_x_small;
  default: /* big x big */
    PROT_GC(u = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,u,v), v);
    if (u==TaggedZero) CFUN__PROCEED(v);
    if (TaggedIsSmall(u)) type -= 2; /* now u is small */
    PROT_GC(v = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,v,u), u);
    if (v==TaggedZero) CFUN__PROCEED(u);
    if (TaggedIsSmall(v)) type -= 1; /* now v is small */
    goto again;
  }
} 

CFUN__PROTO(fu1_intpart, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$float_integer_part", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  flt64_t f = TaggedToFloat(t);
  CFUN__LASTCALL(flt64_to_blob_GC, aint(f));
}

CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$float_fractional_part", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  flt64_t f = TaggedToFloat(t);
  CFUN__LASTCALL(flt64_to_blob_GC, f-aint(f));
}

CFUN__PROTO(fu1_floor, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$floor", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  CFUN__PROCEED(t);

 nonsmall:
  if (IsFloat(t)) {
    flt64_t f;
    GetFiniteFloat(f, t, 1);
    CFUN__LASTCALL(bn_from_float_GC, floor(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_round, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$round", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  CFUN__PROCEED(t);

 nonsmall:
  if (IsFloat(t)) {
    flt64_t f;
    GetFiniteFloat(f, t, 1);
    CFUN__LASTCALL(bn_from_float_GC, round(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_ceil, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$ceiling", 2);
  tagged_t t=x0;
  EvalArith1Sw(Number, t, small, nonsmall);

 small:
  CFUN__PROCEED(t);

 nonsmall:
  if (IsFloat(t)) {
    flt64_t f;
    GetFiniteFloat(f, t, 1);
    CFUN__LASTCALL(bn_from_float_GC, ceil(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu2_pow, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$**", 3);
  tagged_t t=x0,u=x1;
  EvalArith2(Number, t, u);
  CFUN__LASTCALL(flt64_to_blob_GC, pow(TaggedToFloat(t),TaggedToFloat(u)));
}

CFUN__PROTO(fu1_exp, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$exp", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, exp(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_log, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$log", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, log(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sqrt", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, sqrt(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_sin, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sin", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, sin(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_cos, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$cos", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, cos(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_atan, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$atan", 2);
  tagged_t t=x0;
  EvalArith1(Number, t);
  CFUN__LASTCALL(flt64_to_blob_GC, atan(TaggedToFloat(t)));
}

/* --------------------------------------------------------------------------- */
/* TODO:[JF] New arithmetic functions, integrated into is/2 ! */

CBOOL__PROTO(prolog_lsb) {
  ERR__FUNCTOR("arithmetic:$lsb", 2);
  int r;
  DEREF(X(0),X(0));
  Sw_NUM_Large_Other(X(0), { goto small; }, { goto nonsmall; }, {});
  ERROR_IN_ARG(X(0),1,ERR_type_error(integer));
 small: r = intval_LSB(GetSmall(X(0))); goto ok;
 nonsmall: r = bn_lsb(TaggedToBignum(X(0))); goto ok;
 ok: CBOOL__LASTUNIFY(X(1), MakeSmall(r)); /* TODO:[JF] assume 'r' is small */
}

CBOOL__PROTO(prolog_msb) {
  ERR__FUNCTOR("arithmetic:$msb", 2);
  int r;
  DEREF(X(0),X(0));
  Sw_NUM_Large_Other(X(0), { goto small; }, { goto nonsmall; }, {});
  ERROR_IN_ARG(X(0),1,ERR_type_error(integer));
 small: r = intval_MSB(GetSmall(X(0))); goto ok;
 nonsmall: r = bn_msb(TaggedToBignum(X(0))); goto ok;
 ok: CBOOL__LASTUNIFY(X(1), MakeSmall(r)); /* TODO:[JF] assume 'r' is small */
}

CBOOL__PROTO(prolog_popcount) {
  ERR__FUNCTOR("arithmetic:$popcount", 2);
  int r;
  DEREF(X(0),X(0));
  Sw_NUM_Large_Other(X(0), { goto small; }, { goto nonsmall; }, {});
  ERROR_IN_ARG(X(0),1,ERR_type_error(integer));
 small: r = intval_POPCOUNT(GetSmall(X(0))); goto ok;
 nonsmall: r = bn_popcount(TaggedToBignum(X(0))); goto ok;
 ok: CBOOL__LASTUNIFY(X(1), MakeSmall(r)); /* TODO:[JF] assume 'r' is small */
}

// '$getbit'(V,I,R) :- R is (V>>I)/\1
CBOOL__PROTO(prolog_getbit) {
  ERR__FUNCTOR("arithmetic:$getbit", 3);
  int r;
  // TODO: use "EvalArith2(Integer, t, u);"
  DEREF(X(1),X(1));
  if (!TaggedIsSmall(X(1))) goto zero; // TODO: 0 if large, errors if not integer
  int i = GetSmall(X(1));
  if (i < 0) goto zero;
  //
  DEREF(X(0),X(0));
  Sw_NUM_Large_Other(X(0), { goto small; }, { goto nonsmall; }, {});
  ERROR_IN_ARG(X(0),1,ERR_type_error(integer));
 small: r = (GetSmall(X(0)) >> i) & 1; goto ok;
 nonsmall: r = bn_getbit(TaggedToBignum(X(0)), i); goto ok;
 ok: CBOOL__LASTUNIFY(X(2), MakeSmall(r));
 zero: CBOOL__LASTUNIFY(X(2), MakeSmall(0));
}
