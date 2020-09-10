/*
 *  arithmetic.c
 *
 *  Arithmetic builtins ('call_builtin' procedures)
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/internals.h>
#include <ciao/eng_registry.h> /* switch_on_function */
#include <ciao/arithmetic.h>
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

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(BSD)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

static inline bool_t float_is_finite(flt64_t f) {
  /* Assume IEEE comparison for floats */
  f = f - f;
  return (f == f);
}

/* --------------------------------------------------------------------------- */

/* Evaluate an arithmetic expression 
   Note: evaluate return v in case of error; */
static CFUN__PROTO(evaluate, tagged_t, tagged_t v) {
  tagged_t t_head_functor;
 restart:
  switch (TagOf(v)) {
  case NUM:
    CFUN__PROCEED(v);
  case LST:
    {
      tagged_t t;
      DerefCdr(t,v);
      if (t!=atom_nil) goto err;
      DerefCar(v,v);
      goto restart;
    }
  case STR:
    if (STRIsLarge(v)) {
      CFUN__PROCEED(v);
    }
    t_head_functor = TagToHeadfunctor(v);
    {
      void *proc = incore_gethash(switch_on_function,t_head_functor)->value.proc;
      if (proc!=NULL) {
        switch (Arity(t_head_functor)) {
        case 1:
          {
            tagged_t t;
            RefArg(t,v,1);
            CFUN__LASTCALL(((ctagged1l_t)proc),t);
          }
        case 2:
          {
            tagged_t t, u;
            RefArg(t,v,1);
            RefArg(u,v,2);
            CFUN__LASTCALL(((ctagged2l_t)proc),t,u);
          }
        }
      }
    }
  default:
    goto err;
  }
 err:
  CFUN__PROCEED(v);
}

/* --------------------------------------------------------------------------- */

#define CheckNumber(Reg,ArgNo) ({ \
  if (!IsNumber(Reg)) { \
    BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo)); \
  } \
})

#define CheckInteger(Reg,ArgNo) ({ \
  if (!TaggedIsSmall(Reg)) { \
    if (!TaggedIsLarge(Reg)) { \
      BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo)); \
    } \
    if (LargeIsFloat(Reg)) { \
      BUILTIN_ERROR(TYPE_ERROR(INTEGER), (Reg), (ArgNo)); \
    } \
  } \
})

/* Dereference (and evaluate if needed) Reg and check that it has type DOM.
   Additional arguments are GC roots (if a heap overflow or GC is needed) */
#define NDEREF_GC(DOM, Reg, ArgNo, ...) ({ \
  tagged_t t1_; \
  DerefSwitch(Reg,t1_, BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));); \
  if (!Is##DOM(Reg)) { \
    PROT_GC(Reg = CFUN__EVAL(evaluate, Reg), __VA_ARGS__); \
    Check##DOM(Reg, ArgNo); \
  } \
})

/* --------------------------------------------------------------------------- */

#if defined(DEBUG)
#define ENSURE_LIVEINFO ({ \
  if (w->liveinfo == NULL) { \
    fprintf(stderr, "PANIC: null liveinfo\n"); _EXIT(-1); \
  } \
})
#else
#define ENSURE_LIVEINFO
#endif

CFUN__PROTO(make_integer_check, tagged_t, intmach_t i);
CFUN__PROTO(flt64_to_blob_GC, tagged_t, flt64_t i);

#define DECL_BIGNUM_FOR_INTVAL(XB) tagged_t XB[2]

/* pre: BN_X0 is STR(blob(bignum)) or NUM */
#define ENSURE_BIGNUM(BN_X, BN_XB, BN_XP) ({ \
  if (TaggedIsSTR((BN_X))) { \
    BN_XP = TaggedToBignum((BN_X)); \
  } else { /* TaggedIsSmall((BN_X)) */ \
    BN_XB[0] = MakeFunctorFix; \
    BN_XB[1] = GetSmall((BN_X)); \
    BN_XP = (bignum_t *)BN_XB; \
  } \
})

CFUN__PROTO(bn_call2, tagged_t, bn_fun2_t f, tagged_t x0, tagged_t y0) {
  bignum_size_t req;
  DECL_BIGNUM_FOR_INTVAL(xb);
  DECL_BIGNUM_FOR_INTVAL(yb);
  bignum_t *x;
  bignum_t *y;

  if (IsFloat(x0) || IsFloat(y0)) {
    SERIOUS_FAULT("bn_call2: called with floats");
  }

  ENSURE_BIGNUM(x0, xb, x);
  ENSURE_BIGNUM(y0, yb, y);

  ENSURE_LIVEINFO;
  req = (*f)(x, y, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)));
  if (req != 0) {
    HeapOverflow_GC(req, x0, y0);
    ENSURE_BIGNUM(x0, xb, x);
    ENSURE_BIGNUM(y0, yb, y);
    if ((*f)(x, y, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)))) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(FinishInt);
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
  req = (*f)(x, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)));
  if (req != 0) {
    HeapOverflow_GC(req, x0);
    ENSURE_BIGNUM(x0, xb, x);
    if ((*f)(x, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)))) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(FinishInt);
}

CFUN__PROTO(bn_from_float_check, tagged_t, flt64_t f) {
  bignum_size_t req;

  ENSURE_LIVEINFO;
  req = bn_from_float(f, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)));
  if (req != 0) { /* expand heap and try again */
    HeapOverflow_GC(req);
    if (bn_from_float(f, (bignum_t *)w->global_top, (bignum_t *)(Heap_End-LIVEINFO__HEAP(w->liveinfo)))) {
      SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  CFUN__LASTCALL(FinishInt);
}

/* NOTE: possible heap overflow (make sure that all GC roots are visible) */
CFUN__PROTO(make_integer_check, tagged_t, intmach_t i) {
  tagged_t *h;

  if (IsInSmiValRange(i)) return MakeSmall(i);

  ENSURE_LIVEINFO;
  h = w->global_top;
  if (HeapDifference(h, Heap_End) < (intmach_t)LIVEINFO__HEAP(w->liveinfo)+3) {
    explicit_heap_overflow(Arg, LIVEINFO__HEAP(w->liveinfo)+3, (short)LIVEINFO__ARITY(w->liveinfo));
    h = w->global_top;
  }
  w->global_top = h+3;
  HeapPush(h, MakeFunctorFix);
  HeapPush(h, (tagged_t)i);
  HeapPush(h, MakeFunctorFix);
  return Tag(STR, h-3);
}

/* NOTE: possible heap overflow (make sure that all GC roots are visible) */
CFUN__PROTO(flt64_to_blob_GC, tagged_t, flt64_t i) {
  tagged_t *h;
  union {
    flt64_t i;
    tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
  } u;

  ENSURE_LIVEINFO;
  h = w->global_top;
  if (HeapDifference(h, Heap_End) < (intmach_t)LIVEINFO__HEAP(w->liveinfo)+4) {
    explicit_heap_overflow(Arg,LIVEINFO__HEAP(w->liveinfo)+4, (short)LIVEINFO__ARITY(w->liveinfo));
    h = w->global_top;
  }
  w->global_top = h+4;

  HeapPush(h, MakeFunctorFloat);
  u.i = i;
#if LOG2_bignum_size == 5
  HeapPush(h, u.p[0]);
  HeapPush(h, u.p[1]);
#elif LOG2_bignum_size == 6
  HeapPush(h, u.p[0]);
  HeapPush(h, 0); /* dummy, for BC_SCALE==2 */
#endif
  HeapPush(h, MakeFunctorFloat);
  return Tag(STR, h-4);
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=:=", 2);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    goto small;
  } else {
    goto nonsmall;
  }
  
 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)==TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__FAIL;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))==0);
  }

 small:
  CBOOL__LASTTEST(t==u);
}

CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=\\=", 2);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    goto small;
  } else {
    goto nonsmall;
  }

 nonsmall:
  if (IsFloat(t) || IsFloat(u)) {
    CBOOL__LASTTEST(TaggedToFloat(t)!=TaggedToFloat(u));
  } else if (TaggedIsSmall(t) || TaggedIsSmall(u)) {
    CBOOL__PROCEED;
  } else {
    CBOOL__LASTTEST(bn_compare(TaggedToBignum(t),TaggedToBignum(u))!=0);
  }
 small:
  CBOOL__LASTTEST(t!=u);
}

CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:<", 2);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    CBOOL__LASTTEST(t<u);
  }
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

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    CBOOL__LASTTEST(t<=u);
  }
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

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    CBOOL__LASTTEST(t>u);
  }
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

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u)) {
    CBOOL__LASTTEST(t>=u);
  }
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

CFUN__PROTO(fu1_minus, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$-", 2);
  tagged_t t=x0;

//  fprintf(stderr, "th = %llx\n", GetSmall(TaggedHigh));
//  fprintf(stderr, "tl = %llx\n", GetSmall(TaggedLow));
//  fprintf(stderr, "th2 = %llx\n", MakeSmall(0x1234));
//  fprintf(stderr, "th2 = %llx\n", MakeSmall(GetSmall(TaggedHigh)));
//  fprintf(stderr, "th2tl = %llx\n", TagOf(TaggedLow));
//  fprintf(stderr, "th2tz = %llx\n", TagOf(TaggedZero));
//  fprintf(stderr, "th2th = %llx\n", TagOf(TaggedHigh));
//  fprintf(stderr, "highint = %llx\n", SmiValMaxPlus1); // ==GetSmall(TaggedHigh)
//  fprintf(stderr, "smivalmin = %llx\n", SmiValMin); // ==GetSmall(TaggedLow)
//
//// GetSmall(TaggedHigh) SmiValMaxPlus1 is not an integer? check it!
//  fprintf(stderr, "IsInSmiValRange(th) = %d\n", IsInSmiValRange(GetSmall(TaggedHigh))); // FALSE
//  fprintf(stderr, "IsInSmiValRange(tl) = %d\n", IsInSmiValRange(GetSmall(TaggedLow)));
  
  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__LASTCALL(make_integer_check, GetSmall(TaggedHigh));
    } else {
      CFUN__PROCEED(TaggedZero-(t-TaggedZero));
    }
  }
  if (IsFloat(t)) {
    CFUN__LASTCALL(flt64_to_blob_GC, -TaggedToFloat(t));
  } else {
    CFUN__LASTCALL(bn_call1,bn_minus,t);
  }
}

CFUN__PROTO(fu1_plus, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$+", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  CFUN__PROCEED(t);
}

CFUN__PROTO(fu1_integer, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$integer", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);
  if (IsFloat(t)) {
    flt64_t f = get_float(t);
    if (!float_is_finite(f)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    CFUN__LASTCALL(bn_from_float_check, f);
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_float, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$float", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (IsFloat(t)) {
    CFUN__PROCEED(t);
  } else {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t));
  }
}

CFUN__PROTO(fu1_add1, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$++", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedHigh-MakeSmallDiff(1)) {
      CFUN__LASTCALL(make_integer_check, GetSmall(TaggedHigh));
    } else {
      CFUN__PROCEED(t+MakeSmallDiff(1));
    }
  } else if (IsFloat(t)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) + 1.0);
  } else {
    CFUN__LASTCALL(bn_call1,bn_incr,t);
  }
}

CFUN__PROTO(fu1_sub1, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$--", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__LASTCALL(make_integer_check, GetSmall(TaggedLow)-1);
    } else {
      CFUN__PROCEED(t-MakeSmallDiff(1));
    }
  } else if (IsFloat(t)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) - 1.0);
  } else {
    CFUN__LASTCALL(bn_call1,bn_decr,t);
  }
}

                                /* binary functions */

CFUN__PROTO(fu2_plus, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$+", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    tagged_t t1;
    if (TaggedIsSmall(t1 = t+(u-TaggedZero))) {
      CFUN__PROCEED(t1);
    } else {
      CFUN__LASTCALL(make_integer_check, GetSmall(t1));
    }
  } else if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) + TaggedToFloat(u));
  } else {
    CFUN__LASTCALL(bn_call2,bn_add,t,u);
  }
}

CFUN__PROTO(fu2_minus, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$-", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    tagged_t t1;
    if (TaggedIsSmall(t1 = t-(u-TaggedZero))) {
      CFUN__PROCEED(t1);
    } else {
      CFUN__LASTCALL(make_integer_check, GetSmall(t1));
    }
  } else if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) - TaggedToFloat(u));
  } else {
    CFUN__LASTCALL(bn_call2,bn_subtract,t,u);
  }
}

CFUN__PROTO(fu2_times, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$*", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    intmach_t st = GetSmall(t);
    intmach_t su = (intmach_t)(u-TaggedZero);
    intmach_t stu;
    tagged_t tu;
    SMUL_OVERFLOW(st, su, stu, { goto overflow; });
    tu = ((tagged_t)stu)+TaggedZero;
    if (TaggedIsSmall(tu)) {
      CFUN__PROCEED(tu);
    }
  overflow:
    {}
  }
  if (IsFloat(t) || IsFloat(u)) {
    CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t) * TaggedToFloat(u));
  } else {
    CFUN__LASTCALL(bn_call2,bn_multiply,t,u);
  }
}

CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$/", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  CFUN__LASTCALL(flt64_to_blob_GC, TaggedToFloat(t)/TaggedToFloat(u));
}

CFUN__PROTO(fu2_idivide, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$//", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
  
  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);
  
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__LASTCALL(make_integer_check, (intmach_t)(t-TaggedZero)/(intmach_t)(u-TaggedZero));
  }

  /*bn_quotient_wanted = TRUE;*/
  CFUN__LASTCALL(bn_call2,bn_quotient_remainder_quot_wanted,t,u);
}

CFUN__PROTO(fu2_rem, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$rem", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((intmach_t)(t-TaggedZero)%(intmach_t)(u-TaggedZero)+TaggedZero);
  }

  /*bn_quotient_wanted = FALSE;*/
  CFUN__LASTCALL(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u);
}

CFUN__PROTO(fu2_mod, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$mod", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    intmach_t rem, denom;
    denom = (intmach_t)(u-TaggedZero);
    rem = (intmach_t)(t-TaggedZero)%denom;
    if ((denom > 0 && rem < 0) || (denom < 0 && rem > 0)) {
      CFUN__PROCEED(rem+denom+TaggedZero);
    } else {
      CFUN__PROCEED(rem+TaggedZero);
    }
  } else {
    tagged_t T_rem;
    /*bn_quotient_wanted = FALSE;*/
    PROT_GC(T_rem = CFUN__EVAL(bn_call2,bn_quotient_remainder_quot_not_wanted,t,u), u);
    if (T_rem != TaggedZero && CFUN__EVAL(fu1_sign,u) != CFUN__EVAL(fu1_sign,T_rem)) {
      /* Note: fu1_sign above always return small ints! */
      CFUN__LASTCALL(bn_call2,bn_add,T_rem,u);
    } else {
      CFUN__PROCEED(T_rem);
    }
  }
}

CFUN__PROTO(fu1_abs, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$abs", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__LASTCALL(make_integer_check, GetSmall(TaggedHigh));
    } else if (t < TaggedZero) {
      CFUN__PROCEED(TaggedZero-(t-TaggedZero));
    } else {
      CFUN__PROCEED(t);
    }
  } else if (IsFloat(t)) {
    flt64_t f;
    f = TaggedToFloat(t);
    if (f < 0.0) {
      CFUN__LASTCALL(flt64_to_blob_GC, -f);
    } else {
      CFUN__PROCEED(t);
    }
  } else {
    if (!bn_positive(TaggedToBignum(t))) { 
      CFUN__LASTCALL(bn_call1,bn_minus,t);
    } else {
      CFUN__PROCEED(t);
    }
  }
}

CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sign", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedZero) {
      CFUN__PROCEED(TaggedZero);
    } else if (t < TaggedZero) {
      CFUN__PROCEED(TaggedZero-MakeSmallDiff(1));
    } else {
      CFUN__PROCEED(TaggedZero+MakeSmallDiff(1));
    }
  } else if (IsFloat(t)) {
    flt64_t f;
    f = TaggedToFloat(t);
    if (f == 0.0) {
      CFUN__PROCEED(t);
    } else if (f < 0.0) {
      CFUN__LASTCALL(flt64_to_blob_GC, -1.0);
    } else {
      CFUN__LASTCALL(flt64_to_blob_GC, 1.0);
    }
  } else {
    if (!bn_positive(TaggedToBignum(t))) {
      CFUN__PROCEED(TaggedZero-MakeSmallDiff(1));
    } else {
      CFUN__PROCEED(TaggedZero+MakeSmallDiff(1));
    }
  }
}

CFUN__PROTO(fu1_not, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$\\", 2);
  tagged_t t=x0;

  NDEREF_GC(Integer, t, 0);
  if (TaggedIsSmall(t)) {
    CFUN__PROCEED(t^(QMask-MakeSmallDiff(1)));
  } else {
    CFUN__LASTCALL(bn_call1,bn_not,t);
  }
}

CFUN__PROTO(fu2_xor, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$#", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED(t^u^TaggedZero);
  } else {
    CFUN__LASTCALL(bn_call2,bn_xor,t,u);
  }
}

CFUN__PROTO(fu2_and, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$/\\", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((t^ZMask)&(u^ZMask))^ZMask;
  } else {
    CFUN__LASTCALL(bn_call2,bn_and,t,u);
  }
}

CFUN__PROTO(fu2_or, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$\\/", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((t^ZMask)|(u^ZMask))^ZMask;
  } else {
    CFUN__LASTCALL(bn_call2,bn_or,t,u);
  }
}

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  tagged_t u;
  
  if (TaggedIsSmall(t)) {
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
      u = 0;
    }
    if (TaggedIsSmall(u)) {
      CFUN__PROCEED(u);
    }
    /*
      intmach_t value = GetSmall(t);

      if (dist<32 &&
      value>=0 && value < (unsigned long)(1<<31)>>dist ||
      value<0 && value >= (long)(-1<<31)>>dist)
      CFUN__LASTCALL(make_integer_check, value<<dist);
    */
  }

  CFUN__LASTCALL(bn_call2, bn_lshift, t, IntvalToTagged(dist));
}

static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, intmach_t dist) {
  if (TaggedIsSmall(t)) {
    if (dist>=tagged__num_size) {
      CFUN__PROCEED(MakeSmall((t>=TaggedZero)-1));
    } else {
      CFUN__PROCEED((intmach_t)((t>>dist)-(TaggedZero>>dist)) & (-MakeSmallDiff(1))) + TaggedZero;
    }
  }

  CFUN__LASTCALL(bn_call2, bn_rshift, t, IntvalToTagged(dist));
}

/* pre: t is an integer */
static inline bool_t int_is_nonneg(tagged_t t) {
  if (TaggedIsSmall(t)) {
    return (t >= TaggedZero);
  } else { /* t is bignum */
    return bn_positive(TaggedToBignum(t));
  }
}

/* 
   For shifting operations we assume it is not possible to allocate
   enough memory to represent any number bigger than
   2^(2^INTMACH_MAX).  Therefore shifting to the left any number by
   more than 2^INTMACH_MAX throws a memory overflow exception and
   shifting to the right any number by more than 2^INTMACH_MAX returns
   0.
*/
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$<<", 3);
  intmach_t dist;
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
 
  if (IsIntegerFix(u)) {
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = TaggedToIntmach(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      CFUN__PROCEED(TaggedZero);
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      if (dist<0) {
        CFUN__LASTCALL(rsh_internal,t,-dist);
      } else {
        CFUN__LASTCALL(lsh_internal,t,dist);
      }
    }
  } else if (bn_positive(TaggedToBignum(u)) && t != TaggedZero) {
    /* u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  } else {
    /* -u > 2^INTMACH_MAX */
    /* was: CFUN__PROCEED(TaggedZero); */
    if (int_is_nonneg(t)) {
      CFUN__PROCEED(TaggedZero);
    } else {
      CFUN__PROCEED(TaggedZero-MakeSmallDiff(1));
    }
  }  
}

CFUN__PROTO(fu2_rsh, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$>>", 3);
  intmach_t dist;
  tagged_t t=x0,u=x1;

  NDEREF_GC(Integer, t, 0, u);
  NDEREF_GC(Integer, u, 1, t);
  
  if (IsIntegerFix(u)) { 
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = TaggedToIntmach(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      if (dist<0) {
        CFUN__LASTCALL(lsh_internal,t,-dist);
      } else {
        CFUN__LASTCALL(rsh_internal,t,dist);
      }
    }
  } else if (bn_positive(TaggedToBignum(u)) || t == TaggedZero) {
    /* u > 2^INTMACH_MAX */
    /* was: CFUN__PROCEED(TaggedZero); */
    if (int_is_nonneg(t)) {
      CFUN__PROCEED(TaggedZero);
    } else {
      CFUN__PROCEED(TaggedZero-MakeSmallDiff(1));
    }
  } else {
    /* -u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  }
}

/* GCD for rat arithm., ch feb 92 */
CFUN__PROTO(fu2_gcd, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  tagged_t u=x0,v=x1;

  int type = 3;                 /* big x big */ 
  
  NDEREF_GC(Integer, u, 0, v);
  NDEREF_GC(Integer, v, 1, u);

  if (TaggedIsSmall(u)) {
    if (u<=TaggedZero) {
      if (u==TaggedLow) {
        PROT_GC(u = CFUN__EVAL(make_integer_check, GetSmall(TaggedHigh)), v);
      } else {
        type -= 2; /* u is not big */
        u = TaggedZero-(u-TaggedZero);
      }
    }
  } else {
    if (!bn_positive(TaggedToBignum(u))) {
      PROT_GC(u = CFUN__EVAL(bn_call1,bn_minus,u), v);
    }
  }

  if (TaggedIsSmall(v)) {
    if (v<=TaggedZero) {
      if (v==TaggedLow) {
        PROT_GC(v = CFUN__EVAL(make_integer_check, GetSmall(TaggedHigh)), u);
      } else {
        type -= 1; /* v is not big */
        v = TaggedZero-(v-TaggedZero);
      }
    }
  } else {
    if (!bn_positive(TaggedToBignum(v))) {
      PROT_GC(v = CFUN__EVAL(bn_call1,bn_minus,v), u);
    }
  }
                                
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

  NDEREF_GC(Number, t, 0);

  flt64_t f;
  f = TaggedToFloat(t);
  CFUN__LASTCALL(flt64_to_blob_GC, aint(f));
}

CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$float_fractional_part", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  flt64_t f;
  f = TaggedToFloat(t);
  CFUN__LASTCALL(flt64_to_blob_GC, f-aint(f));
}

CFUN__PROTO(fu1_floor, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$floor", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    flt64_t f = get_float(t);
    if (!float_is_finite(f)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    CFUN__LASTCALL(bn_from_float_check, floor(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_round, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$round", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    flt64_t f = get_float(t);
    if (!float_is_finite(f)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    CFUN__LASTCALL(bn_from_float_check, round(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu1_ceil, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$ceiling", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    flt64_t f = get_float(t);
    if (!float_is_finite(f)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    CFUN__LASTCALL(bn_from_float_check, ceil(f));
  } else {
    CFUN__PROCEED(t);
  }
}

CFUN__PROTO(fu2_pow, tagged_t, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:$**", 3);
  tagged_t t=x0,u=x1;

  NDEREF_GC(Number, t, 0, u);
  NDEREF_GC(Number, u, 1, t);
  CFUN__LASTCALL(flt64_to_blob_GC, pow(TaggedToFloat(t),TaggedToFloat(u)));
}

CFUN__PROTO(fu1_exp, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$exp", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, exp(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_log, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$log", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, log(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sqrt", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, sqrt(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_sin, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$sin", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, sin(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_cos, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$cos", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, cos(TaggedToFloat(t)));
}

CFUN__PROTO(fu1_atan, tagged_t, tagged_t x0) {
  ERR__FUNCTOR("arithmetic:$atan", 2);
  tagged_t t=x0;

  NDEREF_GC(Number, t, 0);

  CFUN__LASTCALL(flt64_to_blob_GC, atan(TaggedToFloat(t)));
}

