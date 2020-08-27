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
   Note: evaluate return v in case of error;
*/

/* Note: stores bignums in the temporary Numstack area */
static CFUN__PROTO(evaluate, tagged_t, tagged_t v) {
  tagged_t t, u;
  void *proc;

 restart:
  switch (TagOf(v))
    {
    case NUM:
      CFUN__PROCEED(v);

    case LST:
      DerefCdr(t,v);
      if (t==atom_nil) {
        DerefCar(v,v);
        goto restart;
      } else {
        CFUN__PROCEED(v);
      }

    case STR:
      if (STRIsLarge(v)) {
        CFUN__PROCEED(v);
      }
      t = TagToHeadfunctor(v);
      proc = incore_gethash(switch_on_function,t)->value.proc;
      /* TODO: use different function pointer types */
      if (proc!=NULL) {
        switch (Arity(t)) {
        case 1:
          RefArg(t,v,1);
          CFUN__LASTCALL(((ctagged1l_t)proc),t,NULL);
        case 2:
          RefArg(t,v,1);
          RefArg(u,v,2);
          CFUN__LASTCALL(((ctagged2l_t)proc),t,u,NULL);
        }
      }

    default:
      CFUN__PROCEED(v);
    }
}

#define NEVAL(Reg, Exit) ({ \
  if (!IsNumber(Reg)) { \
    Reg = CFUN__EVAL(evaluate, Reg); \
    if (!IsNumber(Reg)) { \
      Exit; \
    } \
  } \
})

#define NDEREF(Reg, ArgNo) ({ \
  tagged_t t1_; \
  DerefSwitch(Reg,t1_, \
              BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));); \
  NEVAL(Reg, BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo))); \
})
/* Like NDEREF but Reg must evaluate to an integer */
#define NDEREF_I(Reg, ArgNo) ({ \
  tagged_t t1_; \
  DerefSwitch(Reg,t1_, \
              BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));); \
  if (!IsInteger(Reg)) { \
    Reg = CFUN__EVAL(evaluate, Reg); \
    if (!TaggedIsSmall(Reg)) { \
      if (!TaggedIsLarge(Reg)) { \
        BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo)); \
      } \
      if (LargeIsFloat(Reg)) { \
        BUILTIN_ERROR(TYPE_ERROR(INTEGER), (Reg), (ArgNo)); \
      } \
    } \
  } \
})

/* --------------------------------------------------------------------------- */

/* NOTE about fu?_* functios:

    - Resetting Numstack_End = NULL must be done from outside
      (otherwise temporaries for 'evaluate' are lost)

    - If liveinfo==NULL, then we use the Numstack temporary area.
    - If liveinfo!=NULL some functions call
      CFUN__EVAL(bn_call,bn_plus,t,0,liveinfo) to make sure that the
      large number is copied to the heap from the temporary area.
*/ 

/* TODO: Do a more direct blob copy */

/* Copy bignum to heap if needed */
static inline CFUN__PROTO(globalize_bn, tagged_t, tagged_t t, bcp_t liveinfo) {
  if (liveinfo==NULL) CFUN__PROCEED(t);
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_plus,t,0, liveinfo));
  }
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1) {
  ERR__FUNCTOR("arithmetic:=:=", 2);
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
  tagged_t t,u;

  Numstack_End = NULL;
  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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

CFUN__PROTO(fu1_minus, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$-", 2);
  tagged_t t;
  
  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(TaggedHigh)));
    } else {
      CFUN__PROCEED(TaggedZero-(t-TaggedZero));
    }
  }
  if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(-TaggedToFloat(t)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_minus,t,0, liveinfo));
  }
}

CFUN__PROTO(fu1_plus, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$+", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);
  /* (identity function) */
  CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
}

CFUN__PROTO(fu1_integer, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$integer", 2);
  tagged_t t;

  t=x0; 
  NDEREF(t, 0); if (TaggedIsSmall(t)) CFUN__PROCEED(t);
  if (IsFloat(t)) {
    if (!float_is_finite(get_float(t))) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_from_float,t,0, liveinfo));
  } else {
    /* (identity function) */
    CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
  }
}

CFUN__PROTO(fu1_float, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$float", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  if (IsFloat(t)) {
    /* (identity function) */
    CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
  } else {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)));
  }
}

CFUN__PROTO(fu1_add1, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$++", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedHigh-MakeSmallDiff(1)) {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(TaggedHigh)));
    } else {
      CFUN__PROCEED(t+MakeSmallDiff(1));
    }
  } else if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) + 1.0));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_incr,t,0, liveinfo));
  }
}

CFUN__PROTO(fu1_sub1, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$--", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(TaggedLow)-1));
    } else {
      CFUN__PROCEED(t-MakeSmallDiff(1));
    }
  } else if (IsFloat(t)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) - 1.0));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_decr,t,0, liveinfo));
  }
}

                                /* binary functions */

CFUN__PROTO(fu2_plus, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$+", 3);
  tagged_t t,u;

  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    tagged_t t1;
    if (TaggedIsSmall(t1 = t+(u-TaggedZero))) {
      CFUN__PROCEED(t1);
    } else {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(t1)));
    }
  } else if (IsFloat(t) || IsFloat(u)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) + TaggedToFloat(u)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_add,t,u, liveinfo));
  }
}

CFUN__PROTO(fu2_minus, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$-", 3);
  tagged_t t,u;

  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    tagged_t t1;
    if (TaggedIsSmall(t1 = t-(u-TaggedZero))) {
      CFUN__PROCEED(t1);
    } else {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(t1)));
    }
  } else if (IsFloat(t) || IsFloat(u)) {
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) - TaggedToFloat(u)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_subtract,t,u, liveinfo));
  }
}

CFUN__PROTO(fu2_times, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$*", 3);
  tagged_t t,u;

  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
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
    CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t) * TaggedToFloat(u)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_multiply,t,u, liveinfo));
  }
}

CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$/", 3);
  tagged_t t,u;

  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
  CFUN__PROCEED(BoxFloatCheck(TaggedToFloat(t)/TaggedToFloat(u)));
}

CFUN__PROTO(fu2_idivide, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$//", 3);
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
  
  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);
  
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED(IntvalToTaggedCheck((intmach_t)(t-TaggedZero)/(intmach_t)(u-TaggedZero)));
  }

  /*bn_quotient_wanted = TRUE;*/
  CFUN__PROCEED(CFUN__EVAL(bn_call,bn_quotient_remainder_quot_wanted,t,u, liveinfo));
}

CFUN__PROTO(fu2_rem, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$rem", 3);
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((intmach_t)(t-TaggedZero)%(intmach_t)(u-TaggedZero)+TaggedZero);
  }

  /*bn_quotient_wanted = FALSE;*/
  CFUN__PROCEED(CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,t,u, liveinfo));
}

CFUN__PROTO(fu2_mod, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$mod", 3);
  intmach_t rem, denom;
  tagged_t T_rem;

  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    denom = (intmach_t)(u-TaggedZero);
    rem = (intmach_t)(t-TaggedZero)%denom;
    CFUN__PROCEED(((denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
                  rem+denom : rem ) + TaggedZero);
  } else {
    /*bn_quotient_wanted = FALSE;*/
    T_rem = CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,t,u, liveinfo);
    CFUN__PROCEED(T_rem != TaggedZero &&
                  CFUN__EVAL(fu1_sign,u, liveinfo) != CFUN__EVAL(fu1_sign,T_rem, liveinfo)
                  ? CFUN__EVAL(bn_call,bn_add,T_rem,u, liveinfo) : T_rem);
  }
}

CFUN__PROTO(fu1_abs, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$abs", 2);
  flt64_t f;
  tagged_t t;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow) {
      CFUN__PROCEED(IntvalToTaggedCheck(GetSmall(TaggedHigh)));
    } else if (t < TaggedZero) {
      CFUN__PROCEED(TaggedZero-(t-TaggedZero));
    } else {
      CFUN__PROCEED(t);
    }
  } else if (IsFloat(t)) {
    f = TaggedToFloat(t);
    CFUN__PROCEED(f < 0.0 ? BoxFloatCheck(-f) : t);
  } else {
    CFUN__PROCEED(!bn_positive(TaggedToBignum(t)) ? CFUN__EVAL(bn_call,bn_minus,t,0, liveinfo) : t);
  }
}

CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$sign", 2);
  flt64_t f;
  tagged_t t;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) {
    CFUN__PROCEED((t==TaggedZero) ? TaggedZero :
                  (t < TaggedZero) ? TaggedZero-MakeSmallDiff(1) :
                  TaggedZero+MakeSmallDiff(1));
  } else if (IsFloat(t)) {
    f = TaggedToFloat(t);
    CFUN__PROCEED((f == 0.0) ? t :
                  (f < 0.0) ? BoxFloatCheck(-1.0) :
                  BoxFloatCheck(1.0));
  } else {
    CFUN__PROCEED((!bn_positive(TaggedToBignum(t))) ? TaggedZero-MakeSmallDiff(1) :
                  TaggedZero+MakeSmallDiff(1));
  }
}

CFUN__PROTO(fu1_not, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$\\", 2);
  tagged_t t;

  t=x0; NDEREF_I(t, 0);
  if (TaggedIsSmall(t)) {
    CFUN__PROCEED(t^(QMask-MakeSmallDiff(1)));
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_not,t,0, liveinfo));
  }
}

CFUN__PROTO(fu2_xor, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$#", 3);
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED(t^u^TaggedZero);
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_xor,t,u, liveinfo));
  }
}

CFUN__PROTO(fu2_and, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$/\\", 3);
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((t^ZMask)&(u^ZMask))^ZMask;
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_and,t,u, liveinfo));
  }
}

CFUN__PROTO(fu2_or, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$\\/", 3);
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    CFUN__PROCEED((t^ZMask)|(u^ZMask))^ZMask;
  } else {
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_or,t,u, liveinfo));
  }
}

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo) {
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
      CFUN__PROCEED(IntvalToTaggedCheck(value<<dist));
    */
  }

  CFUN__PROCEED(CFUN__EVAL(bn_call, bn_lshift, t, IntvalToTagged(dist), liveinfo));
}

static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo) {
  if (TaggedIsSmall(t)) {
    if (dist>=tagged__num_size) {
      CFUN__PROCEED(MakeSmall((t>=TaggedZero)-1));
    } else {
      CFUN__PROCEED((intmach_t)((t>>dist)-(TaggedZero>>dist)) & (-MakeSmallDiff(1))) + TaggedZero;
    }
  }

  CFUN__PROCEED(CFUN__EVAL(bn_call, bn_rshift, t, IntvalToTagged(dist), liveinfo));
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
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$<<", 3);
  intmach_t dist;
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
 
  if (IsIntegerFix(u)) {
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = TaggedToIntmach(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      CFUN__PROCEED(TaggedZero);
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      CFUN__PROCEED(dist<0 ? CFUN__EVAL(rsh_internal,t,-dist, liveinfo) : CFUN__EVAL(lsh_internal,t,dist, liveinfo));
    }
  } else if (bn_positive(TaggedToBignum(u)) && t != TaggedZero) {
    /* u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  } else {
    /* -u > 2^INTMACH_MAX */
    /* was: CFUN__PROCEED(TaggedZero); */
    CFUN__PROCEED(int_is_nonneg(t) ? TaggedZero : TaggedZero-MakeSmallDiff(1));
  }  
}

CFUN__PROTO(fu2_rsh, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$>>", 3);
  intmach_t dist;
  tagged_t t,u;

  t=x0; NDEREF_I(t, 0);
  u=x1; NDEREF_I(u, 1);
  
  if (IsIntegerFix(u)) { 
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = TaggedToIntmach(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      CFUN__PROCEED(dist<0 ? CFUN__EVAL(lsh_internal,t,-dist, liveinfo) : CFUN__EVAL(rsh_internal,t,dist, liveinfo));
    }
  } else if (bn_positive(TaggedToBignum(u)) || t == TaggedZero) {
    /* u > 2^INTMACH_MAX */
    /* was: CFUN__PROCEED(TaggedZero); */
    CFUN__PROCEED(int_is_nonneg(t) ? TaggedZero : TaggedZero-MakeSmallDiff(1));
  } else {
    /* -u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  }
}

/*  GCD for rat arithm., ch feb 92
    
    This works through the following mechanism:
                
    - The arithm. functions (via is/2,</2,...) apply NDEREF to their
      args which calls evaluate which, when it sees terms, calls the
      corresponding function ( max arity = 2). Eval calls this
      functions with liveinfo==NULL which means that numstack_end must
      not be NULL (BoxFloatCheck, IntvalToTaggedCheck)
*/

CFUN__PROTO(fu2_gcd, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  tagged_t u,v;

  int type = 3;                 /* big x big */ 
  
  u=x0; NDEREF_I(u, 0);
  if (TaggedIsSmall(u)) {
    type -= 2;
    if (u<=TaggedZero) {
      u = (u==TaggedLow ? IntvalToTaggedCheck(GetSmall(TaggedHigh))
                        : TaggedZero-(u-TaggedZero));
    }
  } else if (!bn_positive(TaggedToBignum(u))) {
    u = CFUN__EVAL(bn_call,bn_minus,u,0, liveinfo);
  }

  v=x1; NDEREF_I(v, 1);
  if (TaggedIsSmall(v)) {
    type -= 1;
    if (v<=TaggedZero) {
      v = (v==TaggedLow ? IntvalToTaggedCheck(GetSmall(TaggedHigh))
                        : TaggedZero-(v-TaggedZero));
    }
  } else if (!bn_positive(TaggedToBignum(v))) {
    v = CFUN__EVAL(bn_call,bn_minus,v,0, liveinfo);
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
    v = CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo);
    if (v==TaggedZero) CFUN__PROCEED(u);
    goto small_x_small;
  case 2: /* big x small */
    u = CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo);
    if (u==TaggedZero) CFUN__PROCEED(v);
    goto small_x_small;
  default: /* big x big */
    u = CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo); 
    if (u==TaggedZero) CFUN__PROCEED(v);
    if (TaggedIsSmall(u)) type -= 2; /* now u is small */
    v = CFUN__EVAL(bn_call,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo); 
    if (v==TaggedZero) CFUN__PROCEED(u);
    if (TaggedIsSmall(v)) type -= 1; /* now v is small */
    goto again;
  }
} 

CFUN__PROTO(fu1_intpart, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$float_integer_part", 2);
  tagged_t t;

  flt64_t f;

  t=x0; NDEREF(t, 0);

  f = TaggedToFloat(t);
  CFUN__PROCEED(BoxFloatCheck(aint(f)));
}

CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$float_fractional_part", 2);
  tagged_t t;
  flt64_t f;

  t=x0; NDEREF(t, 0);

  f = TaggedToFloat(t);
  CFUN__PROCEED(BoxFloatCheck(f-aint(f)));
}

CFUN__PROTO(fu1_floor, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$floor", 2);
  tagged_t t;
  tagged_t f;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    if (!float_is_finite(get_float(t))) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = BoxFloatCheck(floor(TaggedToFloat(t)));
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_from_float,f,0, liveinfo));
  } else {
    /* (identity function) */
    CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
  }
}

CFUN__PROTO(fu1_round, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$round", 2);
  tagged_t t;
  tagged_t f;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    if (!float_is_finite(get_float(t))) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = BoxFloatCheck(round(TaggedToFloat(t)));
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_from_float,f,0, liveinfo));
  } else {
    /* (identity function) */
    CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
  }
}

CFUN__PROTO(fu1_ceil, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$ceiling", 2);
  tagged_t t;
  tagged_t f;

  t=x0; NDEREF(t, 0);
  if (TaggedIsSmall(t)) CFUN__PROCEED(t);

  if (IsFloat(t)) {
    if (!float_is_finite(get_float(t))) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = BoxFloatCheck(ceil(TaggedToFloat(t)));
    CFUN__PROCEED(CFUN__EVAL(bn_call,bn_from_float,f,0, liveinfo));
  } else {
    /* (identity function) */
    CFUN__PROCEED(CFUN__EVAL(globalize_bn, t, liveinfo));
  }
}

CFUN__PROTO(fu2_pow, tagged_t, tagged_t x0, tagged_t x1, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$**", 3);
  tagged_t t,u;

  t=x0; NDEREF(t, 0);
  u=x1; NDEREF(u, 1);
  CFUN__PROCEED(BoxFloatCheck(pow(TaggedToFloat(t),TaggedToFloat(u))));
}

CFUN__PROTO(fu1_exp, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$exp", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(exp(TaggedToFloat(t))));
}

CFUN__PROTO(fu1_log, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$log", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(log(TaggedToFloat(t))));
}

CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$sqrt", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(sqrt(TaggedToFloat(t))));
}

CFUN__PROTO(fu1_sin, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$sin", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(sin(TaggedToFloat(t))));
}

CFUN__PROTO(fu1_cos, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$cos", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(cos(TaggedToFloat(t))));
}

CFUN__PROTO(fu1_atan, tagged_t, tagged_t x0, bcp_t liveinfo) {
  ERR__FUNCTOR("arithmetic:$atan", 2);
  tagged_t t;

  t=x0; NDEREF(t, 0);

  CFUN__PROCEED(BoxFloatCheck(atan(TaggedToFloat(t))));
}

