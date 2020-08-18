/*
 *  arithmetic.c
 *
 *  Arithmetic builtins ('call_builtin' procedures)
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2020 Ciao Development Team
 */

#include <limits.h>

#include <ciao/eng.h>
#include <ciao/internals.h>
#include <ciao/eng_registry.h> /* switch_on_function */
#include <ciao/arithmetic.h>
#include <ciao/basiccontrol.h>
#include <ciao/eng_bignum.h>

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
#if tagged__size == 32
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  int64_t R = (int64_t)(A) * (int64_t)(B); \
  if (R > INT_MAX || R < INT_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#elif tagged__size == 64
#include <stdint.h>
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  __int128_t R = (__int128_t)(A) * (__int128_t)(B); \
  if (R > INT64_MAX || R < INT64_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#endif
#endif

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo);
static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo);

static bool_t float_is_finite(tagged_t t) {
  /* Assume IEEE comparison for floats */
  flt64_t f = get_float(t);
  f = f - f;
  return (f == f);
}

/* --------------------------------------------------------------------------- */

/*
  Evaluate an arithmetic expression (storing bignums in the temporary
  Numstack area).

  Note: evaluate return v in case of error;
*/
static CFUN__PROTO(evaluate, tagged_t, tagged_t v)
{
  tagged_t t, u;
  void *proc;

 restart:
  switch (TagOf(v))
    {
    case NUM:
      return v;

    case LST:
      DerefCdr(t,v);
      if (t==atom_nil) {
        DerefCar(v,v);
        goto restart;
      } else {
        return v;
      }

    case STR:
      if (STRIsLarge(v))
        return v;
      t = TagToHeadfunctor(v);
      proc = incore_gethash(switch_on_function,t)->value.proc;
      /* TODO: use different function pointer types */
      if (proc!=NULL) {
        switch (Arity(t)) {
        case 1:
          RefArg(t,v,1);
          return (*(ctagged1l_t)proc)(Arg,t,NULL);
        case 2:
          RefArg(t,v,1);
          RefArg(u,v,2);
          return (*(ctagged2l_t)proc)(Arg,t,u,NULL);
        }
      }

    default:
      return v;
    }
}

#define NDEREF(Wam, Reg, ArgNo, Aux)                                    \
  {                                                                     \
    DerefSwitch(Reg,Aux,                                                \
                BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)    \
      if (!IsNumber(Reg))                                               \
        {                                                               \
          Reg = evaluate(Wam, Reg);                                     \
          if(!IsNumber(Reg))                                            \
            BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));       \
        }                                                               \
  }
/* NDEREF_I */
#define NDEREF_I(Wam, Reg, ArgNo, Aux)                                  \
  {                                                                     \
    DerefSwitch(Reg,Aux,                                                \
                BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)    \
      if (!IsInteger(Reg))                                              \
        {                                                               \
          Reg = evaluate(Wam, Reg);                                     \
          if(!TaggedIsSmall(Reg)) {                                        \
            if(!TagIsLarge(Reg))                                        \
              BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));     \
            if(LargeIsFloat(Reg))                                       \
              BUILTIN_ERROR(TYPE_ERROR(INTEGER), (Reg), (ArgNo));       \
          }                                                             \
        }                                                               \
  }

/* --------------------------------------------------------------------------- */

/* NOTE about fu?_* functios:

    - Resetting Numstack_End = NULL must be done from outside
      (otherwise temporaries for 'evaluate' are lost)

    - If liveinfo==NULL, then we use the Numstack temporary area.
    - If liveinfo!=NULL some functions call bn_call(Arg,bn_plus,t,0,
      liveinfo) to make sure that the large number is copied to the
      heap from the temporary area.
*/ 

/* TODO: Do a more direct blob copy */

/* Copy bignum to heap if needed */
static inline CFUN__PROTO(globalize_bn, tagged_t, tagged_t t, bcp_t liveinfo) {
  if (liveinfo==NULL) return t;
  if (IsFloat(t)) {
    return make_float_check(Arg, GetFloat(t), liveinfo);
  } else {
    return bn_call(Arg,bn_plus,t,0, liveinfo);
  }
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:=:=", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t==u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)==GetFloat(u));
  else if (TaggedIsSmall(t) || TaggedIsSmall(u))
    return FALSE;
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))==0);
}

CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:=\\=", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t!=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)!=GetFloat(u));
  else if (TaggedIsSmall(t) || TaggedIsSmall(u))
    return TRUE;
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))!=0);
}

CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:<", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t<u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<GetFloat(u));
  else if (TaggedIsSmall(t))
    return bn_positive((bignum_t *)TagToSTR(u));
  else if (TaggedIsSmall(u))
    return !bn_positive((bignum_t *)TagToSTR(t));
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))<0);
}

CBOOL__PROTO(bu2_numle, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:=<", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t<=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<=GetFloat(u));
  else if (TaggedIsSmall(t))
    return bn_positive((bignum_t *)TagToSTR(u));
  else if (TaggedIsSmall(u))
    return !bn_positive((bignum_t *)TagToSTR(t));
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))<=0);
}

CBOOL__PROTO(bu2_numgt, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:>", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t>u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>GetFloat(u));
  else if (TaggedIsSmall(t))
    return !bn_positive((bignum_t *)TagToSTR(u));
  else if (TaggedIsSmall(u))
    return bn_positive((bignum_t *)TagToSTR(t));
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))>0);
}

CBOOL__PROTO(bu2_numge, tagged_t x0, tagged_t x1)
{
  ERR__FUNCTOR("arithmetic:>=", 2);
  //  tagged_t t1,t,u;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t, 0, t1);
  u=x1; NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t)&&TaggedIsSmall(u))
    return (t>=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>=GetFloat(u));
  else if (TaggedIsSmall(t))
    return !bn_positive((bignum_t *)TagToSTR(u));
  else if (TaggedIsSmall(u))
    return bn_positive((bignum_t *)TagToSTR(t));
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))>=0);
}

/* --------------------------------------------------------------------------- */

CFUN__PROTO(fu1_minus, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$-", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  
  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow)
      return make_integer_check(Arg, GetSmall(TaggedHigh), liveinfo);
    else
      return TaggedZero-(t-TaggedZero);
  } else if (IsFloat(t)) {
    return make_float_check(Arg, -GetFloat(t), liveinfo);
  } else {
    return bn_call(Arg,bn_minus,t,0, liveinfo);
  }
}

CFUN__PROTO(fu1_plus, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$+", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) return t;
  /* (identity function) */
  return globalize_bn(Arg, t, liveinfo);
}

CFUN__PROTO(fu1_integer, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$integer", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) return t;
  if (IsFloat(t)) {
    if (!float_is_finite(t)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    return bn_call(Arg,bn_from_float,t,0, liveinfo);
  } else {
    /* (identity function) */
    return globalize_bn(Arg, t, liveinfo);
  }
}

CFUN__PROTO(fu1_float, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$float", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  if (IsFloat(t)) {
    /* (identity function) */
    return globalize_bn(Arg, t, liveinfo);
  } else {
    return make_float_check(Arg, GetFloat(t), liveinfo);
  }
}

CFUN__PROTO(fu1_add1, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$++", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    if (t==TaggedHigh-MakeSmallDiff(1)) {
      return make_integer_check(Arg, GetSmall(TaggedHigh), liveinfo);
    } else {
      return t+MakeSmallDiff(1);
    }
  } else if (IsFloat(t)) {
    return make_float_check(Arg, GetFloat(t) + 1.0, liveinfo);
  } else {
    return bn_call(Arg,bn_incr,t,0, liveinfo);
  }
}

CFUN__PROTO(fu1_sub1, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$--", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0;
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow)
      return make_integer_check(Arg, GetSmall(TaggedLow)-1, liveinfo);
    else
      return t-MakeSmallDiff(1);
  } else if (IsFloat(t)) {
    return make_float_check(Arg, GetFloat(t) - 1.0, liveinfo);
  } else {
    return bn_call(Arg,bn_decr,t,0, liveinfo);
  }
}

                                /* binary functions */

CFUN__PROTO(fu2_plus, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$+", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  u=X1; 
  NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    if (TaggedIsSmall(t1 = t+(u-TaggedZero)))
      return t1;
    else
      return make_integer_check(Arg, GetSmall(t1), liveinfo);
  } else if (IsFloat(t) || IsFloat(u)) {
    return make_float_check(Arg, GetFloat(t) + GetFloat(u), liveinfo);
  } else {
    return bn_call(Arg,bn_add,t,u, liveinfo);
  }
}

CFUN__PROTO(fu2_minus, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$-", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  u=X1; 
  NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    if (TaggedIsSmall(t1 = t-(u-TaggedZero)))
      return t1;
    else
      return make_integer_check(Arg, GetSmall(t1), liveinfo);
  } else if (IsFloat(t) || IsFloat(u)) {
    return make_float_check(Arg, GetFloat(t) - GetFloat(u), liveinfo);
  } else {
    return bn_call(Arg,bn_subtract,t,u, liveinfo);
  }
}

CFUN__PROTO(fu2_times, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$*", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  u=X1; 
  NDEREF(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    intmach_t st = GetSmall(t);
    intmach_t su = (intmach_t)(u-TaggedZero);
    intmach_t stu;
    tagged_t tu;
    SMUL_OVERFLOW(st, su, stu, { goto overflow; });
    tu = ((tagged_t)stu)+TaggedZero;
    if (TaggedIsSmall(tu)) return tu;
  overflow:
    {}
  }
  if (IsFloat(t) || IsFloat(u)) {
    return make_float_check(Arg, GetFloat(t) * GetFloat(u), liveinfo);
  } else {
    return bn_call(Arg,bn_multiply,t,u, liveinfo);
  }
}

CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$/", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  u=X1; 
  NDEREF(Arg, u, 1, t1);
  return make_float_check(Arg, GetFloat(t)/GetFloat(u), liveinfo);
}

CFUN__PROTO(fu2_idivide, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$//", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  
  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);
  
  if (TaggedIsSmall(t) && TaggedIsSmall(u))
    return make_integer_check(Arg, (intmach_t)(t-TaggedZero)/(intmach_t)(u-TaggedZero), liveinfo);

  /*bn_quotient_wanted = TRUE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_wanted,t,u, liveinfo);
}

CFUN__PROTO(fu2_rem, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$rem", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u))
    return (intmach_t)(t-TaggedZero)%(intmach_t)(u-TaggedZero)+TaggedZero;

  /*bn_quotient_wanted = FALSE;*/
  return bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u, liveinfo);
}

CFUN__PROTO(fu2_mod, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$mod", 3);
  intmach_t rem, denom;
  tagged_t T_rem;

  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);

  if (u == TaggedZero) BUILTIN_ERROR(EVALUATION_ERROR(ZERO_DIVISOR), u, 1);

  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    denom = (intmach_t)(u-TaggedZero);
    rem = (intmach_t)(t-TaggedZero)%denom;
    return ( (denom > 0 && rem < 0) || (denom < 0 && rem > 0) ?
              rem+denom : rem ) + TaggedZero;
  } else {
    /*bn_quotient_wanted = FALSE;*/
    T_rem = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,t,u, liveinfo);
    return ( T_rem != TaggedZero &&
             fu1_sign(Arg,u, liveinfo) != fu1_sign(Arg,T_rem, liveinfo)
             ? bn_call(Arg,bn_add,T_rem,u, liveinfo) : T_rem );
  }
}

CFUN__PROTO(fu1_abs, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$abs", 2);
  flt64_t f;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    if (t==TaggedLow)
      return make_integer_check(Arg, GetSmall(TaggedHigh), liveinfo);
    else if (t < TaggedZero)
      return TaggedZero-(t-TaggedZero);
    else
      return t;
  } else if (IsFloat(t)) {
    return (((f = GetFloat(t)) < 0.0) ? make_float_check(Arg, -f, liveinfo) : t);
  } else {
    return ((!bn_positive((bignum_t *)TagToSTR(t))) ? bn_call(Arg,bn_minus,t,0, liveinfo) : t);
  }
}

CFUN__PROTO(fu1_sign, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$sign", 2);
  flt64_t f;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    return ((t==TaggedZero) ? TaggedZero :
            (t < TaggedZero) ? TaggedZero-MakeSmallDiff(1) :
            TaggedZero+MakeSmallDiff(1));
  } else if (IsFloat(t)) {
    f = GetFloat(t);
    return ((f == 0.0) ? t :
            (f < 0.0) ? make_float_check(Arg, -1.0, liveinfo) :
            make_float_check(Arg, 1.0, liveinfo));
  } else  {
    return ((!bn_positive((bignum_t *)TagToSTR(t))) ? TaggedZero-MakeSmallDiff(1) :
            TaggedZero+MakeSmallDiff(1));
  }
}

CFUN__PROTO(fu1_not, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$\\", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) {
    return t^(QMask-MakeSmallDiff(1));
  } else {
    return bn_call(Arg,bn_not,t,0, liveinfo);
  }
}

CFUN__PROTO(fu2_xor, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$#", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    return t^u^TaggedZero;
  } else {
    return bn_call(Arg,bn_xor,t,u, liveinfo);
  }
}

CFUN__PROTO(fu2_and, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$/\\", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    return ((t^ZMask)&(u^ZMask))^ZMask;
  } else {
    return bn_call(Arg,bn_and,t,u, liveinfo);
  }
}

CFUN__PROTO(fu2_or, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$\\/", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  if (TaggedIsSmall(t) && TaggedIsSmall(u)) {
    return ((t^ZMask)|(u^ZMask))^ZMask;
  } else {
    return bn_call(Arg,bn_or,t,u, liveinfo);
  }
}

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo)
{
  tagged_t u;
  
  if (TaggedIsSmall(t)) {
    switch (dist) {
    case 0:
      return t;
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
    if (TaggedIsSmall(u))
      return u;
    /*
      intmach_t value = GetSmall(t);

      if (dist<32 &&
      value>=0 && value < (unsigned long)(1<<31)>>dist ||
      value<0 && value >= (long)(-1<<31)>>dist)
      return make_integer_check(value<<dist, liveinfo);
    */
  }

  return bn_call(Arg, bn_lshift, t, MakeInteger(Arg,dist), liveinfo);
}

static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, intmach_t dist, bcp_t liveinfo)
{
  if (TaggedIsSmall(t)) {
    if (dist>=tagged__num_size) {
      return MakeSmall((t>=TaggedZero)-1);
    } else {
      return ((intmach_t)((t>>dist)-(TaggedZero>>dist)) & (-MakeSmallDiff(1))) + TaggedZero;
    }
  }

  return bn_call(Arg, bn_rshift, t, MakeInteger(Arg,dist), liveinfo);
}

/* pre: t is an integer */
static inline bool_t int_is_nonneg(tagged_t t) {
  if (TaggedIsSmall(t)) {
    return (t >= TaggedZero);
  } else { /* t is bignum */
    return bn_positive((bignum_t *)TagToSTR(t));
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
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$<<", 3);
  intmach_t dist;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
 
  if (IsIntegerFix(u)) {
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = GetInteger(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      return TaggedZero;
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      return (dist<0 ? rsh_internal(Arg,t,-dist, liveinfo) : lsh_internal(Arg,t,dist, liveinfo));
    }
  } else if (bn_positive((bignum_t *)TagToSTR(u)) && t != TaggedZero) {
    /* u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  } else {
    /* -u > 2^INTMACH_MAX */
    /* was: return TaggedZero; */
    return (int_is_nonneg(t) ? TaggedZero : TaggedZero-MakeSmallDiff(1));
  }  
}

CFUN__PROTO(fu2_rsh, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$>>", 3);
  intmach_t dist;
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF_I(Arg, t, 0, t1);
  u=X1; 
  NDEREF_I(Arg, u, 1, t1);
  
  if (IsIntegerFix(u)) { 
    /* 2^INTMACH_MIN =< u =< 2^INTMACH_MAX */
    dist = GetInteger(u);
    
    if ((intmach_t)dist == INTMACH_MIN) {
      /* u = 2^INTMACH_MIN, i.e. -u > 2^INTMACH_MAX */
      BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
    } else {
      /* abs(u) =< 2^INTMACH_MIN */
      return (dist<0 ? lsh_internal(Arg,t,-dist, liveinfo) : rsh_internal(Arg,t,dist, liveinfo));
    }
  } else if (bn_positive((bignum_t *)TagToSTR(u)) || t == TaggedZero) {
    /* u > 2^INTMACH_MAX */
    /* was: return TaggedZero; */
    return (int_is_nonneg(t) ? TaggedZero : TaggedZero-MakeSmallDiff(1));
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
      not be NULL (make_float_check, make_integer_check)
*/

CFUN__PROTO(fu2_gcd, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  CIAO_REG_1(tagged_t, u);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, v);

  int type = 3;                 /* big x big */ 
  
  u=X0; 
  NDEREF_I(Arg, u, 0, t1);
  if (TaggedIsSmall(u)) {
    type -= 2;
    if (u<=TaggedZero) {
      u = (u==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh), liveinfo)
                        : TaggedZero-(u-TaggedZero));
    }
  } else if (!bn_positive((bignum_t *)TagToSTR(u))) {
    u = bn_call(Arg,bn_minus,u,0, liveinfo);
  }

  v=X1; 
  NDEREF_I(Arg, v, 1, t1);
  if (TaggedIsSmall(v)) {
    type -= 1;
    if (v<=TaggedZero) {
      v = (v==TaggedLow ? make_integer_check(Arg, GetSmall(TaggedHigh), liveinfo)
                        : TaggedZero-(v-TaggedZero));
    }
  } else if (!bn_positive((bignum_t *)TagToSTR(v))) {
    v = bn_call(Arg,bn_minus,v,0, liveinfo);
  }
                                
  if (u==TaggedZero) return v;
  if (v==TaggedZero) return u;
  /*bn_quotient_wanted = FALSE;*/

  for (;;) {
    switch (type) {                     /*     u x v     */
      case 0:                           /* small x small */
  small_x_small:
        { uintmach_t x = GetSmall(u), y = GetSmall(v);
          for (;;) {
            x = x % y; if ( x==0 ) return MakeSmall(y);
            y = y % x; if ( y==0 ) return MakeSmall(x);
          }
        }
      case 1:                           /* small x big   */
        v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo);
        if ( v==TaggedZero ) return u;
        goto small_x_small;
      case 2:                           /*   big x small */
        u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo);
        if ( u==TaggedZero ) return v;
        goto small_x_small;
      case 3:                           /*   big x big   */
        u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo); 
        if ( u==TaggedZero ) return v;
        if ( TaggedIsSmall(u) ) type -= 2;
        v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo); 
        if ( v==TaggedZero ) return u;
        if ( TaggedIsSmall(v) ) type -= 1;
    }
  }
} 

#include <math.h>

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(BSD)
# define aint(f) (f>=0.0 ? floor(f) : ceil(f))
#endif

CFUN__PROTO(fu1_intpart, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$float_integer_part", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  flt64_t f;

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  f = GetFloat(t);
  return make_float_check(Arg, aint(f), liveinfo);
}

CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$float_fractional_part", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  flt64_t f;

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  f = GetFloat(t);
  return make_float_check(Arg, f-aint(f), liveinfo);
}

CFUN__PROTO(fu1_floor, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$floor", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  tagged_t f;

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) return t;

  if (IsFloat(t)) {
    if (!float_is_finite(t)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = make_float_check(Arg, floor(GetFloat(t)), liveinfo);
    return bn_call(Arg,bn_from_float,f,0, liveinfo);
  } else {
    /* (identity function) */
    return globalize_bn(Arg, t, liveinfo);
  }
}

CFUN__PROTO(fu1_round, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$round", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  tagged_t f;

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) return t;

  if (IsFloat(t)) {
    if (!float_is_finite(t)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = make_float_check(Arg, round(GetFloat(t)), liveinfo);
    return bn_call(Arg,bn_from_float,f,0, liveinfo);
  } else {
    /* (identity function) */
    return globalize_bn(Arg, t, liveinfo);
  }
}

CFUN__PROTO(fu1_ceil, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$ceiling", 2);
  tagged_t t,t1;

  tagged_t f;

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TaggedIsSmall(t)) return t;

  if (IsFloat(t)) {
    if (!float_is_finite(t)) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(NAN_OR_INF_TO_INTEGER), t, 1);
    }
    f = make_float_check(Arg, ceil(GetFloat(t)), liveinfo);
    return bn_call(Arg,bn_from_float,f,0, liveinfo);
  } else {
    /* (identity function) */
    return globalize_bn(Arg, t, liveinfo);
  }
}

CFUN__PROTO(fu2_pow, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$**", 3);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, u);

  t=X0; 
  NDEREF(Arg, t, 0, t1);
  u=X1; 
  NDEREF(Arg, u, 1, t1);
  return make_float_check(Arg, pow(GetFloat(t),GetFloat(u)), liveinfo);
}

CFUN__PROTO(fu1_exp, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$exp", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, exp(GetFloat(t)), liveinfo);
}

CFUN__PROTO(fu1_log, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$log", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, log(GetFloat(t)), liveinfo);
}

CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$sqrt", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, sqrt(GetFloat(t)), liveinfo);
}

CFUN__PROTO(fu1_sin, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$sin", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, sin(GetFloat(t)), liveinfo);
}

CFUN__PROTO(fu1_cos, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$cos", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, cos(GetFloat(t)), liveinfo);
}

CFUN__PROTO(fu1_atan, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$atan", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);

  t=X0; 
  NDEREF(Arg, t, 0, t1);

  return make_float_check(Arg, atan(GetFloat(t)), liveinfo);
}

