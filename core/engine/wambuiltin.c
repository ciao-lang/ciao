/*
 *  wambuiltin.c
 *
 *  Routines for 'call_builtin' procedures.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#include <limits.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/support.h>
#include <ciao/indexing.h>
#include <ciao/initial.h> /* switch_on_function */
#include <ciao/wambuiltin.h>
#include <ciao/wamsupport.h>
#include <ciao/misc.h>
#include <ciao/bignum.h>

/* Compatibility with non-clang compilers */
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

/* Signed integer multiplication with overflow checking */
#if tagged__size == 32
#if __has_builtin(__builtin_smul_overflow)
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  if (__builtin_smul_overflow((A), (B), &P)) { OVERFLOW; } \
    })
#else
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  int64_t R = (int64_t)(A) * (int64_t)(B); \
  if (R > INT_MAX || R < INT_MIN) { OVERFLOW; } \
  P = (intmach_t)R; \
})
#endif
#elif tagged__size == 64
#if __has_builtin(__builtin_smulll_overflow)
#define SMUL_OVERFLOW(A, B, P, OVERFLOW) ({ \
  if (__builtin_smulll_overflow((A), (B), &P)) { OVERFLOW; } \
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

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, int dist, bcp_t liveinfo);
static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, int dist, bcp_t liveinfo);

/* --------------------------------------------------------------------------- */

/*
  Evaluate an arithmetic expression (storing bignums in the temporary
  Numstack area).

  Note: evaluate return v in case of error;
*/
static CFUN__PROTO(evaluate, tagged_t, tagged_t v)
{
  tagged_t t, u;
  TInfo Proc;

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
      Proc = incore_gethash(switch_on_function,t)->value.tinfo;
      /* TODO: use different function pointer types */
      if (Proc!=NULL) {
	switch (Arity(t)) {
	case 1:
	  RefArg(t,v,1);
	  return (*Proc)(Arg,t,NULL);
	case 2:
	  RefArg(t,v,1);
	  RefArg(u,v,2);
	  return (*Proc)(Arg,t,u,NULL);
	}
      }

    default:
      return v;
    }
}

#define NDEREF(Wam, Reg, ArgNo, Aux)					\
  {									\
    DerefSwitch(Reg,Aux,						\
		BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)	\
      if (!IsNumber(Reg))						\
	{								\
	  Reg = evaluate(Wam, Reg);					\
	  if(!IsNumber(Reg))						\
	    BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));	\
	}								\
  }
/* NDEREF_I */
#define NDEREF_I(Wam, Reg, ArgNo, Aux)					\
  { 									\
    DerefSwitch(Reg,Aux,						\
		BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)	\
      if (!IsInteger(Reg))						\
	{								\
	  Reg = evaluate(Wam, Reg);					\
	  if(!TagIsSmall(Reg)) {					\
	    if(!TagIsLarge(Reg))					\
	      BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));	\
	    if(LargeIsFloat(Reg))					\
	      BUILTIN_ERROR(TYPE_ERROR(INTEGER), (Reg), (ArgNo));	\
	  }								\
	}								\
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t==u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)==GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t!=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)!=GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive((bignum_t *)TagToSTR(u));
  else if (TagIsSmall(u))
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<=GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive((bignum_t *)TagToSTR(u));
  else if (TagIsSmall(u))
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive((bignum_t *)TagToSTR(u));
  else if (TagIsSmall(u))
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
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>=GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive((bignum_t *)TagToSTR(u));
  else if (TagIsSmall(u))
    return bn_positive((bignum_t *)TagToSTR(t));
  else
    return (bn_compare((bignum_t *)TagToSTR(t),(bignum_t *)TagToSTR(u))>=0);
}

CBOOL__PROTO(bu1_atom, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (TermIsATM(x0));
}

CBOOL__PROTO(bu1_atomic, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (!(x0 & TagBitComplex) || TagIsLarge(x0));
}

CBOOL__PROTO(bu1_float, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsFloat(x0);
}

CBOOL__PROTO(bu1_integer, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsInteger(x0);
}

CBOOL__PROTO(bu1_number, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsNumber(x0);
}

CBOOL__PROTO(bu1_var, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return TRUE;})
  return FALSE;
}

CBOOL__PROTO(bu1_nonvar, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return TRUE;
}

CBOOL__PROTO(bu2_lexeq, tagged_t x0, tagged_t x1)
{
  return (x0==x1 || compare_help(Arg,x0,x1)==0);
}

CBOOL__PROTO(bu2_lexne, tagged_t x0, tagged_t x1)
{
  return (x0!=x1 && compare_help(Arg,x0,x1)!=0);
}

CBOOL__PROTO(bu2_lexlt, tagged_t x0, tagged_t x1)
{
  return (x0!=x1 && compare_help(Arg,x0,x1)<0);
}

CBOOL__PROTO(bu2_lexle, tagged_t x0, tagged_t x1)
{
  return (x0==x1 || compare_help(Arg,x0,x1)<=0);
}

CBOOL__PROTO(bu2_lexgt, tagged_t x0, tagged_t x1)
{
  return (x0!=x1 && compare_help(Arg,x0,x1)>0);
}

CBOOL__PROTO(bu2_lexge, tagged_t x0, tagged_t x1)
{
  return (x0==x1 || compare_help(Arg,x0,x1)>=0);
}

CFUN__PROTO(fu2_compare, tagged_t, tagged_t x1, tagged_t x2, bcp_t liveinfo)
{
  int i;
  
  if (x1==x2)
    return atom_equal;
  else if ((i=compare_help(Arg,x1,x2)) < 0)
    return atom_lessthan;
  else if (i>0)
    return atom_greaterthan;
  else
    return atom_equal;
}


/*---------------------------------------------------------------*/
/*
CBOOL__PROTO(bu3_functor, 
             tagged_t term,
             tagged_t name,
             tagged_t arity)
{
  ERR__FUNCTOR("term_basic:functor", 3);
  tagged_t t0;

  DerefSwitch(term,t0,{goto construct;});
    {
      tagged_t tagarity;
      
      if (TermIsAtomic(term))
	tagarity = TaggedZero;
      else if (!(term & TagBitFunctor))
	term = atom_list,
	tagarity = MakeSmall(2);
      else
	{
	  tagged_t f = TagToHeadfunctor(term);
	  
	  term = SetArity(f,0),
	  tagarity = MakeSmall(Arity(f));
	}

      Unify_constant(tagarity,arity);
      return cunify(Arg,term,name);
    }
 construct:
    {
      DerefSwitch(name, t0, 
		  BUILTIN_ERROR(INSTANTIATION_ERROR, name, 2););
      DerefSwitch(arity, t0,
		  BUILTIN_ERROR(INSTANTIATION_ERROR, arity, 3););

      if (TermIsAtomic(name)) 
	{
	  if (arity == TaggedZero) return cunify(Arg,name,term);
	  else if (arity > TaggedZero) 
	    {
	      if (TagIsATM(name)) 
		{
		  if (arity < MakeSmall(ARITYLIMIT))
		    return 
		      cunify(Arg, make_structure(Arg, SetArity(name,GetSmall(arity))), term);
		  else if (IsInteger(arity)) 
		    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), arity, 3);
		  else
		    BUILTIN_ERROR(TYPE_ERROR(INTEGER),arity, 3);
		}
	      else if (IsInteger(arity))
		BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), name, 2);
	      else
		BUILTIN_ERROR(TYPE_ERROR(INTEGER), arity, 3);
	    }
	  else
	    BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), arity, 3);
	}
      else
	BUILTIN_ERROR(TYPE_ERROR(ATOMIC), name, 2);
    }
    
}
*/

// Old code without exception
CBOOL__PROTO(bu3_functor, 
	     tagged_t term,
	     tagged_t name,
	     tagged_t arity)
{
  tagged_t t0;
  
  DerefSwitch(term,t0,{goto construct;});
    {
      tagged_t tagarity;
      
      if (TermIsAtomic(term))
	tagarity = TaggedZero;
      else if (!(term & TagBitFunctor))
	term = atom_list,
	tagarity = MakeSmall(2);
      else
	{
	  tagged_t f = TagToHeadfunctor(term);
	  
	  term = SetArity(f,0),
	  tagarity = MakeSmall(Arity(f));
	}

      Unify_constant(tagarity,arity);
      return cunify(Arg,term,name);
    }
 construct:
    {
      DerefSwitch(name,t0,;);
      DerefSwitch(arity,t0,;);
      if (TermIsAtomic(name) && (arity==TaggedZero))
	return cunify(Arg,name,term);
      else if (TagIsATM(name) &&
	       (arity>TaggedZero) && (arity<MakeSmall(ARITYLIMIT)))
	return cunify(Arg,
                      make_structure(Arg, SetArity(name,GetSmall(arity))),
                      term);
      else
	return FALSE;
    }
}


/*---------------------------------------------------------------*/

CBOOL__PROTO(bu2_univ, 
	     tagged_t term,
	     tagged_t list)
{ 
  ERR__FUNCTOR("term_basic:=..", 2);
  CIAO_REG_1(tagged_t, car);
  CIAO_REG_2(tagged_t, cdr);
  tagged_t *argp;
  tagged_t *argq;
  int arity;
  tagged_t f;

  DerefSwitch(term,car,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term))
    {
      MakeLST(cdr,term,cdr);
      return cunify(Arg,cdr,list);
    }
  
  if (term & TagBitFunctor)
    f = TagToHeadfunctor(term),
    argp = TagToArg(term,1),
    argq = HeapOffset(argp,Arity(f));
  else
    f = functor_list,
    argp = TagToCar(term),
    argq = HeapOffset(argp,2);

  while HeapYounger(argq,argp)
    {
      HeapDecr(argq);
      RefHeap(car,argq);
      MakeLST(cdr,car,cdr);
    }
  MakeLST(cdr,SetArity(f,0),cdr);
  return cunify(Arg,cdr,list);

 construct:
  cdr = list;
  DerefSwitch(cdr,car, BUILTIN_ERROR(INSTANTIATION_ERROR, list, 2););
  arity = 0;

  if (IsVar(cdr))
    goto bomb;
  if (!TagIsLST(cdr))
    {
      if (cdr == atom_nil)
	BUILTIN_ERROR(DOMAIN_ERROR(NON_EMPTY_LIST), list, 2); 
      else
	BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2); 
    }

  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
  if (cdr==atom_nil)
    {
      if (TermIsAtomic(f))
	return cunify(Arg,f,term);
      else 
	BUILTIN_ERROR(TYPE_ERROR(ATOMIC), f, 2); 
    }
  else if (IsVar(f))
    goto bomb;
  else if (!TagIsATM(f))
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), f, 2); 


  argp = w->global_top;
  HeapPush(w->global_top,f);
  while (TagIsLST(cdr) && arity<ARITYLIMIT)
    {
      DerefCar(car,cdr);
      DerefCdr(cdr,cdr);
      HeapPush(w->global_top,car);
      arity++;
    }
  if (IsVar(cdr))
    goto bomb;
  if (arity==ARITYLIMIT)
    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ARITY), list, 2);
  if (cdr!=atom_nil) 
    BUILTIN_ERROR(TYPE_ERROR(LIST), list, 2);
  
  f = SetArity(f,arity);
  if (f==functor_list)
    {
      w->global_top = argp;
      argq = HeapOffset(w->global_top,1);
      RefHeapNext(car,argq);
      RefHeapNext(cdr,argq);
      HeapPush(w->global_top,car);
      HeapPush(w->global_top,cdr);
      return cunify(Arg,Tag(LST,argp),term);
    }
  else
    {
      *argp = f;
      return cunify(Arg,Tag(STR,argp),term);
    }

 bomb:
  BUILTIN_ERROR(INSTANTIATION_ERROR,list, 2);
}

/*
// Old code without exception
CBOOL__PROTO(bu2_univ, 
             tagged_t term,
             tagged_t list)
{
  CIAO_REG_1(tagged_t, car);
  CIAO_REG_2(tagged_t, cdr);
  tagged_t *argp;
  tagged_t *argq;
  int arity;
  tagged_t f;

  DerefSwitch(term,car,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term))
    {
      MakeLST(cdr,term,cdr);
      return cunify(Arg,cdr,list);
    }
  
  if (term & TagBitFunctor)
    f = TagToHeadfunctor(term),
    argp = TagToArg(term,1),
    argq = HeapOffset(argp,Arity(f));
  else
    f = functor_list,
    argp = TagToCar(term),
    argq = HeapOffset(argp,2);
  
  while HeapYounger(argq,argp)
    {
      HeapDecr(argq);
      RefHeap(car,argq);
      MakeLST(cdr,car,cdr);
    }
  MakeLST(cdr,SetArity(f,0),cdr);
  return cunify(Arg,cdr,list);

 construct:
  cdr = list;
  DerefSwitch(cdr,car,;);
  arity = 0;

  if (IsVar(cdr))
    goto bomb;
  if (!TagIsLST(cdr))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
  if (TermIsAtomic(f) && (cdr==atom_nil))
    return cunify(Arg,f,term);
  else if (IsVar(f))
    goto bomb;
  else if (!TagIsATM(f))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  argp = w->global_top;
  HeapPush(w->global_top,f);
  while (TagIsLST(cdr) && arity<ARITYLIMIT)
    {
      DerefCar(car,cdr);
      DerefCdr(cdr,cdr);
      HeapPush(w->global_top,car);
      arity++;
    }
  if (IsVar(cdr))
    goto bomb;
  if (cdr!=atom_nil || arity==ARITYLIMIT)
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  f = SetArity(f,arity);
  if (f==functor_list)
    {
      w->global_top = argp;
      argq = HeapOffset(w->global_top,1);
      RefHeapNext(car,argq);
      RefHeapNext(cdr,argq);
      HeapPush(w->global_top,car);
      HeapPush(w->global_top,cdr);
      return cunify(Arg,Tag(LST,argp),term);
    }
  else
    {
      *argp = f;
      return cunify(Arg,Tag(STR,argp),term);
    }

 bomb:
    USAGE_FAULT("=../2: illegal arguments");
}
*/

/* Support for if/3 */
CBOOL__PROTO(bu1_if, tagged_t x0)
{
  DEREF(x0,x0);
  *TagToCar(x0) = atom_true;
  return TRUE;
}

/* --------------------------------------------------------------------------- */

CFUN__PROTO(fu1_minus, tagged_t, tagged_t X0, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$-", 2);
  CIAO_REG_1(tagged_t, t);
  CIAO_REG_2(tagged_t, t1);
  
  t=X0; 
  NDEREF(Arg, t, 0, t1);
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t)) return t;
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
  if (TagIsSmall(t)) return t;
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
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
    if (TagIsSmall(t1 = t+(u-TaggedZero)))
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
    if (TagIsSmall(t1 = t-(u-TaggedZero)))
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
    intmach_t st = GetSmall(t);
    intmach_t su = (intmach_t)(u-TaggedZero);
    intmach_t stu;
    tagged_t tu;
    SMUL_OVERFLOW(st, su, stu, { goto overflow; });
    tu = ((tagged_t)stu)+TaggedZero;
    if (TagIsSmall(tu)) return tu;
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
  
  if (TagIsSmall(t) && TagIsSmall(u))
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

  if (TagIsSmall(t) && TagIsSmall(u))
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

  if (TagIsSmall(t) && TagIsSmall(u)) {
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
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t)) {
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
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
  if (TagIsSmall(t) && TagIsSmall(u)) {
    return ((t^ZMask)|(u^ZMask))^ZMask;
  } else {
    return bn_call(Arg,bn_or,t,u, liveinfo);
  }
}

static CFUN__PROTO(lsh_internal, tagged_t, tagged_t t, int dist, bcp_t liveinfo)
{
  tagged_t u;
  
  if (TagIsSmall(t)) {
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
    if (TagIsSmall(u))
      return u;
    /*
      intmach_t value = GetSmall(t);

      if (dist<32 &&
      value>=0 && value < (unsigned long)(1<<31)>>dist ||
      value<0 && value >= (long)(-1<<31)>>dist)
      return make_integer_check(value<<dist, liveinfo);
    */
  }

  return bn_call(Arg, bn_lshift, t, MakeInteger(Arg,(intmach_t)dist), liveinfo);
}

static CFUN__PROTO(rsh_internal, tagged_t, tagged_t t, int dist, bcp_t liveinfo)
{
  if (TagIsSmall(t)) {
    if (dist>=tagged__size) {
      return MakeSmall((t>=TaggedZero)-1);
    } else {
      return ((intmach_t)((t>>dist)-(TaggedZero>>dist)) & (-MakeSmallDiff(1))) + TaggedZero;
    }
  }

  return bn_call(Arg, bn_rshift, t, MakeInteger(Arg,(intmach_t)dist), liveinfo);
}

/* 
   For shiffting operations we assume it is not possible to allocate
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
    return TaggedZero;
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
    return TaggedZero;
  } else {
    /* -u > 2^INTMACH_MAX */
    BUILTIN_ERROR(RESOURCE_ERROR(R_STACK), u, 2); 
  }
}

/*  GCD for rat arithm., ch feb 92
    
    This works through the following mechanism:
		
    - The arithm. functions (via is/2,</2,...) apply NDEREF to their
      args which calls evaluate [wamsupport.c] which, when it sees
      terms, calls the corresponding function ( max arity = 2). Eval
      calls this functions with liveinfo==NULL which means that
      numstack_end must not be NULL (make_float_check,
      make_integer_check)
*/

CFUN__PROTO(fu2_gcd, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo)
{
  ERR__FUNCTOR("arithmetic:$gcd", 3);
  CIAO_REG_1(tagged_t, u);
  CIAO_REG_2(tagged_t, t1);
  CIAO_REG_3(tagged_t, v);

  int type = 3;			/* big x big */ 
  
  u=X0; 
  NDEREF_I(Arg, u, 0, t1);
  if (TagIsSmall(u)) {
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
  if (TagIsSmall(v)) {
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
    switch (type) {  			/*     u x v     */
      case 0:				/* small x small */
  small_x_small:
      	{ uintmach_t x = GetSmall(u), y = GetSmall(v);
      	  for (;;) {
	    x = x % y; if ( x==0 ) return MakeSmall(y);
	    y = y % x; if ( y==0 ) return MakeSmall(x);
      	  }
	}
      case 1:				/* small x big   */
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo);
	if ( v==TaggedZero ) return u;
	goto small_x_small;
      case 2:                           /*   big x small */
 	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo);
	if ( u==TaggedZero ) return v;
	goto small_x_small;
      case 3:				/*   big x big   */
	u = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,u,v, liveinfo); 
	if ( u==TaggedZero ) return v;
	if ( TagIsSmall(u) ) type -= 2;
	v = bn_call(Arg,bn_quotient_remainder_quot_not_wanted,v,u, liveinfo); 
	if ( v==TaggedZero ) return u;
	if ( TagIsSmall(v) ) type -= 1;
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
  if (TagIsSmall(t)) return t;

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
  if (TagIsSmall(t)) return t;

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
  if (TagIsSmall(t)) return t;

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

/* ------------------------------------------------------------------------- */

/*
  CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t complex, bcp_t liveinfo)
{  
  ERR__FUNCTOR("term_basic:arg", 3);
  CIAO_REG_1(tagged_t, t0);
  intmach_t i;

  DerefSwitch(number,t0, BUILTIN_ERROR(INSTANTIATION_ERROR, number, 1););
  DerefSwitch(complex,t0, BUILTIN_ERROR(INSTANTIATION_ERROR, complex, 2););

  if (TagIsSmall(number)) i = GetSmall(number);
  else if (TagIsLarge(number) && !LargeIsFloat(number)) return FALSE;
  else BUILTIN_ERROR(TYPE_ERROR(INTEGER), number, 1);

  if (i < 0)
    BUILTIN_ERROR(DOMAIN_ERROR(NOT_LESS_THAN_ZERO), number, 1);

  if (TagIsSTR(complex))
    {
      tagged_t f = TagToHeadfunctor(complex);

      if (i == 0 || i > Arity(f) || f&QMask) return FALSE;

      RefArg(t0,complex,i);
      return t0;
    }
  else if (IsComplex(complex))	/\* i.e. list *\/
    {
      if (i == 1)
	{
	  RefCar(t0,complex);
	  return t0;
	}
      else if (i == 2)
	{
	  RefCdr(t0,complex);
	  return t0;
	}
      else return FALSE;
    }
  // comment next line for full ISO compliance
  else if (IsAtom(complex)) return FALSE;      
  else  BUILTIN_ERROR(TYPE_ERROR(COMPOUND), complex, 2);
 

}
*/

// Old code without exception 
CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t complex, bcp_t liveinfo)
{
  CIAO_REG_1(tagged_t, t0);
  
  DerefSwitch(number,t0,{goto barf1;});
  DerefSwitch(complex,t0,{goto barf2;});

  if (TagIsSTR(complex)) {
    intmach_t i = GetSmall(number);
    tagged_t f = TagToHeadfunctor(complex);

    if (i<=0 || i>Arity(f) || f&QMask) {
      goto barf1;
    }
      
    RefArg(t0,complex,i);
    return t0;
  } else if (IsComplex(complex)) { // i.e. list 
    if (number==MakeSmall(1))	{
      RefCar(t0,complex);
      return t0;
    } else if (number==MakeSmall(2)) {
      RefCdr(t0,complex);
      return t0;
    } else {
      goto barf1;
    }
  } else {
    goto barf2;
  }

 barf1:
  MINOR_FAULT("arg/3: incorrect 1st argument");

 barf2:
  MINOR_FAULT("arg/3: incorrect 2nd argument");
}

