/*
 *  term_compare.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>

CFUN__PROTO(compare__1, int, tagged_t x1, tagged_t x2);

CBOOL__PROTO(bu2_lexeq, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || compare__1(Arg,x0,x1)==0);
}

CBOOL__PROTO(bu2_lexne, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL(compare__1,x0,x1)!=0);
}

CBOOL__PROTO(bu2_lexlt, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL(compare__1,x0,x1)<0);
}

CBOOL__PROTO(bu2_lexle, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL(compare__1,x0,x1)<=0);
}

CBOOL__PROTO(bu2_lexgt, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0!=x1 && CFUN__EVAL(compare__1,x0,x1)>0);
}

CBOOL__PROTO(bu2_lexge, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL(compare__1,x0,x1)>=0);
}

CFUN__PROTO(fu2_compare, tagged_t, tagged_t x1, tagged_t x2) {
  if (x1==x2) {
    CFUN__PROCEED(atom_equal);
  } else {
    int i = CFUN__EVAL(compare__1,x1,x2);
    if (i < 0) {
      CFUN__PROCEED(atom_lessthan);
    } else if (i > 0) {
      CFUN__PROCEED(atom_greaterthan);
    } else {
      CFUN__PROCEED(atom_equal);
    }
  }
}

static CFUN__PROTO(compare_aux, int, tagged_t x1, tagged_t x2);
static CFUN__PROTO(compare_args_aux, int,
                   arity_t arity,
                   tagged_t *pt1, tagged_t *pt2,
                   tagged_t *x1, tagged_t *x2);

/* help function for builtin compare/3
 * returns -1 if u @< v
 * returns  0 if u == v
 * returns +1 if u @> v
 */
CFUN__PROTO(compare__1, int, tagged_t x1, tagged_t x2) {
  int result = compare_aux(Arg,x1,x2);
  VALUETRAIL__UNDO();
  CFUN__PROCEED(result);
}

static CFUN__PROTO(compare_aux, int, tagged_t x1, tagged_t x2) {
  tagged_t u;
  tagged_t v;
  tagged_t t1;
  tagged_t *pt1;
  tagged_t *pt2;

  int i, j, urank, vrank; /* FLO=1, INT=2, ATM=3, COMPLEX=4 */

 in:
  u=x1;
  v=x2;
  DerefSw_HVAorCVAorSVA_Other(u,{ goto var_x; },{});
  DerefSw_HVAorCVAorSVA_Other(v,{ CFUN__PROCEED(1); },{});
  if (u==v) return 0;
  if (TaggedIsSmall(u) && TaggedIsSmall(v)) goto var_var;

  if (u & TagBitComplex) {
    if (u & TagBitFunctor) {
      t1 = TaggedToHeadfunctor(u);
      /* urank = (!(t1&TagBitFunctor) ? 2 : t1&QTAGMASK ? 1 : 4); */
      urank = (!(t1&TagBitFunctor) ? 1 : t1&QTAGMASK ? 2 : 4);
    } else {
      urank = 4;
    }
  } else {
    /*    urank = (u&TagBitFunctor ? 3 : 1);*/
    urank = (u&TagBitFunctor ? 3 : 2);
  }
  if (v & TagBitComplex) {
    if (v & TagBitFunctor) {
      t1 = TaggedToHeadfunctor(v);
      /*      vrank = (!(t1&TagBitFunctor) ? 2 : t1&QTAGMASK ? 1 : 4);*/
      vrank = (!(t1&TagBitFunctor) ? 1 : t1&QTAGMASK ? 2 : 4);
    } else {
      vrank = 4;
    }
  } else {
    /* vrank = (v&TagBitFunctor ? 3 : 1);*/
    vrank = (v&TagBitFunctor ? 3 : 2);
  }

  if (urank<vrank) CFUN__PROCEED(-1);
  if (urank>vrank) CFUN__PROCEED(1);
  /* same rank */
  switch (urank) {
    case 1:                     /* FLO, FLO */
      {
        flt64_t f1 = TaggedToFloat(u);
        flt64_t f2 = TaggedToFloat(v);

        union {
          flt64_t f;
          tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
        } u1, u2;
        u1.f = f1;
        u2.f = f2;

        if (f1<f2) {
          CFUN__PROCEED(-1);
        } else if (f1>f2) {
          CFUN__PROCEED(1);
        } else {
          /* otherwise, compare bits (this is lexicographical ordering) */
          u1.f = -u1.f;
          u2.f = -u2.f;
#if LOG2_bignum_size == 5
          if (u1.p[0] == u2.p[0]) {
            return (u1.p[1] < u2.p[1] ? -1 : u1.p[1] > u2.p[1] ? 1 : 0);
          } else {
            return (u1.p[0] < u2.p[0] ? -1 : /*u1.p[0] > u2.p[0] ?*/ 1 /*: 0*/);
          }
#elif LOG2_bignum_size == 6
          return (u1.p[0] < u2.p[0] ? -1 : u1.p[0] > u2.p[0] ? 1 : 0);
#endif
        }
      }
    case 2:                     /* INT, INT */
      {
        if (TaggedIsSmall(u)&&TaggedIsSmall(v)) {
          return (u<v ? -1 : u>v); 
        } else if (TaggedIsSmall(u)) {
          return (bn_positive(TaggedToBignum(v)) ? -1 : 1);
        } else if (TaggedIsSmall(v)) {
          return (bn_positive(TaggedToBignum(u)) ? 1 : -1);
        } else {
          return bn_compare(TaggedToBignum(u),TaggedToBignum(v));
        }
      }
    case 3:                     /* ATM, ATM */
      break;
    case 4:                     /* COMPLEX, COMPLEX */
      if (u & TagBitFunctor) {
        pt1 = TagpPtr(STR,u); u = *pt1++; i = Arity(u);
      } else {
        pt1 = TagpPtr(LST,u); u = functor_lst; i = 2;
      }
      if (v & TagBitFunctor) {
        pt2 = TagpPtr(STR,v); v = *pt2++; j = Arity(v);
      } else {
        pt2 = TagpPtr(LST,v); v = functor_lst; j = 2;
      }
      
      if (u==v) {
        int result = compare_args_aux(Arg,i,pt1,pt2,&x1,&x2);
        if (result != 0) return result;
        goto in;
      } else if (i!=j) {
        return (i<j ? -1 : 1);
      } else {
        break;
      }
    }

  /* UNSIGNED strcmp */
  {
    unsigned char *up = (unsigned char *)GetString(u);
    unsigned char *vp = (unsigned char *)GetString(v);

    while ((u = *up++) && (v = *vp++)) {
      if (u!=v) return (u<v ? -1 : 1);
    }
    return (u ? 1 : v ? -1 : 0);
  }

 var_x:
  DerefSw_HVAorCVAorSVA_Other(v, { goto var_var; }, {});
  return -1;
 var_var:
  return (u<v ? -1 : u>v ? 1 : 0);
}

static CFUN__PROTO(compare_args_aux, int,
                   arity_t arity,
                   tagged_t *pt1, tagged_t *pt2,
                   tagged_t *x1, tagged_t *x2)
{
  int result;
  tagged_t t1 = ~0; /* Avoid compiler complaints */
  tagged_t t2 = ~0;
  
  /* Adapted from terminating unification of complex structures:
     See cunify_args(). */
  
  VALUETRAIL__TEST_OVERFLOW(2*CHOICEPAD);
  for (result=0; !result && arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (t1 != t2) {
      DerefSw_HVAorCVAorSVA_Other(t1, { goto noforward; }, {});
      DerefSw_HVAorCVAorSVA_Other(t2, { goto noforward; }, {});
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* replace smaller value by larger value,
           using choice stack as value trail */
        if (t1>t2) {
          VALUETRAIL__SET(pt1, t2);
        } else {
          VALUETRAIL__SET(pt2, t1);
        }
      }
    noforward:
      if (arity>1 && t1!=t2) {
        result = compare_aux(Arg,t1,t2);
      }
    }
    pt1++;
    pt2++;
  }
  
  if (!result) {
    *x1 = t1;
    *x2 = t2;
  }
  
  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
  
  CFUN__PROCEED(result);
}
