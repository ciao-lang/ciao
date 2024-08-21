/*
 *  term_compare.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#endif

static CFUN__PROTO(compare__1, int, tagged_t x1, tagged_t x2);

CBOOL__PROTO(bu2_lexeq, tagged_t x0, tagged_t x1) {
  CBOOL__LASTTEST(x0==x1 || CFUN__EVAL(compare__1,x0,x1)==0);
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

static CFUN__PROTO(compare__2, int, tagged_t x1, tagged_t x2);
static CFUN__PROTO(compare__args, int,
                   arity_t arity,
                   tagged_t *pt1, tagged_t *pt2,
                   tagged_t *x1, tagged_t *x2);

/* help function for builtin compare/3
 * returns -1 if u @< v
 * returns  0 if u == v
 * returns +1 if u @> v
 */
static CFUN__PROTO(compare__1, int, tagged_t x1, tagged_t x2) {
  int result = CFUN__EVAL(compare__2,x1,x2);
  VALUETRAIL__UNDO();
  CFUN__PROCEED(result);
}

/* FLO=1, INT=2, ATM=3, COMPLEX=4 */
#define GetRank(u, urank) \
  SwTagC(u, f, { /* NUM */ \
    urank = 2; \
  },{ /* ATM */ \
    urank = 3; \
  },{ /* LST */ \
    urank = 4; \
  },{ /* STR(blob(float)) */ \
    urank = 1; \
  },{ /* STR(blob(bignum)) */ \
    urank = 2; \
  },{ /* STR(struct) */ \
    urank = 4; \
  })

static CFUN__PROTO(compare__2, int, tagged_t x1, tagged_t x2) {
  tagged_t u;
  tagged_t v;
  tagged_t *pt1;
  tagged_t *pt2;
  int i, j, urank, vrank;

 in:
  u=x1;
  v=x2;
  DerefSw_HVAorCVAorSVA_Other(u,{ goto var_x; },{});
  DerefSw_HVAorCVAorSVA_Other(v,{ CFUN__PROCEED(1); },{});
  if (u==v) return 0;
  if (TaggedIsSmall(u) && TaggedIsSmall(v)) goto var_var;

  GetRank(u, urank);
  GetRank(v, vrank);

  if (urank<vrank) CFUN__PROCEED(-1);
  if (urank>vrank) CFUN__PROCEED(1);
  /* same rank */
  switch (urank) {
  case 1: /* FLO, FLO */
    {
      flt64_t f1 = TaggedToFloat(u);
      flt64_t f2 = TaggedToFloat(v);

      if (f1<f2) {
        CFUN__PROCEED(-1);
      } else if (f1>f2) {
        CFUN__PROCEED(1);
      } else {
        /* otherwise, compare bits (this is lexicographical ordering) */
        union {
          flt64_t f;
          tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
        } u1, u2;
        u1.f = f1;
        u2.f = f2;
        //
        u1.f = -u1.f;
        u2.f = -u2.f;
#if LOG2_bignum_size == 5
        if (u1.p[0] == u2.p[0]) {
          CFUN__PROCEED(u1.p[1] < u2.p[1] ? -1 : u1.p[1] > u2.p[1] ? 1 : 0);
        } else {
          CFUN__PROCEED(u1.p[0] < u2.p[0] ? -1 : /*u1.p[0] > u2.p[0] ?*/ 1 /*: 0*/);
        }
#elif LOG2_bignum_size == 6
        CFUN__PROCEED(u1.p[0] < u2.p[0] ? -1 : u1.p[0] > u2.p[0] ? 1 : 0);
#endif
      }
    }
  case 2: /* INT, INT */
    {
      if (TaggedIsSmall(u)&&TaggedIsSmall(v)) {
        CFUN__PROCEED(u<v ? -1 : u>v);
      } else if (TaggedIsSmall(u)) {
        CFUN__PROCEED(bn_positive(TaggedToBignum(v)) ? -1 : 1);
      } else if (TaggedIsSmall(v)) {
        CFUN__PROCEED(bn_positive(TaggedToBignum(u)) ? 1 : -1);
      } else {
        CFUN__PROCEED(bn_compare(TaggedToBignum(u),TaggedToBignum(v)));
      }
    }
  case 3: /* ATM, ATM */
    goto compare_uv;
  case 4: /* COMPLEX, COMPLEX */
    DecompComplex(u, pt1, i);
    DecompComplex(v, pt2, j);
    if (u==v) {
      int result = CFUN__EVAL(compare__args,i,pt1,pt2,&x1,&x2);
      if (result != 0) CFUN__PROCEED(result);
      goto in;
    } else if (i!=j) {
      CFUN__PROCEED(i<j ? -1 : 1);
    } else {
      goto compare_uv;
    }
  }

 compare_uv:
  {
    /* compare u and v atoms (or functor) text */
    unsigned char *up = (unsigned char *)GetString(u);
    unsigned char *vp = (unsigned char *)GetString(v);

    while ((u = *up++) && (v = *vp++)) {
      if (u!=v) CFUN__PROCEED((u<v ? -1 : 1));
    }
    CFUN__PROCEED(u ? 1 : v ? -1 : 0);
  }

 var_x:
  DerefSw_HVAorCVAorSVA_Other(v, {
    goto var_var;
  },{
    CFUN__PROCEED(-1);
  });
 var_var:
  CFUN__PROCEED(u<v ? -1 : u>v ? 1 : 0);
}

static CFUN__PROTO(compare__args, int,
                   arity_t arity,
                   tagged_t *pt1, tagged_t *pt2,
                   tagged_t *x1, tagged_t *x2) {
  int result;
  tagged_t t1 = ~0; /* Avoid compiler complaints */
  tagged_t t2 = ~0;
  
  /* Adapted from terminating unification of complex structures:
     See cunify_args(). */

#if defined(OPTIM_COMP)
  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD); // TODO:[oc-merge] why?
#else
  VALUETRAIL__TEST_OVERFLOW(2*CHOICEPAD);
#endif
  for (result=0; !result && arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    /* TODO: share with code from unify_args_loop in engine/absmach_def.pl */
    if (t1 != t2) {
#if defined(OPTIM_COMP) // TODO:[oc-merge] can this be a SVA?
      HeapDerefSw_HVAorCVA_Other(t1, { goto noforward; }, {});
      HeapDerefSw_HVAorCVA_Other(t2, { goto noforward; }, {});
#else
      DerefSw_HVAorCVAorSVA_Other(t1, { goto noforward; }, {});
      DerefSw_HVAorCVAorSVA_Other(t2, { goto noforward; }, {});
#endif
      if (t1 == t2) {
        goto noforward;
      } else {
        Sw_2xLSTorSTR_Other(t1, t2, { 
          /* replace smaller value by larger value, using choice stack as value trail */
          if (t1>t2) {
            VALUETRAIL__SET(pt1, t2);
          } else {
            VALUETRAIL__SET(pt2, t1);
          }
        }, { goto noforward; });
      }
    noforward:
      if (arity>1 && t1!=t2) result = CFUN__EVAL(compare__2,t1,t2);
    }
    pt1++;
    pt2++;
  }
  
  if (!result) {
    *x1 = t1;
    *x2 = t2;
  }

#if defined(OPTIM_COMP)
  /* TODO: remove.. it seems to be unnecessary */
  // VALUETRAIL__TEST_OVERFLOW(CHOICEPAD); /* TODO: was equiv to CHOICEPAD/2... why???? */
#else
  VALUETRAIL__TEST_OVERFLOW(CHOICEPAD);
#endif

  CFUN__PROCEED(result);
}
