/*
 *  term_typing.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2020 Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/internals.h>
#include <ciao/basiccontrol.h>
#include <ciao/eng_bignum.h>

CBOOL__PROTO(bu1_atom, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__LASTTEST(IsNonvarAtom(x0));
}

CBOOL__PROTO(bu1_atomic, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__LASTTEST(IsNonvarAtomic(x0));
}

CBOOL__PROTO(bu1_float, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__LASTTEST(IsFloat(x0));
}

CBOOL__PROTO(bu1_integer, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__LASTTEST(IsInteger(x0));
}

CBOOL__PROTO(bu1_number, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__LASTTEST(IsNumber(x0));
}

CBOOL__PROTO(bu1_var, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__PROCEED;},{});
  CBOOL__FAIL;
}

CBOOL__PROTO(bu1_nonvar, tagged_t x0) {
  DerefSw_HVAorCVAorSVA_Other(x0,{CBOOL__FAIL;},{});
  CBOOL__PROCEED;
}

#if !defined(OPTIM_COMP)
extern tagged_t atm_var; /* Shared */
extern tagged_t atm_attv; /* Shared */
extern tagged_t atm_float; /* Shared */
extern tagged_t atm_int; /* Shared */
extern tagged_t atm_str; /* Shared */
extern tagged_t atm_atm; /* Shared */
extern tagged_t atm_lst; /* Shared */
#endif

CFUN__PROTO(fu1_type, tagged_t, tagged_t t0) {
  DEREF(t0,t0);
  switch (TagOf(t0)) {
    case UBV:
    case SVA:
    case HVA:
      CFUN__PROCEED(atm_var);
    case CVA:
      CFUN__PROCEED(atm_attv);
    case STR:
      if (STRIsLarge(t0)) {
        CFUN__PROCEED(LargeIsFloat(t0) ? atm_float : atm_int);
      }
      CFUN__PROCEED(atm_str);
    case ATM:
      CFUN__PROCEED(atm_atm);
    case LST:
      CFUN__PROCEED(atm_lst);
    case NUM:
      CFUN__PROCEED(atm_int);
  }
  CFUN__PROCEED(ERRORTAG); /* avoid warnings */
} 

/* ------------------------------------------------------------------------- */

/* ground */
static CBOOL__PROTO(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1);
static CBOOL__PROTO(cground_aux, tagged_t x1);

static CBOOL__PROTO(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1) {
  tagged_t t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1) {
      CBOOL__CALL(cground_aux, t1);
    }
    pt1 = HeapCharOffset(pt1, sizeof(tagged_t));
  }
  *x1 = t1;
  CBOOL__PROCEED;
}

CBOOL__PROTO(cground) {
  CBOOL__LASTCALL(cground_aux, X(0));
}

static CBOOL__PROTO(cground_aux, tagged_t x1) {
  tagged_t u;

 in:
  u=x1;
  DerefSw_HVA_CVA_SVA_Other(u,
              { goto lose; },
              { goto lose; }, /* CVAs are not supported */
              { goto lose; },
              { goto non_var; });
 non_var:
  if (TaggedIsATM(u)) goto win;
  if (TaggedIsSmall(u)) goto win;
  if (TaggedIsLST(u)) {
    CBOOL__CALL(cground_args_aux, 2, TaggedToCar(u), &x1);
    goto in;
  } else { /* structure. */
    tagged_t f = TaggedToHeadfunctor(u);
    if (f&QMask) {    /* large number */
      goto win;
    } else {
      CBOOL__CALL(cground_args_aux, Arity(f), TaggedToArg(u,1), &x1);
      goto in;
    }
  }
 lose:
  CBOOL__FAIL;
 win:
  CBOOL__PROCEED;
}

