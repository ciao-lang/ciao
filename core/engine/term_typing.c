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

CBOOL__PROTO(bu1_atom, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (TermIsATM(x0));
}

//TOTY
CBOOL__PROTO(bu1_atomic, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return (!(x0 & TagBitComplex) || TaggedIsLarge(x0));
}

//TOTY
CBOOL__PROTO(bu1_float, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsFloat(x0);
}

//TOTY
CBOOL__PROTO(bu1_integer, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsInteger(x0);
}

//TOTY
CBOOL__PROTO(bu1_number, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return IsNumber(x0);
}

//TOTY
CBOOL__PROTO(bu1_var, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return TRUE;})
  return FALSE;
}

//TOTY
CBOOL__PROTO(bu1_nonvar, tagged_t x0)
{
  CIAO_REG_2(tagged_t, t0);

  DerefSwitch(x0,t0,{return FALSE;})
  return TRUE;
}

extern tagged_t atm_var; /* Shared */
extern tagged_t atm_attv; /* Shared */
extern tagged_t atm_float; /* Shared */
extern tagged_t atm_int; /* Shared */
extern tagged_t atm_str; /* Shared */
extern tagged_t atm_atm; /* Shared */
extern tagged_t atm_lst; /* Shared */

//TOTY
CFUN__PROTO(fu1_type, tagged_t, tagged_t t0) {
  DEREF(t0,t0);
  switch (TagOf(t0)) {
    case UBV:
    case SVA:
    case HVA:
      return atm_var;
    case CVA:
      return atm_attv;
    case STR:
      if (STRIsLarge(t0)) 
        return LargeIsFloat(t0) ? atm_float : atm_int;
      return atm_str;
    case ATM:
      return atm_atm;
    case LST:
      return atm_lst;
    case NUM:
      return atm_int;
  }
  return (tagged_t)NULL;                                  /* avoid warnings */
} 

/* ------------------------------------------------------------------------- */

//TOTY
/* ground */
static CBOOL__PROTO(cground_args_aux, int arity, tagged_t *pt1, tagged_t *x1);
static CBOOL__PROTO(cground_aux, tagged_t x1);

static CBOOL__PROTO(cground_args_aux, 
                    int arity,
                    tagged_t *pt1,
                    tagged_t *x1)
{
  tagged_t 
    t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && !cground_aux(Arg,t1)) return FALSE;
    (void)HeapNext(pt1);
  }
  *x1 = t1;
  return TRUE;
}

CBOOL__PROTO(cground)
{
  return cground_aux(Arg,X(0));
}

static CBOOL__PROTO(cground_aux, tagged_t x1)
{
  tagged_t u, t1;

 in:
  u=x1;

  SwitchOnVar(u,t1,
              { goto lose; },
              { goto lose; }, /* CVAs are not supported */
              { goto lose; },
              { goto non_var; });

 non_var:
  if (TaggedIsATM(u)) goto win;
  if (TaggedIsSmall(u)) goto win;
  if (TaggedIsLST(u)) {
      if (cground_args_aux(Arg,2,TagToCar(u),&x1))
        goto in;
      else
        goto lose;
    }
  else                          /* structure. */
    {
      t1=TagToHeadfunctor(u);
      if (t1&QMask)     /* large number */
        {
          goto win;
        }
      if (cground_args_aux(Arg,Arity(t1),TagToArg(u,1),&x1))
        goto in;
      else
        goto lose;
    }

 lose:
  return FALSE;
 win:
  return TRUE;
}

