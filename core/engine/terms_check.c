/*
 *  terms_check.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/basiccontrol.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_start.h>
#include <ciao/internals.h>

/* ------------------------------------------------------------------------- */
/* instance */

static CBOOL__PROTO(cinstance_args_aux,
                    int arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2, intmach_t *n);
static CBOOL__PROTO(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n);

CBOOL__PROTO(cinstance_args, 
             int arity,
             tagged_t *pt1,
             tagged_t *pt2,
             intmach_t *n)
{
  tagged_t x1, x2;
  return cinstance_args_aux(Arg,arity,pt1,pt2,&x1,&x2,n) && cinstance_aux(Arg,x1,x2,n);
}

static CBOOL__PROTO(cinstance_args_aux, 
                    int arity,
                    tagged_t *pt1,
                    tagged_t *pt2,
                    tagged_t *x1,
                    tagged_t *x2,
                    intmach_t *n)
{
  tagged_t 
    t1 = ~0,
    t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->choice,2*CHOICEPAD),w->trail_top))
                                /* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (arity>1 && !cinstance_aux(Arg,t1,t2,n))
      return FALSE;
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->choice,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

CVOID__PROTO(pop_choicept);
CVOID__PROTO(push_choicept, try_node_t *alt);

CBOOL__PROTO(cinstance)
{
  tagged_t t1, /* t2,*/ *pt1, *pt2;
  int result;
  intmach_t n = 0;

#if 0
  t1 = X(0);
  t2 = X(1);
#endif

  push_choicept(Arg,fail_alt);  /* try, arity=0 */

  result = cinstance_aux(Arg,X(0),X(1),&n);

  pt1 = pt2 = TagToPointer(w->choice->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = *pt2;        /* old var */
    pt2++;
    *TagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_choicept(Arg);            /* trust */

  return result;
}

static CBOOL__PROTO(cinstance_aux, 
                    tagged_t x1,
                    tagged_t x2,
                    intmach_t *n)
{
  tagged_t u, v, t1, nt;

 in:
  u=x1, v=x2;

  nt = MakeSmall(*n);
  SwitchOnVar(u,t1,
              { goto u_is_hva; },
              { goto lose; }, /* CVAs are not supported */
              { goto u_is_sva; },
              { goto one_non_var; });
  /* note that if deref(u) == deref(v), the following code must do nothing */
 u_is_hva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
              { BindSVA(v, nt); },
              { goto lose; });
  if (u != v) BindHVA(u, nt);
  goto var_win;

 u_is_sva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
              { BindSVA(v, nt); },
              { goto lose; });
  if (u != v) BindSVA(u, nt);
  goto var_win;

 var_win: 
  (*n)++;
  goto win;

 one_non_var:
                                /* one non variable */
  SwitchOnVar(v,t1,
              { BindHVA(v,u); goto win; },
              { BindCVA(v,u); goto win; },
              { BindSVA(v,u); goto win; },
              ;);

                                /* two non variables */
  if (!(v ^= u))                /* are they equal? */
    goto win;
  else if (v>=QMask)            /* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;                   /* restore v */
      if (cinstance_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2,n))
        goto in;
      else
        goto lose;
    }
  else                          /* structure. */
    {
      v ^= u;                   /* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
        goto lose;
      else if (t1&QMask)        /* large number */
        {
          intmach_t i;
        
          for (i = LargeArity(t1)-1; i>0; i--)
            if (*TaggedToArg(u,i) != *TaggedToArg(v,i)) goto lose;
          goto win;
        }
      if (cinstance_args_aux(Arg,Arity(t1),TaggedToArg(u,1),TaggedToArg(v,1),&x1,&x2,n))
        goto in;
      else
        goto lose;
    }

 win:
  return TRUE;

 lose:
  return FALSE;
}

