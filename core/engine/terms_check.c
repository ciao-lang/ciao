/*
 *  terms_check.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/basiccontrol.h>
#include <ciao/runtime_control.h>
#include <ciao/eng_gc.h>
#endif

/* ------------------------------------------------------------------------- */
/* instance */
/* TODO: optimize, port to improlog, make it work with cyclic terms */

static CBOOL__PROTO(cinstance_args_aux,
                    int arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2, intmach_t *n);
static CBOOL__PROTO(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n);

CBOOL__PROTO(cinstance_args, int arity, tagged_t *pt1, tagged_t *pt2, intmach_t *n) {
  tagged_t x1;
  tagged_t x2;
  CBOOL__CALL(cinstance_args_aux, arity, pt1, pt2, &x1, &x2, n);
  CBOOL__LASTCALL(cinstance_aux, x1, x2, n);
}

static CBOOL__PROTO(cinstance_args_aux, 
                    int arity,
                    tagged_t *pt1,
                    tagged_t *pt2,
                    tagged_t *x1,
                    tagged_t *x2,
                    intmach_t *n)
{
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->choice,2*CHOICEPAD),w->trail_top))
                                /* really: < 2*arity */
    choice_overflow(Arg,2*2*CHOICEPAD*sizeof(tagged_t),TRUE);
  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (arity>1) {
      CBOOL__CALL(cinstance_aux,t1,t2,n);
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1;
  *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->choice,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,2*CHOICEPAD*sizeof(tagged_t),TRUE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(cinstance) {
  tagged_t t1;
  // tagged_t t2;
  tagged_t *pt1;
  tagged_t *pt2;
  int result;
  intmach_t n = 0;

#if 0
  t1 = X(0);
  t2 = X(1);
#endif

  /* create a choice point to undo temporary bindings */
  CVOID__CALL(push_choicept, fail_alt);

  /* check if X(0) is an instance of X(1) */
  result = CBOOL__SUCCEED(cinstance_aux, X(0), X(1), &n);

  pt1 = pt2 = TrailTopUnmark(w->choice->trail_top); /* untrail */
  while (TrailYounger(G->trail_top,pt2)) {
    t1 = *pt2; /* old var */
    pt2++;
    *TaggedToPointer(t1) = t1;
  }
  G->trail_top = pt1;

  /* remove the choice point */
  CVOID__CALL(pop_choicept);

  CBOOL__LASTTEST(result);
}

static CBOOL__PROTO(cinstance_aux, 
                    tagged_t x1,
                    tagged_t x2,
                    intmach_t *n)
{
  tagged_t u, v, nt;

 in:
  u=x1, v=x2;

  nt = MakeSmall(*n);
  DerefSw_HVA_CVA_SVA_Other(u,
              { goto u_is_hva; },
              { goto lose; }, /* CVAs are not supported */
              { goto u_is_sva; },
              { goto one_non_var; });
  /* note that if deref(u) == deref(v), the following code must do nothing */
 u_is_hva:
  DerefSw_HVA_CVA_SVA_Other(v,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
              { BindSVA(v, nt); },
              { goto lose; });
  if (u != v) BindHVA(u, nt);
  goto var_win;

 u_is_sva:
  DerefSw_HVA_CVA_SVA_Other(v,
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
  DerefSw_HVA_CVA_SVA_Other(v,
              { BindHVA(v,u); goto win; },
              { BindCVA(v,u); goto win; },
              { BindSVA(v,u); goto win; },
              ;);

                                /* two non variables */
  if (u == v)                   /* are they equal? */
    goto win;
  else if (!TaggedSameTag(u, v))            /* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      if (cinstance_args_aux(Arg,2,TaggedToCar(u),TaggedToCar(v),&x1,&x2,n))
        goto in;
      else
        goto lose;
    }
  else                          /* structure. */
    {
      tagged_t t1;
      if (TaggedToHeadfunctor(u) != (t1=TaggedToHeadfunctor(v)))
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
  CBOOL__PROCEED;

 lose:
  CBOOL__FAIL;
}

