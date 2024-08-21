/*
 *  terms_check.c
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020-2024 The Ciao Development Team
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/basiccontrol.h>
#include <ciao/eng_gc.h>
#endif

/* ------------------------------------------------------------------------- */
/* instance */
/* TODO: optimize, port to improlog, make it work with cyclic terms */

static CBOOL__PROTO(cinstance_args_aux,
                    arity_t arity, tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2, intmach_t *n);
static CBOOL__PROTO(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n);

CBOOL__PROTO(cinstance_args, arity_t arity, tagged_t *pt1, tagged_t *pt2, intmach_t *n) {
  tagged_t x1;
  tagged_t x2;
  CBOOL__CALL(cinstance_args_aux, arity, pt1, pt2, &x1, &x2, n);
  CBOOL__LASTCALL(cinstance_aux, x1, x2, n);
}

// TODO:[oc-merge] missing 2* in OC
#if defined(OPTIM_COMP)
#define ENSURE_CHOICE(Pad) { \
  if (ChoiceCharDifference(w->choice,G->trail_top) < (Pad)) \
    CVOID__CALL(choice_overflow,2*(Pad),TRUE); \
}
#else
#define ENSURE_CHOICE(Pad) { \
  if (ChoiceYounger(ChoiceOffset(w->choice,(Pad)),G->trail_top)) \
    choice_overflow(Arg,2*(Pad)*sizeof(tagged_t),TRUE); \
}
#endif

static CBOOL__PROTO(cinstance_args_aux,
                    arity_t arity,
                    tagged_t *pt1, tagged_t *pt2,
                    tagged_t *x1, tagged_t *x2,
                    intmach_t *n) {
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  /* really: < 2*arity */
  ENSURE_CHOICE(2*CHOICEPAD);

  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (arity>1) {
      CBOOL__CALL(cinstance_aux, t1, t2, n);
    }
    pt1 = HeapCharOffset(pt1, sizeof(tagged_t));
    pt2 = HeapCharOffset(pt2, sizeof(tagged_t));
  }

  *x1 = t1;
  *x2 = t2;

  ENSURE_CHOICE(CHOICEPAD);
  CBOOL__PROCEED;
}

CBOOL__PROTO(cinstance) {
  tagged_t t1;
  // tagged_t t2;
  tagged_t *pt1;
  tagged_t *pt2;
  bool_t result;
  intmach_t n = 0;

  // t1 = X(0);
  // t2 = X(1);

  /* create a choice point to undo temporary bindings */
  CVOID__CALL(push_choicept, fail_alt);

  /* check if X(0) is an instance of X(1) */
  result = CBOOL__SUCCEED(cinstance_aux, X(0), X(1), &n);

  /* untrail temporary bindings */
  /* TODO: merge with untrail in copy_it */
#if defined(OPTIM_COMP)
  pt1 = pt2 = w->choice->trail_top; /* untrail */
#else
  pt1 = pt2 = TrailTopUnmark(w->choice->trail_top); /* untrail */
#endif
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

static CBOOL__PROTO(cinstance_aux, tagged_t x1, tagged_t x2, intmach_t *n) {
  tagged_t u, v, nt;

 in:
  u=x1;
  v=x2;

  /* note that if deref(u) == deref(v), the following code must do nothing */

  nt = MakeSmall(*n);
  DerefSw_HVA_CVA_SVA_Other(u, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* HVA x HVA */ BindHVA(v, nt); if (u != v) { BindHVA(u, nt); } goto var_win;
    }, {
      /* HVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* HVA x SVA */ BindSVA(v, nt); if (u != v) { BindHVA(u, nt); } goto var_win;
    }, {
      /* HVA x NVA */ goto lose;
    });
  }, {
    /* CVA x _ */ goto lose; /* CVAs are not supported */
  }, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* SVA x HVA */ BindHVA(v, nt); if (u != v) { BindSVA(u, nt); } goto var_win;
    }, {
      /* SVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* SVA x SVA */ BindSVA(v, nt); if (u != v) { BindSVA(u, nt); } goto var_win;
    }, {
      /* SVA x NVA */ goto lose;
    });
  }, {
    DerefSw_HVA_CVA_SVA_Other(v, {
      /* NVA x HVA */ BindHVA(v,u); goto win;
    }, {
      /* NVA x CVA */ goto lose; /* CVAs are not supported */
    }, {
      /* NVA x SVA */ BindSVA(v,u); goto win;
    }, { /* NVA x NVA */
      if (u == v) { /* same NVA */
        goto win;
      } else if (!TaggedSameTag(u, v)) { /* not the same type? */
        goto lose;
      } else {
        Sw_NUMorATM_LST_STR(u, { /* NUM ATM */
          /* fail */
          goto lose;
        }, {
          /* LST x LST */
          CBOOL__CALL(cinstance_args_aux, 2, TaggedToCar(u), TaggedToCar(v), &x1, &x2, n);
          goto in;
        }, {
          /* STR x STR */
          tagged_t f = TaggedToHeadfunctor(u);
          tagged_t t1 = TaggedToHeadfunctor(v);
          if (f != t1) {
            goto lose;
          } else {
            if (FunctorIsBlob(t1)) { /* STRBlob x STRBlob */
#if defined(OPTIM_COMP)
              CBOOL__TEST(compare_blob(TagpPtr(STR, u), TagpPtr(STR, v)));
#else
              for (intmach_t i = LargeArity(t1)-1; i>0; i--) {
                if (*TaggedToArg(u,i) != *TaggedToArg(v,i)) goto lose;
              }
#endif
              goto win;
            } else { /* STRStruct x STRStruct */
              CBOOL__CALL(cinstance_args_aux, Arity(t1), TaggedToArg(u,1), TaggedToArg(v,1), &x1, &x2, n);
              goto in;
            }
          }
        });
      }
    });
  });

 var_win:
  (*n)++;
  goto win;

 win:
  /* two non variables */
  CBOOL__PROCEED;
 lose:
  CBOOL__FAIL;
}
