/*
 *  code.h
 */

#ifndef _CIAO_AND_SIM_PLAN_CODE_H
#define _CIAO_AND_SIM_PLAN_CODE_H

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/task_areas.h>
#include <ciao/wam_macros.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* --------------------------- */
/*           Defines           */
/* --------------------------- */

#if !defined(TRUE)
#define	TRUE		1
#define	FALSE		0
#endif

#define TIME 0
#define START 1
#define RESTART 2
#define ANSWER 3
#define FAIL 4
#define FAIL_FAKE 5

#define IEVENTS (goals[goalsSize - 1])
#define IEVENT (IEVENTS.levent[IEVENTS.size-1])

#define EMPTY_LIST 0xA00000F0
#define ENGINE_HeadOfTerm(TERM) (*TagToPointer(TERM))
#define ENGINE_TailOfTerm(TERM) (*(TagToPointer(TERM) + 1))
#define ENGINE_AtomName(ATOM) (((atom_t *)TagToAtom(ATOM))->name)
#define ENGINE_NameOfFunctor(FUNCTOR)					\
  (((atom_t *)TagToAtom(SetArity(TagToHeadfunctor(FUNCTOR),0)))->name)
#define ENGINE_IsAtomTerm(TERM) (IsAtom(TERM)) 
#define ENGINE_ArgOfTerm(A,TERM) (*TagToArg(TERM,A))
#define ENGINE_IntOfTerm(TERM) (GetInteger(TERM)) 

struct lready
{
  unsigned int *goals;
  unsigned int size;
};

#define WAM_GIND(I)       (lwams->wams[(I)].ind) 
#define WAM_FAns(I)       (lwams->wams[(I)].ans) 
#define WAM_iAns(I)       (lwams->wams[(I)].iAns) 
#define WAM_NAns(I)       (lwams->wams[(I)].NAns) 
#define WAM_DEP(I)        (lwams->wams[(I)].depen) 
#define WAM_Ans(I)        (lwams->wams[(I)].ans) 
#define WAM_WAIT(I)       (lwams->wams[(I)].wait)
#define WAM_WAITS(I)      (lwams->wams[(I)].waitS)
#define WAM_IWAIT(I,J)    (lwams->wams[(I)].wait[WAM_WAITS(I)-1].ids[(J)])
#define WAM_IWAIT_O(I,J)  (lwams->wams[(I)].wait[WAM_WAITS(I)-1].idsO[(J)])
#define WAM_IWAITID(I)    (lwams->wams[(I)].wait[WAM_WAITS(I)-1].ids)
#define WAM_IWAITID_O(I)  (lwams->wams[(I)].wait[WAM_WAITS(I)-1].idsO)
#define WAM_IWAITS(I)     (lwams->wams[(I)].wait[WAM_WAITS(I)-1].idsS)
#define WAM_Last(I)       (lwams->wams[(I)].wait[WAM_WAITS(I)-1].last) 
#define WAM_TIME(I)       (lwams->wams[(I)].time)
#define Th_NextType(I)    (goals[(I)].levent[WAM_GIND(I)].type)
#define Th_NextSize(I)    (goals[(I)].levent[WAM_GIND(I)].size)
#define Th_NextValue(I,J) (goals[(I)].levent[WAM_GIND(I)].values[(J)])
#define ITH_W             (lthreads[iTh])

struct wait
{
  unsigned int *ids;
  unsigned int *idsO;
  unsigned int idsS;
  unsigned int last;
};

struct wam
{
  unsigned int ind;
  unsigned int waitS;
  unsigned int time;
  unsigned int ans;
  unsigned int iAns;
  unsigned int NAns;
  int depen;
  struct wait *wait;
};

struct lwam
{
  struct wam *wams;
  unsigned int Nwams;
};

struct event
{
  unsigned int type;
  unsigned int size;
  unsigned int *values;
};

struct levent
{
  struct event *levent;
  unsigned int size;
};

struct prec
{
  unsigned int *values;
  unsigned int size;
  unsigned int father;
};

struct lprec
{
  struct prec *lprec;
  unsigned int size;
};

#endif /* _CIAO_AND_SIM_PLAN_CODE_H */
