/*
 *  term_support.c
 *
 *  Term compiler for assert/record.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/gcdatadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/wamsupport.h>
#include <ciao/wam_macros.h>
#include <ciao/instrdefs.h>
#include <ciao/task_areas.h> /* For register bank reallocation */
#include <ciao/objareas.h>
#include <ciao/float_tostr.h>
#include <ciao/term_support.h>
#include <ciao/start.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/stacks.h>
#include <ciao/nondet.h>
#include <ciao/io_basic.h>
#include <ciao/bignum.h>

/* Unify with occurs-check, using inline checks (var-nonvar cases) */
#define UNIFY_OC_INLINE 1
/* Enable tail optimization in cyclic_term */
#define CYCLIC_TERM_TAIL_OPTIM 1

/* local declarations */

static CBOOL__PROTO(c_term, 
		    tagged_t t,
		    int Xreg,
		    int FreeReg,
		    int x_variables,
		    tagged_t **trail_origo,
		    bcp_t *current_insn);
static CBOOL__PROTO(prolog_constant_codes, 
		    bool_t atomp,
		    bool_t numberp,
		    int ci);
static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P);
static CVOID__PROTO(c_term_mark,
		    tagged_t t,
		    intmach_t temps,
		    intmach_t *cells,
		    intmach_t *maxtemps,
		    intmach_t *bsize,
		    tagged_t **trail_origo);
static CVOID__PROTO(c_term_trail_push,
		    tagged_t t,
		    tagged_t **trail_origo);
static CVOID__PROTO(copy_it, tagged_t *loc);
static CVOID__PROTO(copy_it_nat, tagged_t *loc);

#define TopOfOldHeap TagToHVA(w->global_uncond)
/* NOTE: Aligned to the size of the pointers! */
#define ODDOP(Insn)	   \
  if (((uintptr_t)P)&(sizeof(uintptr_t)-1))	\
    { EMIT(Insn-0); }	\
  else \
    { EMIT(Insn-1); EMIT_Q(0); }
#define EVENOP(Insn) \
  if (!(((uintptr_t)P)&(sizeof(uintptr_t)-1)))	\
    { EMIT(Insn-0); }				\
  else \
    { EMIT(Insn-1); EMIT_Q(0); }
#define EMIT(I) { Last_Insn=P; EMIT_o((I)); }

#define GCTEST(Pad) { \
    if (HeapDifference(w->global_top,Heap_End) < (Pad)) \
      heap_overflow(Arg,Pad); \
    if (ChoiceDifference(w->node,w->trail_top) < (Pad)) \
      choice_overflow(Arg,Pad); \
  }

static CVOID__PROTO(copy_it, tagged_t *loc);
static CVOID__PROTO(copy_it_nat, tagged_t *loc);

/* TODO: Move as a compiler context structure -- JFMC */

/* I believe the following must be shared and locked, because they are
   related to code compilation */

/*static bcp_t */
 /* current_insn, */        /* Inside compile_term_aux and passed around */
 /* last_insn;    */                          /* Inside compile_term_aux */


/*static int*/
 /* x_variables, */       /* Now inside compile_term_aux and pased around */
 /* cells,       */                         /* Inited in compile_term_aux */
 /* bsize,       */                         /* Inited in compile_term_aux */
 /* maxtemps;    */                         /* Inited in compile_term_aux */

/*static tagged_t*/
 /* *trail_origo;*/                         /* Inited in compile_term_aux */


static CVOID__PROTO(c_term_trail_push, tagged_t t, tagged_t **trail_origo);
static CVOID__PROTO(c_term_mark,
		    tagged_t t, intmach_t temps,
		    intmach_t *cells, intmach_t *maxtemps, intmach_t *bsize, tagged_t **trail_origo);
static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P);
static CBOOL__PROTO(c_term, tagged_t t, int Xreg, int FreeReg,
		    int x_variables, tagged_t **trail_origo,
		    bcp_t *current_insn);
CBOOL__PROTO(compile_term, worker_t **new_worker);
CFUN__PROTO(compile_term_aux, instance_t *,
	    tagged_t head, tagged_t body, worker_t **new_worker);

static CVOID__PROTO(c_term_trail_push, tagged_t t, tagged_t **trail_origo) {
  if (!ChoiceDifference(w->node,w->trail_top)) {
    tagged_t *tr = w->trail_top;
    int reloc;

    choice_overflow(Arg,-CHOICEPAD);
    reloc = (char *)w->trail_top - (char *)tr;
    *trail_origo = (tagged_t *)((char *)*trail_origo + reloc);
    tr = w->trail_top;
    while (tr[-1] & QMask)
      TrailPop(tr) += reloc;
    while (TrailYounger(tr,*trail_origo+DynamicPreserved))
      *TagToPointer(TrailPop(tr)) += reloc;
  }
  TrailPush(w->trail_top,t);
}

//#define Tr(Str) fprintf(stderr, "%s\n", (Str));
#define Tr(Str)

static CVOID__PROTO(c_term_mark,
		    tagged_t t,
		    intmach_t temps,
		    intmach_t *cells, intmach_t *maxtemps, intmach_t *bsize,
		    tagged_t **trail_origo) {
  CIAO_REG_2(tagged_t, t1);
  int i, arity;

 start:
  DerefHeapSwitch(t,t1,{goto var_size;});
  /* nonvar */
  if (!(t & TagBitComplex)) { /* NUM or ATM */
    if (t&QMask && t&TagBitFunctor) { /* ATM with QMask mark */
      if (!(t&3)) {
	/* unify_variable */
	Tr("m:x");
	*bsize += FTYPE_size(f_x);
	(*TagToPointer(*TagToPointer(t)))++;
      } else {
	/* unify_value */
	Tr("m:x");
	*bsize += FTYPE_size(f_x);
      }
    } else if (t!=atom_nil) {
      /* unify_ + pad + constant */
      Tr("m:(Q)+t");
      *bsize += FTYPE_size(f_Q)+FTYPE_size(f_t);
    }
    return;
  } else if (!(t & TagBitFunctor)) { /* LST */
    /* unify_ + xi + get_list + xj + 2*u */
    Tr("m:x+o (?)");
    Tr("m:x");
    *bsize += (FTYPE_size(f_x)+
	       FTYPE_size(f_o)+
	       FTYPE_size(f_x)+
	       2*FTYPE_size(f_o));
    *cells += 2;
    if (*maxtemps < temps) {
      *maxtemps = temps;
    }
    RefCar(t1,t);
    Tr("m:o"); /* (from *bsize+=) */
    c_term_mark(Arg, t1, temps+1, cells, maxtemps, bsize, trail_origo);
    RefCdr(t,t);
    Tr("m:o"); /* (from *bsize+=) */
    goto start;
  } else { /* STR or large NUM */
    if (STRIsLarge(t)) {
      arity = LargeSize(TagToHeadfunctor(t)); /* TODO: not 'arity'! */
      /* unify_ + xi + get_large + pad + xj + (functor + largeNum) */
      Tr("m:x+o (?)");
      Tr("m:(Q)+x+o(?)+bnlen");
      *bsize += (FTYPE_size(f_x)+
		 FTYPE_size(f_o)+
		 FTYPE_size(f_Q)+
		 FTYPE_size(f_x)+
		 FTYPE_size(f_o)+
		 arity);
      *cells += 1+(arity / sizeof(tagged_t));
      return;
    } else {
      arity = Arity(TagToHeadfunctor(t));
      /* unify_ + xi + get_structure + pad + functor + xj + arity*u */
      Tr("m:x+o (?)");
      Tr("m:(Q)+x+f");
      *bsize += (FTYPE_size(f_x)+
		 FTYPE_size(f_o)+
		 FTYPE_size(f_Q)+
		 FTYPE_size(f_x)+
		 FTYPE_size(f_f)+
		 arity*FTYPE_size(f_o));
      *cells += 1+arity;
      if (*maxtemps < temps) {
	*maxtemps = temps;
      }
      for (i=1; i<arity; i++) {
	Tr("m:o"); /* (from *bsize+=) */
	RefArg(t1,t,i);
	c_term_mark(Arg, t1, temps+arity-i, cells, maxtemps, bsize, trail_origo);
      }
      Tr("m:o"); /* (from *bsize+=) */
      RefArg(t,t,arity);
      goto start;
    }
  }
 var_size:
  if (t & TagBitCVA) { /* CVA */
    *TagToPointer(t) = Tag(ATM,w->trail_top)|QMask|2;
    c_term_trail_push(Arg,t,trail_origo);
    /* unify_variable + xi + get_constraint + xj + 2*u */
    Tr("m:x");
    Tr("m:o+x");
    Tr("m:o");
    Tr("m:o");
    *bsize += (FTYPE_size(f_x)+
	       FTYPE_size(f_o)+
	       FTYPE_size(f_x)+
	       FTYPE_size(f_o)+
	       FTYPE_size(f_o));
    if (*maxtemps < temps) {
      *maxtemps = temps;
    }
    t = Tag(LST,TagToGoal(t));
    goto start;
  } else { /* HVA */
    *TagToPointer(t) = Tag(ATM,w->trail_top)|QMask;
    c_term_trail_push(Arg,t,trail_origo);
    /* unify_variable + xi */
    Tr("m:x");
    *bsize += FTYPE_size(f_x);
  }
  return;
}

static CFUN__PROTO(emit_unify_void, bcp_t, bcp_t P) {
  switch (BCOp(Last_Insn, FTYPE_ctype(f_o), 0)) {
  case UNIFY_VOID_1:
  case UNIFY_VOID_2:
  case UNIFY_VOID_3:
    BCOp(Last_Insn, FTYPE_ctype(f_o), 0)++;
    break;
  case UNIFY_VOID_4:
    BCOp(Last_Insn, FTYPE_ctype(f_o), 0) = UNIFY_VOID;
    Tr("e:i");
    EMIT_i(5);
    break;
  case UNIFY_VOID:
    BCOp(P, FTYPE_ctype(f_i), -FTYPE_size(f_i))++;
    break;
  default:
    Tr("e:o");
    EMIT(UNIFY_VOID_1);
  }
  return P;
}

static CBOOL__PROTO(c_term, 
		    tagged_t t,
		    int Xreg, int FreeReg,
		    int x_variables,
		    tagged_t **trail_origo,
		    bcp_t *current_insn) {
  bcp_t psave;
  tagged_t *ssave;
  int i, ar = ~0, decr, Treg;
  bcp_t P = *current_insn;
  tagged_t *s = NULL;
  tagged_t t1;

  /* Step 1: Emit GET instruction for term's principal functor. */
  switch (TagOf(t)) { /* t is already dereferenced */
  case LST:
    ar = 2;
    s = TagToLST(t);
    Tr("e:o+x");
    EMIT(GET_LIST);
    EMITtok(f_x, Xop(Xreg));
    break;
  case STR:
    if (STRIsLarge(t)) {
      Tr("e:o(Q)+x+bnlen");
      EVENOP(GET_LARGE);
      EMITtok(f_x, Xop(Xreg));
      P = BCoff(P, compile_large(t, P));
      *current_insn = P;
      return TRUE;
    } else {
      Tr("e:o(Q)+x+f");
      ar = Arity(TagToHeadfunctor(t));
      s = TagToArg(t,1);
      EVENOP(GET_STRUCTURE);
      EMITtok(f_x, Xop(Xreg));
      EMIT_f(TagToHeadfunctor(t));
      break;
    }
  case ATM:
    if (t & QMask) {
      goto term_is_var;
    } else if (t==atom_nil) {
      Tr("e:o+x");
      EMIT(GET_NIL);
      EMITtok(f_x, Xop(Xreg));
      *current_insn = P;
      return TRUE;
    }
  case NUM:
    Tr("e:o(Q)+x+t");
    EVENOP(GET_CONSTANT);
    EMITtok(f_x, Xop(Xreg));
    EMIT_t(t);
    *current_insn = P;
    return TRUE;

  term_is_var:
    {
      if ((t&3)!=3) { /* get_variable */
	Tr("e:o+x+x");
	EMIT(GET_X_VARIABLE);
	EMITtok(f_x, Xop(Xreg));
	EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
	if (t&2) { /* enqueue constraint */
	  c_term_trail_push(Arg,t,trail_origo);
	}
	*TagToPointer(*TagToPointer(t)) |= 3;
      } else { /* get_value */
	Tr("e:o+x+x");
	EMIT(GET_X_VALUE);
	EMITtok(f_x, Xop(Xreg));
	EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
      }
      *current_insn = P;
      return TRUE;
    }
  }

  /* Step 2: Emit tail-recursive UNIFY sequence for all subargs. */
  psave = P;
  ssave = s;
  Treg = reg_bank_size;

  for (i=1; i<=ar; i++) {
    RefHeapNext(t,s);
    DerefHeapSwitch(t,t1,{ goto arg_is_void; });
    switch (TagOf(t)) {
    case LST:
      if ((i==ar) && (Treg==reg_bank_size)) {
	Tr("e:o");
	EMIT(UNIFY_LIST);
	s = TagToLST(t);
	i=0, ar=2;
      } else {
	Tr("e:o+x");
	EMIT(UNIFY_X_VARIABLE);
	EMITtok(f_x, Xop(Treg++));
      }
      break;
    case STR:
      if (STRIsLarge(t)) {
	if ((i==ar) && (Treg==reg_bank_size)) {
	  Tr("e:o(Q)+bnlen");
	  ODDOP(UNIFY_LARGE);
	  P = BCoff(P, compile_large(t, P));
	} else {
	  Tr("e:o+x");
	  EMIT(UNIFY_X_VARIABLE);
	  EMITtok(f_x, Xop(Treg++));
	}
      } else if ((i==ar) && (Treg==reg_bank_size)) {
	Tr("e:o(Q)+f");
	ODDOP(UNIFY_STRUCTURE);
	EMIT_f(TagToHeadfunctor(t));
	s = TagToArg(t,1);
	i=0, ar=Arity(TagToHeadfunctor(t));
      } else {
	Tr("e:o+x");
	EMIT(UNIFY_X_VARIABLE);
	EMITtok(f_x, Xop(Treg++));
      }
      break;
    case ATM:
      if (t & QMask) {
	goto arg_is_var;
      } else if (t==atom_nil) {
	Tr("e:o");
	EMIT(UNIFY_NIL);
	break;
      }
    case NUM:
      Tr("e:o(Q)+t");
      ODDOP(UNIFY_CONSTANT);
      EMIT_t(t);
      break;

    arg_is_var:
      {
	if ((t&3)!=3) { /* unify_variable */
	  Tr("e:o+x");
	  EMIT(UNIFY_X_VARIABLE);
	  EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
	  if (t&2)	/* enqueue constraint */
	    c_term_trail_push(Arg,t,trail_origo);
	  *TagToPointer(*TagToPointer(t)) |= 3;
	} else { /* unify_value */
	  Tr("e:o+x");
	  EMIT(UNIFY_X_VALUE);
	  EMITtok(f_x, Xop(TagToPointer(t) - *trail_origo));
	}
	break;
      }

    arg_is_void:
      P = emit_unify_void(Arg, P);
      break;
    }
  }

  /* Step 3: Scan emitted code and recursively emit code for nested args. */
  *current_insn = P;
  if (FreeReg < x_variables)
    return FALSE;
  if (Treg==reg_bank_size)
    return TRUE;
  decr = Treg-1-FreeReg;
  s = ssave;
  P = psave;
  psave = *current_insn;

  while (P < psave) {
    switch (BcFetchOPCODE()) {
    case UNIFY_LIST:
      DerefHeap(t,s);
      s = TagToLST(t);
      break;
    case UNIFY_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_f));
      DerefHeap(t,s);
      s = TagToArg(t,1);
      break;
    case UNIFY_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_LARGE:
      P = BCoff(P, LargeSize(*(tagged_t *)P));
      (void)HeapNext(s);
      break;
    case UNIFY_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_CONSTANT:
      P = BCoff(P, FTYPE_size(f_t));
    case UNIFY_NIL:
      (void)HeapNext(s);
      break;
    case UNIFY_X_VARIABLE:
      i = Xinv(BCOp(P, FTYPE_ctype(f_x), 0));
      if (i>=reg_bank_size) {
	EMITtok(f_x, Xop(i-decr)); /* TODO: patch? */
	DerefHeapNext(t,s);
	if (!c_term(Arg,t,i-decr,i-decr,x_variables,
		    trail_origo,current_insn))
	  return FALSE;
	break;
      }
    case UNIFY_X_VALUE:
      P = BCoff(P, FTYPE_size(f_x));
      (void)HeapNext(s);
      break;
    case UNIFY_VOID:
      s += BCOp(P, FTYPE_ctype(f_i), 0);
      P = BCoff(P, FTYPE_size(f_i));
      break;
    case UNIFY_VOID_4:
      (void)HeapNext(s);
    case UNIFY_VOID_3:
      (void)HeapNext(s);
    case UNIFY_VOID_2:
      (void)HeapNext(s);
    case UNIFY_VOID_1:
      (void)HeapNext(s);
      break;
    default:
      SERIOUS_FAULT("compile_term: internal error");
    }
  }
  return TRUE;
}

/* ASSERT: X(0) is always a dereferenced list. */
/* Also, returns in the second argument a pointer to a non-null worker
   pointer if the worker has changed, or to null if it has. */

CBOOL__PROTO(compile_term, 
	     worker_t **new_worker) {
  tagged_t head, body;
  instance_t *object;

  RefCar(head,X(0));
  RefCdr(body,X(0));

  /*
#if defined(DEBUG)
  display_term(Arg, head, Output_Stream_Ptr, FALSE);
  putchar('\n');
#endif
  */

  *new_worker = NULL;                             /* may be changed after */

  object = compile_term_aux(Arg, head, body, new_worker);
  Arg = *new_worker == NULL ? Arg : *new_worker;

  Unify_constant(PointerToTerm(object),X(1));
  return TRUE;
}

/* Note on memory consumption: compile_term_aux may increase the size of the
   stacks and the size of the X register bank, so the simple approach "see
   how much the total memory has increased" does not work.  Instead, we try
   to find out exactly where we allocate and deallocate memory for program
   storage.  c_term_mark does not allocate program memory (instead, it may
   call choice_overflow, which expands stacks). */

CFUN__PROTO(compile_term_aux, instance_t *,
	    tagged_t head, tagged_t body,
	    worker_t **new_worker) {
  int truesize;
  //  tagged_t t0, *pt1, *pt2;
  CIAO_REG_1(tagged_t, t0);
  CIAO_REG_2(tagged_t *, pt1);
  CIAO_REG_3(tagged_t *, pt2);
  instance_t *object = NULL;
  
  intmach_t x_variables, cells, bsize, maxtemps;
  tagged_t *trail_origo;
  bcp_t current_insn /*, *last_insn */ ;

  bsize = FTYPE_size(f_o); /* TODO: for DYNAMIC_NECK_PROCEED? */
  cells=CONTPAD;
  maxtemps=0;
  trail_origo = Arg->trail_top-DynamicPreserved;

  Tr("c_term_mark1");
  DerefHeapSwitch(head,t0,{goto car_done;});
  Tr("m:o+x");
  bsize += FTYPE_size(f_o)+FTYPE_size(f_x); /* for "Step 1" of c_term (get + arg + ...) */
  c_term_mark(Arg, head, 0, &cells, &maxtemps, &bsize, &trail_origo);
 car_done:
  Tr("c_term_mark2");
  DerefHeapSwitch(body,t0,{goto cdr_done;});
  Tr("m:o+x");
  bsize += FTYPE_size(f_o)+FTYPE_size(f_x); /* for "Step 1" of c_term (get + arg + ...) */
  c_term_mark(Arg, body, 0, &cells, &maxtemps, &bsize, &trail_origo);
 cdr_done:

  /* allow for heapmargin_call insn */
  if (cells>=STATIC_CALLPAD) { /* (was SOFT_HEAPPAD) */
    Tr("m:o(Q)+l+i");
    bsize += (FTYPE_size(f_o)+
	      FTYPE_size(f_Q)+
	      FTYPE_size(f_l)+
	      FTYPE_size(f_i));
  }

  /* tidy out void vars */
  pt1 = pt2 = trail_origo+DynamicPreserved;
  while (pt1 < Arg->trail_top) {
    t0 = TrailNext(pt1);
    if (*TagToPointer(t0) & 3) {
      *TagToPointer(t0) -= (char *)(pt1-1)-(char *)pt2,	TrailPush(pt2,t0);
    } else {
      *TagToPointer(t0) = t0;
    }
  }
  Arg->trail_top = pt2;
  x_variables = pt2-trail_origo;

				/* ensure enough X registers */
  if (x_variables+maxtemps > reg_bank_size) {
    int size0 = reg_bank_size;
      
    if (x_variables+maxtemps > (1<<14) - WToX0 - maxtemps) /* TODO: ad-hoc size */
      goto sizebomb;

    reg_bank_size=x_variables+maxtemps;

    *new_worker = /* For local use */
      checkrealloc_FLEXIBLE(worker_t,
			    tagged_t,
			    size0,
			    reg_bank_size,
			    Arg);
#if defined(DEBUG)
    fprintf(stderr, "Reallocing WRB from %p to %p\n", Arg, *new_worker);
#endif
    Arg = *new_worker;
  }

  checkalloc_FLEXIBLE_S(instance_t,
                        objsize,
                        char,
                        bsize,
                        object);
  INC_MEM_PROG(object->objsize);
  current_insn = (bcp_t)object->emulcode;
  object->pending_x2 = NULL;
  object->pending_x5 = NULL;

  if (cells>=STATIC_CALLPAD) { /* (was SOFT_HEAPPAD) */
    bcp_t P = current_insn;

    Tr("e:o(Q)+l+i");
    ODDOP(HEAPMARGIN_CALL);
    EMIT_l(cells);
    EMIT_i(DynamicPreserved);
    current_insn = P;
  }

  if (!IsVar(head) &&
      !c_term(Arg,head,0,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;
  if (!IsVar(body) &&
      !c_term(Arg,body,1,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;

  while (Arg->trail_top[-1] & QMask) {
    bcp_t P = current_insn;

    if (!TrailYounger(Arg->trail_top,Trail_Start))
      break;
    t0 = TrailPop(Arg->trail_top);
    if (!c_term(Arg,
                Tag(LST,TagToGoal(*TagToPointer(t0))),
                TagToPointer(t0)-trail_origo,
                  reg_bank_size-1,
                x_variables,
                &trail_origo,
                &current_insn))
      goto sizebomb;
    BCOp(P, FTYPE_ctype(f_o), 0) = GET_CONSTRAINT;
  }

  {
    bcp_t P = current_insn;
    Tr("e:o");
    EMIT_o(DYNAMIC_NECK_PROCEED);
    current_insn = P;
  }
  truesize = SIZEOF_FLEXIBLE_STRUCT(instance_t, char, (char *)current_insn - (char *)object->emulcode);
  if (truesize > object->objsize) {
    DEC_MEM_PROG(object->objsize);
    checkdealloc_FLEXIBLE_S(instance_t, objsize, object);
    SERIOUS_FAULT("bug: memory overrun in assert or record");
  }

  if (IsVar(head)) {
    /* findall record---make it fast */
  } else {
    INC_MEM_PROG(truesize - object->objsize);
    //fprintf(stderr, "resize %x-%x\n", object->objsize, truesize);
    object=(instance_t *)checkrealloc((tagged_t *)object, object->objsize, truesize);
    object->objsize = truesize;
  }

  pt2 = Arg->trail_top;
  Arg->trail_top = trail_origo+DynamicPreserved;
  while (TrailYounger(pt2,Arg->trail_top)) {
    PlainUntrail(pt2,t0,{});
  }

  if (TagIsSTR(head))  {
    DerefArg(t0,head,1);
    if (TagIsSTR(t0)) {
      object->key = TagToHeadfunctor(t0);
    } else if (TagIsLST(t0)) {
      object->key = functor_list;
    } else if (!IsVar(t0)) {
      object->key = t0;
    } else {
      object->key = ERRORTAG;
    }
  } else {
    object->key = ERRORTAG;
  }

  Tr("c_term_end");
  return object;

 sizebomb:
  DEC_MEM_PROG(object->objsize);
  checkdealloc_FLEXIBLE_S(instance_t, objsize, object);
  SERIOUS_FAULT("term too large in assert or record");
}


#if defined(OLD_DATABASE)
/* Support for current_key/2: given a '$current instance'/2 instance,
   the key of which is a large number, decode the key. */
tagged_t decode_instance_key(instance_t *inst) {
  bcp_t P = inst->emulcode;
  int xreg = -1;

  for (;;) {
    switch (BcFetchOPCODE()) {
    case HEAPMARGIN_CALLQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case HEAPMARGIN_CALL:
      P = BCoff(P, FTYPE_size(f_l)+FTYPE_size(f_i));
      break;
    case GET_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_CONSTANT:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_t));
      break;
    case GET_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_f));
      break;
    case GET_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case GET_LARGE:
      if (xreg == Xinv(BCOp(P, FTYPE_ctype(f_x), 0)))
	return MakeLarge(BCoff(P, FTYPE_size(f_x)));
      P = BCoff(P, 
		FTYPE_size(f_x)+
		LargeSize((*(tagged_t *)(BCoff(P, FTYPE_size(f_x))))));
      break;
    case UNIFY_CONSTANTQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_CONSTANT:
      P = BCoff(P, FTYPE_size(f_t));
      break;
    case UNIFY_STRUCTUREQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_STRUCTURE:
      P = BCoff(P, FTYPE_size(f_f));
      break;
    case UNIFY_LARGEQ:
      P = BCoff(P, FTYPE_size(f_Q));
    case UNIFY_LARGE:
      P = BCoff(P, LargeSize(*(tagged_t *)P));
      break;
    case UNIFY_X_VARIABLE:
      if (xreg == -1)
	xreg = Xinv(BCOp(P, FTYPE_ctype(f_x), 0));
    case UNIFY_X_VALUE:
    case UNIFY_VOID:
    case GET_LIST:
    case GET_CONSTRAINT:
    case GET_NIL:
      P = BCoff(P, FTYPE_size(f_x));
      break;
    case GET_X_VARIABLE:
    case GET_X_VALUE:
      P = BCoff(P, FTYPE_size(f_x)+FTYPE_size(f_x));
    default:
      break;
    }
  }
}
#endif

/* Shared, no locked --- but it should be private! */
static int radixlim1;
static int radixlim2;
static int radixlim3;

bool_t prolog_init_radix(void) {
  int radix = GetSmall(current_radix);

  if (radix<10) {
    radixlim1 = '0'+radix;
    radixlim2 = 'a';
    radixlim3 = 'A';
  } else {
    radixlim1 = '0'+10;
    radixlim2 = 'a'+radix-10;
    radixlim3 = 'A'+radix-10;
  }
  return TRUE;
}

static CBOOL__PROTO(prolog_constant_codes, bool_t a,bool_t n, int ci);

CBOOL__PROTO(prolog_name) {
  // ERR__FUNCTOR("atomic_basic:name", 2);
  return prolog_constant_codes(Arg,TRUE,TRUE,1);
}

CBOOL__PROTO(prolog_atom_codes) {
  // ERR__FUNCTOR("atomic_basic:atom_codes", 2);
  return prolog_constant_codes(Arg,TRUE,FALSE,1);
}

CBOOL__PROTO(prolog_number_codes_2) {
  // ERR__FUNCTOR("atomic_basic:number_codes", 2);
  return prolog_constant_codes(Arg,FALSE,TRUE,1);
}

CBOOL__PROTO(prolog_number_codes_3) {
  // ERR__FUNCTOR("atomic_basic:number_codes", 3);
  return prolog_constant_codes(Arg,FALSE,TRUE,2);
}

 /* INTEGER :== [minus]{digit} */
 /* FLOAT :== [minus]{digit}.{digit}[exp[sign]{digit}] */
 /* ATOM :== {char} */

/* string_to_number() tries to convert the string pointed to by AtBuf in a
   number contained in the tagged word *strnum.  If this cannot be done
   (e.g., AtBuf does not correspond syntactically to a number), FALSE is
   returned.  Otherwise, TRUE is returned and the conversion is done */

CBOOL__PROTO(string_to_number, 
	     char *AtBuf,
	     int base,
	     tagged_t *strnum,
	     int arity) {
  bool_t sign = FALSE;
  char *s = AtBuf;
  int i, d;
  flt64_t m = 0.0;
  double num;
  int exp=0,e=0,se=1;
  i = *s++;
  if (i=='-') {
    sign = TRUE;
    i = *s++;
  }
  /* First, consider special cases */
  if ((i=='0') &&
     (s[0]==FLOAT_POINT) &&
     (s[1]=='N') &&
     (s[2]=='a') &&
     (s[3]=='n') &&
     (s[4]=='\0'))
  {
    *strnum=MakeFloat(Arg,0.0/0.0); /* Nan */
    return TRUE;
  }
  if ((i=='0') &&
     (s[0]==FLOAT_POINT) &&
     (s[1]=='I') &&
     (s[2]=='n') &&
     (s[3]=='f') &&
     (s[4]=='\0'))
  {
    if (sign) 
      num = -1.0/0.0;
    else
      num = 1.0/0.0;
    *strnum=MakeFloat(Arg,num); /* Inf */
    return TRUE;
  }
  d = char_digit[i];
  while (0 <= d && d < base) {
    i = *s++;
    d = char_digit[i];
  }
  if ((s - AtBuf) - sign > 1) {
    if (i==0) {
      /* It is an integer, either a small or a bignum */
      tagged_t *h = w->global_top;
      tagged_t t;
      int req = bn_from_string(AtBuf, (bignum_t *)h, (bignum_t *)(Heap_End-CONTPAD), base);

      if (req) {
        explicit_heap_overflow(Arg ,req+CONTPAD, arity);
        h = w->global_top;
        if (bn_from_string(Atom_Buffer, (bignum_t *)h, (bignum_t *)(Heap_End-CONTPAD), base))
          SERIOUS_FAULT("miscalculated size of bignum");
      }

      req = LargeArity(h[0]);
      if (req==2 && IntIsSmall((intmach_t)h[1])) {
        t = MakeSmall(h[1]);
      } else {
        w->global_top += req+1;
        h[req] = h[0];
        t = Tag(STR,h);
      }
      *strnum = t;                 /* The tagged_t word --- small or bignum */
      return TRUE;
    }

/* It is a float. Note that if is a float, after the point the digits
   must not be upper case to avoid confussion with the exponent
   indicator. This is true only if base > 10 + 'e'-'a'*/
    if (i==FLOAT_POINT) {
      s = AtBuf + (sign ? 1 : 0);
      i = *s++;
      do {
	m = m * base+char_digit[i];
	i = *s++;
      } while(i!=FLOAT_POINT);
      i = *s++;
      d = char_digit[i];
      if ((0 <= d) && (d < base)) {
	char exponent_lower = exponents_lower[base];
	char exponent_upper = exponents_upper[base];
        do {
	  m = m * base + char_digit[i];
	  exp++;
          i = *s++;
	  d = char_digit[i];
        } while ((0 <= d) && (d < base) && 
                 (i != exponent_lower) && (i != exponent_upper));
        if ((i==exponent_lower) || (i==exponent_upper)) {
          i = *s++;
          if ((i=='+') || (i=='-')) {
	    if (i=='-') se=-1;
	    i = *s++;
	  }
	  d = char_digit[i];
          if ((0<=d) && (d<base)) {
            do {
	      e = e * base + d;
              i = *s++;
	      d = char_digit[i];
	    } while ((0<=d) && (d<base));
          } else {
	    i = -1;
	  }
        }
      } else {
	i = -1;
      }
      if (i!=-1) {
	if (se==1) {
	  exp = -exp + e;
	} else {
	  exp = -exp - e;
	}
	num = m * powl_int(base, exp);
	*strnum = MakeFloat(Arg, sign ? -num : num);
	return TRUE;
      }
    }
  }
  /* Could not make the conversion --- maybe a non-numeric atom */
  return FALSE;
}

/*

  Note: The ci parameter indicates where are the String argument.  If
  ci = 2, then there are 3 parameters, indicating that the second
  parameter could be the numeric base.

 */
static CBOOL__PROTO(prolog_constant_codes, 
		    bool_t atomp,
		    bool_t numberp,
		    int ci)
{
  /* Note: This ERR__FUNCTOR is not related to an exported predicate,
     since prolog_constant_codes is called from other places. I have
     some ideas of refining error reporting without hindering
     performance (please, ask me before changing this code). -- JFMC
   */
  ERR__FUNCTOR("atomic_basic:$constant_codes", 3);
  char *s;
  int i, base;
  tagged_t car, cdr;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (ci==2) DEREF(X(2),X(2));

  /* Construct a character string from the input list */
  cdr = X(ci);
  s = Atom_Buffer;
  for (i=0; cdr!=atom_nil; i++) {
    if (IsVar(cdr)) {
      goto construct_list;
    } else if (!TagIsLST(cdr)) {
      BUILTIN_ERROR(TYPE_ERROR(LIST),X(ci),ci+1);
    } else if (i == Atom_Buffer_Length){
      EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
      s = Atom_Buffer+i;
    }
    DerefCar(car,cdr);
    if (IsVar(car)) {
      goto construct_list;
    }
    if (!TagIsSmall(car) || (car<=TaggedZero) || (car>=MakeSmall(256))) {
      BUILTIN_ERROR(REPRESENTATION_ERROR(CHARACTER_CODE_LIST),X(ci),ci+1);
    }
    *s++ = GetSmall(car);
    DerefCdr(cdr,cdr);
  }
  if (i == Atom_Buffer_Length) {
    EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
    s = Atom_Buffer+i;
  }
  *s++ = '\0';

  /* s contains now the string of character codes, and i its size */

#if !defined(USE_DYNAMIC_ATOM_SIZE)
  if (i>=MAXATOM)
    BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ATOM_LENGTH), X(0), 1);
#endif

  if (numberp) {
    tagged_t result;
    if (ci==2) {
      if (IsInteger(X(1))) {
	base = GetSmall(X(1));
      } else {
	BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
      }
    } else { // if (ci==1)
      base = GetSmall(current_radix);
    }
    if ((base < 2)||(base > 36)) {
      BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK),X(1),2);
    }
    if (string_to_number(Arg, Atom_Buffer, base, &result, ci+1)) {
      return cunify(Arg, result, X(0));
    }
  }
  return atomp && cunify(Arg,init_atom_check(Atom_Buffer),X(0));

 construct_list:
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2);
  }

  if (numberp && IsNumber(X(0))) {
    if (ci==2) {
      if (IsInteger(X(1))) {
	base = GetSmall(X(1));
      } else {
	BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
      }
    } else { // if (ci==1)
      base = GetSmall(current_radix);
    }
    if ((base < 2)||(base > 36)) {
      BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK),X(1),2);
    }
    number_to_string(Arg,X(0),base);
    s = Atom_Buffer;
  } else if (atomp && TagIsATM(X(0))) {
    s = GetString(X(0));
  } else {
    if (numberp) {
      if (atomp) {
	BUILTIN_ERROR(TYPE_ERROR(ATOMIC),X(0),1);
      } else {
	BUILTIN_ERROR(TYPE_ERROR(NUMBER),X(0),1);
      }
    } else {
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
    }
  }

  s += (i = strlen(s));

  if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i<<1)) {
    explicit_heap_overflow(Arg,CONTPAD+(i<<1),ci+1);
  }

  cdr = atom_nil;
  while (i>0)	{
    i--;
    MakeLST(cdr,MakeSmall(*((unsigned char *)--s)),cdr);
  }
  return cunify(Arg,cdr,X(ci));
}

CBOOL__PROTO(prolog_atom_length) {
  ERR__FUNCTOR("atomic_basic:atom_length", 2);
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,STRICT_ATOM);

  if (!IsInteger(X(1)) && !IsVar(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
  }

#if defined(USE_ATOM_LEN)
  return cunify(Arg,MakeSmall(GetAtomLen(X(0))),X(1));
#else
  return cunify(Arg,MakeSmall(strlen(GetString(X(0)))),X(1));
#endif
}

/* sub_atom(Atom, Before, Lenght, Sub_atom) */
CBOOL__PROTO(prolog_sub_atom)
{
  ERR__FUNCTOR("atomic_basic:sub_atom", 4);
  char *s, *s1;
  int l, b, atom_length;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  if (!IsInteger(X(1)))
    ERROR_IN_ARG(X(1),2,INTEGER);
  if (!IsInteger(X(2)))
    ERROR_IN_ARG(X(2),3,INTEGER);

  s = GetString(X(0));
#if defined(USE_ATOM_LEN)
  l = GetAtomLen(X(0));
#else
  l = strlen(s);
#endif

  b = GetInteger(X(1));
  if (b < 0 || b > l)
    return FALSE;

  atom_length = GetInteger(X(2));
  if (atom_length < 0 || atom_length+b > l)
    return FALSE;

  s += b;

  if (Atom_Buffer_Length <= atom_length)
    EXPAND_ATOM_BUFFER(atom_length+1);

  s1 = Atom_Buffer;

  strncpy(s1, s, atom_length);

  *(s1+atom_length) = '\0';

  return cunify(Arg,init_atom_check(Atom_Buffer),X(3));

}

CBOOL__PROTO(prolog_atom_concat)
{
  ERR__FUNCTOR("atomic_basic:atom_concat", 3);
  int new_atom_length;
  char *s, *s1, *s2;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  if (TagIsATM(X(0))) {
    s1 = GetString(X(0));

    if (TagIsATM(X(1))) {
      if (!TagIsATM(X(2)) && !IsVar(X(2))) {
        BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(2),3);
      }
/* atom_concat(+, +, ?) */
      s2 = GetString(X(1));

#if defined(USE_ATOM_LEN)
      new_atom_length = GetAtomLen(X(0)) + GetAtomLen(X(1)) + 1;
#else
      new_atom_length = strlen(s1) + strlen(s2) + 1;
#endif

#if defined(USE_DYNAMIC_ATOM_SIZE)
      if (new_atom_length >= Atom_Buffer_Length) {
	EXPAND_ATOM_BUFFER(new_atom_length);
      }
#else
      if (new_atom_length > MAXATOM) {
	BUILTIN_ERROR(REPRESENTATION_ERROR(MAX_ATOM_LENGTH), X(2), 3);
      }
#endif

      /* Append the two strings in atom_buffer */
      s = Atom_Buffer;
      while (*s1)
        *s++ = *s1++;
      while (*s2)
        *s++ = *s2++;
      *s = '\0';
      return cunify(Arg,init_atom_check(Atom_Buffer),X(2));
    } else if (IsVar(X(1))) {
      if (!TagIsATM(X(2))) { ERROR_IN_ARG(X(2),3,STRICT_ATOM); }
      /* atom_concat(+, -, +) */
      s2 = GetString(X(2));

#if defined(USE_ATOM_LEN)
      new_atom_length = GetAtomLen(X(2))+1;
#else
      new_atom_length = strlen(s2) + 1;
#endif
      if (new_atom_length >= Atom_Buffer_Length)
          EXPAND_ATOM_BUFFER(new_atom_length);

      for ( ; *s1 && *s2 ; s1++, s2++)
        if (*s1 != *s2) return FALSE;

      if (*s1) return FALSE;

      s = Atom_Buffer;

      strcpy(s, s2);

      return cunify(Arg,init_atom_check(Atom_Buffer),X(1));
    } else {
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(1),2);
    }
  } else if (IsVar(X(0))) {
    if (!TagIsATM(X(2)))
        { ERROR_IN_ARG(X(2),3,STRICT_ATOM); }

    if (TagIsATM(X(1))) {
/* atom_concat(-, +, +) */

      s1 = GetString(X(1));
      s2 = GetString(X(2));

#if defined(USE_ATOM_LEN)
      if ((new_atom_length = (GetAtomLen(X(2)) - GetAtomLen(X(1)))) < 0)
        return FALSE;
#else
      if ((new_atom_length = strlen(s2)-strlen(s1)) < 0)
        return FALSE;
#endif

      if (new_atom_length >= Atom_Buffer_Length) 
	EXPAND_ATOM_BUFFER(new_atom_length+1);

      s = s2+new_atom_length;

      if (strcmp(s1, s)) /* different */
        return FALSE;

      s = Atom_Buffer;

      strncpy(s, s2, new_atom_length);

      *(s+new_atom_length) = '\0';

      return cunify(Arg,init_atom_check(Atom_Buffer),X(0));
    } else if (IsVar(X(1))) {
/* atom_concat(-, -, +) */

      s2 = GetString(X(2));
#if defined(USE_ATOM_LEN)
      new_atom_length = GetAtomLen(X(2))+1;
#else
      new_atom_length = strlen(s2)+1;
#endif
      if (new_atom_length >= Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length);
      X(3) = TaggedZero;
      push_choicept(Arg,address_nd_atom_concat);
      return nd_atom_concat(Arg);
    } else {
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(1),2);
    }
  } else {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
  }
}

/* Precond: 2<=abs(base)<=36 */
CVOID__PROTO(number_to_string, tagged_t term, int base) {
  if (TagIsSmall(term)) {
    intmach_t l = GetSmall(term);
    char hibase = 'a'-10;
    bool_t sx = (l>=0);
    intmach_t digit;
    char *c0, *c, d;

    if (base<0) {
      hibase = 'A'-10;
      base = -base;
    }
    c = Atom_Buffer;
    if (!sx) {
      *c++ = '-';
      l = -l;
    }

    do {
      digit = l % base;
      l /= base;
      *c++ = (digit<10 ? '0'+digit : hibase+digit);
    } while (l>0);

    *c++ = 0;
    for (c0=Atom_Buffer+1-sx, c-=2; c0<c; c0++, c--) {
      d = *c0;
      *c0 = *c;
      *c = d;
    }
  } else if (IsFloat(term)) {
    union {
      flt64_t i;
      tagged_t p[sizeof(flt64_t)/sizeof(tagged_t)];
    } u;
    char *cbuf;
    int eng_flt_signif = (int)((IEEE754_MANTISSA_LENGTH + 1) * invlog2[base] + 1);

    /* f = GetFloat(term); */
#if LOG2_bignum_size == 5
    u.p[0] = *TagToArg(term,1);
    u.p[1] = *TagToArg(term,2);
#elif LOG2_bignum_size == 6
    u.p[0] = *TagToArg(term,1);
#endif

    /* Print using the default precision.  'p' is a new 'prolog' :-)
       format not considered by, e.g., the printf family, to
       implement the expected Prolog behavior */

    /*       if (1024 > Atom_Buffer_Length) */
    /* 	EXPAND_ATOM_BUFFER(102400); */
    cbuf = Atom_Buffer;
    cbuf = float_to_string(cbuf, eng_flt_signif, 'p', u.i, base);
  } else {
    bn_to_string(Arg,(bignum_t *)TagToSTR(term),base);
  }
}


/* copy_term(?Old,?New):
 * Algorithm:
 * If Old is a plain variable, just return.
 * Otherwise allocate a frame containing Old and New.
 * The frame slot for Old will be progressively replaced by a copy of Old.
 * Thus all relevant parts of the old and new structures are reachable from
 * the frame slot, should GC occur.
 * While copying, all old variables encountered are bound to their copies.
 * This requires a choicepoint.
 * Finally untrail but trail any new CVA:s, deallocate frame & choicept,
 * and unify copy with New.
 */
CBOOL__PROTO(prolog_copy_term) {
  tagged_t t1, t2, *pt1, *pt2;

  t1 = X(0);
  SwitchOnVar(t1,t2,
	      { return TRUE; },
	      {},
	      { return TRUE; },
	      {});

  X(0) = t1;
  push_choicept(Arg,fail_alt);	/* try, arity=0 */
  push_frame(Arg,2);		/* allocate, size=2 */

  copy_it(Arg,&w->frame->term[0]); /* do the copying */

  pt1 = pt2 = TagToPointer(w->node->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = TrailNext(pt2);	/* old var */
    *TagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_frame(Arg);
  pop_choicept(Arg);		/* trust */
  return cunify(Arg,X(0),X(1));
}

static CVOID__PROTO(copy_it, tagged_t *loc) {
  tagged_t t1, t2, *pt1, *pt2;
  int i;
  int term_so_far;		/* size of new heap before copying subterms */

 start:
  RefHeap(t1,loc);
  SwitchOnHeapVar(t1,t2,{goto copy_hva;},{goto copy_cva;},{});

  if (IsAtom(t1) || IsNumber(t1)) {                           /* NUM, ATM */
    *loc = t1;
    return;
  } else if (t1 & TagBitFunctor) {                                 /* STR */
    pt1 = TagToSTR(t1);
    pt2 = w->global_top;
    *loc = Tag(STR,pt2);
    t2 = HeapNext(pt1), HeapPush(pt2,t2);
    for (i=Arity(t2); i>0; --i) {
      RefHeapNext(t1,pt1);
      HeapPush(pt2,t1);
    }
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    for (i=Arity(t2); i>1; --i)
      copy_it(Arg,HeapOffset(TopOfOldHeap,term_so_far-i));
  } else {				                           /* LST */
    pt1 = TagToLST(t1);
    pt2 = w->global_top;
    *loc = Tag(LST,pt2);
  copy_2_cells:
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    copy_it(Arg,HeapOffset(w->global_top,-2));
  }
  GCTEST(CHOICEPAD);
  loc = HeapOffset(TopOfOldHeap,term_so_far-1);
  goto start;

 copy_hva:
  if (CondHVA(t1)){		                                   /* HVA */
    PreLoadHVA(*loc,loc);
    t2 = TagHVA(loc);
    BindHVA(t1,t2);
  } else *loc = t1;
  return;

 copy_cva:
  if (CondCVA(t1)){		                       /* new 3-field CVA */
    pt1 = TagToGoal(t1);
    pt2 = w->global_top;
    LoadCVA(t2,pt2);
    BindCVA_NoWake(t1,t2);
    *loc = t2;
    goto copy_2_cells;
  } else *loc = t1;
  return;
}

/* Do not copy attributes */
CBOOL__PROTO(prolog_copy_term_nat)
{
  tagged_t t1, t2, *pt1, *pt2;

  t1 = X(0);
  SwitchOnVar(t1,t2,
	      { return TRUE; },
	      { return TRUE; },
	      { return TRUE; },
	      {});

  X(0) = t1;
  push_choicept(Arg,fail_alt);	/* try, arity=0 */
  push_frame(Arg,2);		/* allocate, size=2 */

  copy_it_nat(Arg,&w->frame->term[0]); /* do the copying */

  pt1 = pt2 = TagToPointer(w->node->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = TrailNext(pt2);	/* old var */
    *TagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_frame(Arg);
  pop_choicept(Arg);		/* trust */
  return cunify(Arg,X(0),X(1));
}


static CVOID__PROTO(copy_it_nat, tagged_t *loc)
{
  tagged_t t1, t2, *pt1, *pt2;
  int i;
  int term_so_far;		/* size of new heap before copying subterms */

 start:
  RefHeap(t1,loc);
  SwitchOnHeapVar(t1,t2,{goto copy_hva;},{goto skip_cva;},{});

  if (IsAtom(t1) || IsNumber(t1)) {                           /* NUM, ATM */
    *loc = t1;
    return;
  } else if (t1 & TagBitFunctor) {                                 /* STR */
    pt1 = TagToSTR(t1);
    pt2 = w->global_top;
    *loc = Tag(STR,pt2);
    t2 = HeapNext(pt1), HeapPush(pt2,t2);
    for (i=Arity(t2); i>0; --i) {
      RefHeapNext(t1,pt1);
      HeapPush(pt2,t1);
    }
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    for (i=Arity(t2); i>1; --i)
      copy_it_nat(Arg,HeapOffset(TopOfOldHeap,term_so_far-i));
  } else {				                           /* LST */
    pt1 = TagToLST(t1);
    pt2 = w->global_top;
    *loc = Tag(LST,pt2);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    copy_it_nat(Arg,HeapOffset(w->global_top,-2));
  }
  GCTEST(CHOICEPAD);
  loc = HeapOffset(TopOfOldHeap,term_so_far-1);
  goto start;

 copy_hva:
  if (CondHVA(t1)){		                                   /* HVA */
    PreLoadHVA(*loc,loc);
    t2 = TagHVA(loc);
    BindHVA(t1,t2);
  } else *loc = t1;
  return;

 skip_cva:
  if (CondCVA(t1)){  /* This code is equivalent to taking out the attribute;
                        xref bu1_detach_attribute() */
    PreLoadHVA(*loc,loc);
    t2 = TagHVA(loc);
    BindCVA_NoWake(t1,t2);
  } else  *loc = t1;
  return;
}



/* Copy a term in a remote worker to the local worker.  Returns the local
   term pointer.  It has (nontermination) problems when copying structures
   with self references. */

CFUN__PROTO(cross_copy_term, tagged_t, tagged_t remote_term)
{
  X(0) = remote_term;
  LoadHVA(X(1), w->global_top);
#if defined(DEBUG)
  if (!prolog_copy_term(Arg))
    fprintf(stderr, "Could not copy term in cross_copy_term!!!!\n");
#else
  prolog_copy_term(Arg);
#endif
  return X(1);
}

/* c_cyclic_term: checks that the term t is cyclic.

   Calls c_cyclic_ptr with the reference of a tagged word, using GC
   bits as marks for already visited nodes. If the tagged word is a
   compound term:

     - If GC bit is on, exit. The term is cyclic.
     - Otherwise, set the GC bit, call recursively, and unset the GC
       bit.

   There are two implementations
     - One fully recursive (macro CYCLIC_TERM_TAIL_OPTIM undefined)
     - One with tail optimization  (macro CYCLIC_TERM_TAIL_OPTIM 
       defined), that avoid  using C stack for the last argument.

   In case of the tail optimized version the GC bits of the rightmost
   branch are removed together.

*/

static CBOOL__PROTO(c_cyclic_ptr, tagged_t *pt);
#if defined(CYCLIC_TERM_TAIL_OPTIM)
static CVOID__PROTO(unmark_rightmost_branch, tagged_t *ptr);
#endif

CBOOL__PROTO(c_cyclic_term, tagged_t t) {
  tagged_t *ptr;
  int i;

  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    return c_cyclic_ptr(Arg, TagToPointer(t));
  case LST: 
    ptr = TagToLST(t);
    i = 2;
    goto args;
  case STR:
    ptr = TagToSTR(t);
    t = *ptr;
    if (t&QMask) return FALSE; /* large number */
    i = Arity(t);
    ptr++;
    goto args;
  default:
    return FALSE;
  }
 args:
  for (; i >= 1; i--, ptr++) {
    if (c_cyclic_ptr(Arg, ptr)) return TRUE;
  }
  return FALSE;
}

CBOOL__PROTO(c_cyclic_ptr, tagged_t *pt) {
  tagged_t t;
  tagged_t *ptr, *ptr1;
  int i;

  ptr = pt;
 start:
  t = *ptr;
  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    ptr = TagToPointer(t);
    if (*ptr == t) goto acyclic; /* free variable */
    goto start;
  case LST: 
    if (gc_IsMarked(t)) goto cyclic;
    ptr1 = TagToLST(t);
    i = 2;
    goto args;
  case STR:
    if (gc_IsMarked(t)) goto cyclic;
    ptr1 = TagToSTR(t);
    t = *ptr1;
    if (t&QMask) goto acyclic;  /* large number */
    i = Arity(t);
    ptr1++;
    goto args;
  default:
    goto acyclic;
  }
 args:
  gc_MarkM(*ptr); /* mark cell */
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  for (; i > 1; i--, ptr1++) {
    if (c_cyclic_ptr(Arg, ptr1)) goto cyclic;
  }
  ptr = ptr1;
  goto start;
#else
  for (; i >= 1; i--, ptr1++) {
    if (c_cyclic_ptr(Arg, ptr1)) { gc_UnmarkM(*ptr); goto cyclic; }
  }
  /* acyclic */
  gc_UnmarkM(*ptr);
  goto acyclic;
#endif

 acyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  unmark_rightmost_branch(Arg, pt); /* ensure marks are removed */
#endif
  return FALSE;
 cyclic:
#if defined(CYCLIC_TERM_TAIL_OPTIM)
  unmark_rightmost_branch(Arg, pt); /* ensure marks are removed */
#endif
  return TRUE;
}

#if defined(CYCLIC_TERM_TAIL_OPTIM)
CVOID__PROTO(unmark_rightmost_branch, tagged_t *ptr) {
  tagged_t t;

 start:
  t = *ptr;
  switch(TagOf(t)){
  case SVA:
  case HVA:
  case CVA:
    ptr = TagToPointer(t);
    if (*ptr == t) return;
    goto start;
  case LST: 
    if (!gc_IsMarked(t)) return;
    gc_UnmarkM(*ptr);
    ptr = TagToLST(*ptr);
    ptr++;
    goto start;
  case STR:
    if (!gc_IsMarked(t)) return;
    gc_UnmarkM(*ptr);
    ptr = TagToSTR(*ptr);
    ptr += Arity(*ptr);
    goto start;
  default:
    return;
  }
}
#endif

CBOOL__PROTO(prolog_cyclic_term)
{
  return c_cyclic_term(Arg, X(0));
}

/* unifiable(Term1, Term2, Unifier)
 *
 * Algortihm: tries unify Term1 and Term2, then untrail the
 * unification while saving the unifier into a prolog list which is
 * eventually unified with Unifier. The function treats attributed 
 * variable as classical ones.
 */
CBOOL__PROTO(prolog_unifiable)
{
  tagged_t t, t1;
  tagged_t * limit, *tr;

  /* Forces trailing of bindings and saves the top of the trail. */
  push_choicept(Arg,fail_alt);	
  /* Saves the arguments in case of GC. */
  push_frame(Arg,3);

  if (!cunify(Arg, X(0), X(1))) return FALSE;  

  /* Makes sure there is enough place in the heap to construct the
     unfiers list. */
  GCTEST((w->trail_top - TagToPointer(w->node->trail_top)) * 5);

  t = atom_nil;
  tr = w->trail_top;
  limit = TagToPointer(w->node->trail_top);
   
  while (TrailYounger(tr, limit)) {
    t1 = TrailPop(tr);

    HeapPush(w->global_top, SetArity(atom_equal, 2));
    HeapPush(w->global_top, t1);
    HeapPush(w->global_top, *TagToPointer(t1));
    HeapPush(w->global_top, Tag(STR, HeapOffset(w->global_top, -3)));
    HeapPush(w->global_top, t);
    t = Tag(LST, HeapOffset(w->global_top, -2));

    *TagToPointer(t1) = t1;
  }

  /* Ignores possible wakes caused by unification of attributed
     variables */ 
  if (TestEvent) Heap_Warn_Soft = Heap_Warn;
  
  w->trail_top = limit;
  pop_frame(Arg);
  pop_choicept(Arg);	

  return cunify(Arg, X(2), t);
}  

/* ------------------------------------------------------------------------- */
/* Deref variable v occurs in term x */
/* (needed for unifyOC) */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1);

static CBOOL__PROTO(var_occurs_args_aux, 
		    tagged_t v,
		    int arity,
		    tagged_t *pt1,
		    tagged_t *x1) {
  tagged_t 
    t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && var_occurs(Arg,v,t1)) return TRUE;
    (void)HeapNext(pt1);
  }
  *x1 = t1;
  return FALSE;
}

static CBOOL__PROTO(var_occurs, tagged_t v, tagged_t x1) {
  tagged_t u, t1;

 in:
  u=x1;

  SwitchOnVar(u,t1,
	      { goto var; },
	      { goto var; },
	      { goto var; },
	      { goto non_var; });

 non_var:
  if (TagIsATM(u)) goto lose;
  if (TagIsSmall(u)) goto lose;
  if (TagIsLST(u)) {
    if (!var_occurs_args_aux(Arg,v,2,TagToCar(u),&x1))
      goto in;
    else
      goto win;
  } else { /* structure. */
    t1=TagToHeadfunctor(u);
    if (t1&QMask) { /* large number */
	  goto lose;
    } if (!var_occurs_args_aux(Arg,v,Arity(t1),TagToArg(u,1),&x1)) {
      goto in;
    } else {
      goto win;
    }
  }

 var:
  if (v == u) goto win; else goto lose;

 win:
  return TRUE;
 lose:
  return FALSE;
}
#endif

/* ------------------------------------------------------------------------- */
/* Unify with occurs-check */

#if defined(UNIFY_OC_INLINE)
static CBOOL__PROTO(cunifyOC_args_aux,
		    int arity, tagged_t *pt1, tagged_t *pt2,
		    tagged_t *x1, tagged_t *x2);
static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2);

/* Unify the argument lists of two compund terms. (with occurs-check)
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
CBOOL__PROTO(cunifyOC_args, 
	     int arity,
	     tagged_t *pt1,
	     tagged_t *pt2) {
  tagged_t x1, x2;
  return (cunifyOC_args_aux(Arg,arity,pt1,pt2,&x1,&x2) && cunifyOC_aux(Arg,x1,x2));
}

static CBOOL__PROTO(cunifyOC_args_aux, 
		    int arity,
		    tagged_t *pt1,
		    tagged_t *pt2,
		    tagged_t *x1,
		    tagged_t *x2) {
  tagged_t t1 = ~0;
  tagged_t t2 = ~0;
  tagged_t t3;

  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD-w->value_trail),w->trail_top)) {
    /* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  }
  for (; arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (t1 != t2) {
      DerefHeapSwitch(t1,t3,{ goto noforward; });
      DerefHeapSwitch(t2,t3,{ goto noforward; });
      if (t1!=t2 && IsComplex(t1&t2)) {
	/* NOTE: do forward args from pt2 to pt1 */ 
      noforward:
        if (arity>1 && !cunifyOC_aux(Arg,t1,t2))
          return FALSE;
      } else if (t1 != t2) {
        return FALSE;
      }
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1;
  *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD-w->value_trail),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}
#endif 

/* Unify two terms. (with occurs-checks)
 * x1 - first term
 * x2 - second term
 */

CBOOL__PROTO(cunifyOC, tagged_t x1, tagged_t x2) {
#if defined(UNIFY_OC_INLINE)
  /* Use a recursive version of Robinson's 1965 unification algorithm
     with inline occurs-check */
  return cunifyOC_aux(Arg,x1,x2);
#else
  /* Otherwise, check cyclic later. This may be less efficient than
     the first algorithm depending on cost of cyclic term checks,
     e.g., f(X,X,X)=f(...,...,...) redoes work for each arg */
  return cunify(Arg,x1,x2) && !c_cyclic_term(Arg, x1);
#endif
}

#if defined(UNIFY_OC_INLINE)
#define OccurCheck(U,V,OCCUR) \
  { if (CBOOL__SUCCEED(var_occurs, (U), (V))) { OCCUR; } }

static CBOOL__PROTO(cunifyOC_aux, tagged_t x1, tagged_t x2) {
  tagged_t u, v, t1;

 in:
  u=x1, v=x2;

  SwitchOnVar(u,t1,
	      {goto u_is_hva;},
	      {goto u_is_cva;},
	      {goto u_is_sva;},
	      {});

				/* one non variable */
  SwitchOnVar(v,t1,
	      { OccurCheck(v,u,{goto lose;}); BindHVA(v,u); goto win; },
	      { OccurCheck(v,u,{goto lose;}); BindCVA(v,u); goto win; },
	      { OccurCheck(v,u,{goto lose;}); BindSVA(v,u); goto win; },
	      {});

				/* two non variables */
  if (!(v ^= u)) {		/* are they equal? */
    goto win;
  } else if (v>=QMask) {		/* not the same type? */
    goto lose;
  } else if (!(u & TagBitComplex)) { /* atomic? (& not LNUM)*/
    goto lose;
  } else if (!(u & TagBitFunctor)) { /* list? */
    v ^= u;			/* restore v */
    if (cunifyOC_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2)) {
      goto in;
    } else {
      goto lose;
    }
  } else {				/* structure. */
    v ^= u;			/* restore v */
    if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v))) {
      goto lose;
    } else if (t1&QMask) {	/* large number */
      int i;
	
      for (i = LargeArity(t1)-1; i>0; i--)
	if (*TagToArg(u,i) != *TagToArg(v,i)) goto lose;
      goto win;
    }
    if (cunifyOC_args_aux(Arg,Arity(t1),TagToArg(u,1),TagToArg(v,1),&x1,&x2)) {
      goto in;
    } else {
      goto lose;
    }
  }

 u_is_hva:
  SwitchOnVar(v,t1, {
      if (u==v) {
      } else if (YoungerHeapVar(TagToHVA(v),TagToHVA(u))) {
	BindHVA(v,u);
      } else {
	BindHVA(u,v); 
      } 
    }, {
      BindHVA(u,v);
    }, {
      BindSVA(v,u);
    }, {
      OccurCheck(u,v,{goto lose;}); BindHVA(u,v);
    });
  goto win;

 u_is_cva:
  SwitchOnVar(v,t1, {
      BindHVA(v,u);
    }, { if (u==v) {
      } else if (YoungerHeapVar(TagToCVA(v),TagToCVA(u))) {
	BindCVA(v,u);
      } else {
	BindCVA(u,v); 
      } 
    }, {
      BindSVA(v,u);
    }, {
      OccurCheck(u,v,{goto lose;}); BindCVA(u,v);
    });
  goto win;

 u_is_sva:
  for (; TagIsSVA(v); v = t1) {
    RefSVA(t1,v);
    if (v == t1) {
      if (u==v) {
      } else if (YoungerStackVar(TagToSVA(v),TagToSVA(u))) {
	BindSVA(v,u);
      } else {
	BindSVA(u,v);
      }
      goto win;
    }
  }
  OccurCheck(u,v,{ goto lose; }); BindSVA(u,v);

 win:
  return TRUE;

 lose:
  return FALSE;
}
#endif

CBOOL__PROTO(prolog_unifyOC) {
  return cunifyOC(Arg, X(0), X(1));
}

/* --------------------------------------------------------------------------- */

/* Return length of list, or -1 if not a list */
CFUN__PROTO(c_list_length, int, tagged_t list) {
  int len;
  DEREF(list, list);
  for (len=0; list!=atom_nil; len++) {
    if (IsVar(list)) break;
    if (!TagIsLST(list)) break;
    DerefCdr(list,list);
  }
  return (list==atom_nil) ? len : (-1);
}

