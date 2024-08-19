/*
 *  attributes.c
 *
 *  See Copyright Notice in ciaoengine.pl
 *
 *  Portions of this code based on code by Christian Holzbaur
 *  [christian@ai.univie.ac.at], Copyright (C) 1992 DMCAI
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_start.h>
#include <ciao/attributes.h>
#include <ciao/eng_gc.h>
#endif

extern definition_t *address_true;

CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x) {
  DerefSw_CVA_Other(x, {}, { CFUN__PROCEED(ERRORTAG); } ); /* fail */
  CFUN__PROCEED(*TaggedToGoal(x));
}

/* Suspend a goal on a variable. Stolen from suspend_goal in misc.c */
static inline CBOOL__PROTO(bu2_attach_attribute_, tagged_t var, tagged_t constr, tagged_t susp) {
  DerefSw_HVAorCVAorSVA_Other(constr, { USAGE_FAULT("attach_attribute/2: type error"); }, {});
  tagged_t *h = G->heap_top;
  DEREF(var,var);
  if (TaggedIsHVA(var)) {
    tagged_t t0;
    LoadCVA(t0,h);
    if (CondHVA(var)) TrailPush(G->trail_top,var);
    *TagpPtr(HVA,var) = t0;
  } else if (TaggedIsSVA(var)) { /* unsafe value */
    tagged_t t0;
    tagged_t *ptr = h;
    LoadHVA(t0,ptr);
    h = ptr;
    BindSVA(var,t0);
    var = t0;
    LoadCVA(t0,h);
    *TagpPtr(HVA,var) = t0;
  } else {
    USAGE_FAULT("attach_attribute/2: type error");
  }

  HeapPush(h,constr);
  HeapPush(h,susp); /* func */
  G->heap_top = h;

  TEST_CHOICE_OVERFLOW(w->choice, CHOICEPAD);
  CBOOL__PROCEED;
}

/* Suspend a goal on a variable (similar to suspend_goal) */
CBOOL__PROTO(bu2_attach_attribute, tagged_t var, tagged_t constr) {
  CBOOL__LASTCALL(bu2_attach_attribute_, var, constr, PointerToTerm(address_true));
}  

/* (special version for mutables) */
CBOOL__PROTO(bu2_attach_attribute_weak, tagged_t var, tagged_t constr) {  
  CBOOL__LASTCALL(bu2_attach_attribute_, var, constr, MakeSmall(0));
}  

/* (similar to defrost) */
CBOOL__PROTO(bu1_detach_attribute, tagged_t x) {
  DerefSw_CVA_Other(x,{},{ USAGE_FAULT("detach_attribute/2: type error"); });
  tagged_t t; 
  tagged_t *h = G->heap_top;
  LoadHVA(t,h);
  BindCVANoWake(x,t); /* trailed */
  G->heap_top = h;
  CBOOL__PROCEED;
}  
 
/* TODO: think about optimizations a-la setarg */

CBOOL__PROTO(bu2_update_attribute, tagged_t x, tagged_t constr) {
  DerefSw_HVAorCVAorSVA_Other(constr,{ USAGE_FAULT("update_attribute/2: type error"); }, {});
  DerefSw_CVA_Other(x,{},{ USAGE_FAULT("update_attribute/2: type error"); });
  tagged_t t;
  tagged_t *h = G->heap_top;
  LoadCVA(t,h); 
  HeapPush(h,constr);
  HeapPush(h,PointerToTerm(address_true)); /* func */
  BindCVANoWake(x,t); /* trailed */
  G->heap_top = h;
  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/*  
   (Called from bc_aux.h)
   Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment.  
   Each pending unification pushes 4 heap elems - cf enter_predicate
*/

/* TODO: share code with collect_one_pending_unification */

CVOID__PROTO(collect_pending_unifications, intmach_t wake_count) {
  intmach_t sofar=0;
  tagged_t *tr = G->trail_top;
  tagged_t *h;
  tagged_t *tr0 = NULL;
#if defined(OPTIM_COMP)
  tagged_t *limit = w->choice->trail_top;
#else
  tagged_t *limit = TrailTopUnmark(w->choice->trail_top);  
#endif
   
  h = G->heap_top;
  X(0) = atom_nil;
  while (sofar<wake_count && TrailYounger(tr,limit))  {
    tagged_t ref, value;
    
    TrailDec(tr);
    ref = *tr; // (tr points to the popped element)
    if (!TaggedIsCVA(ref)) continue;
    value = *TagpPtr(CVA,ref);
    if (value==ref) { 
      PANIC_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    *TaggedToPointer(ref) = ref; /* untrail */
    
    HeapPush(h, ref); 
    HeapPush(h, value);  
    HeapPush(h, Tagp(LST,h-2));
    HeapPush(h, X(0));
    X(0) = Tagp(LST,h-2);
    
    if (!CondCVA(ref)) {
      tr0=tr;
      *tr=0;
    }
  }
  G->heap_top = h;
  SetWakeCount(0);
  
  if (sofar<wake_count) {
    PANIC_FAULT("wake - unable to find all goals");
  }

  /* now compress the trail */
  if (tr0) {
    CompressTrailNoGC(tr0);
  }
}                  

/* TODO: share code with collect_pending_unifications */

CVOID__PROTO(collect_one_pending_unification) {
  intmach_t sofar=0;
  tagged_t *tr = G->trail_top;
  tagged_t *tr0 = NULL;
#if defined(OPTIM_COMP)
  tagged_t *limit = w->choice->trail_top;
#else
  tagged_t *limit = TrailTopUnmark(w->choice->trail_top);  
#endif
  
  while ( !sofar && TrailYounger(tr,limit)) {
    tagged_t ref, value;
    
    TrailDec(tr);
    ref = *tr; // (tr points to the popped element)
    if (!TaggedIsCVA(ref)) continue;
    value = *TagpPtr(CVA,ref);
    if (value==ref) { 
      PANIC_FAULT("wake - unable to find all goals");  
    }
    
    sofar++;
    *TaggedToPointer(ref) = ref; /* untrail */
    
#if 0 /* old attributes */
    X(0) = *TaggedToGoal(ref);
#else /* new attributes */
    X(0) = ref;
#endif
    X(1) = value;
    
    if (!CondCVA(ref)) {
      tr0=tr;
      *tr=0;
    }
  }
  SetWakeCount(0);
  
  if (!sofar) {
    PANIC_FAULT("wake - unable to find all goals");
  }
  
  /* now compress the trail */
  if (tr0) {
    CompressTrailNoGC(tr0);
  }
}

/* --------------------------------------------------------------------------- */
/* TODO: benchmark, fix, and enable this code */

#if defined(USE_FAST_MULTIATTR)

/* C versions of the Multi-attributes Accessors */
/* Author: Remy Haemmerle */

CBOOL__PROTO(setarg);

CBOOL__PROTO(c_setarg, intmach_t, tagged_t, tagged_t, bool_t);

#define MULTI_ATTR_F SetArity(atom_att, 3)

#define CELL_KEY(CELL)  *TaggedToArg((CELL),1)
#define CELL_VAL(CELL)  *TaggedToArg((CELL),2)
#define CELL_NEXT(CELL) *TaggedToArg((CELL),3)

#define DEREF_AND_ENSURE_ATOM_MODKEY(KEY) ({                        \
  DerefSw_HVAorCVAorSVA_Other((KEY),                                \
           BUILTIN_ERROR(ERR_instantiation_error, (KEY), 2);,{});       \
  if (!TaggedIsATM((KEY))) {                                        \
    if ((!TaggedIsSTR((KEY))) ||                                    \
        (TaggedToHeadfunctor((KEY)) != SetArity(atom_user, 1)))        \
      { BUILTIN_ERROR(ERR_type_error(atom), (KEY), 2); }         \
    (KEY) = atom_user;                                              \
  }                                                                 \
})

#define LOOK_FOR_CELL(CELL, KEY, K, BEFORE_CODE, NIL_CODE) ({ \
  do {                                                      \
    BEFORE_CODE;                                            \
    (CELL) = CELL_NEXT(CELL);                               \
    if ((CELL) == atom_nil) { NIL_CODE; }                   \
    (K) = CELL_KEY(CELL);                                   \
  } while((KEY) < (K));                                     \
})

/* The following code checks the variable uses multi-attributes. 
   Otherwise it silently fails. */
#define ACCESS_ATTR_STR(ATTRVAR, COMPLEX) ({ \
  (COMPLEX) = *TaggedToGoal((ATTRVAR)); \
  if (!TaggedIsSTR((COMPLEX)) || \
      TaggedToHeadfunctor((COMPLEX)) != MULTI_ATTR_F) { \
    return FALSE; \
  } \
})

CBOOL__PROTO(get_attr__3) {
  ERR__FUNCTOR("attr_rt:get_attr", 3);

  tagged_t var, k, key, complex, tmp;

  var=X(0); DEREF(var, var);
  
  if (IsVar(var) && VarIsCVA(var)) { 
    key=X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key); 
    ACCESS_ATTR_STR(var, complex);

    LOOK_FOR_CELL(complex, key, k, ;, {return FALSE;});
        
    if (key == k) { 
      tmp = CELL_VAL(complex);
      CBOOL__LASTUNIFY(X(2), tmp);
    }
  }
  
  return FALSE;
}

CBOOL__PROTO(put_attr__3) {
  ERR__FUNCTOR("attr_rt:put_attr", 3);

  tagged_t var, k, key, val, complex, prev, next, *ptr;

  var=X(0); 
  DerefSw_HVAorCVAorSVA_Other(var, goto put_attr__3__var;, {});
  
  BUILTIN_ERROR(ERR_uninstantiation_error, var, 1);

 put_attr__3__var:

  key = X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key);
  val = X(2); DEREF(val, val);

  if (VarIsCVA(var)) { 
    ACCESS_ATTR_STR(var, complex);
    
    LOOK_FOR_CELL(complex, key, k, 
                  {prev = complex;}, 
                  {next = atom_nil; goto insert_cell;});

    if (key == k)  {
      if (!IsVar(val)) {
        /* setarg/3 is safe, since val is not variable. */
        return c_setarg(Arg, 2, complex, val, TRUE);
      }
      /* setarg/3 is not safe, since val is variables. Then replace the all cell. */
      next = CELL_NEXT(complex);
    } else { 
      next = complex; 
    }

  insert_cell:
    ptr = G->heap_top;
    complex = Tagp(STR, ptr);
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, key);
    HeapPush(ptr, val);
    HeapPush(ptr, next);
    G->heap_top = ptr;

    return c_setarg(Arg, 3, prev, complex, TRUE);
  } else {
    ptr = G->heap_top;
    complex = Tagp(STR, ptr);
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, TaggedZero);
    HeapPush(ptr, TaggedZero);
    HeapPush(ptr, Tagp(STR, HeapOffset(ptr,1)));
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, key);
    HeapPush(ptr, val);
    HeapPush(ptr, atom_nil);
    G->heap_top = ptr;
     
    return bu2_attach_attribute(Arg, var, complex);
  }

}

CBOOL__PROTO(del_attr__2) {
  ERR__FUNCTOR("attr_rt:del_attr", 2);

  tagged_t var, k, key, complex, prev;

  var=X(0); DEREF(var, var);

  if (!IsVar(var) || !VarIsCVA(var)) { return TRUE; }
  
  ACCESS_ATTR_STR(var, complex);
    
  key = X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key);
 
  LOOK_FOR_CELL(complex, key, k, 
                { prev = complex; }, 
                { return TRUE; });
  
  if (key == k)  {
    return c_setarg(Arg, 3, prev, CELL_NEXT(complex), TRUE);
  }

  return TRUE;
}

/*
CBOOL__PROTO(type_attr__3) {
  ERR__FUNCTOR("attr_rt:type_attr", 3);

  tagged_t t, k, key, complex, type;
 
  key = X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key);

  t=X(0); DEREF(t, t);
  switch (TagOf(t)) {
  case UBV:
  case SVA:
  case HVA:
    type = atm_var; break;  
  case CVA:
    ACCESS_ATTR_STR(t, complex);
    LOOK_FOR_CELL(complex, key, k, {}, {break;});
    if (k == key) { type = atm_attv; break;} 
    else { type = atm_var; break; }
  case STR:
    if (STRIsLarge(t)) {
      if (LargeIsFloat(t)) { type = atm_float; break; }
      else { type = atm_int; break; }
    } else { type = atm_str; break; }
  case ATM:
    type = atm_atm; break;
  case LST:
    type = atm_lst; break;
  case NUM:
    type = atm_int; break;
  }

  return cunif(type, X(2));
}  
*/

/* wrapper for setarg */ 
CBOOL__PROTO(c_setarg,
             intmach_t number,
             tagged_t complex,
             tagged_t newarg,
             bool_t backtrackable) {
  X(0) = MakeSmall(number);
  X(1) = complex;
  X(2) = newarg;
  X(3) = (backtrackable ? atom_on : atom_true);

  return setarg(Arg);
}

#endif 
