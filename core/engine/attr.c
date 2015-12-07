/*
 *  attr.c
 *
 *  Copyright (C) 1992 Department of Medical Cybernetics & Artificial
 *    Intelligence.  University of Vienna.  Freyung 6.  A-1010 Vienna,
 *    Austria.
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author: Christian Holzbaur [christian@ai.univie.ac.at]
 *    Permission to use this software for any purpose is subject to
 *    the USER AGREEMENT between the DMCAI and the User.
 */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/start.h>
#include <ciao/attr.h>
#include <ciao/stacks.h>
    
extern definition_t *address_true;                         /* Shared */

extern tagged_t atm_var; /* Shared */
extern tagged_t atm_attv; /* Shared */
extern tagged_t atm_float; /* Shared */
extern tagged_t atm_int; /* Shared */
extern tagged_t atm_str; /* Shared */
extern tagged_t atm_atm; /* Shared */
extern tagged_t atm_lst; /* Shared */

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
 
CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x) {
  tagged_t t;

  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) { 
      return *TagToGoal(x);
    }
  }); 
  return ERRORTAG;				                  /* fail */
}

/* 
   suspend a goal on a variable. Stolen from suspend_goal in misc.c .
*/

CBOOL__PROTO(bu2_attach_attribute,
	     tagged_t var,
	     tagged_t constr) {
  tagged_t t0;
  tagged_t *h = w->global_top;
  tagged_t *tr = w->trail_top;
        
  DerefSwitch(constr,t0,{USAGE_FAULT("attach_attribute/2: type error");}); 
  DEREF(var,var);
  if (TagIsHVA(var)) {
    LoadCVA(t0,h);
    if (CondHVA(var))	{
      TrailPush(tr,var);
      *TagToHVA(var) = t0;
    } else {
      *TagToHVA(var) = t0;
    }
  } else {
    if (TagIsSVA(var)) { 			          /* unsafe value */
      tagged_t *ptr = h;
      LoadHVA(t0,ptr);
      h = ptr;
      BindSVA(var,t0);
      var = t0;
      LoadCVA(t0,h);
      *TagToHVA(var) = t0;
    } else {
      USAGE_FAULT("attach_attribute/2: type error");
    }
  }
  
  HeapPush(h,constr);
  HeapPush(h,PointerToTerm(address_true));	                  /* func */
  
  w->global_top = h;
  w->trail_top = tr;
  if (ChoiceYounger(w->node,TrailOffset(tr,CHOICEPAD)))
    choice_overflow(Arg,CHOICEPAD); 
  return TRUE;
}  

/* a la defrost */

CBOOL__PROTO(bu1_detach_attribute, tagged_t x) {
  tagged_t t; 
  tagged_t *h = w->global_top;
  
  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) {
      LoadHVA(t,h);
      BindCVA_NoWake(x,t);			               /* trailed */
      w->global_top = h;
      return TRUE;
    }
  });
  USAGE_FAULT("detach_attribute/2: type error");
}  
 
/* think about optimizations a la setarg */

CBOOL__PROTO(bu2_update_attribute,
	     tagged_t x,
	     tagged_t constr) {
  tagged_t t;
  tagged_t *h = w->global_top;
              
  DerefSwitch(constr,t,{USAGE_FAULT("update_attribute/2: type error");}); 
  DerefSwitch(x,t,{
    if ( VarIsCVA(x) ) { 
      LoadCVA(t,h); 
      HeapPush(h,constr);
      HeapPush(h,PointerToTerm(address_true));	                  /* func */
      BindCVA_NoWake(x,t);			               /* trailed */
      w->global_top = h;
      return TRUE;
    }
  }); 
  USAGE_FAULT("update_attribute/2: type error"); 
}  

/*  
   Called from wam.c
   Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment.  
   Each pending unification pushes 4 heap elems - cf enter_predicate: wam.c
*/

CVOID__PROTO(collect_pending_unifications, intmach_t wake_count) {
  intmach_t sofar=0;
  tagged_t *tr = w->trail_top;
  tagged_t *h = w->global_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = TagToPointer(w->node->trail_top);  
   
  X(0) = atom_nil;
  while (sofar<wake_count && TrailYounger(tr,limit))  {
    tagged_t ref, value;
    
    ref = TrailPop(tr);
    if (!TagIsCVA(ref))
      continue;
    RefCVA(value,ref); 
    if (value==ref) { 
      SERIOUS_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    *TagToPointer(ref) = ref;     		               /* untrail */
    
    HeapPush( h, ref); 
    HeapPush( h, value);  
    HeapPush( h, Tag(LST,h-2));
    HeapPush( h, X(0));
    X(0) = Tag(LST,h-2);
    
    if ( !CondCVA(ref))	
      tr0=tr, *tr=0; 
  }
  w->global_top = h;
  Heap_Warn_Soft = Heap_Start;			     /* make WakeCount==0 */
  
  if (sofar<wake_count) {
    SERIOUS_FAULT("wake - unable to find all goals");
  }
  
                                                /* now compress the trail */
  
  if (tr0) {
    h = tr = tr0;
    while (TrailYounger(w->trail_top,tr)){
      tagged_t ref;
      
      if ((ref = TrailNext(tr)))
        TrailPush(h,ref);
    }
    w->trail_top = h;
  }
}                  

CVOID__PROTO(collect_one_pending_unification) {
  intmach_t sofar=0;
  tagged_t *tr = w->trail_top;
  tagged_t *tr0 = NULL;
  tagged_t *limit = TagToPointer(w->node->trail_top);  
  
  while ( !sofar && TrailYounger(tr,limit)) {
    tagged_t ref, value;
    
    ref = TrailPop(tr);
    if (!TagIsCVA(ref))
      continue;
    RefCVA(value,ref); 
    if (value==ref) { 
      SERIOUS_FAULT("wake - unable to find all goals");  
    }
    
    sofar++; 
    *TagToPointer(ref) = ref;     		               /* untrail */
    
    /*   X(0) = *TagToGoal(ref);*/
    X(0) = ref ;
    X(1) = value;
    
    if ( !CondCVA(ref))	
      tr0=tr, *tr=0; 
  }
  Heap_Warn_Soft = Heap_Start;			     /* make WakeCount==0 */
  
  if ( !sofar ) {
    SERIOUS_FAULT("wake - unable to find all goals");
  }
  
                                                /* now compress the trail */
  
  if (tr0) {
    tagged_t *h = tr = tr0;
    while (TrailYounger(w->trail_top,tr))
      {
        tagged_t ref;
        
        if ((ref = TrailNext(tr)))
          TrailPush(h,ref);
      }
    w->trail_top = h;
  }
}

#if defined(USE__FAST_MULTIATTR)

/************************************************ 
 * C versions of the Multi-attributes Accessors *
 *                                              *
 * Author: Remy Haemmerle                       *
 * Copyright: CLIP group, 2013                  *
 ************************************************/

CBOOL__PROTO(setarg);

CBOOL__PROTO(c_setarg, intmach_t, tagged_t, tagged_t, bool_t);

#define MULTI_ATTR_F SetArity(atom_att, 3)

#define CELL_KEY(CELL)  *TagToArg((CELL),1)
#define CELL_VAL(CELL)  *TagToArg((CELL),2)
#define CELL_NEXT(CELL) *TagToArg((CELL),3)

#define DEREF_AND_ENSURE_ATOM_MODKEY(KEY) ({				\
      tagged_t  tmp;							\
      DerefSwitch((KEY),						\
		  tmp,							\
		  BUILTIN_ERROR(INSTANTIATION_ERROR, (KEY), 2););	\
      if (!TagIsATM((KEY))) {						\
	if ((!TagIsSTR((KEY))) ||					\
	    (TagToHeadfunctor((KEY)) != SetArity(atom_user, 1)))	\
	  { BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), (KEY), 2); }		\
	(KEY) = atom_user;						\
      }									\
    })

#define LOOK_FOR_CELL(CELL, KEY, K, BEFORE_CODE, NIL_CODE) ({	\
      do {							\
	BEFORE_CODE;						\
	(CELL) = CELL_NEXT(CELL);				\
	if ((CELL) == atom_nil) { NIL_CODE; }			\
	(K) = CELL_KEY(CELL);					\
      } while( (KEY) < (K) );					\
    })

/* 
   The following code checks the variable uses multi-attributes. 
   Otherwise it silently fails.
*/
#define ACCESS_ATTR_STR(ATTRVAR, COMPLEX) ({		\
      (COMPLEX) = *TagToGoal((ATTRVAR));		\
      if (!TagIsSTR((COMPLEX)) ||			\
	  TagToHeadfunctor((COMPLEX)) != MULTI_ATTR_F)	\
	{ return FALSE; }				\
})

CBOOL__PROTO(get_attr__3) {
  ERR__FUNCTOR("attr_rt:get_attr", 3);

  tagged_t var, k, key, complex, tmp;

  var=X(0); DEREF(var, var);
  
  if ( IsVar(var) && VarIsCVA(var) ) { 

    key=X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key); 
    ACCESS_ATTR_STR(var, complex);

    LOOK_FOR_CELL(complex, key, k, ;, {return FALSE;});
        
    if (key == k) { 
      tmp = CELL_VAL(complex);
      return cunify(Arg, X(2), tmp);
    }
  }
  
  return FALSE;
}

CBOOL__PROTO(put_attr__3) {
  ERR__FUNCTOR("attr_rt:put_attr", 3);

  tagged_t var, k, key, val, complex, prev, next, tmp, *ptr;

  var=X(0); 
  DerefSwitch(var, tmp, goto put_attr__3__var;);
  
  BUILTIN_ERROR(UNINSTANTIATION_ERROR, var, 1);

 put_attr__3__var:

  key = X(1); DEREF_AND_ENSURE_ATOM_MODKEY(key);
  val = X(2); DEREF(val, val);

  if ( VarIsCVA(var) ) { 
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
     
    ptr = w->global_top;
    complex = Tag(STR, ptr);
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, key);
    HeapPush(ptr, val);
    HeapPush(ptr, next);
    w->global_top = ptr;

    return c_setarg(Arg, 3, prev, complex, TRUE);
    
  } else {
    
    ptr = w->global_top;
    complex = Tag(STR, ptr);
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, TaggedZero);
    HeapPush(ptr, TaggedZero);
    HeapPush(ptr, Tag(STR, HeapOffset(ptr,1)));
    HeapPush(ptr, SetArity(atom_att, 3));
    HeapPush(ptr, key);
    HeapPush(ptr, val);
    HeapPush(ptr, atom_nil);
    w->global_top = ptr;
     
    return bu2_attach_attribute(Arg, var, complex);
  }

}


CBOOL__PROTO(del_attr__2) {
  ERR__FUNCTOR("attr_rt:del_attr", 2);

  tagged_t var, k, key, complex, prev;

  var=X(0); DEREF(var, var);

  if ( !IsVar(var) || !VarIsCVA(var) ) { return TRUE; }
  
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

/*
  wrapper for setarg
*/ 

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
