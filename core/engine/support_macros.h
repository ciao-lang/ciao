/*
 *  support_macros.h
 *
 *  General runtime support macros.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_SUPPORT_MACROS_H
#define _CIAO_SUPPORT_MACROS_H

#include <ciao/absmach_predef.h>
#include <ciao/eng_dbg.h>
#include <ciao/threads.h>
#include <ciao/locks_prim.h>

#define NULL_TRAIL_ENTRY MakeSmall(0)
#define IsCanceled(T) (T == NULL_TRAIL_ENTRY)
#define NullifyTrailEntry(P) *(P) = NULL_TRAIL_ENTRY

/* ------------------------------------------------------------------------- */

#include <ciao/global_defs.h>

#define DerefHeap(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefHeap(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefCar(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefCdr(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefArg(m_i,Ptr,I); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  RefHeapNext(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  tagged_t m_i; \
  tagged_t m_j; \
 \
  m_i = X; \
  DerefSwitch(m_i,m_j,;) \
  Xderef = m_i; \
}

#define SwitchOnVar(Reg,Aux,HVACode,CVACode,SVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (Reg & TagBitSVA) \
	    { RefSVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else SVACode \
	    } \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}

#define SwitchOnHeapVar(Reg,Aux,HVACode,CVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}



#define DerefSwitch(Reg,Aux,VarCode) \
{ \
  if (IsVar(Reg)) \
    do \
      if (Reg == (Aux = *TagToPointer(Reg))) \
	{VarCode;break;} \
    while (IsVar(Reg=Aux)); \
}

#define DerefHeapSwitch(Reg,Aux,VarCode) DerefSwitch(Reg,Aux,VarCode)


#define YoungerHeapVar(Q,R)	HeapYounger(Q,R)
#define YoungerStackVar(Q,R)	StackYounger(Q,R)

#if defined(PARBACK) || defined(ANDPARALLEL)
#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond) || !OnStack(TagToPointer(X)))
#else
#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond))
#endif
#define CondStackvar(X)		CondSVA(X)

/* segfault patch -- jf */
CVOID__PROTO(trail_push_check, tagged_t x);

/* segfault patch -- jf */
#define BindCVA_NoWake(U,V) \
{ \
  TrailPushCheck(w->trail_top,U); \
  *TagToCVA(U) = V; \
}

#define BindCVA(U,V)				\
  {						\
    Wake;					\
    TrailPushCheck(w->trail_top,U);		\
    *TagToCVA(U) = V;				\
  }

#define BindSVA(U,V)				\
  {						\
    if (CondSVA(U))				\
      TrailPushCheck(w->trail_top,U);		\
    *TagToSVA(U) = V;				\
  }

#define BindHVA(U,V)				\
  {						\
    if (CondHVA(U))				\
      TrailPushCheck(w->trail_top,U);		\
    *TagToHVA(U) = V;				\
  }

#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)

//TODO: nullify fake trail entries with a predicate which makes nothing.
#define PlainUntrail(TR,Ref,CONT)					\
  {									\
    Ref = TrailPop(TR);							\
    if (!IsVar(Ref))							\
      {if (!IsCanceled(Ref)) CONT}					\
    else								\
      *TagToPointer(Ref) = Ref;						\
  } 

/* SERIOUS_FAULT - a fault that should not occur- indicating a corruption
                  such as following the STR tag not coming to a FNT tag
		  this kind of fault may not need to be testing in final
		  version but must in testing cause a total abort
   USAGE_FAULT   - a fault in the usage(incorrect parameters) of a 
                  builtin predicate - an error message is written.
   MINOR_FAULT   - a fault that should result in a error message being
                  written somewhere, but the builtin predicate just
		  fails and is not aborted
*/


/* Exit code from wam() when aborting */
#define WAM_ABORT -32768 /* see exceptions.pl */
#define WAM_INTERRUPTED -32767

#include <setjmp.h>
#include <ciao/os_signal.h>

extern SIGJMP_BUF abort_env;

void failc(char *mesg);

/* Exit for child processes (avoid flushing and closign standard I/O from parent) */
#define _EXIT(Code) { fflush(NULL); _exit((Code)); }

#define SERIOUS_FAULT(Y) { \
    failc(Y); \
    SIGLONGJMP(abort_env, WAM_ABORT); \
  }
                          
#define MAJOR_FAULT(Y) { failc(Y); return FALSE; }

#define USAGE_FAULT(Y) { failc(Y); return FALSE; }

#define MINOR_FAULT(Y) { return FALSE; }


/* Error codes, xref errhandle.pl, internals.pl //) */
/* OGRAMA: error classification ISO PROLOG */


/* For any change / addition to this list:
   
            PLEASE DO READ AND UPDATE THE CORRESPONDING FACTS IN
                        core/engine/internals.pl

    Error messages are given by core/lib/errhandle.pl .  Learn how these
    work before updating anything here.

*/


/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define UNINSTANTIATION_ERROR   2
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*START_TYPE+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*START_DOM+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*START_EXIST+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*START_PERM+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*START_REPRES+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*START_EVAL+D)
#define RESOURCE_ERROR(D)       (RANGE_PER_ERROR*START_RES+D)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*START_SYNTAX)
#define SYSTEM_ERROR            (RANGE_PER_ERROR*START_SYSTEM)
#define FOREIGN_ERROR           (RANGE_PER_ERROR*START_FOREIGN)
#define USER_EXCEPTION          (RANGE_PER_ERROR*START_USER)

#define RANGE_PER_ERROR 100                    /* Enough number of errors */

#define START_INST    0
#define START_TYPE    1
#define START_DOM     2
#define START_EXIST   3
#define START_PERM    4
#define START_REPRES  5
#define START_EVAL    6
#define START_RES     7
#define START_SYNTAX  8
#define START_SYSTEM  9
#define START_FOREIGN 10
#define START_USER    11

/* TYPE_ERRORS */
#define STRICT_ATOM          0
#define ATOMIC               1
#define TY_BYTE              2
#define CHARACTER            3
#define COMPOUND             4
#define EVALUABLE            5
#define IN_BYTE              6
#define INTEGER              7
#define LIST                 8
#define NUMBER               9
#define PREDICATE_INDICATOR 10
/* RH: Not ISO anymore (from corrigendum 2) */
/* #define VARIABLE            11  */
#define CALLABLE            12

/* DOMAIN_ERRORS */
#define CHARACTER_CODE_LIST     0
#define SOURCE_SINK             1
#define STREAM                  2
#define IO_MODE                 3
#define NON_EMPTY_LIST          4
#define NOT_LESS_THAN_ZERO      5
#define OPERATOR_PRIORITY       6
#define PROLOG_FLAG             7
#define READ_OPTION             8
#define FLAG_VALUE              9
#define CLOSE_OPTION           10
#define STREAM_OPTION          11
#define STREAM_OR_ALIAS        12
#define STREAM_POSITION        13
#define STREAM_PROPERTY        14
#define WRITE_OPTION           15
#define OPERATOR_SPECIFIER     16


/* EXISTENCE_ERRORS */
#define PROCEDURE 0
/* SOURCE_SINK and STREAM already defined */
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define ACCESS      0
#define CREATE      1
#define INPUT       2
#define MODIFY      3
#define OPEN        4
#define OUTPUT      5
#define REPOSITION  6

/* OBJECTS */
#define BINARY_STREAM        0
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/
#define TEXT_STREAM          3
#define FLAG                 4
#define OPERATOR             5
#define PAST_END_OF_STREAM   6
#define PRIVATE_PROCEDURE    7
#define STATIC_PROCEDURE     8



/* REPRESENTATION_ERROR */

/* CHARACTER_CODE_LIST already defined */
/* #define CHARACTER_CODE_LIST     0 */
#define IN_CHARACTER_CODE     1
#define MAX_ARITY             2
/*#define CHARACTER            3*/
#define MAX_INTEGER           4
#define MIN_INTEGER           5
#define CHARACTER_CODE        6
#define NAN_OR_INF_TO_INTEGER 7
#define MAX_ATOM_LENGTH       8  /* Unneeded with dynamic atom sizes */

/* EVALUATION_ERROR */
#define FLOAT_OVERFLOW 0
#define INT_OVERFLOW   1
#define E_UNDEFINED    2
#define E_UNDERFLOW    3
#define ZERO_DIVISOR   4

/* RESOURCE_ERROR */
#define R_UNDEFINED    0
#define R_STACK        1



/* OGRAMA: OLD VERSION ---------------------------------------------------- */ 
/* #define TYPE_ERROR(Type) (32+Type) */ /* includes also domain errors */ 

/* #define INSTANTIATION_ERROR 1
#define READ_PAST_EOS_ERROR 2
#define NO_READ_PERMISSION 3
#define NO_WRITE_PERMISSION 4
#define NO_SUCH_FILE 5
#define NO_OPEN_PERMISSION 6
#define NO_ACCESS_PERMISSION 7
#define SYSTEM_ERROR 8 OGRAMA */

/* Type codes for TYPE_ERROR  //) */
/* #define STRICT_ATOM 0
#define ATOMIC 1
#define TY_BYTE 2
#define CALLABLE 3
#define COMPOUND 4
#define EVALUABLE 5
#define IN_BYTE 6
#define INTEGER 7
#define LIST 8
#define NUMBER 9
#define PREDICATE_INDICATOR 10
#define VARIABLE 11 
*/

/*
#define CHARACTER_CODE_LIST 32 First domain code 
#define STREAM_OR_ALIAS 33
#define SOURCE_SINK 34  OGRAMA */
/* END OLD VERSION ----------------------------------------------------------- */

/* Exceptions (backport from optim_comp) */
/* TODO: pass worker as argument to macros? */

/* usage: goto, continue, break is forbidden inside CODE! */
#define EXCEPTION__CATCH(CODE, HANDLER) ({ \
  SIGJMP_BUF catch_exception__handler; \
  SIGJMP_BUF *catch_exception__old_handler; \
  catch_exception__old_handler = w->misc->errhandler; \
  w->misc->errhandler = &catch_exception__handler; \
  if (SIGSETJMP(catch_exception__handler)) { \
    /* just in case of a worker expansion */ \
    w = desc->worker_registers; \
    w->misc->errhandler = catch_exception__old_handler; \
    HANDLER; \
  } else { \
    CODE; \
  } \
})

#define UNLOCATED_EXCEPTION(Code) {		\
    ErrCode = Code;				\
    ErrFuncName = "unknown";			\
    ErrFuncArity = -1;				\
    ErrArgNo = 0;				\
    Culprit = TaggedZero;			\
    EXCEPTION__THROW;				\
  }

#define EXCEPTION__THROW SIGLONGJMP(*w->misc->errhandler, 1)

/* Throwing exceptions from builtins */

#define ERR__FUNCTOR(NAME, ARITY) \
  static char *const err__name = NAME; static const int err__arity = ARITY; 

#define BUILTIN_ERROR(Code,Culpr,ArgNo) ({ \
  ErrCode = Code; \
  ErrFuncName = err__name; \
  ErrFuncArity = err__arity; \
  ErrArgNo = ArgNo; Culprit = Culpr; \
  EXCEPTION__THROW; \
})

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) ({ \
  BUILTIN_ERROR(IsVar(Arg) ? INSTANTIATION_ERROR : TYPE_ERROR(ReqType), \
                Arg, ArgNo); \
})

/* MakeLST(To,Car,Cdr):
   
   Set 'To' to a term tagged_t LST
   whose car and cdr are 'Car' and Cdr'.

   'To' may be identical to 'Car' or 'Cdr'.
*/
#define MakeLST(To,Car,Cdr) \
{ tagged_t makelst_car = (Car); \
  HeapPush(w->global_top,makelst_car); \
  HeapPush(w->global_top,Cdr); \
  To = Tag(LST,HeapOffset(w->global_top,-2)); \
}

/* MakeSTR(To,Functor):
   
   Set 'To' to a term tagged_t STR
   whose principal functor is 'Functor'.  
   Space is allocated for the arguments, but they are not filled in.
*/
#define MakeSTR(To,Functor) \
{ \
  HeapPush(w->global_top,Functor); \
  To = Tag(STR,HeapOffset(w->global_top,-1)); \
  w->global_top = HeapOffset(w->global_top,Arity(Functor)); \
}

#define Unify_constant(U,V) \
{ tagged_t m_t0, m_u=U, m_t1=V; \
  SwitchOnVar(m_t1,m_t0,{BindHVA(m_t1,m_u);}, \
	            {BindCVA(m_t1,m_u);}, \
	            {BindSVA(m_t1,m_u);}, \
		    {if (m_t1!=m_u) return FALSE;}) \
}

#define EXPAND_ATOM_BUFFER(new_max_atom_length) \
{ \
     Atom_Buffer = \
       checkrealloc_ARRAY(char, \
			  Atom_Buffer_Length,	       \
			  new_max_atom_length,	       \
			  Atom_Buffer);		       \
    Atom_Buffer_Length = new_max_atom_length; \
    UpdateHeapMargins(); \
}


extern intmach_t (*eng_goal_from_thread_id)(THREAD_ID id);

#endif /* _CIAO_SUPPORT_MACROS_H */
