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
#include <ciao/eng_debug.h>
#include <ciao/os_threads.h>

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

//TODO:[merge-oc] DerefSw_HVA_CVA_SVA_Other?
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


#define YoungerHeapVar(Q,R)     HeapYounger(Q,R)
#define YoungerStackVar(Q,R)    StackYounger(Q,R)

#if defined(PARBACK) || defined(ANDPARALLEL)
#define CondHVA(X)              (!OffHeaptop(X,w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondCVA(X)              (!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondSVA(X)              (!OffStacktop(X,w->local_uncond) || !OnStack(TagToPointer(X)))
#else
#define CondHVA(X)              (!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)              (!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)              (!OffStacktop(X,w->local_uncond))
#endif
#define CondStackvar(X)         CondSVA(X)

/* segfault patch -- jf */
CVOID__PROTO(trail_push_check, tagged_t x);

/* segfault patch -- jf */
#define BindCVA_NoWake(U,V) \
{ \
  TrailPushCheck(w->trail_top,U); \
  *TagToCVA(U) = V; \
}

#define BindCVA(U,V)                            \
  {                                             \
    Wake;                                       \
    TrailPushCheck(w->trail_top,U);             \
    *TagToCVA(U) = V;                           \
  }

#define BindSVA(U,V)                            \
  {                                             \
    if (CondSVA(U))                             \
      TrailPushCheck(w->trail_top,U);           \
    *TagToSVA(U) = V;                           \
  }

#define BindHVA(U,V)                            \
  {                                             \
    if (CondHVA(U))                             \
      TrailPushCheck(w->trail_top,U);           \
    *TagToHVA(U) = V;                           \
  }

#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)

//TODO: nullify fake trail entries with a predicate which makes nothing.
#define PlainUntrail(TR,Ref,CONT)                                       \
  {                                                                     \
    Ref = TrailPop(TR);                                                 \
    if (!IsVar(Ref))                                                    \
      {if (!IsCanceled(Ref)) CONT}                                      \
    else                                                                \
      *TagToPointer(Ref) = Ref;                                         \
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

/* =========================================================================== */
/* Error codes, xref errhandle.pl, internals.pl //) */

/* OGRAMA: error classification ISO PROLOG */
/* JFMC: renamed macros */

/* For any change / addition to this list:
   
            PLEASE DO READ AND UPDATE THE CORRESPONDING FACTS IN
                        core/engine/internals.pl

    Error messages are given by core/lib/errhandle.pl .  Learn how these
    work before updating anything here.
*/
/* TODO:[oc-merge] generate automatically, update */

/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define UNINSTANTIATION_ERROR   2
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*error_start(type)+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*error_start(dom)+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*error_start(exist)+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*error_start(perm)+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*error_start(repres)+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*error_start(eval)+D)
#define RESOURCE_ERROR(D)       (RANGE_PER_ERROR*error_start(res)+D)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*error_start(syntax))
#define SYSTEM_ERROR            (RANGE_PER_ERROR*error_start(system))
/* TODO:[oc-merge] add FOREIGN_ERROR in optim-comp */
#define FOREIGN_ERROR           (RANGE_PER_ERROR*error_start(foreign))
#define USER_EXCEPTION          (RANGE_PER_ERROR*error_start(user))

#define RANGE_PER_ERROR 100                    /* Enough number of errors */
#define error_start(KEY) error_start__##KEY
#define error_start__inst    0
#define error_start__type    1
#define error_start__dom     2
#define error_start__exist   3
#define error_start__perm    4
#define error_start__repres  5
#define error_start__eval    6
#define error_start__res     7
#define error_start__syntax  8
#define error_start__system  9
/* TODO:[oc-merge] add error_start(foreign) in optim-comp */
#define error_start__foreign 10
#define error_start__user    11

/* TYPE_ERRORS */
#define TYPE_ERRORS(KEY) TYPE_ERRORS__##KEY
#define TYPE_ERRORS__atom 0
#define TYPE_ERRORS__atomic 1
#define TYPE_ERRORS__byte 2
#define TYPE_ERRORS__character 3
#define TYPE_ERRORS__compound 4
#define TYPE_ERRORS__evaluable 5
#define TYPE_ERRORS__in_byte 6
#define TYPE_ERRORS__integer 7
#define TYPE_ERRORS__list 8
#define TYPE_ERRORS__number 9
#define TYPE_ERRORS__predicate_indicator 10
#define TYPE_ERRORS__variable 11
#define TYPE_ERRORS__callable 12
//--
/* TODO:[oc-merge] unfold */
#define STRICT_ATOM TYPE_ERRORS(atom)
#define ATOMIC TYPE_ERRORS(atomic)
/* TODO:[oc-merge] renamed from BYTE, backport to optim-comp */
#define TY_BYTE TYPE_ERRORS(byte)
/* TODO:[oc-merge] used? */
#define CHARACTER TYPE_ERRORS(character)
#define COMPOUND TYPE_ERRORS(compound)
#define EVALUABLE TYPE_ERRORS(evaluable)
#define IN_BYTE TYPE_ERRORS(in_byte)
#define INTEGER TYPE_ERRORS(integer)
#define LIST TYPE_ERRORS(list)
#define NUMBER TYPE_ERRORS(number)
#define PREDICATE_INDICATOR TYPE_ERRORS(predicate_indicator)
/* RH: Not ISO anymore (from corrigendum 2) */
/* #define VARIABLE TYPE_ERRORS(variable) */
#define CALLABLE TYPE_ERRORS(callable)

/* DOMAIN_ERRORS */
#define DOMAIN_ERRORS(KEY) DOMAIN_ERRORS__##KEY
#define DOMAIN_ERRORS__character_code_list 0
#define DOMAIN_ERRORS__source_sink 1
#define DOMAIN_ERRORS__stream 2
#define DOMAIN_ERRORS__io_mode 3
#define DOMAIN_ERRORS__non_empty_list 4
#define DOMAIN_ERRORS__not_less_than_zero 5
#define DOMAIN_ERRORS__operator_priority 6
#define DOMAIN_ERRORS__prolog_flag 7
#define DOMAIN_ERRORS__read_option 8
#define DOMAIN_ERRORS__flag_value 9
#define DOMAIN_ERRORS__close_option 10
#define DOMAIN_ERRORS__stream_option 11
#define DOMAIN_ERRORS__stream_or_alias 12
#define DOMAIN_ERRORS__stream_position 13
#define DOMAIN_ERRORS__stream_property 14
#define DOMAIN_ERRORS__write_option 15
#define DOMAIN_ERRORS__operator_specifier 16
//--
/* TODO:[oc-merge] unfold */
/* TODO:[oc-merge] not in optim-comp */
#define CHARACTER_CODE_LIST     DOMAIN_ERRORS(character_code_list)
/* TODO:[oc-merge] not in optim-comp */
#define SOURCE_SINK             DOMAIN_ERRORS(source_sink)
/* TODO:[oc-merge] not in optim-comp */
#define STREAM                  DOMAIN_ERRORS(stream)
#define IO_MODE DOMAIN_ERRORS(io_mode)
/* TODO:[oc-merge] not renamed in optim-comp */
#define NON_EMPTY_LIST DOMAIN_ERRORS(non_empty_list)
#define NOT_LESS_THAN_ZERO DOMAIN_ERRORS(not_less_than_zero)
#define OPERATOR_PRIORITY DOMAIN_ERRORS(operator_priority)
#define PROLOG_FLAG DOMAIN_ERRORS(prolog_flag)
#define READ_OPTION DOMAIN_ERRORS(read_option)
#define FLAG_VALUE DOMAIN_ERRORS(flag_value)
#define CLOSE_OPTION DOMAIN_ERRORS(close_option)
#define STREAM_OPTION DOMAIN_ERRORS(stream_option)
#define STREAM_OR_ALIAS DOMAIN_ERRORS(stream_or_alias)
#define STREAM_POSITION DOMAIN_ERRORS(stream_position)
#define STREAM_PROPERTY DOMAIN_ERRORS(stream_property)
#define WRITE_OPTION DOMAIN_ERRORS(write_option)
#define OPERATOR_SPECIFIER DOMAIN_ERRORS(operator_specifier)

/* EXISTENCE_ERRORS */
#define EXISTENCE_ERRORS(KEY) EXISTENCE_ERRORS__##KEY
#define EXISTENCE_ERRORS__procedure 0
#define EXISTENCE_ERRORS__source_sink 1
#define EXISTENCE_ERRORS__stream 2
//--
#define PROCEDURE EXISTENCE_ERRORS(procedure)
/* TODO:[oc-merge] unfold */
/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK EXISTENCE_ERRORS(source_sink) */
/* #define STREAM EXISTENCE_ERRORS(stream) */

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define PERMISSION_TYPES(KEY) PERMISSION_TYPES__##KEY
#define PERMISSION_TYPES__access 0
#define PERMISSION_TYPES__create 1
#define PERMISSION_TYPES__input 2
#define PERMISSION_TYPES__modify 3
#define PERMISSION_TYPES__open 4
#define PERMISSION_TYPES__output 5
#define PERMISSION_TYPES__reposition 6
//--
/* TODO:[oc-merge] unfold */
#define ACCESS PERMISSION_TYPES(access)
#define CREATE PERMISSION_TYPES(create)
#define INPUT PERMISSION_TYPES(input)
#define MODIFY PERMISSION_TYPES(modify)
#define OPEN PERMISSION_TYPES(open)
#define OUTPUT PERMISSION_TYPES(output)
#define REPOSITION PERMISSION_TYPES(reposition)

/* OBJECTS */
#define PERMISSION_OBJECTS(KEY) PERMISSION_OBJECTS__##KEY
#define PERMISSION_OBJECTS__binary_stream 0
#define PERMISSION_OBJECTS__source_sink 1
#define PERMISSION_OBJECTS__stream 2
#define PERMISSION_OBJECTS__text_stream 3
#define PERMISSION_OBJECTS__flag 4
#define PERMISSION_OBJECTS__operator 5
#define PERMISSION_OBJECTS__past_end_of_stream 6
#define PERMISSION_OBJECTS__private_procedure 7
#define PERMISSION_OBJECTS__static_procedure 8
//--
/* TODO:[oc-merge] unfold */
#define BINARY_STREAM PERMISSION_OBJECTS(binary_stream)
/* SOURCE_SINK and STREAM already defined */
/* #define SOURCE_SINK PERMISSION_OBJECTS(binary_stream) */
/* #define STREAM PERMISSION_OBJECTS(binary_stream) */
#define TEXT_STREAM PERMISSION_OBJECTS(text_stream)
#define FLAG PERMISSION_OBJECTS(flag)
#define OPERATOR PERMISSION_OBJECTS(operator)
#define PAST_END_OF_STREAM PERMISSION_OBJECTS(past_end_of_stream)
#define PRIVATE_PROCEDURE PERMISSION_OBJECTS(private_procedure)
#define STATIC_PROCEDURE PERMISSION_OBJECTS(static_procedure)

/* REPRESENTATION_ERROR */

/* CHARACTER_CODE_LIST already defined */
#define REPRESENTATION_ERRORS(KEY) REPRESENTATION_ERRORS__##KEY
#define REPRESENTATION_ERRORS__character_code_list 0
#define REPRESENTATION_ERRORS__in_character_code 1
#define REPRESENTATION_ERRORS__max_arity 2
#define REPRESENTATION_ERRORS__character 3
#define REPRESENTATION_ERRORS__max_integer 4
#define REPRESENTATION_ERRORS__min_integer 5
#define REPRESENTATION_ERRORS__character_code 6
#define REPRESENTATION_ERRORS__nan_or_inf_to_integer 7
#define REPRESENTATION_ERRORS__max_atom_length 8
//--
/* TODO:[oc-merge] unfold */
/* #define CHARACTER_CODE_LIST REPRESENTATION_ERRORS(character_code_list) */
#define IN_CHARACTER_CODE REPRESENTATION_ERRORS(in_character_code)
#define MAX_ARITY REPRESENTATION_ERRORS(max_arity)
/* #define CHARACTER REPRESENTATION_ERRORS(character) */
#define MAX_INTEGER REPRESENTATION_ERRORS(max_integer)
#define MIN_INTEGER REPRESENTATION_ERRORS(min_integer)
#define CHARACTER_CODE REPRESENTATION_ERRORS(character_code)
/* TODO:[oc-merge] new */
#define NAN_OR_INF_TO_INTEGER REPRESENTATION_ERRORS(nan_or_inf_to_integer)
#define MAX_ATOM_LENGTH       REPRESENTATION_ERRORS(max_atom_length)  /* Unneeded with dynamic atom sizes */

/* EVALUATION_ERROR */
#define EVALUATION_ERRORS(KEY) EVALUATION_ERRORS__##KEY
#define EVALUATION_ERRORS__float_overflow 0
#define EVALUATION_ERRORS__int_overflow 1
#define EVALUATION_ERRORS__e_undefined 2
#define EVALUATION_ERRORS__e_underflow 3
#define EVALUATION_ERRORS__zero_divisor 4
//--
/* TODO:[oc-merge] unfold */
#define FLOAT_OVERFLOW EVALUATION_ERRORS(float_overflow)
#define INT_OVERFLOW EVALUATION_ERRORS(int_overflow)
#define E_UNDEFINED EVALUATION_ERRORS(e_undefined)
#define E_UNDERFLOW EVALUATION_ERRORS(e_underflow)
#define ZERO_DIVISOR EVALUATION_ERRORS(zero_divisor)

/* TODO:[oc-merge] new */
/* RESOURCE_ERROR */
#define RESOURCE_ERRORS(KEY) RESOURCE_ERRORS__##KEY
#define RESOURCE_ERRORS__r_undefined 0
#define RESOURCE_ERRORS__r_stack 1
//--
/* TODO:[oc-merge] unfold */
#define R_UNDEFINED RESOURCE_ERRORS(r_undefined)
#define R_STACK RESOURCE_ERRORS(r_stack)

/* =========================================================================== */

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

#define UNLOCATED_EXCEPTION(Code) {             \
    ErrCode = Code;                             \
    ErrFuncName = "unknown";                    \
    ErrFuncArity = -1;                          \
    ErrArgNo = 0;                               \
    Culprit = TaggedZero;                       \
    EXCEPTION__THROW;                           \
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

#define LSTCELLS 2 /* Cells allocated in MakeLST */

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

#define CBOOL__UnifyCons(U,V) \
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
                          Atom_Buffer_Length,          \
                          new_max_atom_length,         \
                          Atom_Buffer);                \
    Atom_Buffer_Length = new_max_atom_length; \
    UpdateHeapMargins(); \
}


extern intmach_t (*eng_goal_from_thread_id)(THREAD_ID id);

#endif /* _CIAO_SUPPORT_MACROS_H */
