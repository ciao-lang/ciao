/*
 *  absmach_predef.h
 *
 *  Preliminary definitions for the Ciao abstract machine
 *
 *  NOTE: This is a reduced version from homonymous version in
 *    optim_comp.
 *
 *  Copyright (C) 2013-2015 Ciao Development Team
 */

#ifndef _CIAO_ABSMACH_PREDEF_H
#define _CIAO_ABSMACH_PREDEF_H

/* ------------------------------------------------------------------------- */
/* Function attributes */

#define C_NORET    __attribute__((noreturn))
#define C_AINLINE  inline __attribute__((always_inline))
#define C_NOINLINE __attribute__((noinline))

/* ------------------------------------------------------------------------- */
/* System type definitions */

#include <stdint.h>

/* 32 bits in 32 bit architectures, 64 bits in 64 bit architectures */
typedef uintptr_t intp_t; /* integer that can contain pointers */

typedef double flt64_t;

/* ------------------------------------------------------------------------- */

#if defined(x86_64) || defined(Sparc64) || defined(ppc64) /* 64-bit */
/* Definitions for 64-bit tag scheme */
typedef int64_t intmach_t;
typedef uint64_t uintmach_t;
typedef int64_t intval_t;
typedef uint64_t uintval_t;
typedef uint64_t tagged_t;
typedef uint64_t uinttag_t;
typedef uint64_t functor_t;
typedef int64_t stagged_t;
#define tagged__size 64
#else
/* Definitions for 32-bit tag scheme */
typedef int32_t intmach_t;
typedef uint32_t uintmach_t;
typedef int32_t intval_t;
typedef uint32_t uintval_t;
typedef uint32_t tagged_t;
typedef uint32_t uinttag_t;
typedef uint32_t functor_t;
typedef int32_t stagged_t;
#define tagged__size 32
#endif

typedef int32_t bool_t; /* TODO: Make it like 'char' */
#if !defined(TRUE)
#define FALSE 0
#define TRUE 1
#endif

/* Sizes */
#if tagged__size == 64
#define INTMACH_MAX INT64_MAX
#define INTMACH_MIN INT64_MIN
#elif tagged__size == 32
#define INTMACH_MAX INT32_MAX
#define INTMACH_MIN INT32_MIN
#endif

/* Type to hold time values (ticks, etc.) */
typedef int64_t inttime_t;

/* ------------------------------------------------------------------------- */
/* Macros for formatting integers */

#include <inttypes.h> /* for PRI* macros */
#if tagged__size == 64
#define PRIum PRIu64 /* intmach_t using %u */
#define PRIdm PRId64 /* intmach_t using %d */
#define PRIxm PRIx64 /* intmach_t using %x */
#elif tagged__size == 32
#define PRIum PRIu32 /* intmach_t using %u */
#define PRIdm PRId32 /* intmach_t using %d */
#define PRIxm PRIx32 /* intmach_t using %x */
#endif

/* ------------------------------------------------------------------------- */
/* Worker argument abstraction (from optim_comp) */

#define CHANGE_WORKER(NEW_WORKER, CODE) { \
  worker_t *old_w; \
  old_w = w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
  SET_WORKER(old_w); \
}
#define LOCAL_WORKER(NEW_WORKER, CODE) { \
  worker_t *w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
}

#if defined(USE_GLOBAL_WORKER)
/* TODO: worker_t not yet defined? */
extern __thread worker_t *w;
#define DECL_WORKER_ARG
#define DECL_WORKER_ARGC
#define WORKER_ARG
#define WORKER_ARGC
#define WORKERATTR
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER) 
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE) 
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
#else
#define DECL_WORKER_ARG worker_t *w
#define DECL_WORKER_ARGC DECL_WORKER_ARG,
#define WORKER_ARG w
#define WORKER_ARGC WORKER_ARG,
#if defined(USE_REGWORKERARG)
/* TODO: does not work in gcc prior to 3.4.0 (CAST) */
#define WORKERATTR __attribute__ ((regparm(1)))
#else
#define WORKERATTR
#endif
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER)
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) LOCAL_WORKER(NEW_WORKER, CODE)
#endif

/* ------------------------------------------------------------------------- */
/* Calling protocols (from optim_comp) */

/*
  The C code that implements predicates, builtins or complex abstract
  machine operations can be called following several protocols.

  - CFUN: a deterministic predicate with one output term
  - CBOOL: semideterministic predicate returns the success state
  - CINSNP: returns the continuation pointer (for compilation to C)
  - CVOID: deterministic predicate
  
  --JFMC
*/

#define CINSNP__PROTO(NAME, ...) bcp_t WORKERATTR NAME(DECL_WORKER_ARG , ## __VA_ARGS__)
#define CINSNP__INSNCONT(NAME, ...) NAME(WORKER_ARG , ## __VA_ARGS__)
#define CINSNP__LASTCALL(NAME, ...) return NAME(WORKER_ARG , ## __VA_ARGS__)
#define CINSNP__GOTO(NAME) return NAME
#define CINSNP__PROCEED return SUCCESS_INSNP
#define CINSNP__FAIL return FAIL_INSNP

#define CFUN__PROTO(NAME, TYPE, ...) TYPE WORKERATTR NAME(DECL_WORKER_ARG , ## __VA_ARGS__)
#define CFUN__EVAL(NAME, ...) NAME(WORKER_ARG , ## __VA_ARGS__)
#define CFUN__LASTCALL(NAME, ...) return NAME(WORKER_ARG , ## __VA_ARGS__)
#define CFUN__PROCEED(X) return (X)

#define CBOOL__PROTO(NAME, ...) bool_t WORKERATTR NAME(DECL_WORKER_ARG , ## __VA_ARGS__)
#define CBOOL__SUCCEED(NAME, ...) NAME(WORKER_ARG , ## __VA_ARGS__)
#define CBOOL__CALL(NAME, ...) if (!NAME(WORKER_ARG , ## __VA_ARGS__)) return FALSE
#define CBOOL__LASTCALL(NAME, ...) return NAME(WORKER_ARG , ## __VA_ARGS__)
#define CBOOL__TEST(X) if (!(X)) return FALSE
#define CBOOL__LASTTEST(X) return (X)
#define CBOOL__PROCEED return TRUE
#define CBOOL__FAIL return FALSE

#define CVOID__PROTO(NAME, ...) void WORKERATTR NAME(DECL_WORKER_ARG , ## __VA_ARGS__)
#define CVOID__CALL(NAME, ...) NAME(WORKER_ARG , ## __VA_ARGS__)
#define CVOID__PROCEED return

/* ------------------------------------------------------------------------- */

#define AddrBCOp(B,T,X) ({T* __t = (T*)((char *)(B)+(X)); __t; }) 
#define BCOp(B,T,X) (*AddrBCOp(B,T,X))
#define BCoff(P,X) ((bcp_t)((char *)(P)+(X)))
/* TODO: clang seems to generate worse (larger at least) code for BCOp */
#define BCOp0(B,T,X) (*((T*)((char *)(B)+(X))))

/*
#define BCOp(B,T,X) (*((T*)((char *)(B)+(X))))
#define BCoff(P,X) ((bcp_t)((char *)(P)+(X)))
*/

/* ------------------------------------------------------------------------- */

typedef struct worker_ worker_t; /* TODO: worker_t may not be defined yet */
typedef bool_t (*cbool_pred_t)(worker_t *w);

/* ------------------------------------------------------------------------- */
/* Runtime definitions for the current instruction set */

#if tagged__size == 64
#define BC64 1
#else
#define BC32 1
#endif

/* Scale factor for offsets in 64-bit mode (when loading 32-bit
   bytecode from a .po file in qread.c or from a list in
   objareas.c) */
#if defined(BC64)
#define BC_SCALE 2
#else
#define BC_SCALE 1
#endif

/* Pointer to bytecode */
typedef void *bcp_t;

/*
 * NOTE: Extracted from automatically generated basiccontrol.native.h
 *       Do not modify without synchronizing changes to the ImProlog
 *       sources!
 */

#define QS(KEY) QS__##KEY
#define QS__integer 2
#define QS__poffset 3
#define QS__functor 5
#define QS__tagged 6
#define QS__emul_entry 7
#define QS__builtin_entry 9
#define QS__small 8
#define QL(KEY) QL__##KEY
#define QL__uint16 8
#define QL__uint32 6
#define QL__uint64 9
#define QL__baseptr 3

#define FTYPEDEF(KEY) FTYPEDEF__##KEY
#define FTYPEDEF__basic 0
#define FTYPEDEF__str 1
#define FTYPEDEF__array 2
#define FTYPEDEF__blob 3

#define FTYPE_id(KEY) FTYPE_id__##KEY
#define FTYPE_id__f_o 15
#define FTYPE_id__f_e 8
#define FTYPE_id__f_f 9
#define FTYPE_id__f_i 10
#define FTYPE_id__f_l 11
#define FTYPE_id__f_g 12
#define FTYPE_id__f_p 13
#define FTYPE_id__f_t 14
#define FTYPE_id__f_x 16
#define FTYPE_id__f_y 17
#define FTYPE_id__f_z 18
#define FTYPE_id__f_C 5
#define FTYPE_id__f_E 6
#define FTYPE_id__f_Q 19
#define FTYPE_id__f_Y 3
#define FTYPE_id__f_Z 4
#define FTYPE_id__f_b 7
#define FTYPE_size(KEY) FTYPE_size__##KEY
#define FTYPE_ctype(KEY) FTYPE_ctype__##KEY
#if defined(BC64)
/* Double-sized bytecode (for 64-bits) */
#define FTYPE_ctype__f_o uint32_t
#define  FTYPE_size__f_o 4
#define FTYPE_ctype__f_e uint32_t
#define  FTYPE_size__f_e 4
#define FTYPE_ctype__f_f uint64_t
#define  FTYPE_size__f_f 8
#define FTYPE_ctype__f_i uint32_t
#define  FTYPE_size__f_i 4
#define FTYPE_ctype__f_i_signed int32_t /* hack... */
#define FTYPE_ctype__f_l uint64_t
#define  FTYPE_size__f_l 8
#define FTYPE_ctype__f_g liveinfo_t
#define  FTYPE_size__f_g 12
//#define FTYPE_ctype__f_p char *
#define FTYPE_ctype__f_p bcp_t
#define  FTYPE_size__f_p 8
#define FTYPE_ctype__f_t uint64_t
#define  FTYPE_size__f_t 8
#define FTYPE_ctype__f_x uint32_t
#define  FTYPE_size__f_x 4
#define FTYPE_ctype__f_y uint32_t
#define  FTYPE_size__f_y 4
#define FTYPE_ctype__f_z uint32_t
#define  FTYPE_size__f_z 4
#define FTYPE_ctype__f_C char *
#define  FTYPE_size__f_C 8
//#define FTYPE_ctype__f_Cc cbool_t
#define FTYPE_ctype__f_Cb CInfo
#define  FTYPE_size__f_Cb 8
#define FTYPE_ctype__f_Cf TInfo
#define  FTYPE_size__f_Cf 8
#define FTYPE_ctype__f_Ci cinsnp_t
#define  FTYPE_size__f_Ci 8
#define FTYPE_ctype__f_E definition_t *
#define  FTYPE_size__f_E 8
#define FTYPE_ctype__f_Q uint32_t
#define  FTYPE_size__f_Q 4
#else /* defined(BC32) */
/* Default 32-bit bytecode */
#define FTYPE_ctype__f_o uint16_t
#define  FTYPE_size__f_o 2
#define FTYPE_ctype__f_e uint16_t
#define  FTYPE_size__f_e 2
#define FTYPE_ctype__f_f uint32_t
#define  FTYPE_size__f_f 4
#define FTYPE_ctype__f_i uint16_t
#define  FTYPE_size__f_i 2
#define FTYPE_ctype__f_i_signed int16_t /* hack... */
#define FTYPE_ctype__f_l uint32_t
#define  FTYPE_size__f_l 4
#define FTYPE_ctype__f_g liveinfo_t
#define  FTYPE_size__f_g 6
//#define FTYPE_ctype__f_p char *
#define FTYPE_ctype__f_p bcp_t
#define  FTYPE_size__f_p 4
#define FTYPE_ctype__f_t uint32_t
#define  FTYPE_size__f_t 4
#define FTYPE_ctype__f_x uint16_t
#define  FTYPE_size__f_x 2
#define FTYPE_ctype__f_y uint16_t
#define  FTYPE_size__f_y 2
#define FTYPE_ctype__f_z uint16_t
#define  FTYPE_size__f_z 2
#define FTYPE_ctype__f_C char *
#define  FTYPE_size__f_C 4
//#define FTYPE_ctype__f_Cc cbool_t
#define FTYPE_ctype__f_Cb CInfo
#define  FTYPE_size__f_Cb 4
#define FTYPE_ctype__f_Cf TInfo
#define  FTYPE_size__f_Cf 4
#define FTYPE_ctype__f_Ci cinsnp_t
#define  FTYPE_size__f_Ci 4
#define FTYPE_ctype__f_E definition_t *
#define  FTYPE_size__f_E 4
#define FTYPE_ctype__f_Q uint16_t
#define  FTYPE_size__f_Q 2
#endif

typedef uint8_t ftype_typeid_t;

typedef struct fmt fmt_t;
typedef struct ftype_base ftype_base_t;
typedef struct ftype_str ftype_str_t;
typedef struct ftype_array ftype_array_t;
typedef struct ftype_basic ftype_basic_t;
typedef struct ftype_blob ftype_blob_t;
typedef struct absmachdef absmachdef_t;

struct fmt {
  intmach_t type;
  intmach_t i;
  intmach_t n;
  intmach_t value;
};
struct ftype_base {
  intmach_t type;
};
struct ftype_str {
  intmach_t type;
  intmach_t arity;
  ftype_typeid_t *args;
};
struct ftype_array {
  intmach_t type;
  intmach_t itype;
  intmach_t argtype;
};
struct ftype_basic {
  intmach_t type;
  intmach_t size;
  intmach_t smethod;
  intmach_t lmethod;
};
struct ftype_blob {
  intmach_t type;
};
struct absmachdef {
  ftype_typeid_t ftype_id_i;
  ftype_typeid_t ftype_id_o;
  ftype_base_t **ins_info;
  intmach_t ins_n;
  ftype_base_t **ftype_info;
  intmach_t ftype_n;
  intmach_t q_pad1;
  intmach_t q_pad2;
  intmach_t tagged_size;
  intmach_t size_align;
};

#define FTYPEDEF_BASIC FTYPEDEF(basic)
#define FTYPEDEF_STR FTYPEDEF(str)
#define FTYPEDEF_ARRAY FTYPEDEF(array)
#define FTYPEDEF_BLOB FTYPEDEF(blob)

/* ------------------------------------------------------------------------- */
/* Runtime type info for unboxed data */
/* TODO: use a single absmach_postdef.h file? */
/* TODO: move to absmach definition */

#define FINISH_OP 32767

/* max depth of format stack */
#define FMTSTK 4
/* types of formats: one for arrays, other for structures */
#define FMT_STR 1
#define FMT_ARRAY 2

#define FMT_PUSH_STR(ARGS, ARITY) ({ \
  fmt_s++; \
  fmt[fmt_s].type = FMT_STR; \
  fmt[fmt_s].value = (intmach_t)(ARGS); \
  fmt[fmt_s].n = (ARITY); \
  fmt[fmt_s].i = 0; \
})

#define FMT_PUSH_ARRAY(ELEM, LEN) ({ \
  fmt_s++; \
  fmt[fmt_s].type = FMT_ARRAY; \
  fmt[fmt_s].value = (intmach_t)(ELEM); \
  fmt[fmt_s].n = (LEN); \
  fmt[fmt_s].i = 0; \
})

#define FMT_POP() ({ \
  fmt_s--; \
})

#define FMT_ADVANCE(ID) ({ \
  switch (fmt[fmt_s].type) { \
  case FMT_STR: \
    (ID) = ((ftype_typeid_t *)fmt[fmt_s].value)[fmt[fmt_s].i]; \
    break; \
  case FMT_ARRAY: \
    (ID) = ((ftype_typeid_t)fmt[fmt_s].value); \
    break; \
  default: \
    goto corrupted; \
  } \
  fmt[fmt_s].i++; \
})

/* TODO: do not ALWAYS use static FTYPE here */
/* TODO: store information for f_Y, f_Z and f_g in tables */
#define FMT_LOOP(ABSMACHDEF, FMT_ROOT, ID, ARRAY_I, OP, GET_OP, EMIT_OP, GET_ARRAY_I, EMIT_ARRAY_I, EMIT_BLOB, EMIT_BASIC) ({ \
  intmach_t fmt_s; /* stack level */ \
  fmt_t fmt[FMTSTK]; \
  fmt_s = -1; \
  FMT_PUSH_STR((FMT_ROOT), 1); \
  while (1) { \
  again: \
    if (fmt_s == -1) { \
      /* level 0, get next instruction */ \
      intval_t op; \
      GET_OP; \
      FMT_PUSH_STR(FTYPE_str__args((ABSMACHDEF)->ins_info, op), FTYPE_str__arity((ABSMACHDEF)->ins_info, op)); \
      (ID) = (ABSMACHDEF)->ftype_id_o; \
      EMIT_OP; \
      goto emit; \
    } else { \
      if (fmt[fmt_s].i >= fmt[fmt_s].n) { \
	/* pop one level */ \
	FMT_POP(); \
	goto again; \
      } else { \
	FMT_ADVANCE((ID)); \
      } \
      switch (FTYPE_type((ABSMACHDEF)->ftype_info, (ID))) { \
      case FTYPEDEF_BASIC: { \
          EMIT_BASIC; \
	  goto emit; \
        } \
      case FTYPEDEF_BLOB: { \
          EMIT_BLOB; \
	  goto emit; \
        } \
      case FTYPEDEF_STR: { \
	  /* push one level */ \
	  FMT_PUSH_STR(FTYPE_str__args((ABSMACHDEF)->ftype_info, (ID)), FTYPE_str__arity((ABSMACHDEF)->ftype_info, (ID))); \
	  goto again; \
        } \
      case FTYPEDEF_ARRAY: { \
	  intval_t ARRAY_I; \
	  GET_ARRAY_I; \
	  /* push one level */ \
	  FMT_PUSH_ARRAY(FTYPE_array__argtype((ABSMACHDEF)->ftype_info, (ID)), ARRAY_I); \
	  ftype_id = FTYPE_array__itype((ABSMACHDEF)->ftype_info, (ID)); \
	  EMIT_ARRAY_I; \
	  goto emit; \
        } \
      } \
    } \
  emit: \
    {} \
  } \
})

#define FTYPE_BASIC(SIZE, SMETHOD, LMETHOD) (ftype_base_t *)(&(ftype_basic_t){.type = FTYPEDEF_BASIC, .size = (SIZE), .smethod = (SMETHOD), .lmethod = (LMETHOD)})
#define FTYPE_STR(ARITY, ARGS) (ftype_base_t *)(&(ftype_str_t){.type = FTYPEDEF_STR, .arity = (ARITY), .args = (ftype_typeid_t *)(&((ftype_typeid_t[])ARGS))})
#define FTYPE_STR0() (ftype_base_t *)(&(ftype_str_t){.type = FTYPEDEF_STR, .arity = 0, .args = NULL})
#define FTYPE_ARRAY(ITYPE, ARGTYPE) (ftype_base_t *)(&(ftype_array_t){.type = FTYPEDEF_ARRAY, .itype = (ITYPE), .argtype = (ARGTYPE)})
#define FTYPE_BLOB() (ftype_base_t *)(&(ftype_blob_t){.type = FTYPEDEF_BLOB})

#define BRACES(...) {__VA_ARGS__}

#define FTYPE_type(F,ID) (((ftype_base_t *)(F)[(intmach_t)(ID)])->type)
#define FTYPE_str__args(F,ID) (((ftype_str_t *)(F)[(intmach_t)(ID)])->args)
#define FTYPE_str__arity(F,ID) (((ftype_str_t *)(F)[(intmach_t)(ID)])->arity)
#define FTYPE_array__itype(F,ID) (((ftype_array_t *)(F)[(intmach_t)(ID)])->itype)
#define FTYPE_array__argtype(F,ID) (((ftype_array_t *)(F)[(intmach_t)(ID)])->argtype)
#define FTYPE_basic__size(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->size)
#define FTYPE_basic__smethod(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->smethod)
#define FTYPE_basic__lmethod(F,ID) (((ftype_basic_t *)(F)[(intmach_t)(ID)])->lmethod)

/* ------------------------------------------------------------------------- */
/* runtime absmach definition */

/* Align size to absmach->size_align */
#define ABSMACH__ALIGN_SIZE(ABSMACH, SIZE) (((((SIZE)+(ABSMACH)->size_align-1))/(ABSMACH)->size_align)*(ABSMACH)->size_align)

#define absmach__ftype__size(ABS, A) FTYPE_basic__size((ABS)->ftype_info, (A))
#define absmach__ftype__smethod(ABS, A) FTYPE_basic__smethod((ABS)->ftype_info, (A))

/* ------------------------------------------------------------------------- */
/* Liveinfo */

/* Note: lives in bytecode, needs precise packing */
#define LIVEINFO__SIZE (FTYPE_size(f_l) + FTYPE_size(f_i))
#define LIVEINFO__HEAP(P) BCOp((P), FTYPE_ctype(f_l), 0)
#define LIVEINFO__ARITY(P) BCOp((P), FTYPE_ctype(f_i), FTYPE_size(f_l))
#define LIVEINFO__INIT(P, HEAP, ARITY) ({ \
      char *ptr = (P); /* conform to strict-aliasing rules */	\
      LIVEINFO__HEAP(ptr) = (HEAP);				\
      LIVEINFO__ARITY(ptr) = (ARITY);				\
    })
typedef char liveinfo_t[LIVEINFO__SIZE];

/* ------------------------------------------------------------------------- */
/* Dynamic compiler */
/* TODO: generate automatically? */

#define EMITtok(T, X) { \
  BCOp(P, FTYPE_ctype(T), 0) = (X); \
  P = BCoff(P, FTYPE_size(T)); \
}
#define EMIT_Q(X) EMITtok(f_Q, (X))
#define EMIT_o(X) EMITtok(f_o, (X))
#define EMIT_e(X) EMITtok(f_e, (X))
//#define EMIT_x(X) EMITtok(f_x, Xop((X)))
#define EMIT_f(X) EMITtok(f_f, (X))
#define EMIT_t(X) EMITtok(f_t, (X))
#define EMIT_l(X) EMITtok(f_l, (X))
#define EMIT_E(X) EMITtok(f_E, (X))
#define EMIT_C(X) EMITtok(f_C, (X))
#define EMIT_i(X) EMITtok(f_i, (X))
/*
#define EMIT_Blob(BLOB) {					 \
  P = BCoff(P, copy_blob(TagpPtr(STR,(BLOB)), (tagged_t *)P)); \
}
*/

#endif /* _CIAO_ABSMACH_PREDEF_H */
