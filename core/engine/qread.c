/*
 *  qread.c
 *
 *  Reader of quickload objects.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Developer Team
 */

#include <stdlib.h> /* atoi(), atol(), atof() */
#include <stddef.h>

#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/term_support.h>

#include <ciao/qread.h>
#include <ciao/alloc.h>
#include <ciao/stacks.h>

#include <ciao/objareas.h>
#include <ciao/bignum.h>

#if defined(ALLOW_COMPRESSED_CODE)
#if defined(LINUX) || defined(Win32) || defined(BSD)
#include <string.h>
#else
#include <strings.h>
#endif
#endif

/* --------------------------------------------------------------------------- */

/* Shared? Might be; then, only one thread may read in ql's at a time.
   Since it is not a very common situation, we might as well lock ql
   reading.  In any case, access to program area should be locked, and so we
   do not loose anything. */

#define WORKSTRINGLEN (STATICMAXATOM)
static char workstring[WORKSTRINGLEN]; 

/* Read a int16_t integer */
static int qr_int16(FILE *f) {
  char *ws = workstring;
  while ((*ws++ = GETC(f))) {
#if defined(DEBUG)
    if ((ws - workstring) > WORKSTRINGLEN) {
      SERIOUS_FAULT("workstring length exceeded in qr_int16()");
    }
#endif
  }
  return atoi(workstring);
}

/* Read a int32_t integer */
static int32_t qr_int32(FILE *f)
{
  char *ws = workstring;
  while ((*ws++ = GETC(f))) {
#if defined(DEBUG)
    if ((ws - workstring) > WORKSTRINGLEN) {
      SERIOUS_FAULT("workstring length exceeded in qr_int32()");
    }
#endif
  }
    
  return atol(workstring);
}

static CFUN__PROTO(qr_large, tagged_t, FILE *f) {
  int used_length = 0;
  char *ws = Atom_Buffer;

  while ((ws[used_length++] = GETC(f))) {
    if (used_length == Atom_Buffer_Length) {
      EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
      ws = Atom_Buffer; /* New buffer */
    }
  }

  if (bn_from_string(ws,(bignum_t *)w->global_top,(bignum_t *)Heap_End,GetSmall(current_radix))) {
    SERIOUS_FAULT("$qload: miscalculated heap usage");
  } else {
    tagged_t *h = w->global_top;
    int ar = LargeArity(h[0]);
      
    if (ar==2 && IntIsSmall((intmach_t)h[1])) {
      return MakeSmall(h[1]);
    } else {
      w->global_top += ar+1;
      h[ar] = h[0];
      return Tag(STR,h);
    }
  }
}

static flt64_t qr_flt64(FILE *f) {
  char *ws = workstring;

  while((*ws++ = GETC(f))) {
#if defined(DEBUG)
    if ((ws - workstring) > WORKSTRINGLEN) {
      SERIOUS_FAULT("workstring length exceeded in qr_flt64()");
    }
#endif
  }
  return atof(workstring);
}

static CFUN__PROTO(qr_string, char *, FILE *f) {
  int used_length = 0;
  char *ws = Atom_Buffer;   /* Try to avoid indirection through WAM */

  while ((ws[used_length++] = GETC(f))) {
    if (used_length == Atom_Buffer_Length) {
      EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
      ws = Atom_Buffer;
    }
  }
  return ws;
}

/* --------------------------------------------------------------------------- */
/* Enable bytecode rewriting on load (experimental).
 *
 * This enables changes in size of internal data structures from same
 * bytecode.
 */

#if defined(DEBUG)
#define DUMP_INSTR 1
#endif

#include <ciao/io_basic.h> /* display_term() */

#if tagged__size == 64
#define PTRdigits "16"
#else
#define PTRdigits "8"
#endif

#define FuncName(Func) (SetArity((Func)->printname, 0))
//#define FuncArity(Func) (Arity((Func)->printname))
#define FuncArity(Func) ((Func)->arity)

/* (for debugging) */
#define TRACE_REWRITE(_)
//#define TRACE_REWRITE(X) X

#if defined(BC64)
/* Rewrite bytecode and patch operands */
#define PATCH_BC32 1
#define USE_REWRITE_BYTECODE 1
#endif

#if defined(USE_REWRITE_BYTECODE) || defined(DUMP_INSTR)
#include <ciao/absmachdef.h>
#include <ciao/instrdefs.h>
#define FTYPE_name(ID) ftype_name[(intmach_t)(ID)]
char *ftype_name[] = {
  [(intmach_t)FTYPE_id(f_o)]="o",
  [(intmach_t)FTYPE_id(f_e)]="e",
  [(intmach_t)FTYPE_id(f_f)]="f",
  [(intmach_t)FTYPE_id(f_i)]="i",
  [(intmach_t)FTYPE_id(f_l)]="l",
  [(intmach_t)FTYPE_id(f_g)]="g",
  [(intmach_t)FTYPE_id(f_p)]="p",
  [(intmach_t)FTYPE_id(f_t)]="t",
  [(intmach_t)FTYPE_id(f_x)]="x",
  [(intmach_t)FTYPE_id(f_y)]="y",
  [(intmach_t)FTYPE_id(f_z)]="z",
  [(intmach_t)FTYPE_id(f_C)]="C",
  [(intmach_t)FTYPE_id(f_E)]="E",
  [(intmach_t)FTYPE_id(f_Q)]="Q",
  [(intmach_t)FTYPE_id(f_Y)]="Y",
  [(intmach_t)FTYPE_id(f_Z)]="Z",
  [(intmach_t)FTYPE_id(f_b)]="b"
};
#endif

#if defined(USE_REWRITE_BYTECODE) || defined(DUMP_INSTR)
/* todo[ts]: make optional (only when alignment is required) */
void check_align(bcp_t p, intmach_t size) {
  /* TODO: move to some code verifier... this indicates a compiler bug! */
  if (((intp_t)p) % size != 0) { 
    fprintf(stderr, "Panic: corrupted bytecode (unaligned field)\n");
    exit(1);
  }
}
#endif

#if defined(USE_REWRITE_BYTECODE)
CFUN__PROTO(rewrite_instr, bcp_t, bcp_t p, bcp_t begin);

CVOID__PROTO(bytecode_rewrite, bcp_t begin, bcp_t end) {
  bcp_t p;

  p = begin;
#if PATCH_BC32
  {
    intmach_t v;
    v = BCOp(p, FTYPE_ctype(f_i), 0);
    BCOp(p, FTYPE_ctype(f_i), 0) = v * BC_SCALE;
  }
#endif
  p = BCoff(p, FTYPE_size(f_i));

  TRACE_REWRITE({
      fprintf(stderr, "  bytecode - 0x%" PRIxm " bytes - start at 0x%" PRIxm ", det at 0x%" PRIxm "\n",
	      (intmach_t)((char *)end - (char *)p),
	      (uintmach_t)FTYPE_size(f_i),
	      (uintmach_t)BCOp(begin, FTYPE_ctype(f_i), 0));
    });
  while (p != NULL && p < end) {
    p = CFUN__EVAL(rewrite_instr, p, begin);
  }
}

void bytecode_rewrite__z(intmach_t j) {
  if (j&1) {
    TRACE_REWRITE({ fprintf(stderr, "(unsafe)%ld", (long)Yinv(j+1)); });
  } else {
    TRACE_REWRITE({ fprintf(stderr, "%ld", (long)Yinv(j)); });
  }
}

/* For hardwired constants from emulator_data:x_and_y_offsets/2 */
#define BC32_WToX0 37
#define BC32_EToY0 2
#define BC32_Xinv(X) (((X)/4)-BC32_WToX0)
#define BC32_Yinv(X) (((X)/4)-BC32_EToY0)

CFUN__PROTO(rewrite_instr, bcp_t, bcp_t p, bcp_t begin) {
  TRACE_REWRITE(const char *name);
  const ftype_typeid_t *format;
  intmach_t i, j, k;
  intmach_t arity;
  FTYPE_ctype(f_o) opcode;
  
  TRACE_REWRITE({ fprintf(stderr, "  %0" PTRdigits PRIxm ": ", (uintmach_t)((char *)p - (char *)begin)); });
  check_align(p, FTYPE_size(f_o));
  opcode = BCOp(p, FTYPE_ctype(f_o), 0);

  p = BCoff(p, FTYPE_size(f_o));
  if (opcode >= INS_OPCOUNT) {
    fprintf(stderr, "Panic: corrupted bytecode (invalid opcode 0x%x)\n", (int)opcode);
    CFUN__PROCEED(NULL);
  }
  
  TRACE_REWRITE({ name = ins_name[opcode]; });
  format = FTYPE_str__args(abscurr.ins_info, opcode);
  arity = FTYPE_str__arity(abscurr.ins_info, opcode);
  
  TRACE_REWRITE({ fprintf(stderr, "%s", name); });
  
  for (i = 0; i < arity; i++) {
    TRACE_REWRITE({ fprintf(stderr, " %s(", FTYPE_name(format[i])); });
    switch (format[i]) {
    case FTYPE_id(f_e):
      check_align(p, FTYPE_size(f_e));
#if PATCH_BC32
      {
	intmach_t e;
	e = BCOp(p, FTYPE_ctype(f_e), 0);
	BCOp(p, FTYPE_ctype(f_e), 0) = Yop(BC32_Yinv(e));
      }
#endif
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_e), 0)); });
      p = BCoff(p, FTYPE_size(f_e));
      break;
    case FTYPE_id(f_f):
      check_align(p, FTYPE_size(f_f));
      TRACE_REWRITE({
	  tagged_t t;
	  t = BCOp(p, FTYPE_ctype(f_f), 0);
	  fprintf(stderr, "%s/%ld", GetString(t), (long)Arity(t));
	});
      p = BCoff(p, FTYPE_size(f_f));
      break;
    case FTYPE_id(f_i):
      check_align(p, FTYPE_size(f_i));
#if PATCH_BC32
      if (opcode == BRANCH) { /* the operand is an offset */
	intmach_t v;
	v = BCOp(p, FTYPE_ctype(f_i), 0);
	BCOp(p, FTYPE_ctype(f_i), 0) = v * BC_SCALE;
      }
#endif
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0)); });
      p = BCoff(p, FTYPE_size(f_i));
      break;
    case FTYPE_id(f_l):
      check_align(p, FTYPE_size(f_l));
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)(intmach_t)BCOp(p, FTYPE_ctype(f_l), 0)); });
      p = BCoff(p, FTYPE_size(f_l));
      break;
    case FTYPE_id(f_g):
      /* TODO: expand in two sub-formats! use compound definitions! */
      TRACE_REWRITE({ fprintf(stderr, "h:"); });
      check_align(p, FTYPE_size(f_l)); 
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_l), 0)); });
      p = BCoff(p, FTYPE_size(f_l));
      TRACE_REWRITE({ fprintf(stderr, ",a:"); });
      check_align(p, FTYPE_size(f_i));
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0)); });
      p = BCoff(p, FTYPE_size(f_i));
      break;
    case FTYPE_id(f_p):
      check_align(p, FTYPE_size(f_p));
      TRACE_REWRITE({ fprintf(stderr, "0x%lx", (long)((char *)BCOp(p, FTYPE_ctype(f_p), 0) - (char *)begin)); });
      p = BCoff(p, FTYPE_size(f_p));
      break;
    case FTYPE_id(f_t):
      check_align(p, FTYPE_size(f_t));
      TRACE_REWRITE({
	  tagged_t t;
	  t = BCOp(p, FTYPE_ctype(f_t), 0);
	  //CVOID__CALL(display_term, t, Output_Stream_Ptr, TRUE);
	  /* (not relocated yet!) */
	  fprintf(stderr, "%" PRIxm, t);
	});
      p = BCoff(p, FTYPE_size(f_t));
      break;
    case FTYPE_id(f_x):
      check_align(p, FTYPE_size(f_x));
#if PATCH_BC32
      {
	intmach_t x;
	x = BCOp(p, FTYPE_ctype(f_x), 0);
	BCOp(p, FTYPE_ctype(f_x), 0) = Xop(BC32_Xinv(x));
      }
#endif
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)Xinv(BCOp(p, FTYPE_ctype(f_x), 0))); });
      p = BCoff(p, FTYPE_size(f_x));
      break;
    case FTYPE_id(f_y):
      check_align(p, FTYPE_size(f_y));
#if PATCH_BC32
      {
	intmach_t y;
	y = BCOp(p, FTYPE_ctype(f_y), 0);
	BCOp(p, FTYPE_ctype(f_y), 0) = Yop(BC32_Yinv(y));
      }
#endif
      TRACE_REWRITE({ fprintf(stderr, "%ld", (long)Yinv(BCOp(p, FTYPE_ctype(f_y), 0))); });
      p = BCoff(p, FTYPE_size(f_y));
      break;
    case FTYPE_id(f_z):
      check_align(p, FTYPE_size(f_z));
#if PATCH_BC32
      {
	intmach_t z;
	z = BCOp(p, FTYPE_ctype(f_z), 0);
	if (z & 1) {
	  z = Yop(BC32_Yinv(z + 1)) - 1;
	} else {
	  z = Yop(BC32_Yinv(z));
	}
	BCOp(p, FTYPE_ctype(f_z), 0) = z;
      }
#endif
      bytecode_rewrite__z(BCOp(p, FTYPE_ctype(f_z), 0));
      p = BCoff(p, FTYPE_size(f_z));
      break;
    case FTYPE_id(f_C):
      check_align(p, FTYPE_size(f_C));
      TRACE_REWRITE({
	  char *ptr;
	  ptr = BCOp(p, FTYPE_ctype(f_C), 0);
	  fprintf(stderr, "%p", ptr);
	});
      p = BCoff(p, FTYPE_size(f_C));
      break;
    case FTYPE_id(f_E):
      check_align(p, FTYPE_size(f_E));
      TRACE_REWRITE({
	  definition_t *d;
	  d = BCOp(p, FTYPE_ctype(f_E), 0);
	  //fprintf(stderr, "%s/%ld", GetString(FuncName(d)), (long)FuncArity(d));
	  /* (not relocated yet) */
	  fprintf(stderr, "%p", d);
	});
      p = BCoff(p, FTYPE_size(f_E));
      break;
    case FTYPE_id(f_Q):
      check_align(p, FTYPE_size(f_Q));
      p = BCoff(p, FTYPE_size(f_Q)); break;
    case FTYPE_id(f_Y):
      check_align(p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	TRACE_REWRITE({ if (j > 0) fprintf(stderr, " "); });
	check_align(p, FTYPE_size(f_y));
#if PATCH_BC32
	{
	  intmach_t y;
	  y = BCOp(p, FTYPE_ctype(f_y), 0);
	  BCOp(p, FTYPE_ctype(f_y), 0) = Yop(BC32_Yinv(y));
	}
#endif
	TRACE_REWRITE({ fprintf(stderr, "%ld", (long)Yinv(BCOp(p, FTYPE_ctype(f_y), 0))); });
	p = BCoff(p, FTYPE_size(f_y));
      }
      break;
    case FTYPE_id(f_Z):
      check_align(p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	TRACE_REWRITE({ if (j > 0) fprintf(stderr, " "); });
	check_align(p, FTYPE_size(f_z));
#if PATCH_BC32
	{
	  intmach_t z;
	  z = BCOp(p, FTYPE_ctype(f_z), 0);
	  if (z & 1) {
	    z = Yop(BC32_Yinv(z + 1)) - 1;
	  } else {
	    z = Yop(BC32_Yinv(z));
	  }
	  BCOp(p, FTYPE_ctype(f_z), 0) = z;
	}
#endif
	TRACE_REWRITE({ bytecode_rewrite__z(BCOp(p, FTYPE_ctype(f_z), 0)); });
	p = BCoff(p, FTYPE_size(f_z));
      }
      break;
    case FTYPE_id(f_b):
      /* todo[ts]: change if more blob types are added */
      check_align(p, FTYPE_size(f_i));
      TRACE_REWRITE({
	tagged_t *h = w->global_top;
	tagged_t t;
	t = MakeBlob((tagged_t *)p);
	CVOID__CALL(display_term, t, Output_Stream_Ptr, TRUE);
	w->global_top = h;
      });
      //      p = BCoff(p, BlobFunctorSizeAligned(*(tagged_t *)p)+sizeof(functor_t));
      p = BCoff(p, BlobFunctorSizeAligned((tagged_t *)p));
      break;
    default:
      TRACE_REWRITE({ fprintf(stderr, "?"); });
      goto end;
    }
    TRACE_REWRITE({ fprintf(stderr, ")"); });
  }
 end:
  TRACE_REWRITE({ fprintf(stderr, "\n"); });
  CFUN__PROCEED(p);
}
#endif

#if defined(DUMP_INSTR)
void dump_instr_z(intmach_t j) {
  if (j&1) {
    fprintf(stderr, "(unsafe)%ld", (long)Yinv(j+1));
  } else {
    fprintf(stderr, "%ld", (long)Yinv(j));
  }
}

CVOID__PROTO(dump_instr, bcp_t p) {
  const char *name;
  const ftype_typeid_t *format;
  definition_t *d;
  tagged_t t;
  intmach_t i, j, k;
  intmach_t arity;
  FTYPE_ctype(f_o) opcode;
  
  fprintf(stderr, "  %0" PTRdigits PRIxm ": ", (intmach_t)p);
  check_align(p, FTYPE_size(f_o));
  opcode = BCOp(p, FTYPE_ctype(f_o), 0);

  p = BCoff(p, FTYPE_size(f_o));
  if (opcode >= INS_OPCOUNT) {
    fprintf(stderr, "Panic: corrupted bytecode (invalid opcode 0x%x)\n", (int)opcode);
    return;
  }
  
  name = ins_name[opcode];
  format = FTYPE_str__args(abscurr.ins_info, opcode);
  arity = FTYPE_str__arity(abscurr.ins_info, opcode);
  
  fprintf(stderr, "%s", name);
  
  for (i = 0; i < arity; i++) {
    fprintf(stderr, " %s(", FTYPE_name(format[i]));
    switch (format[i]) {
    case FTYPE_id(f_e):
      check_align(p, FTYPE_size(f_e));
      fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_e), 0));
      p = BCoff(p, FTYPE_size(f_e));
      break;
    case FTYPE_id(f_f):
      check_align(p, FTYPE_size(f_f));
      t = BCOp(p, FTYPE_ctype(f_f), 0);
      fprintf(stderr, "%s/%ld", GetString(t), (long)Arity(t));
      p = BCoff(p, FTYPE_size(f_f));
      break;
    case FTYPE_id(f_i):
      check_align(p, FTYPE_size(f_i));
      fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0));
      p = BCoff(p, FTYPE_size(f_i));
      break;
    case FTYPE_id(f_l):
      check_align(p, FTYPE_size(f_l));
      fprintf(stderr, "%ld", (long)(intmach_t)BCOp(p, FTYPE_ctype(f_l), 0));
      p = BCoff(p, FTYPE_size(f_l));
      break;
    case FTYPE_id(f_g):
      /* TODO: expand in two sub-formats! use compound definitions! */
      fprintf(stderr, "h:");
      check_align(p, FTYPE_size(f_l)); 
      fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_l), 0));
      p = BCoff(p, FTYPE_size(f_l));
      fprintf(stderr, ",a:");
      check_align(p, FTYPE_size(f_i));
      fprintf(stderr, "%ld", (long)BCOp(p, FTYPE_ctype(f_i), 0));
      p = BCoff(p, FTYPE_size(f_i));
      break;
    case FTYPE_id(f_p):
      check_align(p, FTYPE_size(f_p));
      fprintf(stderr, "0x%lx", (long)BCOp(p, FTYPE_ctype(f_p), 0));
      p = BCoff(p, FTYPE_size(f_p));
      break;
    case FTYPE_id(f_t):
      check_align(p, FTYPE_size(f_t));
      t = BCOp(p, FTYPE_ctype(f_t), 0);
      CVOID__CALL(display_term, t, Output_Stream_Ptr, TRUE);
      /* (if not relocated yet:) */
      // fprintf(stderr, "%" PRIxm, t);
      p = BCoff(p, FTYPE_size(f_t));
      break;
    case FTYPE_id(f_x):
      check_align(p, FTYPE_size(f_x));
      fprintf(stderr, "%ld", (long)Xinv(BCOp(p, FTYPE_ctype(f_x), 0)));
      p = BCoff(p, FTYPE_size(f_x));
      break;
    case FTYPE_id(f_y):
      check_align(p, FTYPE_size(f_y));
      fprintf(stderr, "%ld", (long)Yinv(BCOp(p, FTYPE_ctype(f_y), 0)));
      p = BCoff(p, FTYPE_size(f_y));
      break;
    case FTYPE_id(f_z):
      check_align(p, FTYPE_size(f_z));
      dump_instr_z(BCOp(p, FTYPE_ctype(f_z), 0));
      p = BCoff(p, FTYPE_size(f_z));
      break;
    case FTYPE_id(f_C):
      check_align(p, FTYPE_size(f_C));
      {
	char *ptr;
	ptr = BCOp(p, FTYPE_ctype(f_C), 0);
	fprintf(stderr, "%p", ptr);
      }
      p = BCoff(p, FTYPE_size(f_C));
      break;
    case FTYPE_id(f_E):
      check_align(p, FTYPE_size(f_E));
      d = BCOp(p, FTYPE_ctype(f_E), 0);
      fprintf(stderr, "%s/%ld", GetString(FuncName(d)), (long)FuncArity(d));
      /* (if not relocated yet:) */
      //fprintf(stderr, "%p", d);
      p = BCoff(p, FTYPE_size(f_E));
      break;
    case FTYPE_id(f_Q):
      check_align(p, FTYPE_size(f_Q));
      p = BCoff(p, FTYPE_size(f_Q)); break;
    case FTYPE_id(f_Y):
      check_align(p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	if (j > 0) fprintf(stderr, " ");
	check_align(p, FTYPE_size(f_y));
	fprintf(stderr, "%ld", (long)Yinv(BCOp(p, FTYPE_ctype(f_y), 0)));
	p = BCoff(p, FTYPE_size(f_y));
      }
      break;
    case FTYPE_id(f_Z):
      check_align(p, FTYPE_size(f_i));
      k = BCOp(p, FTYPE_ctype(f_i), 0); p = BCoff(p, FTYPE_size(f_i));
      for (j = 0; j < k; j++) {
	if (j > 0) fprintf(stderr, " ");
	check_align(p, FTYPE_size(f_z));
	dump_instr_z(BCOp(p, FTYPE_ctype(f_z), 0));
	p = BCoff(p, FTYPE_size(f_z));
      }
      break;
    case FTYPE_id(f_b):
      /* todo[ts]: change if more blob types are added */
      check_align(p, FTYPE_size(f_i));
      {
	tagged_t *h = w->global_top;
	tagged_t t;
	t = MakeBlob((tagged_t *)p);
	CVOID__CALL(display_term, t, Output_Stream_Ptr, TRUE);
	w->global_top = h;
      }
      fprintf(stderr, "%llx\n", *(tagged_t *)p);
      p = BCoff(p, BlobFunctorSizeAligned((tagged_t *)p));
      break;
    default:
      fprintf(stderr, "?");
      goto end;
    }
    fprintf(stderr, ")");
  }
 end:
  fprintf(stderr, "\n");
}
#endif

/* --------------------------------------------------------------------------- */

CVOID__PROTO(getbytecode32, FILE *f,
	     bcp_t P,
	     int length) {
  char c;
  bcp_t begin = P;

  while ((c=GETC(f))) {
    switch (c) {
    case 'G': {
      int i = 0;
      char *ws = Atom_Buffer;
      tagged_t t, *h;
      
      while ((c = ws[i++] = GETC(f))) {
        if (i == Atom_Buffer_Length) {
	  EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
	  ws = Atom_Buffer;
        }
      }

      /* TODO: This is can be improved. 

         We ensure that there is at least (length/sizeof(tagged_t)+4)
	 available words in the heap. We load the number into the
	 heap, copy it to the bytecode, and then move the heap pointer
	 back.
      */
      /* TODO: Use Numstack for temporaries? */
      {
	int arity = 2;
	int cells = (length / sizeof(tagged_t)) + 4 + arity;
	if (HeapDifference(w->global_top,Heap_End)<cells) {
	  explicit_heap_overflow(Arg,cells,5);
	}
      }
      h = w->global_top;
//#if BC_SCALE==2
//      fprintf(stderr, "trace: reading number %s\n", Atom_Buffer);
//#endif
      if (!string_to_number(w, Atom_Buffer, 10, &t, 5)) {
	SERIOUS_FAULT("$qread: wrong number!");
      }
      int sz;
#if BC_SCALE==2
      sz = compile_large_bc32(t, P);
#else
      sz = compile_large(t, P);
#endif
      P = BCoff(P, sz);
//#if BC_SCALE==2
//      fprintf(stderr, "trace: sz=%d\n", sz);
//#endif
      w->global_top = h; /* TODO: does not move! */
      break;
    }
      
    case '+': {
      char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode32() [+]")
#endif
        ;

      EMIT_l(atol(workstring));
      break;
    }
      
    case 'C': {
      char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode32() [C]")
#endif
        ;

      EMIT_C((char *)builtintab[atoi(workstring)]);
      break;
    }
      
    default: {
      char *ws = workstring;
      
      *ws++ = c;
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
         SERIOUS_FAULT("workstring length exceeded in getbytecode32() [default]")
#endif
        ;
      /* TODO: assumes that f_o,f_x,f_y,etc. have the same size */
      EMIT_o(atoi(workstring));
    }
    }
  }

  ptrdiff_t truesize = (char *)P - (char *)begin;
  if (truesize > length) {
    SERIOUS_FAULT("bug: memory overrun in getbytecode32()");
  }

#if defined(USE_REWRITE_BYTECODE)
  CVOID__CALL(bytecode_rewrite, begin, P);
#endif
}

/* --------------------------------------------------------------------------- */
/* Definitions for quickload bytecode format. */

/* Control codes to be detected */ 

#define ISCOMPRESSED 12 /* Ctrl-L */
#define ISSCRIPT 35 /* # */

/* Quick load instructions: 'A'..'O' */

#define ENSURE_SPACE 64
#define LOAD_ATOM 65
#define LOAD_FUNCTOR 66
#define LOAD_NUMBER_S 67
#define LOAD_NUMBER_L 68
#define LOAD_NUMBER_F 69

#define LOAD_VARIABLE 70
#define LOAD_NIL 71
#define LOAD_LIST 72
#define LOAD_TUPLE 73
#define LOAD_ARGUMENT 74
#define LOAD_DBNODE 75

#define RETURN 76

#define RELOC_POINTER 77
#define RELOC_EMUL_ENTRY 78
#define RELOC_COUNTER 79

/* --------------------------------------------------------------------------- */
/* local declarations */

static CVOID__PROTO(load_dbnode32, int Li, FILE *f, int codelength, int counter_cnt);

#define QLARRAY(I) qlarray[I]
#define QLCHECK(I) { if ((I)+qloffset >= qllimit) expand_qload(); }

typedef struct qlinfo_ qlinfo_t;
struct qlinfo_ {
  qlinfo_t *next;
  tagged_t *qlarray;
  int qloffset;
  int qllimit;
};

#if defined(BUFFERED_PO)

#define QLBFSIZE 1024
int qlbuffidx, qlbuffend;
unsigned char qlbuff[QLBFSIZE];

/*
  Use an internal buffer of QLBFSIZE chars to store the contents of the .po
  files being read in.  When the buffer is full, we fill it again at once;
  the previous method was calling getc() once and again.  Preliminary tests
  show this method to be between 3 times (for dynamic executables, as
  ciaosh) to 5 times (for static stuff, as ciaoc) faster.  
*/

int buffered_input(FILE *stream) {
  if (qlbuffidx == qlbuffend) {
    if (qlbuffend < QLBFSIZE) return EOF;
    if (!(qlbuffend = 
          fread(qlbuff, sizeof(unsigned char), QLBFSIZE, stream)))
        return EOF;                /* Could not read after buffer emptied */
    qlbuffidx = 0;
  } 
  return (int)qlbuff[qlbuffidx++];
}

#endif

#if defined(ALLOW_COMPRESSED_CODE)

int  Last, PrefixSize, Size[4096], Buffer[3840], BufferP;
char *Dict[4096], *First, Vault[200000], compressed, remaining;

/* Allows the load of compressed bytecode. The compression algorithm used is
   based on Lempel-Ziv, reducing bytecode size to about 1/3 and varying load
   time to about 125% when using buffered input and to less than 60% when using
   unbuffered one (note: even thorough this, the load of compressed bytecode
   is faster when buffered input is enabled). Files containing compressed
   bytecode are recognized because they begin by ^L the bytecode sequence. */

#define InLZ(n,n2) { for (; size < n; size += 8) \
	                  i += GETC_LZ(stream)*(1<<size); \
                     Buffer[--BufferP] = i % n2; \
      	             i /= n2; \
                     size -= n; }    
      
int readLZ(FILE *stream) {
  int  i = 0;
  char size = 0;

  if (compressed) {
    if (remaining)
      return (int)First[PrefixSize-remaining--];  
    if (!BufferP) { 
      BufferP = 3840;
      while (BufferP > 3584)
        InLZ(9,512);
      while (BufferP > 3072)
        InLZ(10,1024);	  
      while (BufferP > 2048)
        InLZ(11,2048);
      while (BufferP) 
        InLZ(12,4096);
      BufferP = 3840; 
    }
    if ((i=Buffer[--BufferP]) == 256) 
      return EOF;
    if (Last == 4095) 
      (First = &Vault[Last = 256])[0] = i;
    else { 
      Size[++Last] = PrefixSize+1;            
      (Dict[Last] = First)[PrefixSize] = Dict[i][0];
      (void)memmove(First += Size[Last],Dict[i],Size[i]);
    }
    remaining = (PrefixSize=Size[i])-1;
    return (int)First[0];
  }
  else return GETC_LZ(stream);
}
#endif

tagged_t *qlarray=NULL;                   /* Shared, but with locked access */
int qloffset=0, qllimit=0;                /* Shared, locked access */
qlinfo_t *qlstack=NULL;                   /* Shared, locked access */

CBOOL__PROTO(push_qlinfo) {
  qlinfo_t *p = checkalloc_TYPE(qlinfo_t);
  
#if defined(BUFFERED_PO)
  qlbuffidx = QLBFSIZE; /* Empty */
  qlbuffend = QLBFSIZE; 
#endif  
#if defined(ALLOW_COMPRESSED_CODE)
  compressed = 0;
#endif

  p->next = qlstack;
  qlstack = p;
  p->qllimit = qllimit;
  p->qloffset = qloffset;
  p->qlarray = qlarray;
  qllimit = QLOADSIZE;
  qloffset = qllimit>>1;
  qlarray = checkalloc_ARRAY(tagged_t, qllimit) + qloffset;
  
  return TRUE;
}

CBOOL__PROTO(pop_qlinfo)
{
  qlinfo_t *p = qlstack;

  qlstack = p->next;
  checkdealloc_ARRAY(tagged_t, qllimit, qlarray-qloffset);
  qlarray = p->qlarray;
  qllimit = p->qllimit;
  qloffset = p->qloffset;
  checkdealloc_TYPE(qlinfo_t, p);

  return TRUE;
}

void expand_qload(void) {
  int i;
  int o = qloffset;
  /*intmach_t prog_mem = mem_prog_count;*/ /* preserve over reallocs */
  
  qlarray = checkrealloc_ARRAY(tagged_t,
			       qllimit,
			       qllimit*2,
			       qlarray-o);
  for (i=qllimit; i>0;) {
    --i;
    qlarray[i+o]=qlarray[i];
  }
  qllimit<<=1;
  qloffset<<=1;
  qlarray+=qloffset;
  /*mem_prog_count = prog_mem;*/
}

emul_info_t *latest_bytecode;               /* Shared, locked access */
int latest_bytecode_size;                   /* Shared, locked access */

static CVOID__PROTO(load_dbnode32,
		    int Li,
		    FILE *f,
		    int codelength,
		    int counter_cnt) {
  emul_info_t *db;
  /*int i;*/

  codelength *= BC_SCALE;

#if defined(GAUGE)
  /* TODO: size was aligned to 4 -- not needed now? JFMC */
  checkalloc_FLEXIBLE_S(emul_info_t,
			objsize,
			char,
			(codelength + counter_cnt*sizeof(intmach_t)),
			db);
#else
  checkalloc_FLEXIBLE_S(emul_info_t,
			objsize,
			char,
			codelength,
			db);
#endif

  getbytecode32(Arg,f,(bcp_t)db->emulcode,codelength);
  latest_bytecode = db;  
  latest_bytecode_size = codelength;
  db->next.ptr = NULL;
  db->subdefs = NULL;
#if defined(GAUGE)
  db->counters = (intmach_t *)((char *)db+db->objsize)-counter_cnt;
  for (i=0; i<counter_cnt; i++)
    db->counters[i] = 0;
#endif
  QLARRAY(Li) = PointerToTerm(db);
}

void reloc_pointer(int Li, intmach_t Label) {
  void *pos;

  Label *= BC_SCALE;
  do {
    pos = BCoff(latest_bytecode->emulcode, Label);
    Label = (*(intmach_t *)pos) * BC_SCALE;
    *(intmach_t *)pos = QLARRAY(Li);
  } while(Label != 0);
}

/* Patch up the counter indexes in the BUMP_COUNTER instructions. */
void reloc_counter(intmach_t Label) {
#if defined(GAUGE)
  void *pos;
  int counter_cnt = NumberOfCounters(latest_bytecode);
  intmach_t *current_counter = latest_bytecode->counters + counter_cnt;

  Label *= BC_SCALE;
  /*
    Counters are linked together in REVERSE order.  Since
    make_bytecode_object assigns counters in order of occurrence,
    we must do the same here.
   */
  while (Label) {
    pos = BCoff(latest_bytecode->emulcode, Label);
    Label = (*(intmach_t *)pos) * BC_SCALE;
    *(intmach_t **)pos = --current_counter;
    --counter_cnt;
  }
  if (counter_cnt != 2) {
    SERIOUS_FAULT("$qload: counter counts don't match");
  }
#endif
}

void reloc_emul_entry(int Li, intmach_t Label) {
  void *pos;
  definition_t *addr = parse_definition(QLARRAY(Li));

  Label *= BC_SCALE;
  do {
    pos = BCoff(latest_bytecode->emulcode, Label);
    Label = (*(intmach_t *)pos) * BC_SCALE;
    *(definition_t **)pos = addr;
  } while(Label != 0);
}

/* Read 32-bit .po files (fits into 64-bit bytecode if needed) */
CBOOL__PROTO(qread1, 
	     FILE *qfile,
	     tagged_t *rungoal) {
  int Li = 0, Lj = 0;
  tagged_t *h = w->global_top;
  int pad;
  int c = GETC(qfile);
  
  while (c!=EOF) {
    // fprintf(stderr, "qr:%d\n", c);
    switch (c) {
#if defined(ALLOW_COMPRESSED_CODE)
    case ISCOMPRESSED:
      compressed = 1;
      Last = 4095;
      remaining = BufferP = 0;
      { 
	int i = 0;
	for(;i < 257; Size[i++] = 1) (Dict[i]=&Vault[i])[0]= i%256;
      }
      break;
#endif
    case ISSCRIPT:
      {
	int chr;	    
	do chr = GETC(qfile);
	while ((chr != EOF) && (chr != 12));
      }
      break;
    case ENSURE_SPACE:
      if (HeapDifference(h,Heap_End) < (pad=qr_int32(qfile))){
	w->global_top = h;
	explicit_heap_overflow(Arg,pad,2);
	h = w->global_top;
      }
      break;
    case LOAD_ATOM:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      QLARRAY(Li) = init_atom_check(qr_string(Arg, qfile));
      break;
    case LOAD_FUNCTOR:
      Li = qr_int16(qfile);
      Lj = qr_int16(qfile);
      QLCHECK(Li);
      QLCHECK(Lj);
      QLARRAY(Li) = SetArity(QLARRAY(Lj),qr_int16(qfile));
      break;
    case LOAD_NUMBER_S: /* NOTE: may be signed! see wamql.pl */
      Li = qr_int16(qfile);
      QLCHECK(Li);
      QLARRAY(Li) = MakeSmall(qr_int16(qfile));
      break;
    case LOAD_NUMBER_L:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      w->global_top = h;
      QLARRAY(Li) = qr_large(Arg,qfile);
      h = w->global_top;
      break;
    case LOAD_NUMBER_F:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      w->global_top = h;
      QLARRAY(Li) = MakeFloat(Arg,qr_flt64(qfile));
      h = w->global_top;
      break;
    case LOAD_VARIABLE:
      Li = qr_int16(qfile);
      QLCHECK(Li); 
      LoadHVA(QLARRAY(Li),h);
      break;
    case LOAD_NIL:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      QLARRAY(Li) = atom_nil;
      break;
    case LOAD_LIST:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      QLARRAY(Li) = Tag(LST,h);
      break;
    case LOAD_TUPLE:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      QLARRAY(Li) = Tag(STR,h);
      break;
    case LOAD_ARGUMENT:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      HeapPush(h,QLARRAY(Li));
      break;
    case LOAD_DBNODE:
      Li = qr_int16(qfile);
      QLCHECK(Li);
      Lj = qr_int16(qfile);
      load_dbnode32(Arg,Li,qfile,Lj,qr_int16(qfile));
      break;
    case RETURN:
      Li = qr_int16(qfile);
      *rungoal = QLARRAY(Li);
      w->global_top = h;
      return TRUE;
    case RELOC_POINTER:
      Li = qr_int16(qfile);
      reloc_pointer(Li,qr_int32(qfile));
      break;
    case RELOC_EMUL_ENTRY:
      Li = qr_int16(qfile);
      reloc_emul_entry(Li,qr_int32(qfile));
      break;
    case RELOC_COUNTER:
      reloc_counter(qr_int32(qfile));
      break;
    }
    c=GETC(qfile);
  }
  w->global_top = h;
  return FALSE;
}

CBOOL__PROTO(prolog_qread) {
  stream_node_t *s;
  tagged_t goal;

  if ((s = stream_to_ptr(X(0), 'r')) != NULL) {
    if (qread1(Arg,s->streamfile, &goal)) {
      return cunify(Arg,goal,X(1));
    }
  }

  Unify_constant(MakeSmall(-1),X(1));
  return TRUE;
}

