/*
 *  ciao_gluecode.h
 *
 *  Extended Ciao/C API for gluecode generation (foreign interface).
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_GLUECODE_H
#define _CIAO_GLUECODE_H

#include <ciao_prolog.h>
#include <ciao/eng.h>

#if defined(OPTIM_COMP)
#define CiaoDeclCtx(CTX) goal_descriptor_t sctx; ciao_ctx CTX;
#define CiaoInitCtx(CTX) CTX = &sctx; (CTX)->worker_registers = w;
#else
#define CiaoDeclCtx(CTX) ciao_ctx CTX;
#define CiaoInitCtx(CTX) CTX = w->misc->goal_desc_ptr;
#endif
#define CiaoSetImplicitCtx(CTX) ciao_implicit_ctx = (CTX);

#include <setjmp.h>

/* TODO: decide which exception should be raised when an exception happens in
   the C code */

extern jmp_buf ciao_gluecode_jmpbuf;
#define GLUECODE_TRY(Call) ({ \
  if (setjmp(ciao_gluecode_jmpbuf)) { \
    BUILTIN_ERROR(ERR_foreign_error, X(0), -1); \
  } else { \
    Call; \
  } \
})

ciao_term ciao_ref(ciao_ctx ctx, tagged_t x);

#endif /* _CIAO_GLUECODE_H */
