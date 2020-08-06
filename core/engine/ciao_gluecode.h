/*
 *  ciao_gluecode.h
 *
 *  Extended Ciao/C API for gluecode generation (foreign interface).
 *
 *  Copyright (C) 2002-2020 The Ciao Development Team
 */

#ifndef _CIAO_GLUECODE_H
#define _CIAO_GLUECODE_H

#include <ciao_prolog.h>
#include <ciao/datadefs.h>

#define CiaoDeclCtx(CTX) ciao_ctx CTX;
#define CiaoInitCtx(CTX) CTX = w->misc->goal_desc_ptr;
#define CiaoSetImplicitCtx(CTX) ciao_implicit_ctx = (CTX);

#include <setjmp.h>

/* TODO: decide which exception should be raised when an exception happens in
   the C code */

extern jmp_buf ciao_gluecode_jmpbuf;
#define GLUECODE_TRY(Call) \
    if (setjmp(ciao_gluecode_jmpbuf)) { \
      BUILTIN_ERROR(FOREIGN_ERROR, X(0), -1); \
    } else { \
      Call; \
    }

ciao_term ciao_ref(ciao_ctx ctx, tagged_t x);

#endif /* _CIAO_GLUECODE_H */
