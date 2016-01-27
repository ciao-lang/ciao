/*
 *  ciao_gluecode.h
 *
 *  Auxiliary definitions for Ciao gluecode files (foreign interface).
 *
 *  Copyright (C) 2016 Jose F. Morales
 *  Copyright (C) 2002 UPM-CLIP
 */

#ifndef _CIAO_GLUECODE_H
#define _CIAO_GLUECODE_H

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/eng_dbg.h>
#include <ciao/global_defs.h>
#include <ciao/threads.h>
#include <ciao/task_areas.h>

#include <ciao_prolog.h>

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
/* TODO: ugly, better modify the gluecode generation */
#define free ciao_free

#endif /* _CIAO_GLUECODE_H */
