/*
 *  ciao_gluecode.h
 *
 *  Auxiliary definitions for Ciao gluecode files (foreign interface).
 *
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

#define DECL_STATE ciao_state state;
#define INIT_STATE state = w->misc->goal_desc_ptr;
/*
// note: remove this code if the previous one is correct -- jf
#define DECL_STATE goal_descriptor sstate; ciao_state state;
#define INIT_STATE state = &sstate; state->worker_registers = w;
*/
#define IMPLICIT_STATE ciao_implicit_state = state;

#include <setjmp.h>

/* TODO: decide which exception should be raised when an exception happens in
   the C code */

extern jmp_buf ciao_gluecode_jmpbuf;
#define GLUECODE_TRY(Call) \
    if (setjmp(ciao_gluecode_jmpbuf)) { \
      BUILTIN_ERROR(FOREIGN_ERROR, X(0), -1);	\
    } else { \
      Call; \
    }

ciao_term ciao_ref(ciao_state state, tagged_t x);
/* TODO: ugly, better modify the gluecode generation */
#define free ciao_free

#endif /* _CIAO_GLUECODE_H */
