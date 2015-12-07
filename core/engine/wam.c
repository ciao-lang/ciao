/*
 *  wam.c
 *
 *  Emulator kernel.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include <ciao/os_signal.h>
#include <ciao/threads.h>
#include <ciao/locks_prim.h>
#include <ciao/datadefs.h>
#include <ciao/configure.h>
#include <ciao/support_macros.h>
#include <ciao/instrdefs.h>
#include <ciao/wam_macros.h>
#include <ciao/task_areas.h>
#include <ciao/wamsupport.h>

#include <ciao/attr.h>
#include <ciao/eng_dbg.h>
#include <ciao/initial.h>
#include <ciao/io_basic.h>
#include <ciao/interrupt.h>
#include <ciao/start.h>
#include <ciao/misc.h>
#include <ciao/nondet.h>
#include <ciao/objareas.h>
#include <ciao/stacks.h>
#include <ciao/support.h>
#include <ciao/term_support.h>
#include <ciao/wam.h>
#include <ciao/locks.h>
#include <ciao/timing.h>
#include <ciao/profile_hooks.h>
#include <ciao/tabling.h>

/* private function declarations */

/* Attributed variables support */
extern definition_t *address_pending_unifications;
extern definition_t *address_uvc;
extern definition_t *address_ucc;

#define SAVE_FIELD(Name) desc->wam_private_state.Name = Name

#define SAVE_WAM_STATE \
  SAVE_FIELD(p); \
  SAVE_FIELD(i);\
  SAVE_FIELD(pt1);\
  SAVE_FIELD(pt2);\
  SAVE_FIELD(t0);\
  SAVE_FIELD(t1);\
  SAVE_FIELD(t2);\
  SAVE_FIELD(t3);\
  SAVE_FIELD(ptemp);\
  SAVE_FIELD(wam_exit_code);\
  SAVE_FIELD(ins)

#define RECOVER_FIELD(Name) Name = desc->wam_private_state.Name

#define RECOVER_WAM_STATE \
  RECOVER_FIELD(p);\
  RECOVER_FIELD(i);\
  RECOVER_FIELD(pt1);\
  RECOVER_FIELD(pt2);\
  RECOVER_FIELD(t0);\
  RECOVER_FIELD(t1);\
  RECOVER_FIELD(t2);\
  RECOVER_FIELD(t3);\
  RECOVER_FIELD(ptemp);\
  RECOVER_FIELD(wam_exit_code);\
  RECOVER_FIELD(ins)

/* Macros for conditional code inside other macros */

#if defined(TABLING)
#define ON_TABLING(X) X
#else
#define ON_TABLING(X)
#endif

#if defined(ANDPARALLEL)
#define ON_ANDPARALLEL(X) X
#else
#define ON_ANDPARALLEL(X)
#endif

#if defined(DEBUG)
#define ON_DEBUG(X) X
#else
#define ON_DEBUG(X)
#endif

#if defined(DEBUG_NODE)
#define ON_DEBUG_NODE(X) X
#else
#define ON_DEBUG_NODE(X)
#endif

#include "wamloop.c"


