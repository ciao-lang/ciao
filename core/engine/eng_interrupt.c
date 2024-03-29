/*
 *  eng_interrupt.c
 *
 *  Interrupt handlers.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 Ciao Development Team
 */

#include <ciao/eng.h>

#include <sys/types.h>

#include <ciao/os_signal.h>
#include <ciao/eng_interrupt.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>

static void abortmsg(int rc);

/* I/O predicate at ^C, else NULL */
/* Probably Shared, since only one worker should receive a ^C */
definition_t *int_address = NULL;

/* Interrupt an specific worker */
CVOID__PROTO(interrupt_worker, int signal_number) {
  SetCIntEvent();
#if defined(LINUX)||defined(EMSCRIPTEN)
/* From the manpage: Unlike on BSD systems, signals under Linux are reset to
  their default behavior when raised.  Therefore we have to SIGNAL()
  interrupt_h here again.  Be careful of restating which signals should
  interrupt_h respond to. */
#endif 
  if (int_address) SIGLONGJMP(abort_env, -1); 
}

static void interrupt_h(int signal_number) {
  /* exit if wam is not initialized */
  if (!in_abort_context) engine_exit(-1);

#if defined(OPTIM_COMP) && defined(USE_PROLOG_DEBUGGER)
  debug_status = 0;
#endif
  WITH_WORKER(get_my_worker(), {
    CVOID__CALL(interrupt_worker, signal_number);
  });
}

CVOID__PROTO(control_c_normal) {
#if !defined(OPTIM_COMP)
  // TODO:[oc-merge] needed?
  UnsetCIntEvent();
#endif
  if (Input_Stream_Ptr->isatty) {
    SIGNAL(SIGINT,interrupt_h);
  }
}

/* Non-control-C exception handling. --GB & MC */

/* set system exception signal handling */
void enable_conditions(void) {
  SIGNAL(SIGFPE,  abortmsg);
  SIGNAL(SIGSEGV, abortmsg);
  SIGNAL(SIGILL,  abortmsg);
#if defined(_WIN32) || defined(_WIN64)
  /* No SIGSYS and SYGBUS in MinGW */
#elif defined(LINUX)||defined(EMSCRIPTEN)
  /* Signal handlers in LINUX must reinstall the handler for the signal */
  SIGNAL(SIGBUS,  abortmsg);
  /* No SIGSYS ("bad systema call") signal in Linux */
#else
  SIGNAL(SIGBUS,  abortmsg);
  SIGNAL(SIGSYS, abortmsg);
#endif
#if defined(_WIN32) || defined(_WIN64)
  /* No SIGPIPE MinGW */
#else
  SIGNAL(SIGPIPE, SIG_IGN); /* handle EPIPE error codes ourselves (write()) */
#endif
}

static void abortmsg(int rc) {
  switch(rc) {
  case SIGINT:
    SERIOUS_FAULT("interrupted");
    break;
  case SIGFPE:
    SERIOUS_FAULT("floating point exception");
    break;
  case SIGSEGV:
    PANIC_FAULT("segmentation violation");
    break;
  case SIGILL:
    PANIC_FAULT("illegal instruction");
    break;
#if defined(_WIN32) || defined(_WIN64)
#else
  case SIGBUS:
    PANIC_FAULT("bus error");
    break;
#if defined(LINUX)||defined(EMSCRIPTEN)
#else
  case SIGSYS:
    PANIC_FAULT("bad system call arg");
    break;
#endif
#endif
  default:
    {
      char message[1024];
      sprintf(message, "miscellaneous error condition: received signal number %d\n", rc);
      PANIC_FAULT(message);
    }
    break;
  }
}
