/*
 *  interrupt.c
 *
 *  Interrupt handlers.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <sys/types.h>

#include <ciao/os_signal.h>
#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/task_areas.h>
#include <ciao/interrupt.h>
#include <ciao/tasks.h>
#include <ciao/start.h>

static void abortmsg(int rc);

/* I/O predicate at ^C, else NULL */
/* Probably Shared, since only one worker should receive a ^C */
definition_t *int_address = NULL;

static void abortmsg(int rc);

static void interrupt_h(int signal_number)
{
  worker_t *w; 

  if (!wam_initialized)                            /* wam not initialized */
    at_exit(-1);

  Arg = get_my_worker();

  Int_Heap_Warn = Heap_Start;
  SetEvent;
#if defined(LINUX)
/* From the manpage: Unlike on BSD systems, signals under Linux are reset to
  their default behavior when raised.  Therefore we have to SIGNAL()
  interrupt_h here again.  Be careful of restating which signals should
  interrupt_h respond to. */
#endif 
  if (int_address)
    SIGLONGJMP(abort_env, -1); 
}

CVOID__PROTO(control_c_normal)
{
  Int_Heap_Warn = Heap_Warn;
  if (Input_Stream_Ptr->isatty)
    SIGNAL(SIGINT,interrupt_h);
}


/* Non-control-C exception handling. --GB & MC */

void enable_conditions()
/* set system exception signal handling */
{
  SIGNAL(SIGFPE,  abortmsg);
  SIGNAL(SIGSEGV, abortmsg);
  SIGNAL(SIGILL,  abortmsg);
#if defined(_WIN32) || defined(_WIN64)
  /* No SIGSYS and SYGBUS in MinGW */
#elif defined(LINUX)
  /* Signal handlers in LINUX must reinstall the handler for the signal */
  SIGNAL(SIGBUS,  abortmsg);
  /* No SIGSYS ("bad systema call") signal in Linux */
#else
  SIGNAL(SIGBUS,  abortmsg);
  SIGNAL(SIGSYS, abortmsg);
#endif
}

static void abortmsg(int rc)
{
  char message[1024];
  switch( rc )
    {
    case SIGINT:
      SERIOUS_FAULT("interrupted");
      break;
    case SIGFPE:
      SERIOUS_FAULT("floating point exception");
      break;
    case SIGSEGV:
      SERIOUS_FAULT("segmentation violation");
      break;
    case SIGILL:
      SERIOUS_FAULT("illegal instruction");
      break;
#if defined(_WIN32) || defined(_WIN64)
#else
    case SIGBUS:
      SERIOUS_FAULT("bus error");
      break;
#if defined(LINUX)
#else
    case SIGSYS:
      SERIOUS_FAULT("bad system call arg");
      break;
#endif
#endif
    default:
      sprintf(message, "miscellaneous error condition: received signal number %d\n", rc);
      SERIOUS_FAULT(message);
      break;
      }
}


