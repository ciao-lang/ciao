/*
 *  eng_debug.c
 *
 *  Support for debugging and tracing the engine code
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 The Ciao Development Team
 */

#include <ciao/eng.h>
#include <ciao/io_basic.h>
#include <ciao/stream_basic.h>
#include <ciao/eng_debug.h> /* already in eng.h */

/* ------------------------------------------------------------------------- */
/* safe print routines (works even with gc bits on) */

#if defined(DEBUG)

static void wr_functor_1(definition_t *func) {
  if (!(func->printname & 1))
    printf("%s/%d", GetString(func->printname), func->arity);
  else
    printf("(?)"); // TODO: deprecate subdef?
} 

void wr_functor(char *s, definition_t *func) {
  printf("%s: ",s);
  wr_functor_1(func);
  putchar('\n');
}

#define TRACE_CALLS_SHOW_ARG1  1
//#define TRACE_CALLS_SHOW_ARGS  1

CVOID__PROTO(wr_call, char *s, definition_t *func) {
  int i;

  printf("%s: ",s);

  if (!(func->printname & 1)) {
#if defined(TRACE_CALLS_SHOW_ARG1) || defined(TRACE_CALLS_SHOW_ARGS) /* display args */
    printf("%s", GetString(func->printname));
    if (func->arity > 0) {
      putchar('(');
      DerefDisplayTerm(X(0), Output_Stream_Ptr, TRUE);
#if defined(TRACE_CALLS_SHOW_ARGS) /* display all args */
      for (i = 1; i < func->arity; i++) {
        printf(",");
        DerefDisplayTerm(X(i), Output_Stream_Ptr, TRUE);
      }
#else
      for (i = 1; i < func->arity; i++) printf(",_");
#endif
      putchar(')');
    }
#else /* only display functor */
    printf("%s/%d", GetString(func->printname), func->arity);
#endif
  } else {
    printf("(?)"); // TODO: deprecate subdef?
  }

  putchar('\n');
}

#else

int eng_debug__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif
