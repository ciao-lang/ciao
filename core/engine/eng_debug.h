/*
 *  eng_debug.h
 *
 *  Support for debugging and tracing the engine code
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_DEBUG_H
#define _CIAO_ENG_DEBUG_H

#if !defined(OPTIM_COMP)
extern bool_t stop_on_pred_calls;
#endif

/* ------------------------------------------------------------------------- */
/* INSCOUNT (instruction-level profiler) */

#if defined(OPTIM_COMP)

#if defined(USE_LOWRTCHECKS)||defined(DEBUG_TRACE)
#define USE_DEBUG_INSCOUNT 1
#endif

#if defined(USE_DEBUG_INSCOUNT)
extern intmach_t debug_inscount;
bool_t dump_cond(void);
void init_debug_inscount(void);
#define INSCOUNT_NEXT() debug_inscount++
#endif

#endif /* defined(OPTIM_COMP) */

/* ------------------------------------------------------------------------- */
/* Low-level runtime checks (debugging) */

#if defined(OPTIM_COMP)

#if defined(USE_LOWRTCHECKS)
CBOOL__PROTO(proofread, char *text, intmach_t arity, bool_t force);
void dump_tagged(tagged_t t);
CVOID__PROTO(dump_call, char *s, definition_t *func);

#define RTCHECK(X) X
#else
#define RTCHECK(X) {}
#endif 

/* TODO: give good definitions */
#define RTERROR() {}
#define RTERRORBOOL() FALSE

/* TODO: fix name */
#define TYPE_RTCHECK(Type, X) \
  RTCHECK({if (!Type((X))) TRACE_PRINTF("{violated type assertion at %s:%d}", __FILE__, __LINE__);})

#endif /* defined(OPTIM_COMP) */

/* ------------------------------------------------------------------------- */
/* Debug trace */

#if defined(OPTIM_COMP)

#if defined(DEBUG_TRACE)

/* TODO: move tracing outside the debug code? add tracing levels? */
#define DEBUG__TRACE(COND, ...) ({ \
  if ((COND)) { TRACE_PRINTF(__VA_ARGS__); } \
})
/* debug_trace options */
extern bool_t debug_predtrace;
extern bool_t debug_dynlink;
extern bool_t debug_gc;
extern bool_t debug_threads;
extern bool_t debug_choicepoints;
extern bool_t debug_concchoicepoints;
extern bool_t debug_mem;
extern bool_t debug_conc;
extern bool_t debug_setarg;
extern bool_t debug_atomgc;

bool_t debug_trace__get_opt(const char *arg);

#else
#define DEBUG__TRACE(COND, ...)
#endif

#else /* !defined(OPTIM_COMP) */

#if defined(DEBUG)
extern bool_t debug_dynlink;
extern bool_t debug_gc;
extern bool_t debug_threads;
extern bool_t debug_choicepoints;
extern bool_t debug_concchoicepoints;
extern bool_t debug_mem;
extern bool_t debug_conc;
#endif

#endif /* !defined(OPTIM_COMP) */

#endif /* _CIAO_ENG_DEBUG_H */
