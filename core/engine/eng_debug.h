/*
 *  eng_debug.h
 *
 *  Support for debugging and tracing the engine code
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_DEBUG_H
#define _CIAO_ENG_DEBUG_H

/* ------------------------------------------------------------------------- */
/* Stream for debug trace */

/* Stream for tracing (statistics, predicate trace, etc.) */

#if defined(OPTIM_COMP)

#if defined(USE_DEBUG_INSCOUNT) || \
    defined(USE_LOWRTCHECKS) || \
    defined(DEBUG_TRACE) || \
    defined(PROFILE_STATS) || \
    defined(PROFILE_EVENTFREQ) || \
    defined(PROFILE_BLOCKFREQ) || \
    defined(USE_GC_STATS)
#if !defined(USE_TRACE_OUTPUT)
#define USE_TRACE_OUTPUT 1
#endif
#endif

#if defined(USE_TRACE_OUTPUT)
// #include <ciao/io_basic.h> /* StreamPrintf */
extern stream_node_t *stream_trace; /* Shared */
#define TraceFile stream_trace->streamfile
// TODO: StreamPrintf requires 'w' (in order to throw IO exceptions)
//   which is not always available, use fprintf instead of:
//     StreamPrintf(stream_trace, __VA_ARGS__);
#define TRACE_PRINTF(...) ({ \
  fprintf(TraceFile, __VA_ARGS__); \
  fflush(TraceFile); \
})
#endif
#else
#define TraceFile stderr
#define TRACE_PRINTF(...) do { \
  fprintf(stderr, __VA_ARGS__); \
  fflush(stderr); \
} while(0)
#endif

/* ------------------------------------------------------------------------- */
/* INSCOUNT (instruction-level profiler) */

#if defined(OPTIM_COMP)

#if defined(USE_LOWRTCHECKS)||defined(DEBUG_TRACE)
#define USE_DEBUG_INSCOUNT 1
#endif

#endif /* defined(OPTIM_COMP) */

#if defined(USE_DEBUG_INSCOUNT) /* TODO: currently only OPTIM_COMP */
extern intmach_t debug_inscount;
bool_t dump_cond(void);
void init_debug_inscount(void);
#define INSCOUNT_NEXT() debug_inscount++
#define TRACE_INSCOUNT() TRACE_PRINTF("[time = %" PRIdm "] ", debug_inscount)
#else
#define TRACE_INSCOUNT() {}
#endif

/* ------------------------------------------------------------------------- */
/* Low-level runtime checks (debugging) */

#if defined(USE_LOWRTCHECKS)
#define RTCHECK(X) X
#else
#define RTCHECK(X) {}
#endif 

#if defined(OPTIM_COMP)

#if defined(USE_LOWRTCHECKS)
CBOOL__PROTO(proofread, char *text, arity_t arity, bool_t force);
void dump_tagged(tagged_t t);
CVOID__PROTO(dump_call, char *s, definition_t *func);
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

#if defined(DEBUG_TRACE)
#define DEBUG__TRACE(COND, ...) do { \
  if ((COND)) { TRACE_PRINTF(__VA_ARGS__); } \
} while(0)
#else
#define DEBUG__TRACE(COND, ...)
#endif

#if defined(DEBUG_TRACE)
/* debug_trace options */
extern bool_t debug_predtrace;
extern bool_t debug_dynlink;
extern bool_t debug_gc;
extern bool_t debug_threads;
extern bool_t debug_choicepoints;
extern bool_t debug_concchoicepoints;
extern bool_t debug_mem;
extern bool_t debug_conc;
#if defined(OPTIM_COMP)
extern bool_t debug_setarg;
extern bool_t debug_atomgc;
#else
extern bool_t debug_instrace;
#endif
//
bool_t debug_trace__get_opt(const char *arg);

#endif /* !defined(DEBUG_TRACE) */

CBOOL__PROTO(set_trace_calls);

#endif /* _CIAO_ENG_DEBUG_H */
