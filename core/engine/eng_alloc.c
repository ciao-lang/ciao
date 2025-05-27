/*
 *  eng_alloc.c
 *
 *  Checked memory allocation.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#if defined(Solaris)||defined(LINUX)||defined(EMSCRIPTEN)||defined(DARWIN)||defined(BSD)
#include <string.h>
#else
#include <memory.h>
#endif
#include <unistd.h>

#include <ciao/eng.h>

#if !defined(USE_OWN_MALLOC)
#include <stdlib.h>
#endif

#if defined(USE_OWN_MALLOC)
char *own_malloc(intmach_t size);
char *own_realloc(char *ptr, intmach_t size_in_chars);
void own_free(char *ptr);
void init_own_malloc(void);
#endif

#if defined(USE_OWN_MALLOC)
#  define Malloc(p)     own_malloc((p))
#  define Free(p)       own_free((char *)(p))
#  define Realloc(p, s) own_realloc((char *)(p), (s))
#else
#  define Malloc(p)     malloc((p))
#  define Free(p)       free((char *)(p))
#  define Realloc(p, s) realloc((char *)(p), (s))
#endif                                                       

#if !defined(OPTIM_COMP)
/* segfault patch -- jf */
#if SMALLPTR_BASE != 0
#define ENSURE_ADDRESSABLE(P, SIZE) \
  (((tagged_t)(P) >= (tagged_t)SMALLPTR_BASE) && \
  (((tagged_t)(P) - (tagged_t)SMALLPTR_BASE) < POINTERMASK) && \
   (((tagged_t)(P) - (tagged_t)SMALLPTR_BASE + (SIZE)) < POINTERMASK))
#else
#define ENSURE_ADDRESSABLE(P, SIZE) \
  ((tagged_t)(P) < POINTERMASK)
#endif
#endif

/* --------------------------------------------------------------------------- */
/* Memory errors */

char *tryalloc_errstring;

/* Annotate the error message and return NULL */
#define MEMORY_FAULT(MESSAGE) {                 \
    tryalloc_errstring = (MESSAGE);             \
    return NULL;                                \
  }

/* Raise the fault (stored by a MEMORY_FAULT) if ALLOC_CALL returns
   NULL */
#define CHECK_FOR_MEMORY_FAULT(ALLOC_CALL)  {      \
    char *__ptr = (ALLOC_CALL);                    \
    if (!__ptr) SERIOUS_FAULT(tryalloc_errstring); \
    return __ptr;                                  \
  }

/* --------------------------------------------------------------------------- */
/* Debug trace */

/* TODO: move the real debug code here... debug.h only handles common stuff? */
#if defined(DEBUG_TRACE)
#if defined(OPTIM_COMP)
/* condition to show memory operation */
/* TODO: make configurable */
bool_t dump_memory_cond(char *p, intmach_t size) {
  return debug_mem && size > 100000; /* do not dump small blocks */
}
/* alloc */
#define DEBUG__TRACE_ALLOC(P, SIZE) { \
  dump_memory_alloc((char *)(P), (SIZE)); \
}
void dump_memory_alloc(char *p, intmach_t size) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("alloc %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size);
}
/* realloc */
#define DEBUG__TRACE_REALLOC(PTR, DECR, P, SIZE) { \
  dump_memory_realloc((char *)(PTR), (DECR), (char *)(P), (SIZE)); \
}
void dump_memory_realloc(char *p, intmach_t size, char *p2, intmach_t size2) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("realloc %p-%p (0x%lx bytes) to %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size, p2, (char *)p2+size2, (long)size2);
}
/* free */
#define DEBUG__TRACE_FREE(PTR, DECR) { \
  dump_memory_free((char *)(PTR), (DECR)); \
}
void dump_memory_free(char *p, intmach_t size) {
  if (!dump_memory_cond(p, size)) return;
  TRACE_PRINTF("free %p-%p (0x%lx bytes)\n", p, (char *)p+size, (long)size);
}
#else /* !OPTIM_COMP */
#define DEBUG__TRACE_ALLOC(p, size) { \
  if (debug_mem) printf("tryalloc returned %p, %" PRIdm " chars\n", p, size); \
}
#define DEBUG__TRACE_REALLOC(ptr, decr, p, size) { \
  if (debug_mem) printf("tryrealloc returned %p, %" PRIdm " chars\n", p, size); \
}
#define DEBUG__TRACE_FREE(ptr, decr) { \
  if (debug_mem) printf("checkdealloc freed %p, %" PRIdm " chars\n", ptr, decr); \
}
#endif
#else /* !DEBUG_TRACE */
#define DEBUG__TRACE_ALLOC(P, SIZE)
#define DEBUG__TRACE_REALLOC(PTR, DECR, P, SIZE)
#define DEBUG__TRACE_FREE(PTR, DECR)
#endif

/* --------------------------------------------------------------------------- */
/* Memory management lock (for internal structures) */

/* Locks for accessing the shared memory.  Although malloc() is
   MT-safe in Solaris (how 'bout Linux?), the engine uses several
   internal data structures for its own memory management which should
   be correctly updated.  So we use our own lock for providing an
   atomic access to tryalloc/tryrealloc/checkdealloc (MCL).
*/
#if defined(OPTIM_COMP)
SLOCK mem_mng_l;
#else
extern SLOCK mem_mng_l;
#endif

/* --------------------------------------------------------------------------- */

/* Total # bytes grabbed by malloc/realloc/free . Not accurately
   measured -- it depends on the mem. mang. own data structures
   (MCL) */
intmach_t total_mem_count = 0; /* Shared */
/* # bytes used by the Prolog program & database code.  Probably not
   accurately measured (patched here and there) (MCL).  */
intmach_t mem_prog_count = 0; /* Shared */

#define USE_TINY_BLOCKS 1
#if defined(USE_TINY_BLOCKS)
/* From an execution profile, 24 seems a good threshhold (MCL) */
#define THRESHHOLD 24
#define NTINY 682
#endif

#if defined(USE_TINY_BLOCKS)
static char *tiny_blocks = NULL; /* Shared & locked */
#endif

#if defined(USE_TINY_BLOCKS)
static char *get_tiny_blocks(void) {
  intmach_t i;
  char *ptr;
  char *p;
  char *tail = NULL;

  /* ptr was a call to checkalloc, but I made a direct call to
     Malloc() because of recursive locks stopping the engine.  Thus,
     the total amount of memory is also increased here. */

  ptr = Malloc(NTINY*THRESHHOLD);
  if (!ptr) {
    Release_slock(mem_mng_l);
    MEMORY_FAULT("Memory allocation failed [Malloc()]");
  }
  if (!ENSURE_ADDRESSABLE(ptr, NTINY*THRESHHOLD)) {
    Release_slock(mem_mng_l);
    MEMORY_FAULT("Memory out of addressable bounds! [Malloc()]");
  }

  p = ptr + NTINY*THRESHHOLD;
  total_mem_count += NTINY*THRESHHOLD;

  for (i=NTINY; i>0; i--) {
    p -= THRESHHOLD;
    *((char **)p) = tail;
    tail = p;
  }
  tiny_blocks = ptr+THRESHHOLD;
  return ptr;
}
#endif

char *tryalloc(intmach_t size) {
  char *p;
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (size<=THRESHHOLD) {
    p = tiny_blocks;
    if (p) {
      tiny_blocks = *((char **)p);
    } else {
      p = get_tiny_blocks();
      if (!p) { /* get_tiny_block fails */
        Release_slock(mem_mng_l);
        return NULL;
      }
    }
  } else { /* size > THRESHHOLD */
#endif
    p = Malloc(size);

    if (!p) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory allocation failed [Malloc()]");
    }
    if (!ENSURE_ADDRESSABLE(p, size)) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory out of addressable bounds! [Malloc()]");
    }

    total_mem_count += size;
#if defined(USE_TINY_BLOCKS)
  }
#endif
  DEBUG__TRACE_ALLOC(p, size);
  Release_slock(mem_mng_l);
  return p;
}

char *checkalloc(intmach_t size) {
  CHECK_FOR_MEMORY_FAULT(tryalloc(size));
}

void checkdealloc(char *ptr, intmach_t decr) {
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    *((char **)ptr) = tiny_blocks;
    tiny_blocks = ptr;
  } else {
#endif
    total_mem_count -= decr;
    Free(ptr);
#if defined(USE_TINY_BLOCKS)
  }
#endif
  DEBUG__TRACE_FREE(ptr, decr);
  Release_slock(mem_mng_l);
}

char *tryrealloc(char *ptr, intmach_t decr, intmach_t size) {
  char *p;
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    if (size<=THRESHHOLD) {
      /* leave in the same tiny block */
      p = ptr;
    } else {
      /* move from a tiny block to a new tiny block */
      p = Malloc(size);
      if (!p) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory allocation failed [Malloc()]");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory out of addressable bounds [Malloc()]");
      }
      total_mem_count += size;
#if defined(OPTIM_COMP)
      // TODO: alignment was not necessary, right?
      memcpy(p, ptr, decr);
#else
      memcpy(p, ptr, ALIGN_TO(sizeof(tagged_t), decr));
#endif
      *((char **)ptr) = tiny_blocks;
      tiny_blocks = ptr;
    }
  } else { /* decr > THRESHHOLD */
    if (size<=THRESHHOLD) {
      /* move from a big block to a new tiny block */
      p = tiny_blocks;
      if (!p) {
        p = get_tiny_blocks();
        if (!p) { /* get_tiny_block fails */
          Release_slock(mem_mng_l);
          return NULL;
        }
      }
#if defined(OPTIM_COMP)
      // TODO: alignment was not necessary, right?
      memcpy(p, ptr, size);
#else
      memcpy(p, ptr, ALIGN_TO(sizeof(tagged_t), size));
#endif
    } else {
#endif
      /* leave in a big block */
      p = Realloc(ptr, size);
      if (!p) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory allocation failed [in Realloc()]");
      }
      if (!ENSURE_ADDRESSABLE(p, size)) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory out of addressable bounds [Realloc()]");
      }
      total_mem_count += (size-decr);
#if defined(USE_TINY_BLOCKS)
    }
  }
#endif
  DEBUG__TRACE_REALLOC(ptr, decr, p, size);
  Release_slock(mem_mng_l);
  return p;
}

char *checkrealloc(char *ptr, intmach_t decr, intmach_t size) {
  CHECK_FOR_MEMORY_FAULT(tryrealloc(ptr,decr,size));
}

void init_alloc(void) {
#if defined(OPTIM_COMP) /* see init_locks() for !OPTIM_COMP */
  Init_slock(mem_mng_l);
#endif
#if defined(USE_OWN_MALLOC)
  init_own_malloc();
#endif
}

