/*
 *  alloc.c
 *
 *  Checked memory allocation.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(BSD)
#include <string.h>
#else
#include <memory.h>
#endif
#include <unistd.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/alloc.h>

#if defined(USE_OWN_MALLOC)
#include <ciao/own_malloc.h>
#endif

#if defined(USE_OWN_MALLOC)
#  define Malloc(p)     own_malloc(p)
#  define Free(p)       own_free(p)
#  define Realloc(p, s) own_realloc(p, s)
#else
#  define Malloc(p)     malloc(p)
#  define Free(p)       free((char *)p)
#  define Realloc(p, s) realloc((char *)p, s)
#endif                                                       

#if defined(sequent) 
#  define Malloc(p) shmalloc(p)
#  define Free(p)   free(p)
#  define Realloc(p, s) realloc(p, s)
#endif                                                         /* sequent */

char *tryalloc_errstring;

#define MEMORY_FAULT(MESSAGE) {			\
    tryalloc_errstring = (MESSAGE);		\
    return NULL;				\
  }

#define CHECK_FOR_MEMORY_FAULT(ALLOC_CALL)  {	   \
    tagged_t *__ptr = (ALLOC_CALL);		   \
    if (!__ptr) SERIOUS_FAULT(tryalloc_errstring); \
    return __ptr;				   \
  }

/* segfault patch -- jf */
#if SMALLPTR_BASE
#define ENSURE_ADDRESSABLE(P, SIZE) \
  (((tagged_t)(P) >= (tagged_t)SMALLPTR_BASE) && \
  (((tagged_t)(P) - (tagged_t)SMALLPTR_BASE) < POINTERMASK) && \
   (((tagged_t)(P) - (tagged_t)SMALLPTR_BASE + (SIZE)) < POINTERMASK))
#else
#define ENSURE_ADDRESSABLE(P, SIZE) \
  ((tagged_t)(P) < POINTERMASK)
#endif

static tagged_t *get_tiny_blocks(void);

/* Total # bytes grabbed by malloc/realloc/free . Not accurately
   measured -- it depends on the mem. mang. own data structures
   (MCL) */
intmach_t total_mem_count = 0;                                 /* Shared */

/* Locks for accessing the shared memory.  Although malloc() is
   MT-safe in Solaris (how 'bout Linux?), the engine uses several
   internal data structures for its own memory management which should
   be correctly updated.  So we use our own lock for providing an
   atomic access to tryalloc/tryrealloc/checkdealloc (MCL).
*/

extern SLOCK    mem_mng_l;

#define USE_TINY_BLOCKS 1
#if defined(USE_TINY_BLOCKS)
/* From an execution profile, 24 seems a good threshhold (MCL) */
#define THRESHHOLD 24
#define NTINY 682
#endif

#if defined(USE_TINY_BLOCKS)
static tagged_t *tiny_blocks = NULL;                     /* Shared & locked */
#endif

#if defined(USE_TINY_BLOCKS)
static tagged_t *get_tiny_blocks(void)
{
  intmach_t i;
  tagged_t *ptr;
  tagged_t *p;
  tagged_t tail = 0;

  /* prt was a call to checkalloc, but I made a direct call to
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

  p = ptr + (NTINY*THRESHHOLD)/sizeof(tagged_t);
  total_mem_count += NTINY*THRESHHOLD;

  for (i=NTINY; i>0; i--) {
    p -= THRESHHOLD/sizeof(tagged_t);
    p[0] = tail;
    tail = (tagged_t)p;
  }
  tiny_blocks = ptr + THRESHHOLD/sizeof(tagged_t);
  return ptr;
}
#endif

tagged_t *tryalloc(intmach_t size)
{
  tagged_t *p;
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (size<=THRESHHOLD) {
    if ((p=tiny_blocks))
      tiny_blocks = (tagged_t *)p[0];
    else {
      p=get_tiny_blocks();
      if (!p) {                                      /* get_tiny_block fails */
	Release_slock(mem_mng_l);
	return NULL;
      }
    }
  } else {                                           /* size > THRESHHOLD */
#endif
    p = (tagged_t *)Malloc(size);

    if (!p) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory allocation failed [Malloc()]");
    }
    /* segfault patch -- jf */
    if (!ENSURE_ADDRESSABLE(p, size)) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory out of addressable bounds! [Malloc()]");
    }

    total_mem_count += size;
#if defined(USE_TINY_BLOCKS)
  }
#endif
#if defined(DEBUG)
  if (debug_mem)
    printf("tryalloc returned %x, %d chars\n", (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

tagged_t *checkalloc(intmach_t size)
{
  CHECK_FOR_MEMORY_FAULT(tryalloc(size));
}

void checkdealloc(tagged_t *ptr, intmach_t decr)
{
  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    ptr[0] = (tagged_t)tiny_blocks;
    tiny_blocks = ptr;
  } else {
#endif
    total_mem_count -= decr;
    Free(ptr);
#if defined(USE_TINY_BLOCKS)
  }
#endif
#if defined(DEBUG)
  if (debug_mem)
    printf("checkdealloc freed %x, %d chars\n", (unsigned int)ptr, decr);
#endif
  Release_slock(mem_mng_l);
}

tagged_t *tryrealloc(tagged_t *ptr, intmach_t decr, intmach_t size)
{
  tagged_t *p;

  Wait_Acquire_slock(mem_mng_l);

#if defined(USE_TINY_BLOCKS)
  if (decr<=THRESHHOLD) {
    if (size<=THRESHHOLD)
      p = ptr;
    else {
      p = Malloc(size);                       /* Was a call to checkalloc */
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
      memcpy(p, ptr, ALIGN_TO(sizeof(tagged_t), decr));
      ptr[0] = (tagged_t)tiny_blocks;
      tiny_blocks = ptr;
    }
  } else {                                           /* decr > THRESHHOLD */
    if (size<=THRESHHOLD) {
      if (!(p=tiny_blocks)) {
	p=get_tiny_blocks(); 
	if (!p) {                                    /* get_tiny_block fails */
	  Release_slock(mem_mng_l);
	  return NULL;
	}
      }
      memcpy(p, ptr, ALIGN_TO(sizeof(tagged_t), size));
    } else {
#endif
      p = (tagged_t *)Realloc(ptr,size);
      if (!p) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory allocation failed [in Realloc()]");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	MEMORY_FAULT("Memory out of addressable bounds [Realloc()]");
      }
      total_mem_count += (size-decr);
#if defined(USE_TINY_BLOCKS)
    }
  }
#endif
#if defined(DEBUG)
  if (debug_mem)
  printf("tryrealloc returned %x, %d chars\n",
         (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

tagged_t *checkrealloc(tagged_t *ptr, intmach_t decr, intmach_t size)
{
  CHECK_FOR_MEMORY_FAULT(tryrealloc(ptr,decr,size));
}

void init_alloc(void) {
  // Init_slock(mem_mng_l); /* see init_locks() */
#if defined(USE_OWN_MALLOC)
  init_own_malloc();
#endif
}

