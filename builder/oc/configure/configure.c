/*
 *  configure.c
 *
 *  Obtain the value of system and architecture parameters that do not
 *  change from run to run, so that those values can be static data
 *  during native code compilation.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2019 The Ciao Development Team
 */

#if defined(__svr4__) || defined(Solaris) || defined(DARWIN) || defined(BSD)
# include <stdlib.h>                                         /* malloc() */
#else                                                            /* SunOS */
# include <sys/types.h>
# include <malloc.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <stdint.h> /* C99 integer types */
#include <inttypes.h> /* PRI* macros for printf */

/* mask for upper X bits reserved */
#define PTRTAGMASK(X) (((((uintptr_t)1)<<(X))-1)<<((sizeof(uintptr_t)*8)-(X)))
/* addressable space when X bits are reserved */
#define ADDRESSABLE(X) (1<<((sizeof(uintptr_t)*8)-(X)))

#define MAX_UPPER_BITS 5

/* ------------------------------------------------------------------------- */
/* configure__endianness: Obtain system endianness
 * 
 * IS_BIG_ENDIAN: 1 if the system is big-endian, 0 if the system is little-endian
 */

void configure__endianness(void) {
  union {
    unsigned short as_short[2];
    unsigned int as_int;
  } u;
  u.as_int = 1;
  printf("#define IS_BIG_ENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* Configure mmap */

#if defined(USE_MMAP)
#include "engine__own_mmap.h"
#define USE_OWN_MALLOC 1
#endif

#if defined(USE_MMAP)

/* Mininum amount of reserved bits for mmap (trying to reserve bigger
   memory regions crashes the process) */
#define MIN_MMAP_RESERVED_BITS 3

#define N_TRY_ADDR 9
uintptr_t try_addr[N_TRY_ADDR] = { 
    0x60000000, 0x70000000, 0x80000000, 0x40000000, 0x90000000,
    0x30000000, 0xA0000000, 0x20000000, 0xB0000000};
void find_mmap_base(int reserved_bits, uintptr_t *mmap_base, size_t *mmap_size) {
  void *pointer;
  int i = 0;

  /* NOTE: Chunks of code identical to those in own_malloc.c - keep
     them in sync! */

  for(i = 0; i < N_TRY_ADDR; i++){
    pointer = (void *)try_addr[i];
    if (!own_fixed_mmap(pointer, ADDRESSABLE(reserved_bits)))
      break;
  }

  /* Give memory back */
  own_fixed_munmap((void *)pointer, ADDRESSABLE(reserved_bits));

  if (i < N_TRY_ADDR) {
    *mmap_base = (uintptr_t)pointer;
    *mmap_size = ADDRESSABLE(reserved_bits);
  } else {
    fprintf(stderr, "{bug: could not find a fixed address for mmap}");
    *mmap_base = (uintptr_t)0;
    *mmap_size = 0;
  }
}
#endif

/* ------------------------------------------------------------------------- */
/* configure__alloc: Memory management configuration
 *
 * MallocBase: 0'MMMM...0000000 where M are the top bits for pointers
 *   returned by malloc()
 * MIN_MEM_ALLOC: minimum amount of memory that makes malloc return
 *   pointers in that region
 *
 * Some systems (namely, LINUX) allocate memory in different parts of
 * the memory depending on how much we ask.  The result is that blocks
 * can be scattered so that the "unmutable four top bits" assumption
 * is broken (i.e., the mapped memory can't be fit into the pointer
 * part of a tagged word) and MallocBase is not useful at all.  We try
 * to find out dynamically if this ever happens, and at which point.
 * This will serve to calculate the minimum amount of memory to be
 * requested at a time from the system, and a (hopefully) correct
 * MallocBase.
 *  
 * If we, anyway, choose to build a system without snooping for a
 * good MallocBase, just use the first pointer the system returns.
 */

/* 
    Before switching to ld linker scripts, things to know (statically):

    * Do we have a working mmap() [which makes it necessary to use
    own_malloc()]?  If so, does it have the ANONYMOUS flag?  If it doesn't,
    does "/dev/zero" work?  We assume we can know this before compiling, and
    find out where does memory start.

    * Otherwise, do we need to grab a minimum amount of memory from malloc
    to make the region stable?

    * If we don't, then just take the first points malloc() gives us.
*/

#define MIN_MEM_BLOCK_CHARS 16384
#define MAX_MEM_BLOCK_CHARS (32*1024*1024)

#define ALIGN sizeof(uintptr_t)                        /* Minimum block size */

#if defined(USE_OWN_MALLOC)
void find_malloc_base(uintptr_t tagmask, uintptr_t *malloc_base0, size_t *min_mem_alloc0) {
  /* Use system malloc to allocate blocks of memory */
  uintptr_t malloc_base;
  void *chunk;
  size_t min_mem_alloc;
  size_t size;
  if (tagmask == 0) {
    /* No base or special min mem alloc value is required when tagmask is 0 */
    malloc_base = 0;
    min_mem_alloc = MIN_MEM_BLOCK_CHARS;
  } else {
    /* Obtain turn point (assumes that tagmask != 0) */ 
    size = ALIGN;
    while(1) {
      chunk = (void *)malloc(size);
      if (chunk == NULL) {
        /* tagmask is never 0, use MIN_MEM_BLOCK_CHARS */
        malloc_base = (uintptr_t)0;
        min_mem_alloc = MIN_MEM_BLOCK_CHARS;
        break;
      } else {
        if (((uintptr_t)chunk & tagmask) == 0) {
          /* not yet non-zero, continue */
          free(chunk);
          size *= 2;
          if (size > MAX_MEM_BLOCK_CHARS) {
            /* not found, assume tag bits are always 0 */
            malloc_base = (uintptr_t)0;
            min_mem_alloc = MIN_MEM_BLOCK_CHARS;
            break;
          }
        } else {
          /* Use that one, assume that there will be no more changes in
             the upper bits */
          malloc_base = (uintptr_t)chunk & tagmask;
          free(chunk);
          min_mem_alloc = (size > MIN_MEM_BLOCK_CHARS ?
                           size : MIN_MEM_BLOCK_CHARS);
          break;
        }
      }
    }
  }
  *malloc_base0 = malloc_base;
  *min_mem_alloc0 = min_mem_alloc;
}
#endif

void configure__alloc(void) {
  int bits;
#if defined(USE_OWN_MALLOC)
  for (bits = MAX_UPPER_BITS; bits >= 0; bits--) {
#if defined(USE_MMAP)
    if (bits >= MIN_MMAP_RESERVED_BITS) {
      /* Use mmap to allocate blocks of memory */
      uintptr_t mmap_base;
      size_t mmap_size;
      find_mmap_base(bits, &mmap_base, &mmap_size);
      printf("#define MmapAllowed%d 1\n", bits);
      printf("#define MallocBase%d 0x%" PRIxPTR "\n", bits, (uintptr_t)mmap_base);
      printf("#define MmapSize%d 0x%" PRIxPTR "\n", bits, (uintptr_t)mmap_size);
    } else {
      printf("#define MmapAllowed%d 0\n", bits);
#endif
      /* Use system malloc to allocate blocks of memory */
      uintptr_t malloc_base;
      size_t min_mem_alloc;
      find_malloc_base(PTRTAGMASK(bits), &malloc_base, &min_mem_alloc);
      printf("#define MallocBase%d 0x%" PRIxPTR "\n", bits, (uintptr_t)malloc_base);
      printf("#define MIN_MEM_ALLOC_%d 0x%" PRIxPTR "\n", bits, (uintptr_t)min_mem_alloc);
#if defined(USE_MMAP)
    }
#endif
  }
  printf("#if !defined(USE_OWN_MALLOC)\n#define USE_OWN_MALLOC 1\n#endif\n");
#else /* !defined(USE_OWN_MALLOC) */
  /* Trust that the malloc implementation gives always pointers in the
     same region */
  uintptr_t malloc_base;
  void *chunk;
  for (bits = MAX_UPPER_BITS; bits >= 0; bits--) {
    // chunk = (void *)malloc(MIN_MEM_BLOCK_CHARS); /* TODO: use different sizes? */
    chunk = (void *)malloc(ALIGN);
    malloc_base = (uintptr_t)chunk & PTRTAGMASK(bits);
    free(chunk);
    printf("#define MallocBase%d 0x%" PRIxPTR "\n", bits, malloc_base);
  }
#endif
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int main(void) {
  configure__endianness();
  configure__alloc();
  return 0;
}

