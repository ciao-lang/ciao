/*
 *  configure.c
 *
 *  Obtain the value of system and architecture parameters that do not
 *  change from run to run, so that those values can be static data
 *  during native code compilation.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
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
/* configure__fpbits:
 *
 * ENG_FLT_SIGNIF <int>: number of significant digits in a float
 * ENG_FLT_ROUND <float>: a number to be used for rounding purposes
 *   when floats are printed
 *  
 * (so that display(5.347) exactly shows 5.347)
 */

/*
  safe_addition is intended to force A and B to be stored prior to doing the
  addition of A and B , for use in situations where optimizers might hold
  one of these in a register.

  There is a problem with gcc (3.1, at least): when using -O3, which turns
  inlining on, the number of digits in the (decimal) mantissa returned is
  20, due to the inlining of the safe_addition function, and the further
  optimization this brings about.  In order to avoid this in general, I have
  put the volatile keyword which tells the optimizer not to throw away
  references to the ret_val variable (since it may be read/written to by
  other threads :-), even if safe_addition is inlined.
*/

double safe_addition(volatile double *a, volatile double *b) {
  volatile double ret_val;
  ret_val = *a + *b;
  return ret_val;
} 

/* Computing the accuracy of floats - Changed so that display(5.347) = 5.347 */

void find_fp_bits(int *t) {
  volatile static double one = 1.0;
  static int base = 2; 

  /* 'base' was originally found out from the implementation of the FPU/FP
     routines; it is tipically 2 in mos FP implementations.  I suppose,
     then, that we can choose whatever base is appropriate for us and use
     the loop below to determine the number of significant digits in
     (pseudo-) base 10. */
    
  volatile double a, c, tmp1;
  volatile int lt;

  lt = 0;
  a = c = one;
  while (c == one) {
    ++lt;
    a *= base;
    c = safe_addition(&a, &one);
    tmp1 = -a;
    c = safe_addition(&c, &tmp1);
  }
  *t = lt;
} 

void get_mask_descr(int size,
		    volatile unsigned int *lx,
		    volatile unsigned int *ly,
		    unsigned int *mask,
		    unsigned int *indx,
		    unsigned int *shft) {
  for(*indx=0; *indx<size; (*indx)++) {
    *mask = lx[*indx] ^ ly[*indx];
    if(*mask) {
      *shft = 0;
      while(((*mask >> *shft) & (unsigned int)1)==0) {
	(*shft)++;
      }
      return;
    }
  }
  *indx=0;
  *shft=0;
  *mask=0;
}

#define EMIT_MASK_DESC(STD, PART, MASK, INDEX, SHIFT) {         \
    printf("#define " STD "_MASK_" PART "  0x%08X\n", (MASK));	\
    printf("#define " STD "_INDEX_" PART " %d\n", (INDEX));	\
    printf("#define " STD "_SHIFT_" PART " %d\n", (SHIFT));	\
  }
#define EMIT_MASK_DESC0(STD, PART, MASK, INDEX, SPLIT) {         \
    printf("#define " STD "_MASK_" PART "  0x%08X\n", (MASK));	\
    printf("#define " STD "_INDEX_" PART " %d\n", (INDEX));	\
    printf("#define " STD "_SPLIT_" PART " %d\n", (SPLIT));	\
  }

/* This function will calculate some values related to the internal
   representation of the double numbers according with the ieee754
   standard */

union dbl {
  double f;
  unsigned int i[sizeof(double)/sizeof(unsigned int)];
};
typedef union dbl dbl_t;

void configure_ieee754(void) {
  volatile dbl_t x, y, z;
  unsigned int mask;
  unsigned int index;
  unsigned int shift;
  int i;
  int size = sizeof(x) / sizeof(mask);

  x.f = 1; y.f = -x.f;
  get_mask_descr(size, x.i, y.i, &mask, &index, &shift);
  EMIT_MASK_DESC("IEEE754", "NEGATIVE", mask, index, shift);

  x.f = 1; y.f = 2;
  get_mask_descr(size, x.i, y.i, &mask, &index, &shift);
  EMIT_MASK_DESC("IEEE754", "EXPONENT", mask, index, shift);

  x.f = 1; y.f = 1;
  /* This has 20 bits */
  z.f = x.f; for (i=1; i<=20; i++) { y.f /= 2; x.f += y.f; }
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift);
  EMIT_MASK_DESC("IEEE754", "MANTISSA0", mask, index, shift);

  /* This has 32 bits */
  z.f = x.f; for (i=1; i<=32; i++) { y.f /= 2; x.f += y.f; }
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift);
  EMIT_MASK_DESC("IEEE754", "MANTISSA1", mask, index, shift);
  
  printf("#define IEEE754_MANTISSA_LENGTH 52\n\n");
}

#define USE_LONG_DOUBLE

union ldbl {
  long double f;
  unsigned int i[sizeof(long double)/sizeof(unsigned int)];
};
typedef union ldbl ldbl_t;

#define CONFIGURE_IEEE854() { \
  unsigned int mask; \
  unsigned int index; \
  unsigned int shift; \
  unsigned int split; \
  int i; \
  int size = orig_size; \
 \
  /* It is necessary to init to 0 to avoid garbage in the unused bits: */ \
  for (i = 0; i < size; i++) { x.i[i] = 0; y.i[i] = 0; z.i[i] = 0; } \
 \
  x.f = 1; y.f = -x.f; \
  get_mask_descr(size, x.i, y.i, &mask, &index, &shift); \
  EMIT_MASK_DESC("IEEE854", "NEGATIVE", mask, index, shift); \
 \
  x.f = 1; y.f = 2; \
  get_mask_descr(size, x.i, y.i, &mask, &index, &shift); \
  EMIT_MASK_DESC("IEEE854", "EXPONENT", mask, index, shift); \
 \
  x.f = 1; y.f = 1; \
 \
  /* This has mantissa0_length bits (plus a fixed bit if 31) */ \
  z.f = x.f; \
  y.f /= 2; x.f += y.f; \
  get_mask_descr(size, x.i, z.i, &mask, &index, &split); \
  split++; \
  for (i=1; i < split; i++) { y.f /= 2; x.f += y.f; } \
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift); \
  EMIT_MASK_DESC0("IEEE854", "MANTISSA0_0", mask, index, mantissa0_length - split); \
 \
  z.f = x.f; for (i=split; i<mantissa0_length; i++) { y.f /= 2; x.f += y.f; } \
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift); \
  EMIT_MASK_DESC("IEEE854", "MANTISSA0_1", mask, index, shift); \
 \
  x.i[index] = x.i[index] | (mask << shift); \
 \
  /* This has 32 bits */ \
  z.f = x.f; \
  y.f /= 2; x.f += y.f; \
  get_mask_descr(size, x.i, z.i, &mask, &index, &split); \
  split++; \
  for (i=1; i < split; i++) { y.f /= 2; x.f += y.f; } \
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift); \
  EMIT_MASK_DESC0("IEEE854", "MANTISSA1_0", mask, index, 32 - split); \
 \
  z.f = x.f; for(i=split; i < 32; i++) { y.f /= 2; x.f += y.f; } \
  get_mask_descr(size, x.i, z.i, &mask, &index, &shift); \
  EMIT_MASK_DESC("IEEE854", "MANTISSA1_1", mask, index, shift); \
 \
  printf("#define IEEE854_MANTISSA_LENGTH %d\n\n", mantissa0_length + 32); \
}

void configure_ieee854_using_754(void)
{
  volatile dbl_t x, y, z;
  int orig_size = sizeof(double) / sizeof(unsigned int);
  unsigned int mantissa0_length=20;
  CONFIGURE_IEEE854();
}

void configure_ieee854_using_854(void)
{
  volatile ldbl_t x, y, z;
  int orig_size = sizeof(long double) / sizeof(unsigned int);
  unsigned int mantissa0_length=31;
  printf("#define USE_LONG_DOUBLE\n");
  CONFIGURE_IEEE854();
}

void configure_ieee854(void) {
#if defined(USE_LONG_DOUBLE)
  int orig_size = sizeof(long double) / sizeof(unsigned int);
#else
  int orig_size = sizeof(double) / sizeof(unsigned int);
#endif

  if (orig_size==2) {
    configure_ieee854_using_754();
  }
#if defined(DARWIN) && defined(ppc)
  else if (orig_size==4)
  {
    printf("/* NOTE: in some new PPC platforms, the size of long double is 128 bits, */\n");
    printf("/* and the functions to handle that numbers has not been implemented yet */\n");
    printf("/* Using double instead long double. */\n");
    configure_ieee854_using_754();
  }
  else if (orig_size==3)
#else
    else
#endif
  {
    configure_ieee854_using_854();
  }
}

void configure__fpbits(void) {
  int bits;
  double f;
  int i, j;

  find_fp_bits(&bits);

  i = (bits*0.301029995663981); /* #significant digits, bits*log_10(2) */

  f = 0.5e-9;			/* rounding factor if above 18 */
  for (j=18; j>i; j--)
    f*=10.0;

  printf("#define ENG_FLT_SIGNIF %d\n", i);
  printf("#define ENG_FLT_ROUND %.*g\n\n", i, f);

  configure_ieee754();
  configure_ieee854();
}

/* ------------------------------------------------------------------------- */
/* configure__endianness: Obtain system endianness
 * 
 * BIGENDIAN: 1 if the system is big-endian, 0 if the system is little-endian
 */

void configure__endianness(void) {
  union {
    unsigned short as_short[2];
    unsigned int as_int;
  } u;
  u.as_int = 1;
  printf("#define BIGENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* Configure mmap */

#if defined(USE_MMAP)
#include <ciao/own_mmap.h>
#define USE_OWN_MALLOC 1
#endif

#if defined(USE_MMAP)

/* Mininum amount of reserved bits for mmap (trying to reserve bigger
   memory regions crashes the process) */
//#define MIN_MMAP_RESERVED_BITS 3
#define MIN_MMAP_RESERVED_BITS 4

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
    //fprintf(stderr, "{bug: could not find a fixed address for mmap}");
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

/* Note: modifies cflags */
void generate_defines(char *cflags) {
  char *p = cflags;
  do {
    /* move 'def' after "-D", exit otherwise */
    p = strstr(p, "-D");
    if (p == NULL) break; else p += 2;
    char *def = p;
    /* move 'p' to next definition (if exists), split string */
    p = strchr(p, ' ');
    if (p != NULL) *p++ = '\0';
    /* split 'def' at "=" sign, obtain 'val' */
    char *val = strchr(def, '=');
    if (val) {
      *val++ = '\0';
      printf("#if !defined(%s)\n#define %s %s\n#endif\n\n", def, def, val);
    } else {
      printf("#if !defined(%s)\n#define %s\n#endif\n\n", def, def);
    }
  } while (p != NULL);
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int startconfig(int argc, char **argv) {
  if (argc > 0) generate_defines(argv[1]);
  configure__endianness();
  configure__alloc();
  configure__fpbits();
  return 0;
}
