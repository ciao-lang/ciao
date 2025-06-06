/*
 *  eng_alloc.h
 *
 *  Checked memory allocation.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_ALLOC_H
#define _CIAO_ENG_ALLOC_H

/* TODO: eng_alloc.h should be type agnostic (do not use tagged_t) */

void init_alloc(void);

char *checkalloc(intmach_t size);
char *checkrealloc(char *ptr, intmach_t decr, intmach_t size);
void checkdealloc(char *ptr, intmach_t decr);

/* Memory management for objects of type Type */

#define checkalloc_TYPE(Type) \
  ((Type *)checkalloc(sizeof(Type)))

#define checkdealloc_TYPE(Type, Ptr) \
  checkdealloc((char *)(Ptr), sizeof(Type))

/*
 * Memory management for flexible structures.
 *
 * Flexible structures are structures whose last field is an array of
 * arbitrary length.
 */

#if defined(__clang__)
#define FLEXIBLE_SIZE
#define SIZEOF_FLEXIBLE_STRUCT(Type, ArrayType, ArrayLen) \
  (sizeof(Type)+(ArrayLen)*sizeof(ArrayType))
#else
#define FLEXIBLE_SIZE 1 /* Use a fixed fake length */
#define SIZEOF_FLEXIBLE_STRUCT(Type, ArrayType, ArrayLen) \
  (sizeof(Type)+((ArrayLen)-FLEXIBLE_SIZE)*sizeof(ArrayType))
#endif

#define checkalloc_FLEXIBLE(Type, ArrayType, ArrayLen) \
  ((Type *)checkalloc(SIZEOF_FLEXIBLE_STRUCT(Type, ArrayType, (ArrayLen))))

#define checkdealloc_FLEXIBLE(Type, ArrayType, ArrayLen, Ptr) \
  checkdealloc((char *)(Ptr), \
               SIZEOF_FLEXIBLE_STRUCT(Type, \
                                      ArrayType, \
                                      (ArrayLen)))

#define checkrealloc_FLEXIBLE(Type, ArrayType, ArrayLen0, ArrayLen1, Ptr) \
  ((Type *)checkrealloc((char *)(Ptr), \
                        SIZEOF_FLEXIBLE_STRUCT(Type, \
                                               ArrayType, \
                                               (ArrayLen0)), \
                        SIZEOF_FLEXIBLE_STRUCT(Type, \
                                               ArrayType, \
                                               (ArrayLen1))))

/* Memory management for flexible objects with a size field */

#define checkalloc_FLEXIBLE_S(Type, SizeField, ArrayType, ArrayLen, Ptr) ({ \
  Type *__ptr; \
  intmach_t __len = SIZEOF_FLEXIBLE_STRUCT(Type, ArrayType, (ArrayLen)); \
  __ptr = (Type *)checkalloc(__len);                               \
  __ptr->SizeField = __len;                                        \
  Ptr = __ptr;                                                     \
})

#define checkdealloc_FLEXIBLE_S(Type, SizeField, Ptr) \
  checkdealloc((char *)(Ptr), \
               (Ptr)->SizeField)

/* Memory management for arrays */

#define checkalloc_ARRAY(ArrayType, ArrayLen) \
  ((ArrayType *)checkalloc((ArrayLen) * sizeof(ArrayType)))

#define checkrealloc_ARRAY(ArrayType, ArrayLen0, ArrayLen1, Ptr) \
  ((ArrayType *)checkrealloc((char *)(Ptr), \
                             (ArrayLen0) * sizeof(ArrayType), \
                             (ArrayLen1) * sizeof(ArrayType)))

#define checkdealloc_ARRAY(ArrayType, ArrayLen, Ptr) \
  checkdealloc((char *)(Ptr), \
               (ArrayLen) * sizeof(ArrayType))

/* Memory management for raw binary buffers (e.g., for stacks) */

#define ALLOC_AREA(I) ((tagged_t *)checkalloc_ARRAY(char, (I)))
#define REALLOC_AREA(START, OLDCOUNT, NEWCOUNT) ((tagged_t *)checkrealloc_ARRAY(char, (OLDCOUNT), (NEWCOUNT), (char *)(START)))

/* --------------------------------------------------------------------------- */
/* TODO: move somewhere else? */

extern intmach_t total_mem_count;
extern intmach_t mem_prog_count;

#if defined(DEBUG_TRACE)
#define INC_MEM_PROG(SIZE) {                                     \
  if (debug_mem) {                                               \
    fprintf(stderr, "Program memory increased by %" PRIdm " bytes\n",    \
            (intmach_t)(SIZE));                                  \
  }                                                              \
  mem_prog_count = mem_prog_count + (SIZE);                      \
  }
#define DEC_MEM_PROG(SIZE) {                                     \
  if (debug_mem) {                                               \
    fprintf(stderr, "Program memory decreased by %" PRIdm " bytes\n",    \
            (intmach_t)(SIZE));                                  \
  }                                                              \
  mem_prog_count = mem_prog_count - (SIZE);                      \
  }
#else
#define INC_MEM_PROG(SIZE) { mem_prog_count = mem_prog_count + (SIZE); }
#define DEC_MEM_PROG(SIZE) { mem_prog_count = mem_prog_count - (SIZE); }
#endif

#endif /* _CIAO_ENG_ALLOC_H */
