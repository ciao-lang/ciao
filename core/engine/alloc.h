/*
 *  alloc.h
 *
 *  Checked memory allocation.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#ifndef _CIAO_ALLOC_H
#define _CIAO_ALLOC_H

/* TODO: alloc.h should be type agnostic (do not use tagged_t) */

extern char *tryalloc_errstring;

extern intmach_t total_mem_count;

void init_alloc(void);

tagged_t *tryalloc(intmach_t size);
tagged_t *tryrealloc(tagged_t *ptr, intmach_t decr, intmach_t size);

tagged_t *checkalloc(intmach_t size);
tagged_t *checkrealloc(tagged_t *ptr, intmach_t decr, intmach_t size);
void checkdealloc(tagged_t *ptr, intmach_t decr);

/* Memory management for objects of type Type */

#define checkalloc_TYPE(Type) \
  ((Type *)checkalloc(sizeof(Type)))

#define checkdealloc_TYPE(Type, Ptr) \
  checkdealloc((tagged_t *)(Ptr), sizeof(Type))

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
  checkdealloc((tagged_t *)(Ptr), \
	       SIZEOF_FLEXIBLE_STRUCT(Type, \
				      ArrayType, \
				      (ArrayLen)))

#define checkrealloc_FLEXIBLE(Type, ArrayType, ArrayLen0, ArrayLen1, Ptr) \
  ((Type *)checkrealloc((tagged_t *)(Ptr), \
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
  __ptr = (Type *)checkalloc(__len);				   \
  __ptr->SizeField = __len;					   \
  Ptr = __ptr;							   \
})

#define checkdealloc_FLEXIBLE_S(Type, SizeField, Ptr) \
  checkdealloc((tagged_t *)(Ptr), \
	       (Ptr)->SizeField)

/* Memory management for arrays */

#define checkalloc_ARRAY(ArrayType, ArrayLen) \
  ((ArrayType *)checkalloc((ArrayLen) * sizeof(ArrayType)))

#define checkrealloc_ARRAY(ArrayType, ArrayLen0, ArrayLen1, Ptr) \
  ((ArrayType *)checkrealloc((tagged_t *)(Ptr), \
                             (ArrayLen0) * sizeof(ArrayType), \
			     (ArrayLen1) * sizeof(ArrayType)))

#define checkdealloc_ARRAY(ArrayType, ArrayLen, Ptr) \
  checkdealloc((tagged_t *)(Ptr), \
	       (ArrayLen) * sizeof(ArrayType))

#endif /* _CIAO_ALLOC_H */
