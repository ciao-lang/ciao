/*
 *  eng_bignum.h
 *
 *  Bignum arithmetics (support for arbitrary size integers)
 *
 *  See Copyright Notice in ciaoengine.pl
 */
 
#ifndef _CIAO_ENG_BIGNUM_H
#define _CIAO_ENG_BIGNUM_H

#if !defined(OPTIM_COMP)
#include <ciao/eng.h>
#endif

#if defined(OPTIM_COMP)

#define BlobCharOffset(X,O)   ((tagged_t *)((char *)(X) + (O)))

#define BlobPushT(T, H, V) ({ \
  *((T *)(H)) = (V); \
  (H) = BlobCharOffset((H), sizeof(T)); \
})
#if 0
/* TODO: necessary?? temporary variable __t used to conform strict-aliasing rules? */
#define BlobPushT(T, H, V) ({                 \
      T *__t = (T *)(H);                        \
      *__t = (V);                               \
      (H) = BlobCharOffset((H), sizeof(T));   \
    })
#endif

#define BlobGetInt32(H) (*((int32_t *)(H)))
#if defined(ALIGN_BLOB_64)
#define BlobPushInt32(H, I) ({ \
  BlobPushT(int32_t, (H), (I)); \
  BlobPushT(int32_t, (H), 0); \
})
#else
#define BlobPushInt32(H, I) ({ \
  BlobPushT(int32_t, (H), (I)); \
})
#endif
#if IS_BIG_ENDIAN || (FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64))
/* (In big endian architectures the bignum representation of int64_t
   has words swapped) */
/* (When FORCE_ALIGNED_ACCESS is set, int64_t must be written to the
   heap as two int32_t halves) */
#define ReadInt64(H) ((int64_t)((*(uint32_t *)(H)) | ((uint64_t)(*((uint32_t *)(H)+1))<<32)))
#define BlobGetInt64(H) ReadInt64((H))
#define BlobPushInt64(H, I) ({ \
  BlobPushT(int32_t, (H), ((uint64_t)(I)&0xffffffff)); \
  BlobPushT(int32_t, (H), ((uint64_t)(I)>>32)); \
})
#define Put32InUnion64(X) ((uint64_t)((uint32_t)(X))<<32)
#else
/* In little endian architectures the int64_t can be read/written directly */
#define BlobGetInt64(H) (*((int64_t *)(H)))
#define BlobPushInt64(H, I) ({ \
  BlobPushT(int64_t, (H), (I)); \
})
#define Put32InUnion64(X) ((uint64_t)((uint32_t)(X)))
#endif

#endif /* defined(OPTIM_COMP) */

#if defined(OPTIM_COMP)
#if FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64)
#define HeapPushFlt64(H, I) ({ \
  union { \
    uint32_t i[2]; \
    flt64_t f; \
  } flt_as_int; \
  flt_as_int.f = (I); \
  BlobPushT(uint32_t, (H), flt_as_int.i[0]); \
  BlobPushT(uint32_t, (H), flt_as_int.i[1]); \
})
#else
#define HeapPushFlt64(H, I) ({ \
  BlobPushT(flt64_t, (H), (I)); \
})
#endif
#else /* !defined(OPTIM_COMP) */
#if LOG2_bignum_size == 5
#define HeapPushFlt64(H, I) ({ \
  union { \
    flt64_t f; \
    int32_t i[2]; \
  } flt_as_int; \
  flt_as_int.f = I; \
  HeapPush(h, flt_as_int.i[0]); \
  HeapPush(h, flt_as_int.i[1]); \
  })
#elif LOG2_bignum_size == 6
#define HeapPushFlt64(H, I) ({ \
  union { \
    flt64_t f; \
    tagged_t i; \
  } flt_as_int; \
  flt_as_int.f = I; \
  HeapPush(h, flt_as_int.i); \
})
#endif
#endif /* !defined(OPTIM_COMP) */

#if defined(OPTIM_COMP)
#if FORCE_ALIGNED_ACCESS && !defined(ABSMACH_OPT__pointer64)
#define UnboxFlt64(H,F) ({ \
  union { \
    uint32_t i[2]; \
    flt64_t f; \
  } flt_as_int; \
  uint32_t *ptr; \
  ptr = (uint32_t *)(H); \
  flt_as_int.i[0] = ptr[0]; \
  flt_as_int.i[1] = ptr[1]; \
  F = flt_as_int.f; \
})
#else
#define UnboxFlt64(H,F) ({ \
  F = (*((flt64_t *)(H))); \
})
#endif
#endif /* defined(OPTIM_COMP) */

bool_t bn_positive(bignum_t *x);
bignum_size_t bn_add(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_incr(bignum_t *x, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_subtract(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_decr(bignum_t *x, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_minus(bignum_t *x, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_and(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_or(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_xor(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_not(bignum_t *x, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_lshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_rshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_compare(bignum_t *x, bignum_t *y);
bignum_size_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_quotient_remainder_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_quotient_remainder_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
int bn_lsb(bignum_t *x);
int bn_msb(bignum_t *x);
int bn_popcount(bignum_t *x);
int bn_getbit(bignum_t *x, int i);
bignum_size_t bn_from_float(flt64_t f, bignum_t *z, bignum_t *zmax);
#if defined(OPTIM_COMP)
flt64_t bn_to_float(bignum_t *bn);
#endif
bignum_size_t bn_from_string(char *x, bignum_t *z, bignum_t *zmax, int base);
CVOID__PROTO(bn_to_string, bignum_t *x, int base);
bignum_size_t bn_length(bignum_t *x);

#if (!defined(OPTIM_COMP)) && BC_SCALE==2
size_t bn_scale_bc32(bignum_t *x);
int bn_canonized_length(bignum_t *x);
#endif

#if !defined(OPTIM_COMP)
/* Finish a bignum stored on top of the heap:
 *  - close the bignum and return a tagged word referencing it
 *  - or return a small integer tagged word (if it fits)
 */
// TODO: ar==2 assumes that sizeof(bignum_t) == sizeof(intmach_t) == sizeof(tagged_t)
static inline CFUN__PROTO(bn_finish, tagged_t) {
  tagged_t *h_ = G->heap_top;
  int ar_ = LargeArity(h_[0]);
  tagged_t r_;
  if (ar_ == 2 && IsInSmiValRange((intmach_t)h_[1])) {
    r_ = MakeSmall(h_[1]);
  } else {
    G->heap_top += ar_+1;
    h_[ar_] = h_[0];
    r_ = Tagp(STR,h_);
  }
  //CFUN__PROCEED(r_);
  return r_;
}
#endif

/* `Out` is the large or small int from `Str` in base `Base`.
 * `Arity` is the number of live X registers (in case of heap GC).
 * (assumes `Str` represents a number)
 */
#define StringToInt(Str, Base, Out, LiveRegs) ({                        \
  bignum_size_t req = bn_from_string((Str),                             \
                                     (bignum_t *)G->heap_top,           \
                                     (bignum_t *)Heap_Warn_Pad(CONTPAD), (Base)); \
  if (req != 0) {                                                       \
    CVOID__CALL(explicit_heap_overflow, (req*sizeof(tagged_t)+CONTPAD)*2, (LiveRegs)); \
    if (bn_from_string((Str),                                           \
                       (bignum_t *)G->heap_top,                         \
                       (bignum_t *)Heap_Warn_Pad(CONTPAD), (Base))) {   \
      SERIOUS_FAULT("miscalculated size of bignum");                    \
    }                                                                   \
  }                                                                     \
  Out = CFUN__EVAL(bn_finish);                                          \
})

/* Like StringToInt, assuming enough heap (no GC) */
/* TODO: only for qread.c */
#define StringToInt_nogc(Str, Base, Out) ({             \
  if (bn_from_string((Str),                             \
                     (bignum_t *)G->heap_top,           \
                     (bignum_t *)Heap_End, (Base))) {   \
    SERIOUS_FAULT("miscalculated heap usage");          \
  }                                                     \
  Out = CFUN__EVAL(bn_finish);                          \
})

#endif /* _CIAO_ENG_BIGNUM_H */
