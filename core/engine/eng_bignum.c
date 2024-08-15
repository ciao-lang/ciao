/*
 *  eng_bignum.c
 *
 *  Bignum arithmetics (support for arbitrary size integers)
 *
 *  See Copyright Notice in ciaoengine.pl
 *
 *  Mostly based on code from Torbjorn Granlund, Johan Andersson, and
 *  Mats Carlsson (Ref: Knuth vol. 2 sec. 4.3.1)
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_bignum.h>
#endif
#include <math.h>

#if defined(OPTIM_COMP)
#define LOG2_bignum_size 5
#endif

#if LOG2_bignum_size == 5 /* 32 bit bignum_t */
#define bignum_LSB LSB32
#define bignum_MSB MSB32
#define bignum_POPCOUNT POPCOUNT32
#elif LOG2_bignum_size == 6 /* 64 bit bignum_t */
#define bignum_LSB LSB64
#define bignum_MSB MSB64
#define bignum_POPCOUNT POPCOUNT64
#endif

#if !defined(OPTIM_COMP)
typedef int bnlen_t; /* TODO: which one is better? */
#else
typedef intmach_t bnlen_t;
#endif

#define BNMIN ((bignum_t)0) /* 0x00....00 */
#define BNMAX (~BNMIN)      /* 0xff....ff */

/* Size in bits of bignum_t and bignum_half_t */
#define BNSIZE (sizeof(bignum_t)*8)
#define HalfUnit (BNSIZE>>1)
#define HalfMask (BNMAX>>HalfUnit) /* 0x0..0f..f */

#define BignumRawLength(b) (*(functor_t *)(b))
#define BignumSetRawLength(b,v) ((*(functor_t *)(b)) = (v))
#define BignumLength(b) FunctorBignumValue(BignumRawLength(b))
#define BignumSetLength(b,l) BignumSetRawLength(b, BlobFunctorBignum(l))

/* --------------------------------------------------------------------------- */

/* Access functions to bignum units (index 1 is the first element) */
#if !defined(OPTIM_COMP)
#define Bn(X,N) X[N]
#else
#define BnStart (sizeof(functor_t)-sizeof(bignum_t))
#define Bn(X,N) (*((bignum_t *)((char *)(X)+BnStart+(N)*sizeof(bignum_t))))
#endif

/* Access functions to bignum half units (index 0 is the first element) */
#if !defined(OPTIM_COMP)
#define Bh(X,N) X[N]
#else
#define Bh(X,N) (*((bignum_half_t *)((char *)(X)+(N)*sizeof(bignum_half_t))))
#endif

#if !defined(OPTIM_COMP)
#if IS_BIG_ENDIAN
#define BignumHalf(P,I) (((bignum_half_t *)(P))[((I)+1)^1])
#else
#define BignumHalf(P,I) (((bignum_half_t *)(P))[(I)+1])
#endif
#else
#if IS_BIG_ENDIAN
#define BignumHalf(p,i) (((bignum_half_t *)((char *)(p)+BnStart))[((i)+1)^1])
#else
#define BignumHalf(p,i) (((bignum_half_t *)((char *)(p)+BnStart))[(i)+1])
#endif
#endif

#if !defined(OPTIM_COMP)
#define bn_to_intmach(P) (((bignum_t)BignumHalf(P,1))+ \
                          (((bignum_t)BignumHalf(P,2))<<HalfUnit))
#else
// TODO: wrong for 64-bits unless sizeof(bignum_t)==sizeof(intmach_t) (backport from core)
// #define BignumToIntmach(p) ((uintmach_t)BignumHalf(p,1) + (((uintmach_t)BignumHalf(p,2))<<HalfUnit))
static inline uintmach_t bn_to_intmach(bignum_t *x) {
  if (BignumRawLength(x) == BlobFunctorFix32) {
    /* todo[ts]: bn_finish should make this case impossible! transform into a runtime check? */
    return (int64_t)BlobGetInt32(HeapCharOffset(x, sizeof(functor_t)));
  } else {
    return BlobGetInt64(HeapCharOffset(x, sizeof(functor_t)));
  }
}
#endif

/* --------------------------------------------------------------------------- */

#define BignumPositive(B) WordPositive(Bn((B), BignumLength(B)))
#define WordPositive(W) ((signed_bignum_t)(W)>=0)

#if !defined(OPTIM_COMP)
#define BignumCheck(P,L,zmax) {            \
  if ((P)+(L)+2 > zmax) return (L)+2; \
  BignumSetLength(P,L); \
}
#else
#define BignumCheck(p,l,zmax) { \
  if (((char *)(p))+(l)*sizeof(bignum_t)+2*sizeof(functor_t) > (char *)(zmax)) return (l)+2*sizeof(functor_t)/sizeof(bignum_t); \
  BignumSetLength((p),(l)); \
}
#endif

/* local declarations */
static void bn_negate(bignum_t *x);
static void bn_canonize(bignum_t *x);
static void bn_mult_knuth(bignum_t *x, bnlen_t xlen, bignum_t *y, bnlen_t ylen, bignum_t *z);
static bignum_size_t bn_div_mod_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
static bignum_size_t bn_div_mod_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);

/* Only intended to be used outside this file -- e.g., by routines
   located at ciao_prolog.c */

bignum_size_t bn_length(bignum_t *x) {
  return BignumLength(x);
}

/* --------------------------------------------------------------------------- */

bool_t bn_positive(bignum_t *x) {
  return BignumPositive(x);
}

/* Pre: the result must fit in x.
   +x might occupy one more word than -x */
static void bn_negate(bignum_t *x) {
  bnlen_t i;
  int k;
  bnlen_t xlen = BignumLength(x);
  for (k=1,i=1; i <= xlen; i++)  {
    Bn(x,i) = ~Bn(x,i)+k;
    if (k && Bn(x,i) != BNMIN) k=0;
  }
}

#if BC_SCALE == 2
/* Extend a bignum for patched 32-bit bytecode when loaded in 64-bit
   mode. In bytecode we expect the size of the bignum in 64-bits to
   exactly *2 the size of bignum in 32-bit mode. Note that this may
   leave holes in bignums.

   This function assumes that there is enough space in 'x'. */

#define Word32Positive(W) ((int32_t)(W)>=0)

size_t bn_scale_bc32(bignum_t *x) {
  int xlen = BignumLength(x);
  bool_t xs = BignumPositive(x);

  /* Compute the size in bignum_bc32_t, scaling xlen with BC_SCALE and
     removing one if the upper 32-bits of the last 64-bit are just the
     propagated sign */
  int ylen = xlen * BC_SCALE;
  bignum_t t = Bn(x,xlen);
  bignum_t high = (t >> HalfUnit)&HalfMask;
  bignum_t low = t&HalfMask;
  if (xs) {
    if (high==(BNMIN&HalfMask) && Word32Positive(low)) ylen--;
  } else {
    if (high==(BNMAX&HalfMask) && !Word32Positive(low)) ylen--;
  }

  /* Force x length to be ylen and propagate sign */
  // fprintf(stderr, "bignum len %d -> %d\n", xlen, ylen);
  for (int i = xlen + 1; i <= ylen; i++) {
    Bn(x,i) = xs ? BNMIN : BNMAX;
  }
  BignumSetLength(x, ylen);

  return (ylen+1)*sizeof(bignum_t); /* LargeArity(x)*sizeof(tagged_t) */
}
#endif

#if BC_SCALE == 2
/* Length of a bignum after bn_cannoize representation (used to work
   with bignums from bn_scale_bc32()) */
int bn_canonized_length(bignum_t *x) {
  int xlen = BignumLength(x);
  bool_t xs = BignumPositive(x);
  int i;

  if (xs) {
    for (i=xlen; i > 1 && Bn(x,i)==BNMIN && WordPositive(Bn(x,i-1)); i--) {}
  } else {
    for (i=xlen; i > 1 && Bn(x,i)==BNMAX && !WordPositive(Bn(x,i-1)); i--) {}
  }
  return i;
}
#endif

static void bn_canonize(bignum_t *x) {
  bnlen_t xlen = BignumLength(x);
  bool_t xs = BignumPositive(x);
  bnlen_t i;

  if (xs) {
    for (i=xlen; i > 1 && Bn(x,i)==BNMIN && WordPositive(Bn(x,i-1)); i--) {}
  } else {
    for (i=xlen; i > 1 && Bn(x,i)==BNMAX && !WordPositive(Bn(x,i-1)); i--) {}
  }

  BignumSetLength(x,i);
}

#if !defined(OPTIM_COMP) /* TODO: any difference? */
#define BignumLarger(x,y) BignumRawLength(x) > BignumRawLength(y)
#else
#define BignumLarger(x,y) BignumLength(x) > BignumLength(y)
#endif

/* --------------------------------------------------------------------------- */

bignum_size_t bn_add(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bignum_t *w, xi, wi;
  bignum_t sign_extension; 
  bnlen_t min, max;
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);
  
  if (BignumLarger(x,y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    sign_extension = (bignum_t)ys-1;
    w = x;
  } else {
    min = BignumLength(x);
    max = BignumLength(y);
    sign_extension = (bignum_t)xs-1;
    w = y;
  }

  BignumCheck(z,max+1,zmax);

  i=0;
 add_no_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi+Bn(y,i);
    if (xi > Bn(z,i)) goto add_with_carry;
  }
  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    wi = Bn(w,i);
    Bn(z,i) = wi+sign_extension;
    if (Bn(z,i) < wi) goto se_with_carry;
  }
  goto check_sign_overflow;

 add_with_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi+Bn(y,i)+1;
    if (Bn(z,i) > xi) goto add_no_carry;
  }
  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    wi = Bn(w,i);
    Bn(z,i) = wi+sign_extension+1;
    if (Bn(z,i) > wi) goto se_no_carry;
  }
  
 check_sign_overflow:
  if (xs == ys) {
    Bn(z,max+1) = (bignum_t)xs-1;
  } else {
    Bn(z,max+1) = WordPositive(Bn(z,max))-1;
  }
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_incr(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  int k;
  bnlen_t xlen = BignumLength(x);
  bnlen_t max = xlen+BignumPositive(x);

  BignumCheck(z,max,zmax);

  for (i=1, k=1; i<=xlen; i++) {
    if ((Bn(z,i)=Bn(x,i)+k)) k=0;
  }

  if (BignumPositive(x)) {
    Bn(z,max) = BNMIN+k;
  }

  bn_canonize(z);
  return 0;
}

// // TODO: just copy, not needed now (JF)
// bignum_size_t bn_plus(bignum_t *x, bignum_t *z, bignum_t *zmax) {
//   bnlen_t xlen = BignumLength(x);
//   bnlen_t i;
//
//   BignumCheck(z,xlen,zmax);
//   for (i=1; i<=xlen; i++) {
//     Bn(z,i) = Bn(x,i);
//   }
// 
//   return 0;
// }

/* --------------------------------------------------------------------------- */

bignum_size_t bn_subtract(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bignum_t *w, xi;
  bignum_t sign_extension;
  bnlen_t min, max;
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);

  if (BignumLarger(x,y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    sign_extension = (bignum_t)ys-1;
    w = x;
  } else {
    min = BignumLength(x);
    max = BignumLength(y);
    sign_extension = (bignum_t)xs-1;
    w = y;
  }

  BignumCheck(z,max+1,zmax);

  i=0;
 subtract_no_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi-Bn(y,i);
    if (Bn(z,i) > xi) goto subtract_with_carry;
  }

  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = Bn(x,i);
      Bn(z,i) = xi-sign_extension;
      if (Bn(z,i) > xi) goto se_with_carry;
    } else {
      Bn(z,i) = sign_extension-Bn(y,i);
      if (Bn(z,i) > sign_extension) goto se_with_carry;
    }
  }
  goto check_sign_overflow;

 subtract_with_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi-Bn(y,i)-1;
    if (xi > Bn(z,i)) goto subtract_no_carry;
  }
  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = Bn(x,i);
      Bn(z,i) = xi-sign_extension-1;
      if (xi > Bn(z,i)) goto se_no_carry;
    } else {
      Bn(z,i) = sign_extension-Bn(y,i)-1;
      if (sign_extension > Bn(z,i)) goto se_no_carry;
    }
  }

 check_sign_overflow:
  if (xs != ys) {
    Bn(z,max+1) = (bignum_t)xs-1;
  } else {
    Bn(z,max+1) = WordPositive(Bn(z,max))-1;
  }
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_decr(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  bnlen_t i, k;
  bnlen_t xlen = BignumLength(x);
  bnlen_t max = xlen+(!BignumPositive(x));
  
  BignumCheck(z,max,zmax);
  
  for (i=1, k=1; i<=xlen; i++) {
    if (~(Bn(z,i)=Bn(x,i)-k)) k=0;
  }
  
  if (!BignumPositive(x)) {
    Bn(z,max) = BNMAX;
  }
  
  bn_canonize(z);
  return 0;
}

/* --------------------------------------------------------------------------- */

bignum_size_t bn_minus(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  bnlen_t xlen = BignumLength(x);
  bnlen_t i;

  if (BignumPositive(x)) {
    BignumCheck(z,xlen,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
  } else {
    BignumCheck(z,xlen+1,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
    Bn(z,xlen+1) = BNMAX;
  }
  bn_negate(z);
  bn_canonize(z);
  return 0;
}

/* --------------------------------------------------------------------------- */

bignum_size_t bn_and(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bnlen_t min, max;
  bignum_t mask;

  if (BignumLarger(x,y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    mask = (bignum_t)BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min = BignumLength(x);
    max = BignumLength(y);
    mask = (bignum_t)BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);

  for (i = 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)&Bn(y,i);
  }

  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)&mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_or(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bnlen_t min, max;
  bignum_t mask;

  if (BignumLarger(x,y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    mask = (bignum_t)BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min = BignumLength(x);
    max = BignumLength(y);
    mask = (bignum_t)BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);

  for (i = 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)|Bn(y,i);
  }

  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)|mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_xor(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bnlen_t min, max;
  bignum_t mask;

  if (BignumLarger(x,y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    mask = (bignum_t)BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min = BignumLength(x);
    max = BignumLength(y);
    mask = (bignum_t)BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);

  for (i = 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)^Bn(y,i);
  }

  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)^mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_not(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  bnlen_t i;
  bnlen_t xlen = BignumLength(x);

  BignumCheck(z,xlen,zmax);
  
  for (i = 1; i <= xlen; i++) {
    Bn(z,i) = ~Bn(x,i);
  }

  /* z must be canonical now */
  return 0;
}

/* --------------------------------------------------------------------------- */

/* This was used to communicate a value from lsh_internal and rsh_internal to
   bn_lshift and bn_rshift, but cannot be passed through bn_call */

/* TODO: pass dist as intmach_t instead of bignum_t, implement
   bn_call_bignum_int */

/*bignum_t bn_shift_dist; */

bignum_size_t bn_lshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax) {
  bignum_t xi;
  bool_t xs = BignumPositive(x);
  bnlen_t xlen = BignumLength(x);
  intmach_t shift = bn_to_intmach(dist);
  bnlen_t div, rem;
  bnlen_t i;

  div = shift >> LOG2_bignum_size;
  rem = shift & ((1<<LOG2_bignum_size)-1);

  BignumCheck(z,xlen+div+(rem>0),zmax);

  /* first perform the `long' shift, if any */
  for (i=1; i<=div; i++) {
    Bn(z,i) = BNMIN;
  }

  /* Then perform the short shift */
  if (rem == 0) { /* copy */
    for (i=1; i <= xlen; i++) {
      Bn(z,div+i) = Bn(x,i);
    }
  } else {
    bnlen_t mer=BNSIZE-rem;
    bignum_t carry=0;

    for (i=1; i<=xlen; i++) {
      xi = Bn(x,i);
      Bn(z,div+i) = xi<<rem | carry;
      carry = xi>>mer;
    }
    Bn(z,div+i) = ((bignum_t)xs-1)<<rem | carry;
  }

  bn_canonize(z);
  return 0;
}

/* TODO: pass dist as intmach_t instead of bignum_t, implement
   bn_call_bignum_int */
bignum_size_t bn_rshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax) {
  bignum_t xi;
  bool_t xs = BignumPositive(x);
  bnlen_t xlen = BignumLength(x);
  intmach_t shift = bn_to_intmach(dist);
  intmach_t div, rem;
  bnlen_t i;

  div = shift >> LOG2_bignum_size;
  rem = shift & ((1<<LOG2_bignum_size)-1);

  if (xlen-div<1) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = (bignum_t)xs-1;
    return 0;
  }

  BignumCheck(z,xlen-div,zmax);

  if (rem==0) {
    for (i=xlen-div; i>=1; i--) {
      Bn(z,i) = Bn(x,div+i);
    }
  } else {
    bnlen_t mer=BNSIZE-rem;
    bignum_t carry=((bignum_t)xs-1)<<mer;

    for (i=xlen-div; i>=1; i--) {
      xi = Bn(x,div+i);
      Bn(z,i) = xi>>rem | carry;
      carry = xi<<mer;
    }
  }

  bn_canonize(z);
  return 0;
}

/* --------------------------------------------------------------------------- */

bignum_size_t bn_compare(bignum_t *x, bignum_t *y) {
  bnlen_t xlen = BignumLength(x);
  bnlen_t ylen = BignumLength(y);
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);

  if (xs != ys) {
    return (xs ? 1 : -1);
  } else if (xlen != ylen) {
    return (xs^(xlen>ylen) ? -1 : 1);
  } else {
    bnlen_t i = xlen+1;
    while (--i) {
      if (Bn(x,i)!=Bn(y,i)) {
        return (Bn(x,i)<Bn(y,i) ? -1 : 1);
      }
    }
    return 0;
  }
}

/* --------------------------------------------------------------------------- */

/* y is shorter than x */
static void bn_mult_knuth(bignum_t *x, bnlen_t xlen,
                          bignum_t *y, bnlen_t ylen,
                          bignum_t *z) {
  bnlen_t i, j;
  bignum_half_t yj;

  for (i=1; i<=xlen+ylen; i++) Bn(z,i) = 0;

  xlen <<= 1;
  ylen <<= 1;
  for (j=1; j<=ylen; j++) {
    if ((yj=BignumHalf(y,j))) {
      bignum_t t=0;
      for (i=1; i<=xlen; i++) {
        t = (bignum_t)BignumHalf(z,i+j-1) + (bignum_t)BignumHalf(x,i)*yj + (t>>HalfUnit);
        BignumHalf(z,i+j-1) = t&HalfMask;
      }
      BignumHalf(z,xlen+j) = (t>>HalfUnit);
    }
  }
}

bignum_size_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  bnlen_t xlen = BignumLength(x);
  bnlen_t ylen = BignumLength(y);

  BignumCheck(z,xlen+ylen,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (xlen>ylen) {
    bn_mult_knuth(x,xlen,y,ylen,z);
  } else {
    bn_mult_knuth(y,ylen,x,xlen,z);
  }
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (sx^sy) bn_negate(z);
  bn_canonize(z); 
  return 0;
}

/* bool_t bn_quotient_wanted; */

// void prbignum(const char *n, bignum_half_t *u, int len) {
//   int i;
//   fprintf(stderr, "%s:[", n);
//   for (i=0; i<len; i++) {
//     if (i != 0) fprintf(stderr, ".");
//     fprintf(stderr, "%08x", BignumHalf(u,i+1));
//   }
//   fprintf(stderr, "]\n");
// }

/* --------------------------------------------------------------------------- */

static bignum_size_t bn_div_mod_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t d;
  bnlen_t xlen = BignumLength(x);
  bnlen_t ylen = BignumLength(y);
  bnlen_t ulen = xlen<<1;
  bnlen_t vlen = ylen<<1;
  bnlen_t i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t *u; /* dividend, size=ulen+1 */
  bignum_half_t *v; /* divisor, size=vlen */
  bignum_half_t *q; /* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;

  while (!BignumHalf(x,ulen) && ulen>1) ulen--;
  while (!BignumHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = 0;
    return 0;
  } else if (vlen==1) {
    /* special case: a simple loop */
    v1 = BignumHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += BignumHalf(x,i);
      BignumHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,(ulen+2)>>1,zmax);
    BignumHalf(z,ulen+1) = 0;
    BignumHalf(z,ulen+2) = 0;
    return 0;
  }
  // prbignum("x", (bignum_half_t *)x, ulen);
  // prbignum("y", (bignum_half_t *)y, vlen);
  BignumCheck(z,(3*ulen+4)>>1,zmax);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1; // TODO: OC had 2

  /* Normalize. */
  v1 = BignumHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
    //    fprintf(stderr, ".. v1: %08x\n", v1);
  }

  /* TODO: factorize this code */
  for (carry=0, i=0; i<ulen; i++) {
    carry += (bignum_t)BignumHalf(x,i+1)<<d;
    Bh(u,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  Bh(u,ulen) = carry;
  // prbignum("u", u-2, ulen+1);
  for (carry=0, i=0; i<vlen; i++) {
    carry += (bignum_t)BignumHalf(y,i+1)<<d;
    Bh(v,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  // prbignum("v", v-2, vlen);
  
  v1 = Bh(v,vlen-1), v2 = Bh(v,vlen-2);
  for (j=ulen; j>=vlen; j--) {
    /* Calculate Bh(q,j). */
    carry = ((bignum_t)Bh(u,j)<<HalfUnit) + Bh(u,j-1);
    if (Bh(u,j)==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }

    v1qhat = (bignum_t)v1*(bignum_t)qj;
    v2qhat = (bignum_t)v2*(bignum_t)qj;
    while (carry-v1qhat < ((bignum_t)1<<HalfUnit) &&
           v2qhat > ((carry-v1qhat)<<HalfUnit)+Bh(u,j-2)) {
      qj--;
      v1qhat -= v1;
      v2qhat -= v2;
    }

    /* Multiply and subtract. */
    if ((Bh(q,j)=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
        carry = (bignum_t)Bh(u,k) - (bignum_t)Bh(v,i)*qj - carry;
        Bh(u,k) = carry & HalfMask;
        carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      }
      carry = (bignum_t)Bh(u,k) - v2qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      k++;
      carry = (bignum_t)Bh(u,k) - v1qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      carry = (bignum_t)Bh(u,j) - carry;
      Bh(u,j) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,j)-carry)>>HalfUnit;
      if (carry) {
        Bh(q,j)--;
        for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
          carry += (bignum_t)Bh(u,k) + (bignum_t)Bh(v,i);
          Bh(u,k) = carry & HalfMask;
          carry = carry>>HalfUnit;
        }
        Bh(u,j) += carry;
      }
    }
  }
  /* Bh(q,vlen .. ulen) is the desired quotient. */
  BignumSetLength(z,(ulen-vlen+3)>>1);
  for (i=1, k=vlen; k<=ulen; i++, k++) {
    BignumHalf(z,i) = Bh(q,k);
  }
  BignumHalf(z,i++) = 0;
  BignumHalf(z,i++) = 0;
  return 0;
}

static bignum_size_t bn_div_mod_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bnlen_t d;
  bnlen_t xlen = BignumLength(x);
  bnlen_t ylen = BignumLength(y);
  bnlen_t ulen = xlen<<1;
  bnlen_t vlen = ylen<<1;
  bnlen_t i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t *u; /* dividend, size=ulen+1 */
  bignum_half_t *v; /* divisor, size=vlen */
  bignum_half_t *q; /* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;

  while (!BignumHalf(x,ulen) && ulen>1) ulen--;
  while (!BignumHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,xlen,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
    return 0;
  } else if (vlen==1) {
    /* special case: a simple loop */
    v1 = BignumHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += BignumHalf(x,i);
      BignumHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,1,zmax);
    Bn(z,1) = carry>>HalfUnit;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1,zmax);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;

  /* Normalize. */
  v1 = BignumHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
  }
  for (carry=0, i=0; i<ulen; i++) {
    carry += (bignum_t)BignumHalf(x,i+1)<<d;
    Bh(u,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  Bh(u,ulen) = carry;
  for (carry=0, i=0; i<vlen; i++) {
    carry += (bignum_t)BignumHalf(y,i+1)<<d;
    Bh(v,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }

  v1 = Bh(v,vlen-1), v2 = Bh(v,vlen-2);
  for (j=ulen; j>=vlen; j--) {
    /* Calculate Bh(q,j). */
    carry = ((bignum_t)Bh(u,j)<<HalfUnit) + Bh(u,j-1);
    if (Bh(u,j)==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }

    v1qhat = (bignum_t)v1*(bignum_t)qj;
    v2qhat = (bignum_t)v2*(bignum_t)qj;
    while (carry-v1qhat < ((bignum_t)1<<HalfUnit) &&
           v2qhat > ((carry-v1qhat)<<HalfUnit)+Bh(u,j-2)) {
      qj--;
      v1qhat -= v1;
      v2qhat -= v2;
    }

    /* Multiply and subtract. */
    if ((Bh(q,j)=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
        carry = (bignum_t)Bh(u,k) - (bignum_t)Bh(v,i)*qj - carry;
        Bh(u,k) = carry & HalfMask;
        carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      }
      carry = (bignum_t)Bh(u,k) - v2qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      k++;
      carry = (bignum_t)Bh(u,k) - v1qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,k)-carry)>>HalfUnit;
      carry = (bignum_t)Bh(u,j) - carry;
      Bh(u,j) = carry & HalfMask;
      carry = ((bignum_t)Bh(u,j)-carry)>>HalfUnit;
      if (carry) {
        Bh(q,j)--;
        for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
          carry += (bignum_t)Bh(u,k) + (bignum_t)Bh(v,i);
          Bh(u,k) = carry & HalfMask;
          carry = carry>>HalfUnit;
        }
        Bh(u,j) += carry;
      }
    }
  }
  /* Bh(u,0 .. vlen-1)>>d is the desired remainder. */
  BignumSetLength(z,(vlen+2)>>1);
  v1qhat = ((bignum_t)1<<d)-1;
  for (carry=0, i=vlen; i>0; i--) {
    carry += Bh(u,i-1);
    BignumHalf(z,i) = carry>>d;
    carry = (carry&v1qhat)<<HalfUnit;
  }
  BignumHalf(z,vlen+1) = 0;
  BignumHalf(z,vlen+2) = 0;
  return 0;
}

bignum_size_t bn_quotient_remainder_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  bignum_size_t value;

  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (sx^sy) {
      bn_negate(z);
    }
    bn_canonize(z);
  }
  return value;
}

bignum_size_t bn_quotient_remainder_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  bignum_size_t value;
  
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_not_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (!sx) {
      bn_negate(z);
    }
    bn_canonize(z);
  }
  return value;
}

/* --------------------------------------------------------------------------- */
/* Bit manipulation operations */

/* bignum_t as bits */
#define BIGNUM_BITSIZE (sizeof(bignum_t)*8)
#define BIGNUM_BITEMPTY ((bignum_t)0)
#define BIGNUM_BITFULL (~(bignum_t)0)

/* Compute the least significant bit of a bignum_t */
int bn_lsb(bignum_t *x) {
  bnlen_t len = BignumLength(x);
  int k = 0;
  for (int i=1; i <= len; i++) {
    if (Bn(x,i) != BIGNUM_BITEMPTY) return (k + bignum_LSB(Bn(x,i)));
    k += BIGNUM_BITSIZE;
  }
  return 0;
}

/* Compute the most significant bit of a bignum_t */
int bn_msb(bignum_t *x) {
  bnlen_t len = BignumLength(x);
  int k = len*BIGNUM_BITSIZE;
  for (int i=len; i > 0; i--) {
    k -= BIGNUM_BITSIZE;
    if (Bn(x,i) != BIGNUM_BITFULL) {
      if (Bn(x,i) == BIGNUM_BITEMPTY) {
        return (k - 1); 
      } else {
        return (k + bignum_MSB(Bn(x,i)));
      }
    }
  }
  return 0;
}

/* Compute the number of bits set in a bignum_t */
int bn_popcount(bignum_t *x) {
  bnlen_t len = BignumLength(x);
  int k = 0;
  for (int i=1; i <= len; i++) {
    k += bignum_POPCOUNT(Bn(x,i));
  }
  return k;
}

/* --------------------------------------------------------------------------- */

#define FLTBITS 64

bignum_size_t bn_from_float(flt64_t f, bignum_t *z, bignum_t *zmax) {
  flt64_t norm = 4294967296.0;  /* 2**(FLTBITS/2) */
  flt64_t norm2 = norm*norm; /* 2**FLTBITS */
  bnlen_t i, exp, div, rem, zlen;
  bool_t sx = TRUE;
#if LOG2_bignum_size == 5
  bignum_t uhigh, ulow;
#elif LOG2_bignum_size == 6
  bignum_t u;
#endif

  if (signbit(f)) {
    sx = FALSE;
    f = -f;
  }

  if (f < 1.0) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = 0;
    goto ret;
  } else if (f != f || f == f/2.0) { /* catch NaN, Infinity */
    SERIOUS_FAULT("bn_from_float: NaN or Infinity");
  }

  /* normalize */
  exp = FLTBITS;
  while (f >= norm2) {
    exp+=FLTBITS; f /= norm2;
  }
  norm2 /= 2.0;
  while (f < norm2) {
    --exp;
    f *= 2.0;
  }

  /* NOTE: Do not use integer division here!
       (-31)>>5==-1
       (-31)/32==0  in C99+ */
  zlen = ((exp-1)>>LOG2_bignum_size)+2; /* ensure there is room for sign bit */
  div = (exp-FLTBITS)>>LOG2_bignum_size;
  rem = exp & ((1<<LOG2_bignum_size)-1);
  BignumCheck(z,zlen,zmax);
  for (i=1; i<=zlen; i++) Bn(z,i) = 0;

#if LOG2_bignum_size == 5
  /* turn off high bit of uhigh since it causes trouble on certain machines */
  f -= norm2;
  uhigh = f/norm;
  ulow = f-norm*uhigh;
  uhigh += (bignum_t)1<<(sizeof(bignum_t)*8-1);
  if (rem==0) {
    if (div>=0) Bn(z,div+1) = ulow;
    if (div>=-1) Bn(z,div+2) = uhigh;
  } else {
    if (div>=0) Bn(z,div+1) = ulow<<rem;
    if (div>=-1) Bn(z,div+2) = (ulow>>(BNSIZE-rem))+(uhigh<<rem);
    if (div>=-2) Bn(z,div+3) = uhigh>>(BNSIZE-rem);
  }
#elif LOG2_bignum_size == 6
  f -= norm2;
  u = f;
  u += (bignum_t)1<<(sizeof(bignum_t)*8-1);
  if (rem==0) {
    if (div>=0) Bn(z,div+1) = u;
  } else {
    if (div>=0) Bn(z,div+1) = u<<rem;
    if (div>=-1) Bn(z,div+2) = u>>(BNSIZE-rem);
  }
#endif
 ret:
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}

flt64_t bn_to_float(bignum_t *bn) {
  intmach_t i = BignumLength(bn);
  flt64_t f = (signed_bignum_t)Bn(bn,i);
  while (i > 1) {
    const bignum_t sbit = (bignum_t)1<<(8*sizeof(bignum_t)-1);
#if LOG2_bignum_size == 5
    const flt64_t norm2 = 4294967296.0; /* 2**32 */
    const flt64_t norm2m1 = 2147483648.0; /* 2**31 */
#elif LOG2_bignum_size == 6
    const flt64_t p32 = 4294967296.0; /* 2**32 */
    const flt64_t norm2 = p32*p32; /* 2**64 */
    const flt64_t norm2m1 = p32*2147483648.0; /* 2**63 */
#endif
    i--;
    bignum_t u = Bn(bn,i);
    if (u & sbit) { /* trouble on some machines */
      f = f*norm2 + norm2m1 + (u - sbit);
    } else {
      f = f*norm2 + u;
    }
  }
  return f;
}

/* Pre: x is a syntactically correct string denoting an integer */
bignum_size_t bn_from_string(char *x, bignum_t *z, bignum_t *zmax, int base) {
  bool_t sx;
  bnlen_t j;
  bnlen_t zlen;
  char cur;
  bignum_t t, digit;
  /* unsigned int radix = GetSmall(current_radix); */

  if (*x=='+') {
    x++;
    sx = TRUE;
  } else if (*x=='-') {
    x++;
    sx = FALSE;
  } else {
    sx = TRUE;
  }

  zlen = 2;
  BignumCheck(z,1,zmax);
  Bn(z,1) = 0; /* always keep a zero pad word */
  for (cur = *x++; cur; cur = *x++) {
    digit = (cur>='a' ? cur-'a'+10 : cur>='A' ? cur-'A'+10 : cur-'0');
    for (j=1; j<zlen; j++) {
      t = (bignum_t)base*BignumHalf(z,j) + digit;
      BignumHalf(z,j) = t&HalfMask;
      digit = t>>HalfUnit;
    }
    if (digit) {
      zlen += 2;
      BignumCheck(z,zlen>>1,zmax);
      BignumHalf(z,j) = digit;
      BignumHalf(z,j+1) = 0;
      BignumHalf(z,j+2) = 0;
      BignumHalf(z,j+3) = 0;
    }
  }
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}

CVOID__PROTO(bn_to_string, bignum_t *x, int base) {
  bnlen_t j, k;
  bnlen_t xlen, slen, dlen;
  bignum_t r, digit, divisor;
  bool_t sx = BignumPositive(x);
  char hibase = 'a'-10;
  bignum_half_t *work;
  char *c0, *c, d;

  if (base<0) {
    hibase = 'A'-10;
    base = -base;
  }
  xlen = BignumLength(x)<<1;

  /* compute divisor = base**N such that divisor <= 1<<16. */
  r = ((bignum_t)1<<HalfUnit)/base;
  for (dlen=1, divisor=base; divisor<=r; dlen++) {
    divisor *= base;
  }

  /* string length <= (words+1)*ceiling(digits/word) */
  slen = ALIGN_TO(sizeof(bignum_t), (xlen+1)*(dlen+1)+1) + (xlen<<1);
  GET_ATOM_BUFFER2(c, slen);
  work = (bignum_half_t *)(c+slen-(xlen<<1));
  if (!sx) {
    *c++ = '-';
    for (k=1, j=0; j<xlen; j++) {
      Bh(work,j) = ~BignumHalf(x,j+1)+k;
      k &= !Bh(work,j);
    }
  } else {
    for (j=0; j<xlen; j++) {
      Bh(work,j) = BignumHalf(x,j+1);
    }
  }
  while (xlen>0 && !Bh(work,xlen-1)) {
    xlen--;
  }

  while (xlen>0) {
    for (j=xlen-1, r=0; j >= 0; j--) {
      digit = ((bignum_t)r<<HalfUnit) + Bh(work,j);
      Bh(work,j) = digit/divisor;
      r = digit%divisor;
    }
    for (j=dlen; j>0; j--) {
      digit = r%base;
      *c++ = (digit<10 ? '0'+digit : hibase+digit);
      r /= base;
    }
    while (xlen>0 && !Bh(work,xlen-1)) {
      xlen--;
    }
  }

  do {
    c--;
  } while (c[0]=='0');
  c[1] = 0;
  for (c0=Atom_Buffer+1-sx; c0<c; c0++, c--) {
    d = *c0;
    *c0 = *c;
    *c = d;
  }
}
