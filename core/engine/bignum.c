/*
 *  bignum.c
 *
 *  Bignum arithmetics (support for arbitrary size integers)
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Authors:
 *    Torbjorn Granlund, Johan Andersson, and Mats Carlsson
 *    Ref: Knuth vol. 2 sec. 4.3.1
 */

#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/alloc.h>
#include <ciao/bignum.h>
#include <math.h>

static void bn_negate(bignum_t *x);
static void bn_canonize(bignum_t *x);
static void bn_mult_knuth(bignum_t *x, int xlen, bignum_t *y, int ylen, bignum_t *z);
static bignum_size_t bn_div_mod_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
static bignum_size_t bn_div_mod_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);

#define BNMIN ((bignum_t)0) /* 0x00....00 */
#define BNMAX (~BNMIN)      /* 0xff....ff */

/* Size in bits of bignum_t and bignum_half_t */
#define FullUnit (8*sizeof(bignum_t))
#define HalfUnit (8*sizeof(bignum_half_t))

#define HalfMask (BNMAX>>HalfUnit) /* 0x0..0f..f */

#define BignumRawLength(B) (*((tagged_t *)B))

#define BignumLength(B) GetBignumLength(BignumRawLength(B))

#define SetBignumLength(B,L) (BignumRawLength(B)=MakeLength(L))

#define BignumPositive(B) WordPositive((B)[BignumLength(B)])

#define WordPositive(W) ((signed_bignum_t)(W)>=0)

#define BignumCheck(P,L) { 	      \
  if ((P)+(L)+2 > zmax) return (L)+2; \
  SetBignumLength(P,L); \
}

#if BIGENDIAN
#define GetHalf(P,I) (((bignum_half_t *)(P))[((I)+1)^1])
#else
#define GetHalf(P,I) (((bignum_half_t *)(P))[(I)+1])
#endif

#define GetFull(P) (((bignum_t)GetHalf(P,1))+ \
                   (((bignum_t)GetHalf(P,2))<<HalfUnit))

/* Only intended to be used outside this file -- e.g., by routines located
   at ciao_prolog.c */

intmach_t bn_length(bignum_t *x) {
  return BignumLength(x);
}

bool_t bn_positive(bignum_t *x) {
  return BignumPositive(x);
}

/* PRECONDITION: The result must fit in x.
   +x might occupy one more word than -x */
static void bn_negate(bignum_t *x) {
  int i, k;
  int xlen = BignumLength(x);

  for (k=1,i=1; i <= xlen; i++)  {
    x[i] = ~x[i]+k;
    if (k && x[i] != BNMIN) k=0;
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
  bignum_t t = x[xlen];
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
    x[i] = xs ? BNMIN : BNMAX;
  }
  SetBignumLength(x, ylen);

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
    for (i=xlen; i > 1 && x[i]==BNMIN && WordPositive(x[i-1]); i--) {}
  } else {
    for (i=xlen; i > 1 && x[i]==BNMAX && !WordPositive(x[i-1]); i--) {}
  }
  return i;
}
#endif

static void bn_canonize(bignum_t *x) {
  int xlen = BignumLength(x);
  bool_t xs = BignumPositive(x);
  int i;

  if (xs) {
    for (i=xlen; i > 1 && x[i]==BNMIN && WordPositive(x[i-1]); i--) {}
  } else {
    for (i=xlen; i > 1 && x[i]==BNMAX && !WordPositive(x[i-1]); i--) {}
  }

  SetBignumLength(x,i);
}

bignum_size_t bn_add(bignum_t *x,
		     bignum_t *y,
		     bignum_t *z,
		     bignum_t *zmax) {
  int i;
  bignum_t *w, xi, wi;
  bignum_t sign_extension; 
  int min, max;
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);
  
  if (BignumRawLength(x) > BignumRawLength(y)) {
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

  BignumCheck(z,max+1);

  i=0;
 add_no_carry:
  for (i++; i<=min; i++) {
    xi = x[i];
    z[i] = xi+y[i];
    if (xi > z[i]) goto add_with_carry;
  }
  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    wi = w[i];
    z[i] = wi+sign_extension;
    if (z[i] < wi) goto se_with_carry;
  }
  goto check_sign_overflow;

 add_with_carry:
  for (i++; i<=min; i++) {
    xi = x[i];
    z[i] = xi+y[i]+1;
    if (z[i] > xi) goto add_no_carry;
  }
  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    wi = w[i];
    z[i] = wi+sign_extension+1;
    if (z[i] > wi) goto se_no_carry;
  }
  
 check_sign_overflow:
  if (xs == ys) {
    z[max+1] = (bignum_t)xs-1;
  } else {
    z[max+1] = WordPositive(z[max])-1;
  }
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_incr(bignum_t *x,
		      bignum_t *ignore,
		      bignum_t *z,
		      bignum_t *zmax) {
  int i, k;
  int xlen = BignumLength(x);
  int max = xlen+BignumPositive(x);

  BignumCheck(z,max);

  for (i=1, k=1; i<=xlen; i++) {
    if ((z[i]=x[i]+k)) k=0;
  }

  if (BignumPositive(x)) {
    z[max] = BNMIN+k;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_plus(bignum_t *x,
		      bignum_t *ignore,
		      bignum_t *z,
		      bignum_t *zmax) {
  int xlen = BignumLength(x);
  int i;

  BignumCheck(z,xlen);
  for (i=1; i<=xlen; i++) {
    z[i] = x[i];
  }

  return 0;
}

bignum_size_t bn_subtract(bignum_t *x,
			  bignum_t *y,
			  bignum_t *z,
			  bignum_t *zmax) {
  int i;
  bignum_t *w, xi;
  bignum_t sign_extension;
  int min, max;
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);
  
  if (BignumRawLength(x) > BignumRawLength(y)) {
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

  BignumCheck(z,max+1);

  i=0;
 subtract_no_carry:
  for (i++; i<=min; i++) {
    xi = x[i];
    z[i] = xi-y[i];
    if (z[i] > xi) goto subtract_with_carry;
  }

  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = x[i];
      z[i] = xi-sign_extension;
      if (z[i] > xi)
	goto se_with_carry;
    } else {
      z[i] = sign_extension-y[i];
      if (z[i] > sign_extension)
	goto se_with_carry;
    }
  }
  goto check_sign_overflow;

 subtract_with_carry:
  for (i++; i<=min; i++) {
    xi = x[i];
    z[i] = xi-y[i]-1;
    if (xi > z[i]) goto subtract_no_carry;
  }
  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = x[i];
      z[i] = xi-sign_extension-1;
      if (xi > z[i]) goto se_no_carry;
    } else {
      z[i] = sign_extension-y[i]-1;
      if (sign_extension > z[i]) goto se_no_carry;
    }
  }
 check_sign_overflow:
  if (xs != ys) {
    z[max+1] = (bignum_t)xs-1;
  } else {
    z[max+1] = WordPositive(z[max])-1;
  }
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_decr(bignum_t *x,
		      bignum_t *ignore,
		      bignum_t *z,
		      bignum_t *zmax) {
  int i, k;
  int xlen = BignumLength(x);
  int max = xlen+(!BignumPositive(x));
  
  BignumCheck(z,max);
  
  for (i=1, k=1; i<=xlen; i++) {
    if (~(z[i]=x[i]-k)) k=0;
  }
  
  if (!BignumPositive(x)) {
    z[max] = BNMAX;
  }
  
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_minus(bignum_t *x,
		       bignum_t *ignore,
		       bignum_t *z,
		       bignum_t *zmax) {
  int xlen = BignumLength(x);
  int i;

  if (BignumPositive(x)) {
    BignumCheck(z,xlen);
    for (i=1; i<=xlen; i++) {
      z[i] = x[i];
    }
  } else {
    BignumCheck(z,xlen+1);
    for (i=1; i<=xlen; i++) {
      z[i] = x[i];
    }
    z[xlen+1] = BNMAX;
  }
  bn_negate(z);
  bn_canonize(z);
  return 0;
}

bignum_size_t bn_and(bignum_t *x,
		     bignum_t *y,
		     bignum_t *z,
		     bignum_t *zmax) {
  int i;
  int min, max;
  bignum_t mask;

  if (BignumRawLength(x) > BignumRawLength(y)) {
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

  BignumCheck(z,max);

  for (i = 1; i <= min; i++) {
    z[i] = x[i]&y[i];
  }

  for (; i <= max; i++) {
    z[i] = x[i]&mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_or(bignum_t *x,
		    bignum_t *y,
		    bignum_t *z,
		    bignum_t *zmax) {
  int i;
  int min, max;
  bignum_t mask;

  if (BignumRawLength(x) > BignumRawLength(y)) {
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

  BignumCheck(z,max);

  for (i = 1; i <= min; i++) {
    z[i] = x[i]|y[i];
  }

  for (; i <= max; i++) {
    z[i] = x[i]|mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_xor(bignum_t *x,
		     bignum_t *y,
		     bignum_t *z,
		     bignum_t *zmax) {
  int i;
  int min, max;
  bignum_t mask;

  if (BignumRawLength(x) > BignumRawLength(y)) {
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

  BignumCheck(z,max);

  for (i = 1; i <= min; i++) {
    z[i] = x[i]^y[i];
  }

  for (; i <= max; i++) {
    z[i] = x[i]^mask;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_not(bignum_t *x,
		     bignum_t *y,
		     bignum_t *z,
		     bignum_t *zmax) {
  int i;
  int xlen = BignumLength(x);

  BignumCheck(z,xlen);
  
  for (i = 1; i <= xlen; i++) {
    z[i] = ~x[i];
  }

  /* z must be canonical now */
  return 0;
}


/* This was used to communicate a value from lsh_internal and rsh_internal to
   bn_lshift and bn_rshift, but cannot be passed through bn_call */

/*bignum_t bn_shift_dist; */

bignum_size_t bn_lshift(bignum_t *x,
			bignum_t *dist,
			bignum_t *z,
			bignum_t *zmax) {
  bignum_t xi;
  bool_t xs = BignumPositive(x);
  int xlen = BignumLength(x);
  int shift = GetFull(dist);
  int div, rem;
  int i;

  div = shift >> LOG2_bignum_size;
  rem = shift & ((1<<LOG2_bignum_size)-1);

  BignumCheck(z,xlen+div+(rem>0));

  /* first perform the `long' shift, if any */
  for (i=1; i<=div; i++) {
    z[i] = BNMIN;
  }

  /* Then perform the short shift */
  if (rem == 0) { /* copy */
    for (i=1; i <= xlen; i++) {
      z[div+i] = x[i];
    }
  } else {
    int mer=FullUnit-rem;
    bignum_t carry=0;

    for (i=1; i<=xlen; i++) {
      xi = x[i];
      z[div+i] = xi<<rem | carry;
      carry = xi>>mer;
    }
    z[div+i] = ((bignum_t)xs-1)<<rem | carry;
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_rshift(bignum_t *x,
			bignum_t *dist,
			bignum_t *z,
			bignum_t *zmax) {
  bignum_t xi;
  bool_t xs = BignumPositive(x);
  int xlen = BignumLength(x);
  int shift = GetFull(dist);
  int div, rem;
  int i;

  div = shift >> LOG2_bignum_size;
  rem = shift & ((1<<LOG2_bignum_size)-1);

  if (xlen-div<1) {
    BignumCheck(z,1);
    z[1] = (bignum_t)xs-1;
    return 0;
  }

  BignumCheck(z,xlen-div);

  if (rem==0) {
    for (i=xlen-div; i>=1; i--) {
      z[i] = x[div+i];
    }
  } else {
    int mer=FullUnit-rem;
    bignum_t carry=((bignum_t)xs-1)<<mer;

    for (i=xlen-div; i>=1; i--) {
      xi = x[div+i];
      z[i] = xi>>rem | carry;
      carry = xi<<mer;
    }
  }

  bn_canonize(z);
  return 0;
}

bignum_size_t bn_compare(bignum_t *x,
			 bignum_t *y) {
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);
  bool_t xs = BignumPositive(x);
  bool_t ys = BignumPositive(y);

  if (xs != ys) {
    return (xs ? 1 : -1);
  } else if (xlen != ylen) {
    return (xs^(xlen>ylen) ? -1 : 1);
  } else {
    int i = xlen+1;

    while (--i) {
      if (x[i]!=y[i]) {
	return (x[i]<y[i] ? -1 : 1);
      }
    }
    return 0;
  }
}

/* y is shorter than x */
static void bn_mult_knuth(bignum_t *x,
			  int xlen,
			  bignum_t *y,
			  int ylen,
			  bignum_t *z) {
  int i, j;
  bignum_half_t yj;

  for (i=1; i<=xlen+ylen; i++) z[i] = 0;

  xlen <<= 1;
  ylen <<= 1;
  for (j=1; j<=ylen; j++) {
    if ((yj=GetHalf(y,j))) {
      bignum_t t=0;

      for (i=1; i<=xlen; i++) {
	t = (bignum_t)GetHalf(z,i+j-1) + (bignum_t)GetHalf(x,i)*yj + (t>>HalfUnit);
	GetHalf(z,i+j-1) = t&HalfMask;
      }
      GetHalf(z,xlen+j) = (t>>HalfUnit);
    }
  }
}

bignum_size_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);

  BignumCheck(z,xlen+ylen);
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
//     fprintf(stderr, "%08x", GetHalf(u,i+1));
//   }
//   fprintf(stderr, "]\n");
// }

static bignum_size_t bn_div_mod_quot_wanted(bignum_t *x,
					    bignum_t *y,
					    bignum_t *z,
					    bignum_t *zmax) {
  int d;
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);
  int ulen = xlen<<1;
  int vlen = ylen<<1;
  int i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t *u; /* dividend, size=ulen+1 */
  bignum_half_t *v; /* divisor, size=vlen */
  bignum_half_t *q; /* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;

  while (!GetHalf(x,ulen) && ulen>1) ulen--;
  while (!GetHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,1);
    z[1] = 0;
    return 0;
  } else if (vlen==1) {
    /* special case: a simple loop */
    v1 = GetHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += GetHalf(x,i);
      GetHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,(ulen+2)>>1);
    GetHalf(z,ulen+1) = 0;
    GetHalf(z,ulen+2) = 0;
    return 0;
  }
  // prbignum("x", (bignum_half_t *)x, ulen);
  // prbignum("y", (bignum_half_t *)y, vlen);
  BignumCheck(z,(3*ulen+4)>>1);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;

  /* Normalize. */
  v1 = GetHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
    //    fprintf(stderr, ".. v1: %08x\n", v1);
  }

  /* TODO: factorize this code */
  for (carry=0, i=0; i<ulen; i++) {
    carry += (bignum_t)GetHalf(x,i+1)<<d;
    u[i] = carry & HalfMask;
    carry >>= HalfUnit;
  }
  u[ulen] = carry;
  // prbignum("u", u-2, ulen+1);
  for (carry=0, i=0; i<vlen; i++) {
    carry += (bignum_t)GetHalf(y,i+1)<<d;
    v[i] = carry & HalfMask;
    carry >>= HalfUnit;
  }
  // prbignum("v", v-2, vlen);
  
  v1 = v[vlen-1], v2 = v[vlen-2];
  for (j=ulen; j>=vlen; j--) {
    /* Calculate q[j]. */
    carry = ((bignum_t)u[j]<<HalfUnit) + u[j-1];
    if (u[j]==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }
 
    v1qhat = (bignum_t)v1*(bignum_t)qj;
    v2qhat = (bignum_t)v2*(bignum_t)qj;
    while (carry-v1qhat < ((bignum_t)1<<HalfUnit) &&
	   v2qhat > ((carry-v1qhat)<<HalfUnit)+u[j-2]) {
      qj--;
      v1qhat -= v1;
      v2qhat -= v2;
    }

    /* Multiply and subtract. */
    if ((q[j]=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
	carry = (bignum_t)u[k] - (bignum_t)v[i]*qj - carry;
	u[k] = carry & HalfMask;
	carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      }
      carry = (bignum_t)u[k] - v2qhat - carry;
      u[k] = carry & HalfMask;
      carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      k++;
      carry = (bignum_t)u[k] - v1qhat - carry;
      u[k] = carry & HalfMask;
      carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      carry = (bignum_t)u[j] - carry;
      u[j] = carry & HalfMask;
      carry = ((bignum_t)u[j]-carry)>>HalfUnit;
      if (carry) {
	q[j]--;
	for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
	  carry += (bignum_t)u[k] + (bignum_t)v[i];
	  u[k] = carry & HalfMask;
	  carry = carry>>HalfUnit;
	}
	u[j] += carry;
      }
    }
  }
  /* q[vlen .. ulen] is the desired quotient. */
  SetBignumLength(z,(ulen-vlen+3)>>1);
  for (i=1, k=vlen; k<=ulen; i++, k++) {
    GetHalf(z,i) = q[k];
  }
  GetHalf(z,i++) = 0;
  GetHalf(z,i++) = 0;
  return 0;
}

static bignum_size_t bn_div_mod_quot_not_wanted(bignum_t *x,
						bignum_t *y,
						bignum_t *z,
						bignum_t *zmax)
{
  int d;
  int xlen = BignumLength(x);
  int ylen = BignumLength(y);
  int ulen = xlen<<1;
  int vlen = ylen<<1;
  int i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t *u; /* dividend, size=ulen+1 */
  bignum_half_t *v; /* divisor, size=vlen */
  bignum_half_t *q; /* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;
  
  while (!GetHalf(x,ulen) && ulen>1) ulen--;
  while (!GetHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,xlen);
    for (i=1; i<=xlen; i++) {
      z[i] = x[i];
    }
    return 0;
  } else if (vlen==1) {
    /* special case: a simple loop */
    v1 = GetHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += GetHalf(x,i);
      GetHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,1);
    z[1] = carry>>HalfUnit;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;

  /* Normalize. */
  v1 = GetHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
  }
  for (carry=0, i=0; i<ulen; i++) {
    carry += (bignum_t)GetHalf(x,i+1)<<d;
    u[i] = carry & HalfMask;
    carry >>= HalfUnit;
  }
  u[ulen] = carry;
  for (carry=0, i=0; i<vlen; i++) {
    carry += (bignum_t)GetHalf(y,i+1)<<d;
    v[i] = carry & HalfMask;
    carry >>= HalfUnit;
  }

  v1 = v[vlen-1], v2 = v[vlen-2];
  for (j=ulen; j>=vlen; j--) {
    /* Calculate q[j]. */
    carry = ((bignum_t)u[j]<<HalfUnit) + u[j-1];
    if (u[j]==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }

    v1qhat = (bignum_t)v1*(bignum_t)qj;
    v2qhat = (bignum_t)v2*(bignum_t)qj;
    while (carry-v1qhat < ((bignum_t)1<<HalfUnit) &&
	   v2qhat > ((carry-v1qhat)<<HalfUnit)+u[j-2]) {
      qj--;
      v1qhat -= v1;
      v2qhat -= v2;
    }

    /* Multiply and subtract. */
    if ((q[j]=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
	carry = (bignum_t)u[k] - (bignum_t)v[i]*qj - carry;
	u[k] = carry & HalfMask;
	carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      }
      carry = (bignum_t)u[k] - v2qhat - carry;
      u[k] = carry & HalfMask;
      carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      k++;
      carry = (bignum_t)u[k] - v1qhat - carry;
      u[k] = carry & HalfMask;
      carry = ((bignum_t)u[k]-carry)>>HalfUnit;
      carry = (bignum_t)u[j] - carry;
      u[j] = carry & HalfMask;
      carry = ((bignum_t)u[j]-carry)>>HalfUnit;
      if (carry) {
	q[j]--;
	for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
	  carry += (bignum_t)u[k] + (bignum_t)v[i];
	  u[k] = carry & HalfMask;
	  carry = carry>>HalfUnit;
	}
	u[j] += carry;
      }
    }
  }
  /* u[0 .. vlen-1]>>d is the desired remainder. */
  SetBignumLength(z,(vlen+2)>>1);
  v1qhat = ((bignum_t)1<<d)-1;
  for (carry=0, i=vlen; i>0; i--) {
    carry += u[i-1];
    GetHalf(z,i) = carry>>d;
    carry = (carry&v1qhat)<<HalfUnit;
  }
  GetHalf(z,vlen+1) = 0;
  GetHalf(z,vlen+2) = 0;
  return 0;
}

bignum_size_t bn_quotient_remainder_quot_wanted(bignum_t *x,
						bignum_t *y,
						bignum_t *z,
						bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  int value;

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

bignum_size_t bn_quotient_remainder_quot_not_wanted(bignum_t *x,
						    bignum_t *y,
						    bignum_t *z,
						    bignum_t *zmax) {
  bool_t sx = BignumPositive(x);
  bool_t sy = BignumPositive(y);
  int value;
  
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

#define FLTBITS 64

bignum_size_t bn_from_float(bignum_t *x,
			    bignum_t *ignore,
			    bignum_t *z,
			    bignum_t *zmax) {
  flt64_t f;
  flt64_t norm = 4294967296.0;	/* 2**(FLTBITS/2) */
  flt64_t norm2 = norm*norm; /* 2**FLTBITS */
#if LOG2_bignum_size == 5
  uint32_t *fp = (uint32_t *)(&f);
#endif
  int i, exp, div, rem, zlen;
  bool_t sx = TRUE;
#if LOG2_bignum_size == 5
  bignum_t uhigh, ulow;
#elif LOG2_bignum_size == 6
  bignum_t u;
#endif

#if LOG2_bignum_size == 5
  fp[0] = x[1];			/* GetFloat(x) */
  fp[1] = x[2];
#elif LOG2_bignum_size == 6
  f = *((flt64_t *)&x[1]); /* GetFloat(x) */
#endif
  if (signbit(f)) {
    sx = FALSE;
    f = -f;
  }

  if (f < 1.0) {
    BignumCheck(z,1);
    z[1] = 0;
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
    --exp; f *= 2.0;
  }

  /* NOTE: Do not use integer division here!
       (-31)>>5==-1
       (-31)/32==0  in C99+ */
  zlen = ((exp-1)>>LOG2_bignum_size)+2; /* ensure there is room for sign bit */
  div = (exp-FLTBITS)>>LOG2_bignum_size;
  rem = exp & ((1<<LOG2_bignum_size)-1);
  BignumCheck(z,zlen);
  for (i=1; i<=zlen; i++) z[i] = 0;

#if LOG2_bignum_size == 5
  /* turn off high bit of uhigh since it causes trouble on certain machines */
  f -= norm2;
  uhigh = f/norm;
  ulow = f-norm*uhigh;
  uhigh += (bignum_t)1<<(sizeof(bignum_t)*8-1);
  if (rem==0) {
    if (div>=0) z[div+1] = ulow;
    if (div>=-1) z[div+2] = uhigh;
  } else {
    if (div>=0) z[div+1] = ulow<<rem;
    if (div>=-1) z[div+2] = (ulow>>(FullUnit-rem))+(uhigh<<rem);
    if (div>=-2) z[div+3] = uhigh>>(FullUnit-rem);
  }
#elif LOG2_bignum_size == 6
  f -= norm2;
  u = f;
  u += (bignum_t)1<<(sizeof(bignum_t)*8-1);
  if (rem==0) {
    if (div>=0) z[div+1] = u;
  } else {
    if (div>=0) z[div+1] = u<<rem;
    if (div>=-1) z[div+2] = u>>(FullUnit-rem);
  }
#endif
 ret:
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}


/* Precond: x is a syntactically correct string denoting an integer */
bignum_size_t bn_from_string(char *x,
			     bignum_t *z,
			     bignum_t *zmax,
			     int base) {
  bool_t sx;
  int j;
  int zlen;
  char cur;
  bignum_t t, digit;
  /* unsigned int radix = GetSmall(current_radix); */

  if (*x=='+') {
    x++; sx=TRUE;
  } else if (*x=='-') {
    x++; sx=FALSE;
  } else {
    sx=TRUE;
  }

  zlen = 2;
  BignumCheck(z,1);
  z[1] = 0; /* always keep a zero pad word */
  for (cur = *x++; cur; cur = *x++) {
    digit = (cur>='a' ? cur-'a'+10 : cur>='A' ? cur-'A'+10 : cur-'0');
    for (j=1; j<zlen; j++) {
      t = (bignum_t)base*GetHalf(z,j) + digit;
      GetHalf(z,j) = t&HalfMask;
      digit = t>>HalfUnit;
    }
    if (digit) {
      zlen += 2;
      BignumCheck(z,zlen>>1);
      GetHalf(z,j) = digit;
      GetHalf(z,j+1) = 0;
      GetHalf(z,j+2) = 0;
      GetHalf(z,j+3) = 0;
    }
  }
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}

CVOID__PROTO(bn_to_string, bignum_t *x, int base) {
  int j, k;
  int xlen, slen, alen, dlen;
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
  for (alen=Atom_Buffer_Length; slen>alen;) {
    alen <<= 1;
  }

  if (alen > Atom_Buffer_Length) {
    EXPAND_ATOM_BUFFER(alen);
  }
  c = Atom_Buffer;
  work = (bignum_half_t *)(c+slen-(xlen<<1));
  if (!sx) {
    *c++ = '-';
    for (k=1, j=0; j<xlen; j++) {
      work[j] = ~GetHalf(x,j+1)+k;
      k &= !work[j];
    }
  } else {
    for (j=0; j<xlen; j++) {
      work[j] = GetHalf(x,j+1);
    }
  }
  while (xlen>0 && !work[xlen-1]) {
    xlen--;
  }

  while (xlen>0) {
    for (j=xlen-1, r=0; j >= 0; j--) {
      digit = ((bignum_t)r<<HalfUnit) + work[j];
      work[j] = digit/divisor;
      r = digit%divisor;
    }
    for (j=dlen; j>0; j--) {
      digit = r%base;
      *c++ = (digit<10 ? '0'+digit : hibase+digit);
      r /= base;
    }
    while (xlen>0 && !work[xlen-1]) {
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
