/* ---------------------------------------------------------------------------
 This file is part of the clpfd package for Ciao

 Copyright (C) 2012-2014 Ciao Development Team

 Authors
   * Remy Haemmerle
   * Jose F. Morales (port to 64 bits)

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--------------------------------------------------------------------------- */

#include <ciao_prolog.h>
#include <ciao/bignum.h>
#include <ciao/termdefs.h>

#define BIGNUM_SIZE (sizeof(bignum_t)*8)
#define BIGNUM_EMPTY ((bignum_t)0)
#define BIGNUM_FULL (~(bignum_t)0)
#if LOG2_bignum_size == 5 /* 32 bit bitnum_t */
/* Require GNU CC */
#define LSB(x) (__builtin_ctzl(x))
#define MSB(x) (BIGNUM_SIZE - 1 - __builtin_clzl(x))
#define BITS_SET(x)  (__builtin_popcountl(x))
#elif LOG2_bignum_size == 6 /* 64 bit bitnum_t */
#define LSB(x) (__builtin_ctzll(x))
#define MSB(x) (BIGNUM_SIZE - 1 - __builtin_clzll(x))
#define BITS_SET(x)  (__builtin_popcountll(x))
#endif

/* The 2 next lines are form bignum.c */
#define BignumRawLength(b) (b)[0]
#define BignumLength(B) GetBignumLength(BignumRawLength(B))

#define WRAPPER_INT_TO_SMALL(WName, PlName, SmallFun, BnFun)		\
  CBOOL__PROTO(WName)							\
  {									\
    ERR__FUNCTOR("int_extra:" PlName, 2);				\
    DEREF(X(0),X(0));							\
    									\
    if (TagIsSmall(X(0)))						\
      return cunify(Arg, X(1), MakeSmall(SmallFun(GetSmall(X(0)))));	\
    else if (TagIsLarge(X(0)) && !LargeIsFloat(X(0)))			\
      return cunify(Arg, X(1), MakeSmall(BnFun(TagToSTR(X(0)))));	\
    else								\
      ERROR_IN_ARG(X(0),1,INTEGER);					\
  }


int bn_lsb(bignum_t *);
int bn_msb(bignum_t *);
int bn_bits_set(bignum_t *);

WRAPPER_INT_TO_SMALL(ciao_lsb, "lsb", LSB, bn_lsb)
WRAPPER_INT_TO_SMALL(ciao_msb, "msb", MSB, bn_msb)
WRAPPER_INT_TO_SMALL(ciao_bits_set, "bits_set", BITS_SET, bn_bits_set)


/* Compute the least significant bit of a bignum_t */
int 
bn_lsb(bignum_t *x)
{
  int i, k;
  int length = BignumLength(x);

  k = 0;
  for (i=1; i <= length; i++) {
    if (x[i] != BIGNUM_EMPTY) return (k + LSB(x[i]));
    k += BIGNUM_SIZE;
  }
  return 0;
}

/* Compute the most significant bit of a bignum_t */
int 
bn_msb(bignum_t *x)
{
  int i, k;
  int length = BignumLength(x);

  k = length*BIGNUM_SIZE;
  for (i=length; i > 0; i--) {
    k -= BIGNUM_SIZE;
    if (x[i] != BIGNUM_FULL) {
      if (x[i] == BIGNUM_EMPTY) {
	return (k - 1); 
      } else {
	return (k + MSB(x[i]));
      }
    }
  }
  return 0;
}

/* Compute the number of bits set in a bignum_t */
int 
bn_bits_set(bignum_t *x)
{
 int i, k;
 int length = BignumLength(x);

  k = 0;
  for(i=1; i <= length; i++) {
    k += BITS_SET(x[i]);
  }

  return k;
}
