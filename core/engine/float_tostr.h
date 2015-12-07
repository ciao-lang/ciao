/*
 *  float_tostr.h
 *
 *  IEEE-754 (64 bits floating point) to string conversion (in an
 *  arbitrary base).
 *
 *  Copyright (C) 2003, 2004 UPM-CLIP
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  Author:
 *    Edison Mera
 */

#ifndef _CIAO_FLOAT_TOSTR_H
#define _CIAO_FLOAT_TOSTR_H

#include <stdint.h>
#include <ciao/float_consts.h>

#define IEEE754_NEGATIVE(x) \
  (((((uint32_t *)(&x))[IEEE754_INDEX_NEGATIVE]) & IEEE754_MASK_NEGATIVE) >> IEEE754_SHIFT_NEGATIVE)
#define IEEE754_EXPONENT(x) \
  (((((uint32_t *)(&x))[IEEE754_INDEX_EXPONENT]) & IEEE754_MASK_EXPONENT) >> IEEE754_SHIFT_EXPONENT)
#define IEEE754_MANTISSA0(x) \
  (((((uint32_t *)(&x))[IEEE754_INDEX_MANTISSA0]) & IEEE754_MASK_MANTISSA0) >> IEEE754_SHIFT_MANTISSA0)
#define IEEE754_MANTISSA1(x) \
  (((((uint32_t *)(&x))[IEEE754_INDEX_MANTISSA1]) & IEEE754_MASK_MANTISSA1) >> IEEE754_SHIFT_MANTISSA1)

#define IEEE854_NEGATIVE(x) \
  (((((uint32_t *)(&x))[IEEE854_INDEX_NEGATIVE]) & IEEE854_MASK_NEGATIVE) >> IEEE854_SHIFT_NEGATIVE)
#define IEEE854_EXPONENT(x) \
  (((((uint32_t *)(&x))[IEEE854_INDEX_EXPONENT]) & IEEE854_MASK_EXPONENT) >> IEEE854_SHIFT_EXPONENT)
#define IEEE854_MANTISSA0(x) \
  ((((((uint32_t *)(&x))[IEEE854_INDEX_MANTISSA0_0]) & IEEE854_MASK_MANTISSA0_0) << IEEE854_SPLIT_MANTISSA0_0) \
  | (((((uint32_t *)(&x))[IEEE854_INDEX_MANTISSA0_1]) & IEEE854_MASK_MANTISSA0_1) >> IEEE854_SHIFT_MANTISSA0_1))
#define IEEE854_MANTISSA1(x) \
  ((((((uint32_t *)(&x))[IEEE854_INDEX_MANTISSA1_0]) & IEEE854_MASK_MANTISSA1_0) << IEEE854_SPLIT_MANTISSA1_0) \
  | (((((uint32_t *)(&x))[IEEE854_INDEX_MANTISSA1_1]) & IEEE854_MASK_MANTISSA1_1) >> IEEE854_SHIFT_MANTISSA1_1))

char * float_to_string(char* buffer, int precision, char format, double x, int base);

#endif /* _CIAO_FLOAT_TOSTR_H */
