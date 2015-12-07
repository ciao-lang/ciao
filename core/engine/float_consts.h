/*
 *  float_consts.h
 *
 *  Constants for floating point to string conversion.
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

#ifndef _CIAO_FLOAT_CONSTS_H
#define _CIAO_FLOAT_CONSTS_H

/* #if defined(ppc) || defined (armv4l) */
#if defined(USE_LONG_DOUBLE)
typedef long double LONG_FLOAT;
#else
typedef double LONG_FLOAT;
#endif

#define IEEE754_ABS_EXP (IEEE754_MASK_EXPONENT >> IEEE754_SHIFT_EXPONENT)       /* 2047 */
#define IEEE754_MIN_EXP (IEEE754_MASK_EXPONENT >> (IEEE754_SHIFT_EXPONENT + 1)) /* 1023 */
#define IEEE754_BIA_EXP (IEEE754_MIN_EXP + IEEE754_MANTISSA_LENGTH)             /* 1075 */
#define IEEE754_MAX_EXP (IEEE754_BIA_EXP - 1)                                   /* 1074 */
#define IEEE754_FIX_BIT (1LL << IEEE754_MANTISSA_LENGTH)        /* 0x0010000000000000LL */

#define IEEE854_ABS_EXP (IEEE854_MASK_EXPONENT >> IEEE854_SHIFT_EXPONENT)
#define IEEE854_MIN_EXP (IEEE854_MASK_EXPONENT >> (IEEE854_SHIFT_EXPONENT + 1))
#define IEEE854_BIA_EXP (IEEE854_MIN_EXP + IEEE_854_MANTISSA_LENGTH)
#define IEEE854_MAX_EXP (IEEE854_BIA_EXP - 1)
#define IEEE854_FIX_BIT (1LL << IEEE854_MANTISSA_LENGTH)

extern double invlog2[];
extern LONG_FLOAT *powtable[];
extern LONG_FLOAT *invpowtable[];

/* #define EXP_CHAR  36 */
/* #define FRAC_SEP  36 */

/* floating point char */
#define FLOAT_POINT '.'

extern int char_digit[256];
extern char digits_upper[];
extern char digits_lower[];
extern char exponents_upper[];
extern char exponents_lower[];

/*
  the next functions must be called before use of any base,
  and only is necessary one time per program execution.
  These functions speed-up the work with other bases rather than
  10, but uses more memory.  Use them when you will work with
  too much numbers.
*/

void fillchardigit(void);
void fillpowtable(int base);
void freepowtable(int base);
void fillallpowtable(void);
void freeallpowtable(void);
LONG_FLOAT powl_int(int base, int exp);

#endif /* _CIAO_FLOAT_CONSTS_H */
