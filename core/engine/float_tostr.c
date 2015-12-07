/*
 *  float_tostr.c
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

#include <ciao/configure.h>
#include <ciao/float_tostr.h>
#include <math.h>
#include <string.h>

/*

Functions that generates for any IEEE-754 64 bits floating point value,
the representation on other base, in the format: +-0.XXXXXXXe+-XXX,
where the Xs represent the digits in the given base.

Based on the paper:

"Printing Floating-Point Numbers Quickly and Accurately.  Robert
G. Burger, R. Kent Dybvig. Indiana University Computer Science
Department. Lindley Hall 215.  Boomington, Indiana 47405.
{burger,dyb}@cs.indiana.edu.  1996 by ACM."

Some ideas has been taked reading the softfloat library, from
John R. Hauser, available at
http://www.cs.berkeley.edu/~jhauser/arithmetic/SoftFloat.html.


*/

/*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 32 is returned.
*----------------------------------------------------------------------------*/

static char countLeadingZeros32(uint32_t a)
{
  static const char countLeadingZerosHigh[] = {
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  };
  char shiftCount;

  shiftCount = 0;
  if ( a < 0x10000 ) {
    shiftCount += 16;
    a <<= 16;
  }
  if ( a < 0x1000000 ) {
    shiftCount += 8;
    a <<= 8;
  }
  shiftCount += countLeadingZerosHigh[ a>>24 ];
  return shiftCount;
}

/*----------------------------------------------------------------------------
| Returns the number of leading 0 bits before the most-significant 1 bit of
| `a'.  If `a' is zero, 64 is returned.
*----------------------------------------------------------------------------*/

static char countLeadingZeros64( uint64_t a )
{
  char shiftCount;

  shiftCount = 0;
  if ( a < ( (uint64_t) 1 )<<32 ) {
    shiftCount += 32;
  }
  else {
    a >>= 32;
  }
  shiftCount += countLeadingZeros32( a );
  return shiftCount;
}

char * nextDigit(
        char buffer[],
        char *start,
        int precision,

        int base,
        char digits[],
        int d,
        int *exp,
        int carrytoexp
)
{
  if(d+1<base) {
    *buffer++ = digits[d+1];
  } else {
    while(buffer>start) {
      d = char_digit[(int)(*--buffer)];
      if(d+1<base) {
        *buffer++ = digits[d+1];
        return buffer;
      }
    }
    if(carrytoexp) {
      *buffer++ = digits[1];
      (*exp)++;
    }
    else {
      if(buffer[-1]==FLOAT_POINT)
	buffer[-2] = digits[1];
      else
	buffer[-1] = digits[1];
    }
  }
  return buffer;
}

#define EXTRA_BITS 24
#define EXTRA_BITS_MOD (1<<EXTRA_BITS)

// this function generates the digits of a number in an exponential format of base B

char * generate(
        char buffer[],
        int precision,
        
        uint64_t r,
        uint64_t s,
        int sa,
        uint64_t m1,
        uint64_t m0,
        int *exp,
        int carrytoexp,
        int base,
        char digits[],
        int isLow,
        int isHigh)
{
  int tc1;
  int tc2;
  int d;
  int i;
  char *p;
  int ra;
  // 15 --> 52 / log2(10)
  char *start;

  if(r==0) {
    *buffer++=digits[0];
    return buffer;
  }

  ra = 0;
  start = buffer;
  i = precision;
  while (1) {
    i--;
    d = (int) (r / (s + 1));
    r = r - d * s;
    ra = ra - d * sa;
    r += ra / EXTRA_BITS_MOD;
    ra %= EXTRA_BITS_MOD;
    if(ra < 0) {
      r--;
      ra += EXTRA_BITS_MOD;
    } else if(ra >= EXTRA_BITS_MOD) {
      r++;
      ra -= EXTRA_BITS_MOD;
    }
/*     d=0; */
    while(r > s || ( r == s && ra >= sa )) {
      d++;
      r -= s;
      ra -= sa;
      if(ra < 0) {
        r--;
        ra += EXTRA_BITS_MOD;
      } else if(ra >= EXTRA_BITS_MOD) {
        r++;
        ra -= EXTRA_BITS_MOD;
      }
    }
    if(i<=0) {
      tc1 = 1;
      tc2 = 1;
    }
    else {
      if(isLow)
        tc1 = (r < m0)||(r == m0 && ra == 0);
      else
        tc1 = r < m0;
      if(isHigh)
        tc2 = (r + m1 > s)||((r + m1 == s)&& ra >= sa);
      else
        tc2 = (r + m1 > s)||((r + m1 == s)&& ra > sa);
    }
    if(!tc1) {
      if(!tc2) {
        *buffer++ = digits[d];
        r *= base;
        ra *= base;
        r += ra / EXTRA_BITS_MOD;
        ra %= EXTRA_BITS_MOD;
        m1 *= base;
        m0 *= base;
      } else {
        buffer = nextDigit(buffer, start, precision, base, digits, d, exp, carrytoexp);
        break;
      }
    } else {
      if(!tc2) {
        *buffer++ = digits[d];
        break;
      } else {
        if ((2*r<s)||(2*r == s && 2*ra < sa)) {
          if(d!=0)
            *buffer++ = digits[d];
          break;
        }
        else {
          buffer = nextDigit(buffer, start, precision, base, digits, d, exp, carrytoexp);
          break;
        }
      }
    }
  }
  p = buffer;
  p--;
  if(p[0]==digits[0]) {
    while(*p==digits[0] && p != buffer) {
      p--;
    }
    p++;
    buffer=p;
  }
  return buffer;
}

#define OWN_MANTISSA_LENGTH 63

static char * scale(
        char *buffer,
        int precision,
        char format,
        uint64_t r,
        uint64_t shift,
        uint64_t m1,
        uint64_t m0,
        int base,
        char digits[],
	char exponents[],
        int isLow,
        int isHigh,
        int e)
{
  int l;
  int exp;
  int32_t scalee;
  LONG_FLOAT scale;
/*   long double scale; */
  //  union ieee854_long_double scaleb;
  uint64_t s = 0;
  int sa = 0;
  //  int k = 8 - EXTRA_BITS;
  int k;
  int not_wrote_exp;
  char *start, *p_at;
  int accuracy;
  char mini_buf[32];
  char *p_mini_buf = mini_buf + 32;

  if(r!=0) {
    l = countLeadingZeros64(r);
    exp = (int)ceil(invlog2[base] * (e + OWN_MANTISSA_LENGTH - l) - 1e-10);
    scale = powl_int(base, exp);
    s = (((uint64_t)IEEE854_MANTISSA0(scale))*0x100000000LL)
      + ((uint64_t)IEEE854_MANTISSA1(scale));
/*     printf("good1 s=%llx\n", s); */
    s = s | IEEE854_FIX_BIT;
    s <<= OWN_MANTISSA_LENGTH - IEEE854_MANTISSA_LENGTH;
    scalee = -IEEE854_EXPONENT(scale) + IEEE854_MIN_EXP
      + OWN_MANTISSA_LENGTH + e - 8;
/*     printf("s=%llx\n", s); */
/*     printf("scalee=%ld\n", scalee); */
    
    /*   r <<= (k + scalee); */
    /*   sa = s % (1<<(8 - k)); */
    /*   s >>= (8 - k); */

    r <<= shift;
    k = 8 + scalee - shift;
    sa = s % (1<<k);
    sa <<= EXTRA_BITS;
    sa >>= k;
    s >>= (short)(k);

/*   printf("EXTRA_BITS-(8 + scalee - shift)=%d\n", EXTRA_BITS-k); */

/*     printf("1.scalee=%lx shift=%llx r=%llx m0=%llx m1=%llx\n", scalee, shift, r, m0, m1); */
  //  printf("(k + scalee - shift)=(%d+%ld-%lld)=%lld\n",k,scalee,shift,(k + scalee - shift));
  //  m1 <<= (k + scalee - shift);
  //  m0 <<= (k + scalee - shift);
  //  printf("2.scalee=%lx shift=%llx r=%llx m0=%llx m1=%llx\n", scalee, shift, r, m0, m1);

  // fix exponent
    if ( ( isHigh && ((r + m1 > s)||((r + m1 == s)&& 0 >= sa)))
	 || ( !isHigh && (r + m1 > s)) ) {
      //(*exp)++;
    } else {
      r  *= base;
      m1 *= base;
      m0 *= base;
      /*     s /= B; */
      exp--;
    }
  }
  else
    exp = 0;

  start = buffer;
  switch(format){
  case 'E':case 'e':
    accuracy = precision + 1;
    buffer = generate(++buffer, accuracy,
	r,
        s,
        sa,
        m1,
        m0,
        &exp,
        1,
        base, digits, isLow, isHigh);
      while(buffer <= start + accuracy)
        *buffer++ = digits[0];
      start[0] = start[1];
      if ( start + 2 < buffer )
        start[1] = FLOAT_POINT;
      else
        buffer--;
      //start[1] = digits[FRAC_SEP];
      break;
    /*
      The format specifier p is for ciao prolog style.
    */
    case 'G':case 'g':case 'p':
      accuracy = precision;
      buffer = generate(buffer + 1, accuracy,
        r,
        s,
        sa,
        m1,
        m0,
        &exp,
        1,
        base, digits, isLow, isHigh);
      if(-5 < exp && exp < 0) {
        accuracy = (int)(buffer - start) - 1;
        buffer -= exp;
        memmove(start - exp + 1, start + 1, accuracy);
        *start++ = digits[0];
        *start++ = FLOAT_POINT;
        while(++exp) {
          *start++ = digits[0];
        }
        exp = 0;
      }
      else if(0 <= exp && exp < precision) {
        char * p_at = start + exp + 1;
        buffer--;
        if(buffer < p_at) {
          memmove(start, start + 1, (int)(buffer - start + 1));
          while(buffer < p_at)
            *buffer++ = digits[0];
          if(format=='p') {
            *buffer++ = FLOAT_POINT;
            *buffer++ = digits[0];
          }  
        } else if(buffer > p_at) {
          memmove(start, start + 1, (int)(p_at - start));
          *p_at = FLOAT_POINT;
          buffer++;
        } else {
          memmove(start, start + 1, (int)(buffer - start + 1));
          if(format=='p') {
            *buffer++ = FLOAT_POINT;
            *buffer++ = digits[0];
          }
        }  
        exp = 0;
      }
      else {
        start[0] = start[1];
        if ( start + 2 < buffer )
          start[1] = FLOAT_POINT;
        else {
          buffer--;
          if(format=='p') {
            *buffer++ = FLOAT_POINT;
            *buffer++ = digits[0];
          }
        }
      }
      break;
    case 'f':
      accuracy = precision + exp + 1;
      if(exp<0){
        *buffer++ = digits[0];
        if(accuracy <= 0) {
          if(precision!=0) {
            *buffer++ = FLOAT_POINT;
            accuracy = precision;
            while(accuracy--)
              *buffer++ = digits[0];
          }
        }
        else {
          p_at = buffer;
          *buffer++ = FLOAT_POINT;
          while(++exp)
            *buffer++ = digits[0];
          buffer = generate(buffer, accuracy,
            r,
            s,
            sa,
            m1,
            m0,
            &exp,
            0,
            base, digits, isLow, isHigh);
          accuracy = precision + 1 - (buffer - p_at);
          while(accuracy--)
            *buffer++ = digits[0];
        }
      }
      else {
        buffer = generate(buffer, accuracy,
          r,
          s,
          sa,
          m1,
          m0,
          &exp,
          1,
          base, digits, isLow, isHigh);
        p_at = start + exp + 1;
        if(buffer <= p_at) {
          while(buffer < p_at)
            *buffer++ = digits[0];
          if(precision > 0) {
            *buffer++ = FLOAT_POINT;
            accuracy = precision;
            while(accuracy--)
              *buffer++ = digits[0];
          }    
        } else {
          accuracy = (int)(buffer - p_at);
          if(accuracy > 0) {
            memmove(p_at + 1, p_at, accuracy);
            *p_at = FLOAT_POINT;
            accuracy = precision - (buffer - p_at);
            buffer++;
            while(accuracy--)
              *buffer++ = digits[0];
          }
        }
      }
      exp = 0;
      break;
  }
  
  if ((format=='e')||(format=='E')||(exp != 0)) {
    *buffer++ = exponents[base];
    if(exp < 0) {
      exp = -exp;
      *buffer++ = '-';
    }
    else {
      if(format!='p')
        *buffer++ = '+';
    }
    // exponent has 2, or 3, digits
    /*
    if ( exp < B ) {
      *buffer++ = digits[exp];
    } else
    */
    *--p_mini_buf='\0';
    not_wrote_exp = (exp==0);
    while(exp!=0){
      *--p_mini_buf = digits[exp%base];
      exp/=base;
    }
    if(not_wrote_exp)
      *--p_mini_buf = digits[0];
    while(*p_mini_buf)
      *buffer++ = *p_mini_buf++;
/*     if ( exp < B*B ) { */
/*       *buffer++ = digits[exp/B]; */
/*       *buffer++ = digits[exp%B]; */
/*     } else { */
/*       *buffer++ = digits[exp/(B*B)]; */
/*       exp %= B*B; */
/*       *buffer++ = digits[exp/B]; */
/*       *buffer++ = digits[exp%B]; */
/*     } */
  }
  return buffer;
}

char * float_to_string(
        char* buffer,
        int precision,
        char format,

        double x,
        int base)
{
/*   union ieee754_double r; */
  int32_t be;
  uint64_t f; 
  uint64_t s;
  int32_t e;
  int okLow;
  int okHigh;
  char *digits;
  char *exponents;

  be = IEEE754_EXPONENT(x);
  f = (((uint64_t)IEEE754_MANTISSA0(x))*0x100000000LL)
    + ((uint64_t)IEEE754_MANTISSA1(x));
  s = IEEE754_NEGATIVE(x);
/*   e = be - IEEE754_BIA_EXP; */

/*   printf("x=%f\nmantissa0=%lx\nmantissa1=%lx\nexponent=%lx\nnegative=%lx\n", */
/* 	 x, */
/* 	 IEEE754_MANTISSA0(x), */
/* 	 IEEE754_MANTISSA1(x), */
/* 	 IEEE754_EXPONENT(x), */
/* 	 IEEE754_NEGATIVE(x)); */
/*   printf("f=%llx\n",f);   */


  if(format=='E'||format=='G') {/* ||format=='p') { */
    digits = digits_lower;
    exponents = exponents_upper;
  } else if(format=='p') {
    /* note that this is not arbitrary: */
    digits = digits_lower;
    exponents = exponents_lower;
  } else {
    digits = digits_upper;
    exponents = exponents_lower;
  }

  if ( be == IEEE754_ABS_EXP ) {
    if ( f == 0 ) {
      if( s )
        *buffer++ = '-';
    /*else
        buffer[(*pos)++] = '+';*/
      // 0.Inf
      *buffer++ = digits[0];
      *buffer++ = FLOAT_POINT;
      *buffer++ = 'I';
      *buffer++ = 'n';
      *buffer++ = 'f';
    }
    else {
      *buffer++ = digits[0];
      *buffer++ = FLOAT_POINT;
      *buffer++ = 'N';
      *buffer++ = 'a';
      *buffer++ = 'n';
    }
  } else {
    if ( s )
      *buffer++ = '-';
    if ( be == 0 && f == 0 ) {
      /*       buffer = scale_zero(buffer, precision, format, digits); */
      f = 0;
      e = 0;
    }
    if ( 1 <= be && be < IEEE754_ABS_EXP) {
      f |= IEEE754_FIX_BIT;
      e = be - IEEE754_BIA_EXP;
    }
    else if ( be == 0 ) {
      //int l = countLeadingZeros64(f) - 11;
      //e = -(l+1074);
      //f <<= l;
      e = -IEEE754_MAX_EXP;
    }
    else
      e = 0;
    //int d = 1;
    okLow = !(f & 1);
    okHigh = !(f & 1);
    if ( e >= 0 ) {
      if ( f != IEEE754_FIX_BIT ) {
	buffer = scale(buffer, precision, format, f, 1, 1, 1, base, digits, exponents, okLow, okHigh, e);
	//scale(buffer, pos, precision, f<<d, 1<<d, 1, 1, B, okLow, okHigh, f, e);
      }
      else {
	buffer = scale(buffer, precision, format, f, 2, 2, 1, base, digits, exponents, okLow, okHigh, e);
	//scale(buffer, pos, precision, f<<(d+1), 1<<(d+1), 2, 1, B, okLow, okHigh, f, e);
        }
    }
    else {
      if (be == 0 || f != IEEE754_FIX_BIT) {
	buffer = scale(buffer, precision, format, f, 1, 1, 1, base, digits, exponents, okLow, okHigh, e);
	//scale(buffer, pos, precision, f<<d, 1<<d, 1, 1, B, okLow, okHigh, f, e);
      } else {
	buffer = scale(buffer, precision, format, f, 2, 2, 1, base, digits, exponents, okLow, okHigh, e);
	//scale(buffer, pos, precision, f<<(d+1), 1<<(d+1), 2, 1, B, okLow, okHigh, f, e);
      }
    }
  }
  *buffer++ = '\0';
  return buffer;
}
