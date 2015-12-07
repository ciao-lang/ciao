/*
 *  float_consts.c
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

#include <ciao/configure.h>
#include <ciao/float_tostr.h>
#include <stddef.h>
#include <stdlib.h>
#include <math.h>


// 309 = ceil(1024 * invlog2[10])
LONG_FLOAT powtable10[309] = {
  1.0e0l, 1.0e1l, 1.0e2l, 1.0e3l, 1.0e4l, 1.0e5l, 1.0e6l, 1.0e7l, 1.0e8l, 1.0e9l,
  1.0e10l, 1.0e11l, 1.0e12l, 1.0e13l, 1.0e14l, 1.0e15l, 1.0e16l, 1.0e17l, 1.0e18l, 1.0e19l,
  1.0e20l, 1.0e21l, 1.0e22l, 1.0e23l, 1.0e24l, 1.0e25l, 1.0e26l, 1.0e27l, 1.0e28l, 1.0e29l,
  1.0e30l, 1.0e31l, 1.0e32l, 1.0e33l, 1.0e34l, 1.0e35l, 1.0e36l, 1.0e37l, 1.0e38l, 1.0e39l,
  1.0e40l, 1.0e41l, 1.0e42l, 1.0e43l, 1.0e44l, 1.0e45l, 1.0e46l, 1.0e47l, 1.0e48l, 1.0e49l,
  1.0e50l, 1.0e51l, 1.0e52l, 1.0e53l, 1.0e54l, 1.0e55l, 1.0e56l, 1.0e57l, 1.0e58l, 1.0e59l,
  1.0e60l, 1.0e61l, 1.0e62l, 1.0e63l, 1.0e64l, 1.0e65l, 1.0e66l, 1.0e67l, 1.0e68l, 1.0e69l,
  1.0e70l, 1.0e71l, 1.0e72l, 1.0e73l, 1.0e74l, 1.0e75l, 1.0e76l, 1.0e77l, 1.0e78l, 1.0e79l,
  1.0e80l, 1.0e81l, 1.0e82l, 1.0e83l, 1.0e84l, 1.0e85l, 1.0e86l, 1.0e87l, 1.0e88l, 1.0e89l,
  1.0e90l, 1.0e91l, 1.0e92l, 1.0e93l, 1.0e94l, 1.0e95l, 1.0e96l, 1.0e97l, 1.0e98l, 1.0e99l,
  1.0e100l, 1.0e101l, 1.0e102l, 1.0e103l, 1.0e104l, 1.0e105l, 1.0e106l, 1.0e107l, 1.0e108l, 1.0e109l,
  1.0e110l, 1.0e111l, 1.0e112l, 1.0e113l, 1.0e114l, 1.0e115l, 1.0e116l, 1.0e117l, 1.0e118l, 1.0e119l,
  1.0e120l, 1.0e121l, 1.0e122l, 1.0e123l, 1.0e124l, 1.0e125l, 1.0e126l, 1.0e127l, 1.0e128l, 1.0e129l,
  1.0e130l, 1.0e131l, 1.0e132l, 1.0e133l, 1.0e134l, 1.0e135l, 1.0e136l, 1.0e137l, 1.0e138l, 1.0e139l,
  1.0e140l, 1.0e141l, 1.0e142l, 1.0e143l, 1.0e144l, 1.0e145l, 1.0e146l, 1.0e147l, 1.0e148l, 1.0e149l,
  1.0e150l, 1.0e151l, 1.0e152l, 1.0e153l, 1.0e154l, 1.0e155l, 1.0e156l, 1.0e157l, 1.0e158l, 1.0e159l,
  1.0e160l, 1.0e161l, 1.0e162l, 1.0e163l, 1.0e164l, 1.0e165l, 1.0e166l, 1.0e167l, 1.0e168l, 1.0e169l,
  1.0e170l, 1.0e171l, 1.0e172l, 1.0e173l, 1.0e174l, 1.0e175l, 1.0e176l, 1.0e177l, 1.0e178l, 1.0e179l,
  1.0e180l, 1.0e181l, 1.0e182l, 1.0e183l, 1.0e184l, 1.0e185l, 1.0e186l, 1.0e187l, 1.0e188l, 1.0e189l,
  1.0e190l, 1.0e191l, 1.0e192l, 1.0e193l, 1.0e194l, 1.0e195l, 1.0e196l, 1.0e197l, 1.0e198l, 1.0e199l,
  1.0e200l, 1.0e201l, 1.0e202l, 1.0e203l, 1.0e204l, 1.0e205l, 1.0e206l, 1.0e207l, 1.0e208l, 1.0e209l,
  1.0e210l, 1.0e211l, 1.0e212l, 1.0e213l, 1.0e214l, 1.0e215l, 1.0e216l, 1.0e217l, 1.0e218l, 1.0e219l,
  1.0e220l, 1.0e221l, 1.0e222l, 1.0e223l, 1.0e224l, 1.0e225l, 1.0e226l, 1.0e227l, 1.0e228l, 1.0e229l,
  1.0e230l, 1.0e231l, 1.0e232l, 1.0e233l, 1.0e234l, 1.0e235l, 1.0e236l, 1.0e237l, 1.0e238l, 1.0e239l,
  1.0e240l, 1.0e241l, 1.0e242l, 1.0e243l, 1.0e244l, 1.0e245l, 1.0e246l, 1.0e247l, 1.0e248l, 1.0e249l,
  1.0e250l, 1.0e251l, 1.0e252l, 1.0e253l, 1.0e254l, 1.0e255l, 1.0e256l, 1.0e257l, 1.0e258l, 1.0e259l,
  1.0e260l, 1.0e261l, 1.0e262l, 1.0e263l, 1.0e264l, 1.0e265l, 1.0e266l, 1.0e267l, 1.0e268l, 1.0e269l,
  1.0e270l, 1.0e271l, 1.0e272l, 1.0e273l, 1.0e274l, 1.0e275l, 1.0e276l, 1.0e277l, 1.0e278l, 1.0e279l,
  1.0e280l, 1.0e281l, 1.0e282l, 1.0e283l, 1.0e284l, 1.0e285l, 1.0e286l, 1.0e287l, 1.0e288l, 1.0e289l,
  1.0e290l, 1.0e291l, 1.0e292l, 1.0e293l, 1.0e294l, 1.0e295l, 1.0e296l, 1.0e297l, 1.0e298l, 1.0e299l,
  1.0e300l, 1.0e301l, 1.0e302l, 1.0e303l, 1.0e304l, 1.0e305l, 1.0e306l, 1.0e307l, 1.0e308l
};

LONG_FLOAT *powtable[37] = {
  NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, powtable10,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL
};

// 324 = ceil((MAX_EXP) * invlog2[10])
LONG_FLOAT invpowtable10[324] = {
  1.0e-0l, 1.0e-1l, 1.0e-2l, 1.0e-3l, 1.0e-4l, 1.0e-5l, 1.0e-6l, 1.0e-7l, 1.0e-8l, 1.0e-9l,
  1.0e-10l, 1.0e-11l, 1.0e-12l, 1.0e-13l, 1.0e-14l, 1.0e-15l, 1.0e-16l, 1.0e-17l, 1.0e-18l, 1.0e-19l,
  1.0e-20l, 1.0e-21l, 1.0e-22l, 1.0e-23l, 1.0e-24l, 1.0e-25l, 1.0e-26l, 1.0e-27l, 1.0e-28l, 1.0e-29l,
  1.0e-30l, 1.0e-31l, 1.0e-32l, 1.0e-33l, 1.0e-34l, 1.0e-35l, 1.0e-36l, 1.0e-37l, 1.0e-38l, 1.0e-39l,
  1.0e-40l, 1.0e-41l, 1.0e-42l, 1.0e-43l, 1.0e-44l, 1.0e-45l, 1.0e-46l, 1.0e-47l, 1.0e-48l, 1.0e-49l,
  1.0e-50l, 1.0e-51l, 1.0e-52l, 1.0e-53l, 1.0e-54l, 1.0e-55l, 1.0e-56l, 1.0e-57l, 1.0e-58l, 1.0e-59l,
  1.0e-60l, 1.0e-61l, 1.0e-62l, 1.0e-63l, 1.0e-64l, 1.0e-65l, 1.0e-66l, 1.0e-67l, 1.0e-68l, 1.0e-69l,
  1.0e-70l, 1.0e-71l, 1.0e-72l, 1.0e-73l, 1.0e-74l, 1.0e-75l, 1.0e-76l, 1.0e-77l, 1.0e-78l, 1.0e-79l,
  1.0e-80l, 1.0e-81l, 1.0e-82l, 1.0e-83l, 1.0e-84l, 1.0e-85l, 1.0e-86l, 1.0e-87l, 1.0e-88l, 1.0e-89l,
  1.0e-90l, 1.0e-91l, 1.0e-92l, 1.0e-93l, 1.0e-94l, 1.0e-95l, 1.0e-96l, 1.0e-97l, 1.0e-98l, 1.0e-99l,
  1.0e-100l, 1.0e-101l, 1.0e-102l, 1.0e-103l, 1.0e-104l, 1.0e-105l, 1.0e-106l, 1.0e-107l, 1.0e-108l, 1.0e-109l,
  1.0e-110l, 1.0e-111l, 1.0e-112l, 1.0e-113l, 1.0e-114l, 1.0e-115l, 1.0e-116l, 1.0e-117l, 1.0e-118l, 1.0e-119l,
  1.0e-120l, 1.0e-121l, 1.0e-122l, 1.0e-123l, 1.0e-124l, 1.0e-125l, 1.0e-126l, 1.0e-127l, 1.0e-128l, 1.0e-129l,
  1.0e-130l, 1.0e-131l, 1.0e-132l, 1.0e-133l, 1.0e-134l, 1.0e-135l, 1.0e-136l, 1.0e-137l, 1.0e-138l, 1.0e-139l,
  1.0e-140l, 1.0e-141l, 1.0e-142l, 1.0e-143l, 1.0e-144l, 1.0e-145l, 1.0e-146l, 1.0e-147l, 1.0e-148l, 1.0e-149l,
  1.0e-150l, 1.0e-151l, 1.0e-152l, 1.0e-153l, 1.0e-154l, 1.0e-155l, 1.0e-156l, 1.0e-157l, 1.0e-158l, 1.0e-159l,
  1.0e-160l, 1.0e-161l, 1.0e-162l, 1.0e-163l, 1.0e-164l, 1.0e-165l, 1.0e-166l, 1.0e-167l, 1.0e-168l, 1.0e-169l,
  1.0e-170l, 1.0e-171l, 1.0e-172l, 1.0e-173l, 1.0e-174l, 1.0e-175l, 1.0e-176l, 1.0e-177l, 1.0e-178l, 1.0e-179l,
  1.0e-180l, 1.0e-181l, 1.0e-182l, 1.0e-183l, 1.0e-184l, 1.0e-185l, 1.0e-186l, 1.0e-187l, 1.0e-188l, 1.0e-189l,
  1.0e-190l, 1.0e-191l, 1.0e-192l, 1.0e-193l, 1.0e-194l, 1.0e-195l, 1.0e-196l, 1.0e-197l, 1.0e-198l, 1.0e-199l,
  1.0e-200l, 1.0e-201l, 1.0e-202l, 1.0e-203l, 1.0e-204l, 1.0e-205l, 1.0e-206l, 1.0e-207l, 1.0e-208l, 1.0e-209l,
  1.0e-210l, 1.0e-211l, 1.0e-212l, 1.0e-213l, 1.0e-214l, 1.0e-215l, 1.0e-216l, 1.0e-217l, 1.0e-218l, 1.0e-219l,
  1.0e-220l, 1.0e-221l, 1.0e-222l, 1.0e-223l, 1.0e-224l, 1.0e-225l, 1.0e-226l, 1.0e-227l, 1.0e-228l, 1.0e-229l,
  1.0e-230l, 1.0e-231l, 1.0e-232l, 1.0e-233l, 1.0e-234l, 1.0e-235l, 1.0e-236l, 1.0e-237l, 1.0e-238l, 1.0e-239l,
  1.0e-240l, 1.0e-241l, 1.0e-242l, 1.0e-243l, 1.0e-244l, 1.0e-245l, 1.0e-246l, 1.0e-247l, 1.0e-248l, 1.0e-249l,
  1.0e-250l, 1.0e-251l, 1.0e-252l, 1.0e-253l, 1.0e-254l, 1.0e-255l, 1.0e-256l, 1.0e-257l, 1.0e-258l, 1.0e-259l,
  1.0e-260l, 1.0e-261l, 1.0e-262l, 1.0e-263l, 1.0e-264l, 1.0e-265l, 1.0e-266l, 1.0e-267l, 1.0e-268l, 1.0e-269l,
  1.0e-270l, 1.0e-271l, 1.0e-272l, 1.0e-273l, 1.0e-274l, 1.0e-275l, 1.0e-276l, 1.0e-277l, 1.0e-278l, 1.0e-279l,
  1.0e-280l, 1.0e-281l, 1.0e-282l, 1.0e-283l, 1.0e-284l, 1.0e-285l, 1.0e-286l, 1.0e-287l, 1.0e-288l, 1.0e-289l,
  1.0e-290l, 1.0e-291l, 1.0e-292l, 1.0e-293l, 1.0e-294l, 1.0e-295l, 1.0e-296l, 1.0e-297l, 1.0e-298l, 1.0e-299l,
  1.0e-300l, 1.0e-301l, 1.0e-302l, 1.0e-303l, 1.0e-304l, 1.0e-305l, 1.0e-306l, 1.0e-307l, 1.0e-308l, 1.0e-309l,
  1.0e-310l, 1.0e-311l, 1.0e-312l, 1.0e-313l, 1.0e-314l, 1.0e-315l, 1.0e-316l, 1.0e-317l, 1.0e-318l, 1.0e-319l,
  1.0e-320l, 1.0e-321l, 1.0e-322l, 1.0e-323l
};

LONG_FLOAT *invpowtable[] = {
  NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, invpowtable10,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

// fast implementation of the powl function, using the repeated square
// method.  returns 0^0=1, to get continuity in x.

LONG_FLOAT powl_int_sm(LONG_FLOAT x, int n) {
  LONG_FLOAT value = 1.0l;
  if(x==1)
    return 1;
  if(n < 0) {
    if(x==0)
      return 1.0l / x;
    x = 1.0l / x;
    n = -n;
  }
  if(x==0)
    return x;
  do {
    if(n & 1)
      value *= x;
    n>>=1;
    x *= x;
  } while(n);
  return value;
}

LONG_FLOAT powl_int(int base, int exp) {
  if (exp<0) {
    if (exp < -ceil((IEEE754_MAX_EXP) * invlog2[base])) {
      return 0;
    }
    if(invpowtable[base]!=NULL) {
      return (invpowtable[base])[-exp];
    }
    else
      return powl_int_sm(base,exp);
  }
  else {
    if (exp >= ceil((IEEE754_MIN_EXP + 1) * invlog2[base])){
      return 1.0/0.0; // Inf
    }
    if(powtable[base]!=NULL){
      return (powtable[base])[exp];
    }
    else
      return powl_int_sm(base,exp);
  }
};

// this line lets to work with bases up to 36.  36 = 10 + 26, i.e,
// #digits + #letters 

// invlog2[X] = log(2)/log(X).

double invlog2[37] = {
                   0,            1.0/0.0,                  1, 0.6309297535714574,                0.5,
  0.4306765580733931, 0.3868528072345416, 0.3562071871080222, 0.3333333333333334, 0.3154648767857287,
  0.3010299956639812, 0.2890648263178878, 0.2789429456511298, 0.2702381544273197, 0.2626495350371936,
  0.2559580248098155,               0.25,  0.244650542118226, 0.2398124665681315, 0.2354089133666382,
  0.2313782131597592,  0.227670248696953, 0.2242438242175754, 0.2210647294575037, 0.2181042919855315,
  0.2153382790366965, 0.2127460535533631, 0.2103099178571525, 0.2080145976765095, 0.2058468324604344,
  0.2037950470905062, 0.2018490865820998,                0.2, 0.1982398631705605, 0.1965616322328226,
  0.1949590218937863, 0.1934264036172708
};

/*
  this line let to generate digits for any base up to 36.
 */

char digits_upper[]    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
char digits_lower[]    = "0123456789abcdefghijklmnopqrstuvwxyz";

/*
  Note that the exponent indicator is p to let the numbers be
  near (not fully compatible yet) to the ISO standar C99:

To: ciao@clip.dia.fi.upm.es
Subject: hex floats in gcc 3.3.1
From: Jose Morales <jfran@clip.dia.fi.upm.es>
Date: 28 Feb 2004 01:09:02 +0100


It seems that gcc supports hex floats (their coding scheme is
different from ours):

---------------------------------------------------------------------------
http://computing.ee.ethz.ch/sepp/gcc-3.3.1-mo/gcc_86.html#SEC86

5.11 Hex Floats

ISO C99 supports floating-point numbers written not only in the usual
decimal notation, such as 1.55e1, but also numbers such as 0x1.fp3
written in hexadecimal format. As a GNU extension, GCC supports this
in C89 mode (except in some cases when strictly conforming) and in
C++. In that format the `0x' hex introducer and the `p' or `P'
exponent field are mandatory. The exponent is a decimal number that
indicates the power of 2 by which the significant part will be
multiplied. Thus `0x1.f' is 1 15/16, `p3' multiplies it by 8, and the
value of 0x1.fp3 is the same as 1.55e1.

Unlike for floating-point numbers in the decimal notation the exponent
is always required in the hexadecimal notation. Otherwise the compiler
would not be able to resolve the ambiguity of, e.g., 0x1.f. This could
mean 1.0f or 1.9375 since `f' is also the extension for floating-point
constants of type float.


 */

char exponents_lower[] = "eeeeeeeeeeeppppppppppppppp___________";
char exponents_upper[] = "EEEEEEEEEEEPPPPPPPPPPPPPPP___________";

int char_digit[256];

void fillchardigit(void) {
  unsigned char i;
  unsigned int j;
  for(j=0;j<=255;j++)
    char_digit[j] = -1;
  for(i='0';i<='9';i++)
    char_digit[i] = i - '0';
  for(i='a';i<='z';i++)
    char_digit[i] = i - 'a' + 10;
  for(i='A';i<='Z';i++)
    char_digit[i] = i - 'A' + 10;
}

void fillpowtable(int base) {
  int i;
  int m = (int)floor(IEEE754_MAX_EXP*invlog2[base]);
  int n = (int)ceil(IEEE754_MIN_EXP*invlog2[base]);

  powtable[base] = (LONG_FLOAT *)malloc((n+1)*sizeof(LONG_FLOAT));
  for(i=0;i<=n;i++) {
    (powtable[base])[i] = powl_int(base, i);
  }

  invpowtable[base] = (LONG_FLOAT *)malloc((m+1)*sizeof(LONG_FLOAT));
  for(i=0;i<=m;i++) {
    (invpowtable[base])[i] = powl_int(base, -i);
  }
};

void freepowtable(int base) {
  free(powtable[base]);
  free(invpowtable[base]);
};

void fillallpowtable(void) {
  int base;
  for(base=2;base<=36;base++)
    if(base!=10)
      fillpowtable(base);
};
void freeallpowtable(void) {
  int base;
  for(base=2;base<=36;base++)
    if(base!=10)
      freepowtable(base);
};
