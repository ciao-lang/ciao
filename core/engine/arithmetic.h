/*
 *  arithmetic.h
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2020 The Ciao Development Team
 */

#ifndef _CIAO_ARITHMETIC_H
#define _CIAO_ARITHMETIC_H

CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numge, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numle, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1);

CFUN__PROTO(fu1_add1, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_float, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_integer, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_minus, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_not, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_plus, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sub1, tagged_t, tagged_t x0);
CFUN__PROTO(fu2_and, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_gcd, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_idivide, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_minus, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_rem, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_mod, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_or, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_plus, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_rsh, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_times, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_xor, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu1_abs, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0);

CFUN__PROTO(fu1_intpart, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_floor, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_round, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_ceil, tagged_t, tagged_t x0);
CFUN__PROTO(fu2_pow, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu1_exp, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_log, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sin, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_cos, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_atan, tagged_t, tagged_t x0);

#endif /* _CIAO_ARITHMETIC_H */
