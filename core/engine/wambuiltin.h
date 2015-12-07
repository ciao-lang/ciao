/*
 *  wambuiltin.h
 *
 *  Routines for 'builtintab' predicates
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#ifndef _CIAO_WAMBUILTIN_H
#define _CIAO_WAMBUILTIN_H

CBOOL__PROTO(bu1_atom, tagged_t x0);
CBOOL__PROTO(bu1_atomic, tagged_t x0);
CBOOL__PROTO(bu1_float, tagged_t x0);
CBOOL__PROTO(bu1_if, tagged_t x0);
CBOOL__PROTO(bu1_integer, tagged_t x0);
CBOOL__PROTO(bu1_nonvar, tagged_t x0);
CBOOL__PROTO(bu1_number, tagged_t x0);
CBOOL__PROTO(bu1_var, tagged_t x0);
CBOOL__PROTO(bu2_lexeq, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexge, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexle, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexne, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numge, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numle, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_univ, tagged_t term, tagged_t list);
CBOOL__PROTO(bu3_functor, tagged_t term, tagged_t name, tagged_t arity);

CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t complex, bcp_t liveinfo);
CFUN__PROTO(fu2_compare, tagged_t, tagged_t x1, tagged_t x2, bcp_t liveinfo);

CFUN__PROTO(fu1_add1, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_float, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_integer, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_minus, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_not, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_plus, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_sub1, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu2_and, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_gcd, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_idivide, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_minus, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_rem, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_mod, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_or, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_plus, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_rsh, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_times, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu2_xor, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu1_abs, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_sign, tagged_t, tagged_t X0, bcp_t liveinfo);

CFUN__PROTO(fu1_intpart, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_floor, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_round, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_ceil, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu2_pow, tagged_t, tagged_t X0, tagged_t X1, bcp_t liveinfo);
CFUN__PROTO(fu1_exp, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_log, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_sin, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_cos, tagged_t, tagged_t X0, bcp_t liveinfo);
CFUN__PROTO(fu1_atan, tagged_t, tagged_t X0, bcp_t liveinfo);

#endif /* _CIAO_WAMBUILTIN_H */
