/*
 *  bignum.h
 *
 *  Bignum arithmetics (support for arbitrary size integers)
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Authors:
 *    Torbjorn Granlund, Johan Andersson, and Mats Carlsson
 *    Ref: Knuth vol. 2 sec. 4.3.1
 */
 
#ifndef _CIAO_BIGNUM_H
#define _CIAO_BIGNUM_H

#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support_macros.h>

bool_t bn_positive(bignum_t *x);
bignum_size_t bn_add(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_incr(bignum_t *x, bignum_t *ignore, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_plus(bignum_t *x, bignum_t *ignore, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_subtract(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_decr(bignum_t *x, bignum_t *ignore, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_minus(bignum_t *x, bignum_t *ignore, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_and(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_or(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_xor(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_not(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_lshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_rshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_compare(bignum_t *x, bignum_t *y);
bignum_size_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_quotient_remainder_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_quotient_remainder_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_from_float(bignum_t *x, bignum_t *ignore, bignum_t *z, bignum_t *zmax);
bignum_size_t bn_from_string(char *x, bignum_t *z, bignum_t *zmax, int base);
CVOID__PROTO(bn_to_string, bignum_t *x, int base);

#if BC_SCALE==2
size_t bn_scale_bc32(bignum_t *x);
int bn_canonized_length(bignum_t *x);
#endif

/* To be used by the test routines in ciao_prolog.c */

intmach_t bn_length(bignum_t *x);

#endif /* _CIAO_BIGNUM_H */
