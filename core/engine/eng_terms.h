/*
 *  eng_terms.h
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_TERMS_H
#define _CIAO_ENG_TERMS_H

#if defined(OPTIM_COMP)
#error "not valid for OPTIM_COMP"
#endif

/* ------------------------------------------------------------------------- */
/* Some definitions for integer types */

/* TODO:[oc-merge] move closer to eng_predefs.pl/eng_predefs.h */
/* Sizes */
#if tagged__size == 64
#define INTMACH_MAX INT64_MAX
#define INTMACH_MIN INT64_MIN
#elif tagged__size == 32
#define INTMACH_MAX INT32_MAX
#define INTMACH_MIN INT32_MIN
#endif

/* --------------------------------------------------------------------------- */
/* Bit manipulation operations */

/* TODO:[JF] use generic __builtin_ctzg, etc.? (simpler code) */

/* operations for uint32_t ('unsigned int') */
#define LSB32(x) (__builtin_ctz(x))
#define MSB32(x) (31 - __builtin_clz(x))
#define POPCOUNT32(x) (__builtin_popcount(x))
#if tagged__size == 64 /* assume LP64 */
/* operations for uint64_t ('unsigned long' or 'unsigned long long') */
#define LSB64(x) (__builtin_ctzl(x))
#define MSB64(x) (63 - __builtin_clzl(x))
#define POPCOUNT64(x) (__builtin_popcountl(x))
#elif tagged__size == 32 /* assume ILP32 */
/* operations for uint64_t ('unsigned long long') */
#define LSB64(x) (__builtin_ctzll(x))
#define MSB64(x) (63 - __builtin_clzll(x))
#define POPCOUNT64(x) (__builtin_popcountll(x))
#endif

#if tagged__size == 64 /* 64 bit intval_t */
#define intval_LSB LSB64
#define intval_MSB MSB64
#define intval_POPCOUNT POPCOUNT64
#elif tagged__size == 32 /* 32 bit intval_t */
#define intval_LSB LSB32
#define intval_MSB MSB32
#define intval_POPCOUNT POPCOUNT32
#endif

/* ------------------------------------------------------------------------- */
/* Macros for formatting integers */

#include <inttypes.h> /* for PRI* macros */
#if tagged__size == 64
#define PRIum PRIu64 /* intmach_t using %u */
#define PRIdm PRId64 /* intmach_t using %d */
#define PRIxm PRIx64 /* intmach_t using %x */
#elif tagged__size == 32
#define PRIum PRIu32 /* intmach_t using %u */
#define PRIdm PRId32 /* intmach_t using %d */
#define PRIxm PRIx32 /* intmach_t using %x */
#endif

#endif /* _CIAO_ENG_TERMS_H */
