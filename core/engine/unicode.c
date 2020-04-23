/*
 *  unicode.c
 *
 *  Support for Unicode source code in Ciao (see unicode_gen.pl)
 *
 *  Copyright (c) 2020 Jose F. Morales
 */

#include <stdio.h>
#include <stdint.h>

/* Automatically generated table */
#include "unicode_tbl.h"

static inline int range_left(int i) {
  return ranges[i]&((1<<VAL_SHIFT)-1);
}
static inline int range_val(int i) {
  return ranges[i]>>VAL_SHIFT;
}

/* Dichotomic search the class of rune `c` in the ranges array
   (requires around log2(RANGES_COUNT)=11 iterations) */
int rune_lookup_class(int c) {
  if (c>=MAX_RUNE) return 0;
  int min = 0;
  int max = RANGES_COUNT;
  int block = c/BLOCK_ELEMS;
  int offset = c&(BLOCK_ELEMS-1);
  int i;
  while(1) {
    i=(max+min)/2; // mid point
    // printf("%d %d %d\n", min, i, max);
    if (i == min) break;
    if (block < range_left(i)) max=i; else min=i;
  };
  return (dict[range_val(i)]>>(offset*CLBITS))&((1<<CLBITS)-1);
}
