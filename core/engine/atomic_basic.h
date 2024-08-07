/*
 *  atomic_basic.h
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ATOMIC_BASIC_H
#define _CIAO_ATOMIC_BASIC_H

CVOID__PROTO(number_to_string, tagged_t term, int base);
CBOOL__PROTO(string_to_number, char *str, int base, tagged_t *strnum, int liveregs);

#endif /* _CIAO_ATOMIC_BASIC_H */
