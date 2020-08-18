#ifndef _CIAO_ATOMIC_BASIC_H
#define _CIAO_ATOMIC_BASIC_H

void prolog_init_radix(void);

CBOOL__PROTO(nd_atom_concat);
CVOID__PROTO(number_to_string, tagged_t term, int base);
CBOOL__PROTO(string_to_number, char *str, int base, tagged_t *strnum, int arity);

/* (for registering) */ 
CBOOL__PROTO(prolog_atom_codes);
CBOOL__PROTO(prolog_atom_length);
CBOOL__PROTO(prolog_sub_atom);
CBOOL__PROTO(prolog_atom_concat);
CBOOL__PROTO(prolog_name);
CBOOL__PROTO(prolog_number_codes_2);
CBOOL__PROTO(prolog_number_codes_3);

#endif /* _CIAO_ATOMIC_BASIC_H */
