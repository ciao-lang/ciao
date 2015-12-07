/*
 *  support.h
 *
 *  General runtime support routines.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_SUPPORT_H
#define _CIAO_SUPPORT_H

typedef bignum_size_t (*bn_fun_t)(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);

CBOOL__PROTO(cunify, tagged_t x1, tagged_t x2);
CBOOL__PROTO(cunify_args, int arity, tagged_t *pt1, tagged_t *pt2);
flt64_t get_float(tagged_t t);
intmach_t get_integer(tagged_t t);
bool_t float_is_finite(tagged_t t);
CFUN__PROTO(bn_call, tagged_t, bn_fun_t f, tagged_t x, tagged_t y, bcp_t liveinfo);
tagged_t init_atom_check(char *str);
CFUN__PROTO(make_float, tagged_t, flt64_t i);
CFUN__PROTO(make_float_check, tagged_t, flt64_t i, bcp_t liveinfo);
CFUN__PROTO(make_integer, tagged_t, intmach_t i);
CFUN__PROTO(make_integer_check, tagged_t, intmach_t i, bcp_t liveinfo);
CFUN__PROTO(make_large, tagged_t, tagged_t *ptr);
CFUN__PROTO(make_structure, tagged_t, tagged_t functor);
definition_t *find_definition(sw_on_key_t **swp, tagged_t term, tagged_t **argl, bool_t insertp);
definition_t *insert_definition(sw_on_key_t **swp, tagged_t tagpname, int arity, bool_t insertp);
definition_t *new_functor(tagged_t tagpname, int arity);
module_t *insert_module(sw_on_key_t **swp, tagged_t mod_atm, bool_t insertp);
module_t *new_module(tagged_t mod_atm);
definition_t *parse_definition(tagged_t complex);
stream_node_t *new_stream(tagged_t streamname, char *streammode, FILE *streamfile);
stream_node_t *stream_to_ptr(tagged_t t, int mode);
stream_node_t *stream_to_ptr_check(tagged_t t, int mode, int *errcode);
void add_definition(sw_on_key_t **swp, sw_on_key_node_t *node, tagged_t key, definition_t *def);
void add_module(sw_on_key_t **swp, sw_on_key_node_t *node, tagged_t key, module_t *mod);
void expand_sw_on_key(sw_on_key_t **psw, try_node_t *otherwise, bool_t deletep);
void failc(char *mesg);
CVOID__PROTO(numstack_init);
/* void update_std_streams(void); */
CBOOL__PROTO(prolog_show_nodes);
CBOOL__PROTO(prolog_show_all_nodes);
CBOOL__PROTO(start_node);
CVOID__PROTO(show_nodes, node_t *cp, node_t *end);

CBOOL__PROTO(cinstance);
CBOOL__PROTO(cground);

#endif /* _CIAO_SUPPORT_H */
