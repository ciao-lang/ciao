/*
 *  modload.h
 *
 *  Low-level module DB and dynamic code loading
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_MODLOAD_H
#define _CIAO_MODLOAD_H

#if defined(OPTIM_COMP)

/* TODO: it should be together with predicates_location */
extern hashtab_t *modules_location; /* Shared */
#if defined(ABSMACH_OPT__oo_extensions)
/* TODO: trait,impl,vtable,etc. */
extern hashtab_t *objfunctor_table; /* Shared */
#endif

CBOOL__PROTO(init_modload);

CINSNP__PROTO(code_call1);

CBOOL__PROTO(load_module_pack, FILE *qfile);

#endif /* defined(OPTIM_COMP) */

#endif /* _CIAO_MODLOAD_H */
