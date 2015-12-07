/*
 *  objareas.h
 *
 *  Database management support code.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_OBJAREAS_H
#define _CIAO_OBJAREAS_H

#include <ciao/absmach_predef.h>

/* static void relocate_table_clocks(sw_on_key_t *sw, instance_clock_t *clocks) */

CBOOL__PROTO(prolog_purge);
CBOOL__PROTO(prolog_erase);
CBOOL__PROTO(prolog_ptr_ref);
CBOOL__PROTO(inserta);
CBOOL__PROTO(insertz);
size_t compile_large(tagged_t t, bcp_t p);
#if BC_SCALE==2
size_t compile_large_bc32(tagged_t t, bcp_t p);
#endif
CBOOL__PROTO(make_bytecode_object);
CFUN__PROTO(active_instance, instance_t *, instance_t *i, int itime, bool_t normal);
CVOID__PROTO(clock_overflow);
void relocate_clocks(instance_t *inst,  instance_clock_t *clocks);
void expunge_instance(instance_t *i);
CFUN__PROTO(active_instance_conc, instance_t *, instance_t *i, int_info_t *pred_root);

#endif /* _CIAO_OBJAREAS_H */
