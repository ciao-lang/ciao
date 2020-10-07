/*
 *  attributes.h
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ATTRIBUTES_H
#define _CIAO_ATTRIBUTES_H

/* TODO: needed here because of tclp libs, provide a better interface? */
CBOOL__PROTO(bu1_detach_attribute, tagged_t x);
CBOOL__PROTO(bu2_attach_attribute, tagged_t var, tagged_t constr);
CBOOL__PROTO(bu2_update_attribute, tagged_t x, tagged_t constr);
CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x);

CVOID__PROTO(collect_one_pending_unification);
CVOID__PROTO(collect_pending_unifications, intmach_t wake_count);

#endif /* _CIAO_ATTRIBUTES_H */
