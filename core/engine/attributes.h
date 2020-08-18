/*
 *  attributes.h
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_ATTRIBUTES_H
#define _CIAO_ATTRIBUTES_H

CBOOL__PROTO(bu1_detach_attribute, tagged_t x);
CBOOL__PROTO(bu2_attach_attribute, tagged_t var, tagged_t constr);
CBOOL__PROTO(bu2_update_attribute, tagged_t x, tagged_t constr);
CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x);
CVOID__PROTO(collect_one_pending_unification);
CVOID__PROTO(collect_pending_unifications, intmach_t wake_count);

// uncomment to enable C version of the multi-attributes accessors.
// #define USE__FAST_MULTIATTR 1

#if defined(USE__FAST_MULTIATTR)
CBOOL__PROTO(get_attr__3);
CBOOL__PROTO(put_attr__3);
CBOOL__PROTO(del_attr__2);
#endif

#endif /* _CIAO_ATTRIBUTES_H */
