/*
 *  attr.h
 *
 *  Copyright (C) 1992 Department of Medical Cybernetics & Artificial
 *    Intelligence.  University of Vienna.  Freyung 6.  A-1010 Vienna,
 *    Austria.
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author: Christian Holzbaur [christian@ai.univie.ac.at]
 *    Permission to use this software for any purpose is subject to
 *    the USER AGREEMENT between the DMCAI and the User.
 */

#ifndef _CIAO_ATTR_H
#define _CIAO_ATTR_H

CBOOL__PROTO(bu1_detach_attribute, tagged_t x);
CBOOL__PROTO(bu2_attach_attribute, tagged_t var, tagged_t constr);
CBOOL__PROTO(bu2_update_attribute, tagged_t x, tagged_t constr);
CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x);
CFUN__PROTO(fu1_type, tagged_t, tagged_t t0);
CVOID__PROTO(collect_one_pending_unification);
CVOID__PROTO(collect_pending_unifications, intmach_t wake_count);

// uncomment to enable C version of the multi-attributes accessors.
// #define USE__FAST_MULTIATTR 1

#if defined(USE__FAST_MULTIATTR)
CBOOL__PROTO(get_attr__3);
CBOOL__PROTO(put_attr__3);
CBOOL__PROTO(del_attr__2);
#endif

#endif /* _CIAO_ATTR_H */
