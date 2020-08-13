/*
 *  runtime_control.h
 *

 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_RUNTIME_CONTROL_H
#define _CIAO_RUNTIME_CONTROL_H

#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept);
#endif
CVOID__PROTO(pop_choicept);
CVOID__PROTO(push_choicept, try_node_t *alt);
CBOOL__PROTO(nd_atom_concat);
CBOOL__PROTO(current_atom);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(current_clauses);
CBOOL__PROTO(prolog_repeat);
CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(module_is_static);

#endif /* _CIAO_RUNTIME_CONTROL_H */
