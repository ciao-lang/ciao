/*
 *  indexing.h
 *
 *  Support for the incremental clause compiler.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_INDEXING_H
#define _CIAO_INDEXING_H

sw_on_key_node_t *incore_gethash(sw_on_key_t *sw, tagged_t key);
sw_on_key_t *new_switch_on_key(intmach_t size, try_node_t *otherwise);
void leave_to_gc(enter_instr_t type, char *info);
CBOOL__PROTO(empty_gcdef_bin);
void relocate_gcdef_clocks(instance_clock_t *clocks);
CBOOL__PROTO(prolog_abolish); /* JFMC */
CBOOL__PROTO(abolish, definition_t *f); /* JFMC */
CBOOL__PROTO(define_predicate);
CBOOL__PROTO(erase_clause);
CBOOL__PROTO(clause_number);
CBOOL__PROTO(compiled_clause);
sw_on_key_node_t *dyn_puthash(sw_on_key_t **swp, tagged_t k);
CBOOL__PROTO(set_property);

#endif /* _CIAO_INDEXING_H */
