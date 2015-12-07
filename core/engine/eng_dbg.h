/*
 *  eng_dbg.h
 *
 *  Support for debugging and tracing the engine code
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#ifndef _CIAO_ENG_DBG_H
#define _CIAO_ENG_DBG_H

// #define DEF_WR_DEBUG 1

#if defined(DEBUG)
#if !defined(DEF_WR_DEBUG)
#define DEF_WR_DEBUG 1
#endif
#endif

#if defined(DEF_WR_DEBUG)
CVOID__PROTO(wr_tagged, tagged_t t);
CVOID__PROTO(wr_tagged_rec, tagged_t t);
void wr_functor(char *s, definition_t *func);
CVOID__PROTO(wr_call, char *s, definition_t *func);
CVOID__PROTO(wr_functor_spec, tagged_t t);
#endif

#if defined(DEBUG)
extern bool_t debug_dynlink;
extern bool_t debug_gc;
extern bool_t debug_threads;
extern bool_t debug_choicepoints;
extern bool_t debug_concchoicepoints;
extern bool_t debug_mem;
extern bool_t debug_conc;
#endif

extern bool_t stop_on_pred_calls;

#endif /* _CIAO_ENG_DBG_H */
