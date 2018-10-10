/*
 *  debug.h
 *
 *  Code for debuging tabling.
 *
 *  XS
 */

#ifndef _CIAO_TABLING_DEBUG_H
#define _CIAO_TABLING_DEBUG_H

#if defined(TABLING)

//#if defined(DEBUG_ALL)
extern tagged_t tabling_debug;
extern tagged_t tabling_debug_tries;
extern tagged_t tabling_trace;
extern tagged_t tabling_print;
extern tagged_t tabling_bypass;
//#endif

#if defined(ANS_COUNTER)
CBOOL__PROTO(print_counters_c);
extern intmach_t ans_no_saved, ans_removed, ans_saved, ans_aggregated;
#endif

CBOOL__PROTO(set_tabling_flag_c);
CBOOL__PROTO(current_tabling_flag_c);

#endif

#endif /* _CIAO_TABLING_DEBUG_H */
