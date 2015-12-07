/*
 *  wamsupport.h
 *
 *  Basic emulator support.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_WAMSUPPORT_H
#define _CIAO_WAMSUPPORT_H

extern bcp_t bootcode;
// #if defined(INTERNAL_CALLING)
// extern bcp_t internal_calling;
// #endif
extern bcp_t startgoalcode;
extern bcp_t startgoalcode_cont;

#define CONTCODE(Arity) BCoff(contcode, (Arity) * (FTYPE_size(f_e) + FTYPE_size(f_o)))
extern bcp_t contcode;

extern bcp_t failcode;
extern bcp_t exitcode;
extern try_node_t *termcode;
extern try_node_t *fail_alt;

void init_some_bytecode(void);

int p2_offset(uintmach_t insn);
try_node_t *def_retry_c(cbool_pred_t proc, int arity);

CBOOL__PROTO(set_trace_calls);
CBOOL__PROTO(run_determ_c, tagged_t goal);

#endif /* _CIAO_WAMSUPPORT_H */
