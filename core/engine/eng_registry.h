#ifndef _CIAO_REGISTRY_H
#define _CIAO_REGISTRY_H

#include <ciao/eng.h>

/* TODO: refine */

extern bool_t trace_calls;
#if defined(DEBUG)
extern bool_t trace_instr;
#endif

extern sw_on_key_t *switch_on_function;

intmach_t lookup_atom_idx(char *str);

#if defined(ABSMACH_OPT__atom_len)
atom_t *new_atom_check(char *str, 
                       unsigned int str_len,
                       unsigned int index);
#else
atom_t *new_atom_check(char *str, 
                       unsigned int index);
#endif

void glb_init_each_time(void);
CVOID__PROTO(init_each_time);
void init_once(void);
void init_locks(void);
CVOID__PROTO(init_streams_each_time);
CVOID__PROTO(local_init_each_time);

extern int reg_bank_size;

worker_t *free_wam(void);
CBOOL__PROTO(program_usage);
CBOOL__PROTO(internal_symbol_usage);
CBOOL__PROTO(statistics);
CBOOL__PROTO(total_usage);
worker_t *create_wam_storage(void);
CVOID__PROTO(create_wam_areas);
CVOID__PROTO(reinitialize_wam_areas);
CVOID__PROTO(release_wam);

#endif /* _CIAO_REGISTRY_H */
