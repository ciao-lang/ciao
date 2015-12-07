/*
 *  wam_alloc.h
 *
 *  Allocation of principal WAM areas.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_WAM_ALLOC_H
#define _CIAO_WAM_ALLOC_H

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

extern intmach_t num_of_predicates;
extern intmach_t mem_prog_count;

#if defined(DEBUG)
#define INC_MEM_PROG(SIZE) { 					 \
  if (debug_mem) { 						 \
    fprintf(stderr, "Program memory increased by %" PRIdm " bytes\n",	 \
	    (intmach_t)(SIZE));					 \
  }                                                              \
  mem_prog_count = mem_prog_count + (SIZE);	                 \
  }
#define DEC_MEM_PROG(SIZE) { 					 \
  if (debug_mem) {						 \
    fprintf(stderr, "Program memory decreased by %" PRIdm " bytes\n",	 \
	    (intmach_t)(SIZE));				         \
  }						                 \
  mem_prog_count = mem_prog_count - (SIZE);                      \
  }
#else
#define INC_MEM_PROG(SIZE) { mem_prog_count = mem_prog_count + (SIZE); }
#define DEC_MEM_PROG(SIZE) { mem_prog_count = mem_prog_count - (SIZE); }
#endif

#endif /* _CIAO_WAM_ALLOC_H */
