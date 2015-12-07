/*
 *  global_defs.h
 *
 *  Global definitions for locks, terms, predicates, predicate tables,
 *  workers, etc.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_GLOBAL_DEFS_H
#define _CIAO_GLOBAL_DEFS_H

#include <ciao/support_macros.h>

/*  INITIALIZATION INFO 
    definitions, structures, and global variables needed for
    initializations */

/* These should be local to a thread */

/* extern tagged_t heap_start, *heap_end, heap_warn, *heap_warn_soft,
   *stack_start, *stack_end, *stack_warn, tagged_choice_start, choice_start,
   *choice_end, trail_start, *trail_end; */

/* extern char *atom_buffer; */  /* Non-shared --- used by each worker */


/* I believe that atom_buffer_length can change dynamically; should be
   private to each worker, then.  Will that pose problems problems with the
   hashing functions? */

/* extern int atom_buffer_length; */  /* Non-shared --- used by each worker */


extern sw_on_key_t *prolog_predicates;    /* Shared -- never changes */
extern sw_on_key_t **predicates_location;                  /* Shared */
extern sw_on_key_t *prolog_modules;    /* Shared -- never changes */
extern sw_on_key_t **modules_location;                  /* Shared */

/* Database locks */
extern SLOCK    prolog_predicates_l;                      /* Pointer to it */
extern SLOCK    prolog_modules_l;                         /* Pointer to it */

/* Wait until new worker Id is generated */
extern SLOCK    worker_id_pool_l;
extern SLOCK    atom_id_l;
extern SLOCK    wam_list_l;

#if defined(PARBACK)
extern SLOCK wam_circular_list_l;   /* Mutex for adding stack sets into list */

extern worker_t *main_worker;  /* Pointer to main worker */

extern volatile int nagents;      /* Number of agents created */
extern SLOCK nagents_l;
#endif

#if defined(ANDPARALLEL)
extern SLOCK wam_circular_list_l;   /* Mutex for adding stack sets into list */
extern SLOCK stackset_expansion_l;  /* Mutex for stack set expansion */

extern worker_t *main_worker;  /* Pointer to main worker */
extern bool_t unwinding_done;         /* Has the unwinding been performed? */

extern int nagents;      /* Number of agents created */
extern SLOCK nagents_l;

/* Parallel goals */
extern int npargoals;
/* Parallel goals stolen */
extern int_par_t *npargoalstaken;
/* Backw exec over par local goal */
extern int_par_t *nlocalbacktr;
/* Backw exec over par rem top goal */
extern int_par_t *nrembacktr_top;
/* Backw exec over par rem trp goal */
extern int_par_t *nrembacktr_trapped;

extern SLOCK npargoals_l;
extern SLOCK npargoalstaken_l;
extern SLOCK nlocalbacktr_l;
extern SLOCK nrembacktr_top_l;
extern SLOCK nrembacktr_trapped_l;

extern bool_t measure;
#endif

#if defined(ANDPARALLEL) && defined(VISANDOR)
/* Event Tracing Flags etc for VisAndOr */
extern int nacagents;    /* Number of active agents */
extern int maxevents;    /* maximum number of events */
extern bool_t gen_event_file;
extern float time_at_event_start;
#endif

#if defined(DEBUG)
extern SLOCK    ops_counter_l;
#endif


extern bool_t wam_initialized;/* Non-shared? --- set by each worker to decide whether re-initialize or exit after a failure */

extern char symbolchar[];               /* Shared --- type of each symbol */

extern char *default_lib_dir;
extern char *default_c_headers_dir;

 /* All atom & functor definitions are shared */

#if defined(MARKERS)
extern tagged_t atom_success;
extern tagged_t atom_failure;
#endif

extern tagged_t atom_share;
extern tagged_t atom_noshare;
extern tagged_t atom_nil;
extern tagged_t atom_list;
extern tagged_t atom_read;
extern tagged_t atom_write;
extern tagged_t atom_append;
extern tagged_t atom_socket;
extern tagged_t atom_symlink;
extern tagged_t atom_regular;
extern tagged_t atom_directory;
extern tagged_t atom_fifo;
extern tagged_t atom_stdout;
extern tagged_t atom_unknown;
extern tagged_t atom_prolog;
extern tagged_t atom_lessthan;
extern tagged_t atom_greaterthan;
extern tagged_t atom_equal;
extern tagged_t atom_off;
extern tagged_t atom_on;
extern tagged_t atom_error;
extern tagged_t atom_trace;
extern tagged_t atom_debug;
extern tagged_t atom_fail;
extern tagged_t atom_all;
extern tagged_t atom_terse;
extern tagged_t atom_verbose;
extern tagged_t atom_compiled;
extern tagged_t atom_interpreted;
extern tagged_t atom_builtin;
extern tagged_t atom_true;
extern tagged_t atom_false;
extern tagged_t atom_retry_hook;
extern tagged_t atom_unprofiled;
extern tagged_t atom_profiled;
/*extern tagged_t atom_public;*/
extern tagged_t atom_wait;
extern tagged_t atom_dynamic;
extern tagged_t atom_concurrent;
extern tagged_t atom_multifile;
extern tagged_t atom_user;
extern tagged_t atom_att;

extern tagged_t atom_self;
extern tagged_t atom_create;

extern tagged_t atom_default_lib_dir;
extern tagged_t atom_default_c_headers_dir;

extern tagged_t atom_block;
extern tagged_t atom_no_block;

#if defined(GAUGE)
extern tagged_t atom_counter;
#endif

#if defined(USE_OVERFLOW_EXCEPTIONS)
extern tagged_t atom_undo_heap_overflow_excep;
#endif
extern tagged_t atom_heap_limit;

extern tagged_t functor_neck;
extern tagged_t functor_list;
extern tagged_t functor_cut;
extern tagged_t functor_minus;
extern tagged_t functor_slash;
extern tagged_t functor_and;
extern tagged_t functor_functor;
extern tagged_t functor_tagged;
extern tagged_t functor_emul_entry;
extern tagged_t functor_builtin;
extern tagged_t functor_Dref;
extern tagged_t functor_Dstream;
extern tagged_t functor_Dlock;
extern tagged_t functor_Dhandler;
extern tagged_t functor_Dsetarg;
extern tagged_t functor_large;
extern tagged_t functor_long;

extern tagged_t functor_active;
extern tagged_t functor_pending;
extern tagged_t functor_failed;
extern tagged_t functor_available;

extern tagged_t current_prompt;
extern tagged_t current_unknown;
/* extern tagged_t current_leash_mode; */
/* extern tagged_t current_maxdepth; */
/* extern tagged_t current_printdepth; */
/* extern tagged_t current_breaklevel; */
extern tagged_t current_compiling;
/* extern tagged_t current_character_escapes_flag; */
extern tagged_t current_ferror_flag;
/* extern tagged_t current_single_var_flag; */
/* extern tagged_t current_discontiguous_flag; */
/* extern tagged_t current_redefine_flag; */
extern tagged_t current_quiet_flag;
extern tagged_t current_gcmode;
extern tagged_t current_gctrace;
extern tagged_t current_gcmargin;
/*extern tagged_t current_debugger_state;*/  /* Now private */
/*extern tagged_t current_debugger_mode;*/   /* Now private */
extern tagged_t current_radix;

extern try_node_t *address_nd_repeat;
extern try_node_t *address_nd_current_instance;
extern try_node_t *address_nd_current_atom;
extern try_node_t *address_nd_current_predicate;
extern try_node_t *address_nd_predicate_property;
extern try_node_t *address_nd_current_stream;
extern try_node_t *address_nd_atom_concat;
#if defined(TABLING)
extern try_node_t *address_nd_fake_choicept;
#endif
#if defined(PARBACK)
extern try_node_t *address_nd_suspension_point;
extern bcp_t restart_point_insn;
#endif

extern definition_t *address_true;
extern definition_t *address_fail;

extern definition_t *address_call;
// #if defined(INTERNAL_CALLING)
// extern definition_t *address_internal_call;
// #endif
extern definition_t *address_interpret_goal;
extern definition_t *address_call_with_cont;
extern definition_t *address_interpret_compiled_goal;
extern definition_t *address_interpret_c_goal;
extern definition_t *address_undefined_goal;
extern definition_t *address_help; 
extern definition_t *address_restart; 
extern definition_t *address_trace;
extern definition_t *address_getct;
extern definition_t *address_getct1;
extern definition_t *address_get;
extern definition_t *address_get2;
extern definition_t *address_get1;
extern definition_t *address_get12;
extern definition_t *address_peek;
extern definition_t *address_peek2;
extern definition_t *address_skip;
extern definition_t *address_skip2;
extern definition_t *address_skip_line;
extern definition_t *address_skip_line1;
extern definition_t *address_error;

CBOOL__PROTO(nd_repeat);
CBOOL__PROTO(nd_current_atom);
CBOOL__PROTO(nd_current_clauses);
CBOOL__PROTO(nd_current_predicate);
CBOOL__PROTO(nd_predicate_property);
CBOOL__PROTO(nd_current_stream);
CBOOL__PROTO(nd_atom_concat);
#if defined(TABLING)
CBOOL__PROTO(nd_fake_choicept);
#endif
#if defined(PARBACK)
CBOOL__PROTO(nd_suspension_point);
#endif

definition_t *define_c_mod_predicate(char *module, char  *pname, int arity, bool_t (*procedure)());
void undefine_c_mod_predicate(char *module, char *pname, int arity); 

module_t *define_c_static_mod(char *module_name);

#endif /* _CIAO_GLOBAL_DEFS_H */
