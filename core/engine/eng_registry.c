/*
 *  eng_registry.c
 *
 *  Global registry for atoms, functors, and predicates.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2020 The Ciao Development Team
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#include <ciao/eng.h>
#include <ciao/eng_start.h>
#include <ciao/attributes.h>
#include <ciao/internals.h>
#include <ciao/basiccontrol.h>

#include <ciao/eng_interrupt.h>

#include <ciao/timing.h>
#include <ciao/stream_basic.h>

#include <ciao/eng_registry.h>

/* (only for registering) */
#include <ciao/rune.h>
#include <ciao/io_basic.h>
#include <ciao/dynamic_rt.h>
#include <ciao/eng_gc.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/qread.h>
#include <ciao/modload.h>

#if defined(PROFILE)
#define __USE_GNU
# include <dlfcn.h>
#endif

/* local declarations */

static CBOOL__PROTO(prolog_atom_mode);
static definition_t *define_builtin(char *pname, int instr, int arity);
static void classify_atom(atom_t *s);
static CBOOL__PROTO(prolog_ciao_c_headers_dir);
static void deffunction(char *atom, int arity, void *proc, int funcno);

statistics_t ciao_stats = {
  0, /*flt64_t ss_tick*/
  0, /*intmach_t ss_global*/
  0, /*intmach_t ss_local*/
  0, /*intmach_t ss_control*/
  0, /*inttime_t gc_tick*/
  0, /*intmach_t gc_count*/
  0, /*intmach_t gc_acc*/

  0, /*inttime_t starttick*/
  0, /*inttime_t lasttick*/

  /* keep in mind that the frequency values can be redefined to better
     values */

  0, /*inttime_t startwalltick*/
  0, /*inttime_t lastwalltick*/
  0, /*inttime_t wallclockfreq*/

  0, /*inttime_t startusertick*/
  0, /*inttime_t lastusertick*/
  0, /*inttime_t userclockfreq*/

  0, /*inttime_t startsystemtick*/
  0, /*inttime_t lastsystemtick*/
  0  /*inttime_t systemclockfreq*/
};                                /* Shared */

sw_on_key_node_t **atmtab; /* Shared --- but need lock when accessing /
                                   reallocing it! */
sw_on_key_t *ciao_atoms;  /* Shared -- need lock when adding atoms */
static char                  /* Shared -- for adding new atoms; need lock */
  *prolog_chars=NULL,
  *prolog_chars_end=NULL;

void *builtintab[64];                                           /* Shared */

                /* Shared -- related with the hashing of arith. functions */
sw_on_key_t *switch_on_function;

bool_t profile                = FALSE;       /* profile execution -- Shared */
bool_t profile_eng            = FALSE;
bool_t profile_rcc            = FALSE;

#if defined(PROFILE)
CVOID__PROTO(profile__hook_nop) {};
CVOID__PROTO(profile__hook_call_nop, definition_t *f) {};
#endif

#if defined(PROFILE)
/* TODO: add macro to define typedefs from prototypes, use ## */
CVOID__PROTO((*profile__hook_fail)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_redo)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_cut)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_call), definition_t *f) = profile__hook_call_nop;
#if defined(PROFILE__TRACER)
CVOID__PROTO((*profile__hook_proceed)) = profile__hook_nop;
#endif
#endif

/* tagged_t *heap_start , *heap_end, *heap_warn, *heap_warn_soft, stack_start,
   *stack_end, *stack_warn, tagged_choice_start, choice_start, choice_end,
   *trail_start, *trail_end; */

/* char *atom_buffer; */ /* Non shared */
/* int atom_buffer_length; */ /* Non shared */

sw_on_key_t  *prolog_predicates = NULL;                    /* Shared */
sw_on_key_t **predicates_location = &prolog_predicates;    /* Shared */

sw_on_key_t  *prolog_modules = NULL;                    /* Shared */
sw_on_key_t **modules_location = &prolog_modules;    /* Shared */

#if defined(USE_THREADS) && defined(USE_POSIX_THREADS)
pthread_attr_t detached_thread;
pthread_attr_t joinable_thread;
#endif

/* All-predicates lock; this might be held for a long time */
SLOCK    prolog_predicates_l;
SLOCK    prolog_modules_l;

/* Memory management lock (for internal structures) */
SLOCK    mem_mng_l;

/* We are giving worker IDs from a pool, in order to speed up task
   creation. This pool needs exclusive access. */
SLOCK    worker_id_pool_l;

/* Count for new atom names; if accessed concurrently, we want no repeated
   identifiers.  */
SLOCK    atom_id_l;

/* The creation of new streams should be atomic. */
LOCK stream_list_l;

bool_t in_abort_context = FALSE;

/* Event Tracing Flags etc */

#if defined(ANDPARALLEL) || defined(PARBACK)
nagents = 1;
SLOCK nagents_l;
#endif

#if defined(ANDPARALLEL)
// Statistics
int npargoals = 0;
int_par_t *npargoalstaken = NULL;
int_par_t *nlocalbacktr = NULL;
int_par_t *nrembacktr_top = NULL;
int_par_t *nrembacktr_trapped = NULL;
SLOCK npargoals_l;
SLOCK npargoalstaken_l;
SLOCK nlocalbacktr_l;
SLOCK nrembacktr_top_l;
SLOCK nrembacktr_trapped_l;
bool_t measure = TRUE;
#endif

#if defined(ANDPARALLEL) && defined(VISANDOR)
int nacagents = 1;
int maxevents = 20000;  /* maximum number of events (20,000 is default) */
bool_t gen_event_file = FALSE;
float time_at_event_start = 0.0;
#endif

/* Shared .... */

#if defined(MARKERS)
tagged_t atom_success;
tagged_t atom_failure;
#endif

tagged_t atom_share;             /* "share" */
tagged_t atom_noshare;           /* "noshare" */
tagged_t atom_nil;              /* "[]" */
tagged_t atom_list;             /* "." */
tagged_t atom_read;             /* "read"  */
tagged_t atom_write;            /* "write" */
tagged_t atom_append;           /* "append" */
tagged_t atom_socket;           /* "socket" */
tagged_t atom_symlink;          /* "symlink" */
tagged_t atom_regular;          /* "regular" */
tagged_t atom_directory;                /* "directory" */
tagged_t atom_fifo;             /* "fifo" */
tagged_t atom_stdout;           /* "stdout" */
tagged_t atom_unknown;          /* "unknown" */
tagged_t atom_prolog;           /* "prolog"  */
tagged_t atom_lessthan;         /* "<" */
tagged_t atom_greaterthan;      /* ">" */
tagged_t atom_equal;            /* "=" */
tagged_t atom_off;              /* "off" */
tagged_t atom_on;               /* "on" */
tagged_t atom_error;            /* "error" */
tagged_t atom_trace;            /* "trace" */
tagged_t atom_debug;            /* "debug" */
tagged_t atom_fail;             /* "fail" */
tagged_t atom_all;              /* "all" */
tagged_t atom_terse;            /* "terse" */
tagged_t atom_verbose;          /* "verbose" */
tagged_t atom_compiled;         /* "compiled" */
tagged_t atom_interpreted;      /* "interpreted" */
tagged_t atom_builtin;          /* "built_in" */
tagged_t atom_true;             /* "true" */
tagged_t atom_false;            /* "false" */
tagged_t atom_retry_hook;       /* "$$retry_hook" */
tagged_t atom_unprofiled;       /* "unprofiled" */
tagged_t atom_profiled;         /* "profiled" */
/* tagged_t atom_public; */             /* "public" */
tagged_t atom_concurrent;       /* "concurrent" */
tagged_t atom_wait;             /* "wait" */
tagged_t atom_dynamic;          /* "dynamic" */
tagged_t atom_multifile;                /* "multifile" */
tagged_t atom_user;            /* "user" */
tagged_t atom_att;             /* "att" */

tagged_t atom_default_ciaoroot;
tagged_t atom_default_c_headers_dir;

tagged_t atom_block;            /* "block" */
tagged_t atom_no_block;         /* "no_block" */

tagged_t atom_self;                   /* "self" */
tagged_t atom_create;                 /* "create" */

#if defined(GAUGE)
tagged_t atom_counter;           /* "counter" */
#endif

tagged_t functor_neck;
tagged_t functor_lst;
tagged_t functor_cut;
tagged_t functor_minus;
tagged_t functor_slash;
tagged_t functor_and;
tagged_t functor_functor;
tagged_t functor_tagged;
tagged_t functor_emul_entry;
tagged_t functor_builtin;
tagged_t functor_Dref;
tagged_t functor_Dstream;
tagged_t functor_Dlock;
tagged_t functor_Dhandler;
tagged_t functor_Dsetarg;
tagged_t functor_large;
tagged_t functor_long;

tagged_t functor_active;
tagged_t functor_pending;
tagged_t functor_failed;
tagged_t functor_available;

tagged_t current_prompt;
tagged_t current_unknown;
/* tagged_t current_leash_mode; */
/* tagged_t current_maxdepth; */
/* tagged_t current_printdepth; */
/* tagged_t current_breaklevel; */
tagged_t current_compiling;
tagged_t current_ferror_flag;
tagged_t current_quiet_flag;
/* tagged_t current_debugger_state; */ /* Now private */
/* tagged_t current_debugger_mode;  */  /* Now private */
tagged_t current_radix;

try_node_t *address_nd_repeat;
try_node_t *address_nd_current_atom;
try_node_t *address_nd_current_predicate;
try_node_t *address_nd_predicate_property;
try_node_t *address_nd_current_stream;
try_node_t *address_nd_atom_concat;

#if defined(TABLING)
try_node_t *address_nd_fake_choicept;
tagged_t functor_forward_trail;
double trail_time;
tagged_t *global_table;
tagged_t *tabling_stack;
tagged_t *global_table_free;
tagged_t *tabling_stack_free;
tagged_t *global_table_end;
tagged_t *tabling_stack_end;
#endif

#if defined(PARBACK)
try_node_t *address_nd_suspension_point;
bcp_t restart_point_insn;
#endif

try_node_t *address_nd_yield;

definition_t *address_true;
definition_t *address_fail;
definition_t *address_call;
// #if defined(INTERNAL_CALLING)
//  definition_t *address_internal_call = NULL;
// #endif
definition_t *address_interpret_goal;
definition_t *address_interpret_compiled_goal;
definition_t *address_interpret_c_goal;
definition_t *address_undefined_goal;
definition_t *address_help; 
definition_t *address_restart; 
definition_t *address_trace;
definition_t *address_getct;
definition_t *address_getct1;
definition_t *address_get;
definition_t *address_get2;
definition_t *address_get1;
definition_t *address_get12;
definition_t *address_peek;
definition_t *address_peek2;
definition_t *address_get_byte1;
definition_t *address_get_byte2;
definition_t *address_peek_byte1;
definition_t *address_peek_byte2;
definition_t *address_skip;
definition_t *address_skip2;
definition_t *address_skip_line;
definition_t *address_skip_line1;
definition_t *address_error;        /* Handle errors in Prolog (DCG)*/

/* Attributed variables support */
definition_t *address_pending_unifications;
definition_t *address_uvc;
definition_t *address_ucc;

/*------------------------------------------------------------*/

/* insert atom in global table */
/*  MCL: there is an implicit assumption that the table is not full */

#if defined(ABSMACH_OPT__atom_len)
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw,
                                      tagged_t key,
                                      char *str,
                                      uintmach_t str_len)
#else
static sw_on_key_node_t *atom_gethash(sw_on_key_t *sw,
                                      tagged_t key,
                                      char *str)
#endif
{
  sw_on_key_node_t *hnode;
#if defined(ATOMGC)
  sw_on_key_node_t *first_erased = NULL;
#endif
  intmach_t i;
  tagged_t t0;

  for (i=0, t0=key & sw->mask;
       ;
       i+=sizeof(sw_on_key_node_t), t0=(t0+i) & sw->mask) {
    hnode = SW_ON_KEY_NODE_FROM_OFFSET(sw, t0);
#if !defined(ATOMGC)
    if ((hnode->key==key 
#if defined(ABSMACH_OPT__atom_len)
         && hnode->value.atomp->atom_len == str_len
#endif
         && strcmp(hnode->value.atomp->name, str)==0) ||
        !hnode->key)
      return hnode;
#else
    if ((hnode->key == key) 
#if defined(ABSMACH_OPT__atom_len)
        && hnode->value.atomp->atom_len == str_len
#endif
        && (strcmp(hnode->value.atomp->name, str) == 0))
      return hnode;
    else if (!hnode->key)
      return first_erased ? first_erased : hnode;
    else if (hnode->key == 1 && !first_erased)
      first_erased = hnode;
#endif
  }
}

intmach_t lookup_atom_idx(char *str) {
  sw_on_key_node_t *hnode;
  unsigned int hashcode = 0;
  intmach_t count, size;
  intmach_t current_mem = total_mem_count;
  char *c = str;

#if defined(ABSMACH_OPT__atom_len)
  uintmach_t atom_len = 0;
#endif
  
  while (*c) {
    hashcode = (hashcode<<1) + *((unsigned char *)c++);
#if defined(ABSMACH_OPT__atom_len)
    atom_len++;
#endif
  }

  hashcode = (hashcode<<3)+4;   /* low bits are masked away; ensure it is
                                   not 0 --- it cannot be 1, either, which is
                                   very important for atom GC */
/*
  while ((hnode=incore_gethash(ciao_atoms, (tagged_t)hashcode)) &&
         hnode->key==(tagged_t)hashcode &&
         strcmp(hnode->value.atomp->name, str)!=0)
    hashcode += 233509<<3;         233509 is prime, and so is
                                   233509&0x1ffff, 233509&0x0ffff, ...,
                                   233509&0x00007
*/

#if defined(ABSMACH_OPT__atom_len)
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str, atom_len);
#else
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str);
#endif

#if defined(ATOMGC)
  if (hnode->key && hnode->key != 1) /* if ATOMGC, '1' marks freed position */
#else
  if (hnode->key)
#endif
    return hnode->value.atomp->index;

  if ((count=ciao_atoms->count) > MaxAtomCount) {
    SERIOUS_FAULT("the atom table is full");
  }

  /* Check for a full table, and expand if needed */

  if ((count+1)<<1 > (size=SwitchSize(ciao_atoms))) {
    sw_on_key_t *new_table = new_switch_on_key(size<<1, NULL);
    intmach_t i;
    sw_on_key_node_t *h1, *h2;

#if defined(ATOMGC) && defined(DEBUG)
    /*printf("Reallocing atom table (count = %d)\n", count);*/
#endif

    for (i=0; i<count; i++){
#if defined(ATOMGC)   /* Actually, if the table is full, no entry should be
                         null... */
       /* size *= 2; */
      if ((h1 = atmtab[i]) != NULL) { /* There may be holes when doing GC */
#if defined(ABSMACH_OPT__atom_len)
        atmtab[i] = h2 = atom_gethash(new_table, 
                                      h1->key, 
                                      str,
                                      h1->value.atomp->atom_len);
#else
        atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
        h2->key = h1->key;
        h2->value.atomp = h1->value.atomp;
      }
#else
      h1 = atmtab[i];
#if defined(ABSMACH_OPT__atom_len)
      atmtab[i] = h2 = atom_gethash(new_table, 
                                    h1->key, 
                                    str,
                                    h1->value.atomp->atom_len);
#else
      atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
      h2->key = h1->key;
      h2->value.atomp = h1->value.atomp;
#endif
    }

    atmtab = checkrealloc_ARRAY(sw_on_key_node_t *,
                                count,
                                2*count,
                                (tagged_t *)atmtab);

#if defined(ATOMGC)      /* Clean up the upper part of the new atom table */
    for (i = count; i < 2*count; i++)
      atmtab[i] = NULL;
    new_table->next_index = count;
#endif

    checkdealloc_FLEXIBLE(sw_on_key_t,
                          sw_on_key_node_t,
                          size,
                          ciao_atoms);
    new_table->count = count;
#if defined(ABSMACH_OPT__atom_len)
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str, atom_len);
#else
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str);
#endif
    ciao_atoms = new_table;
    size = size << 1;
  }
  hnode->key = (tagged_t)hashcode;

#if defined(ATOMGC)
    size = size >> 1;     /* atmtab size is one half of ciao_atoms size */
    count = ciao_atoms->next_index;
    while(atmtab[count])                  /* There must be one free entry */
      count =  (count + 1) % size;
    /*ciao_atoms->next_index+1 == size ? 0 : ciao_atoms->next_index+1;*/
    /* next_index should point to a free entry in the table */
    ciao_atoms->next_index = count;
#endif

#if defined(ABSMACH_OPT__atom_len)
  hnode->value.atomp = new_atom_check(str, atom_len, count);
#else
  hnode->value.atomp = new_atom_check(str, count);
#endif
  atmtab[count] = hnode;

  ciao_atoms->count++;

  INC_MEM_PROG(total_mem_count - current_mem);

  return count;
}

/*-----------------------------------------------------------*/

intmach_t goal_from_thread_id(THREAD_ID id); /* concurrency.c */

void failc(char *mesg) {
  extern char source_path[];

  // Give an error.  We check if we were able to allocate memory at all for
  // the user_eror stream (since we are using the same routines to allocate
  // all memory, either tagged or not, we may have failed to allocate
  // memory for streams).  This should not be necessary once we separate
  // the memory management 

  if (!stream_user_error) {
    fprintf(stderr, "{ERROR (%s): %s}\n", source_path, mesg);
    fprintf(stderr, 
"{Ciao was probably compiled in a machine with a different memory model.}\n");
    fprintf(stderr, 
"{Please recompile Ciao in this machine and try again.}\n");
  } else {
    // Issue a simple message if a single thread is running.
    if (num_tasks_created() > 1) {
      THREAD_ID thrid = Thread_Id;   // Local Id

      intmach_t goal_id = goal_from_thread_id(thrid);
      if (goal_id != 0) {
        fprintf(stderr,
                "{ERROR (%s, goal 0x%" PRIxm ", thread 0x%" PRIxm "): %s}\n",
                source_path, (uintmach_t)goal_id,
                (uintmach_t)thrid, mesg);
      } else {
        fprintf(stderr,
                "{ERROR (%s, thread 0x%" PRIxm "): %s}\n",
                source_path,
                (uintmach_t)thrid, mesg);
      }
    } else {
      fprintf(stderr, "{ERROR: %s}\n", mesg);
    }
  }
  if (!in_abort_context){
    fprintf(stderr, "Wam not initialized, exiting!!!\n");
    engine_exit(-1);
  }
}

/* --------------------------------------------------------------------------- */

/* (see tokenizer.pl) */
static void classify_atom(atom_t *s) {
  const unsigned char *cp;
  bool_t seen_alpha = FALSE;
  bool_t seen_symbol = FALSE;
  
  s->has_dquote = FALSE;        /* TRUE if symbolchars only */
  s->has_squote = FALSE;        /* TRUE if ! ; [] {} OR contains a "'" */
  s->has_special = FALSE;       /* TRUE if needs quoting */

  cp = (const unsigned char *)s->name;
  c_rune_t r;
  int typ;

  NextRune(cp, r, typ);
  /* patch 'solo' typ if there is a single code */
  if (*cp==0 && typ == RUNETY_IDCONT) typ=RUNETY_LOWERCASE;
  
  int typ0 = typ; /* save typ for first rune */

  while (r != 0) {
    if (r=='\'') {
      s->has_squote = s->has_special = TRUE;
    } else if (typ == RUNETY_LOWERCASE ||
               typ == RUNETY_UPPERCASE ||
               typ == RUNETY_DIGIT) {
      seen_alpha = TRUE;
    } else if (typ == RUNETY_SYMBOL) {
      seen_symbol = TRUE;
    } else {
      s->has_special = TRUE;
    }
    NextRune(cp, r, typ);
    if (typ == RUNETY_IDCONT) typ=RUNETY_LOWERCASE; /* not 'solo' now */
  }

  s->has_dquote = (!s->has_special & !seen_alpha);
  s->has_special |= (seen_alpha==seen_symbol);

  /* check cases '!' ';' '[]' '{}' '_...' 'A...' '9...' '.' '/ * ...' */
  /* NB: No point in quoting '... * /' */
  
  cp = (unsigned char *)s->name;
  unsigned char c0 = cp[0];
  unsigned char c1 = cp[1];
  unsigned char c2 = cp[2];
  if (s->has_special && !s->has_squote &&
      ((c0=='!' && c1==0) ||
       (c0==';' && c1==0) ||
       (c0=='[' && c1==']' && c2==0) ||
       (c0=='{' && c1=='}' && c2==0))) {
    s->has_special=FALSE;
    s->has_squote=TRUE;
  } else if (!s->has_special &&
             (typ0 == RUNETY_UPPERCASE ||
              typ0 == RUNETY_DIGIT ||
              typ0 == RUNETY_IDCONT || /* first 'solo' requires special */
              (c0=='.' && c1==0) ||
              (c0=='/' && c1=='*'))) {
    s->has_special=TRUE;
  }
}


/* This astonishing piece of code makes an atom from its name and index in
   the hash table.  The memory for the (atom_t) is taken from a linear
   chunk of memory within which a pointer is advanced to reclaim memory for
   the atoms.  When there is not enough room for a new atom, a new area of
   memory is allocated.  MCL.  Unfortunately:

   a) Some space is probably wasted at the end of the prolog_chars memory
   area every time more memory is needed, and

   b) I could not find an easy way to give memory back to the memory manager
   in the event of atom deletion.

   This scheme is supposed to be very fast, because memory is allocated in
   fixed, but I still have to check if just calling the (general) memory
   manager is really that much inefficient.  Doing so would solve completely
   the problem of freeing atom table memory.
*/

/* MCL: changed to solve problems with fixed amounts of increments and the
   (new) variable max_atom_size */

#define MIN_MEM_CHUNK_SIZE 4096
#define MAX(a, b) (a > b ? a : b)

#if defined(ABSMACH_OPT__atom_len)
atom_t *new_atom_check(char *str,
                       unsigned int str_len,
                       unsigned int index)
#else
atom_t *new_atom_check(char *str,
                       unsigned int index)
#endif
{
  atom_t *s;

#if defined(ABSMACH_OPT__atom_len)
  int len = SIZEOF_FLEXIBLE_STRUCT(atom_t, char, str_len + 1);
#else
  int len = SIZEOF_FLEXIBLE_STRUCT(atom_t, char, strlen(str) + 1);
#endif
  
  /* Adjust to a tagged pointer */
  prolog_chars = (char *)ALIGN_TO(sizeof(tagged_t), (uintptr_t)prolog_chars);
  if (prolog_chars+len > prolog_chars_end) {             /* Out of bounds */
    prolog_chars = checkalloc_ARRAY(char, MAX(MIN_MEM_CHUNK_SIZE, len));
    prolog_chars_end = prolog_chars + MAX(MIN_MEM_CHUNK_SIZE, len);
  }
  
  s = (atom_t *)prolog_chars;
  prolog_chars += len;
  s->index = index;
  (void) strcpy(s->name, str);
#if defined(ABSMACH_OPT__atom_len)
  s->atom_len = str_len;
#endif
  classify_atom(s);

#if defined(USE_THREADS)
  /*  Quite amazingly, the latter seems to be faster than just

    s->atom_lock_l = &s->atom_lock_st;
    Init_slock(s->atom_lock_l);

    in a i586, probably because it helps to keep the size of the atoms 
    smaller, and so it favours the cache behavior.
  */
  Init_lock(s->atom_lock_l);
  Init_slock(s->counter_lock);
  s->atom_lock_counter = 1;                           /* MUTEX by default */
#endif
  return s;
}

/*
void reclassify_atoms(void)
{
  int i;

  for (i=0; i<ciao_atoms->count; i++)
    classify_atom(atmtab[i]->value.atomp);
}
*/              


/* $atom_mode(+Atom, -Context)
 * Context = 2'000 for alpha
 * Context = 2'001 for quote
 * Context = 2'010 for other
 * Context = 2'100 for punct, depending on how Atom is printed.
 */
static CBOOL__PROTO(prolog_atom_mode)
{
  atom_t *atomptr;

  DEREF(X(0),X(0));
  atomptr = TaggedToAtom(X(0));
  if (atomptr->has_special)
    CBOOL__UnifyCons(MakeSmall(1),X(1))
  else if (atomptr->has_dquote)
    CBOOL__UnifyCons(MakeSmall(2),X(1))
  else if (atomptr->has_squote)
    CBOOL__UnifyCons(MakeSmall(4),X(1))
  else
    CBOOL__UnifyCons(TaggedZero,X(1))

  return TRUE;
}

static CBOOL__PROTO(prolog_ciao_root)
{
  DEREF(X(0),X(0));
  CBOOL__LASTUNIFY(X(0), atom_default_ciaoroot);
}

static CBOOL__PROTO(prolog_ciao_c_headers_dir)
{
  DEREF(X(0),X(0));
  CBOOL__LASTUNIFY(X(0), atom_default_c_headers_dir);
}

static definition_t *define_builtin(char *pname, int instr, int arity) {
  definition_t *func;
  intmach_t current_mem = total_mem_count;
  
  func = insert_definition(predicates_location,GET_ATOM(pname),arity,TRUE);
  SetEnterInstr(func,instr);
  INC_MEM_PROG(total_mem_count - current_mem);
  return func;
}

tagged_t deffunctor(char *pname, int arity) {
  return SetArity(GET_ATOM(pname),arity);
}

/*
  module: Module in which the definition goes
  pname:  Prolog predicate name
  arity:  Arity of predicate
  procedure: Pointer to C function
 */
definition_t *define_c_mod_predicate(char *module,
                                     char *pname,
                                     int arity,
                                     cbool0_t procedure)
{
  definition_t *func;
  sw_on_key_node_t *keyval;
  char mod_pname[STATICMAXATOM]; /* Pred. name with module name prepended */
  tagged_t mod_tagpname;    /* Def. of predicate with module name prepended */
  tagged_t key;
  intmach_t current_mem = total_mem_count;
               
  if (strlen(module) + strlen(pname) > STATICMAXATOM){     /* Check sizes */
    char errmsg[STATICMAXATOM+STATICMAXATOM+512];

    strcpy(errmsg, "Predicate ");
    strcat(errmsg, pname);
    strcat(errmsg, " in module ");
    strcat(errmsg, module);
    strcat(errmsg, " gave a predicate name too long!");
    USAGE_FAULT(errmsg);
  }

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = GET_ATOM(mod_pname);
  key = SetArity(mod_tagpname, arity);

  Wait_Acquire_slock(prolog_predicates_l);

  keyval = (sw_on_key_node_t *)incore_gethash(prolog_predicates,key);

  if (keyval->key)
    func = keyval->value.def;
  else {
    func = new_functor(mod_tagpname,arity);
    add_definition(predicates_location,keyval,key,func);
  }

  Release_slock(prolog_predicates_l);

  /* Previously in the else-part (DCG) */
  /*func->properties.public = public;*/
  SetEnterInstr(func,ENTER_C);
  func->code.proc = (void *)procedure;

  INC_MEM_PROG(total_mem_count - current_mem);
  return func;
}

void undefine_c_mod_predicate(char *module, char *pname, int arity) {
  definition_t *f;
  char mod_pname[STATICMAXATOM]; /* Pred. name with module name prepended */
  tagged_t mod_tagpname;    /* Def. of predicate with module name prepended */

  strcpy(mod_pname, module);/* No need to check length -- already and atom */
  strcat(mod_pname, ":");
  strcat(mod_pname, pname);
  mod_tagpname = GET_ATOM(mod_pname);

  f = insert_definition(predicates_location, mod_tagpname, arity, FALSE);

  abolish(NULL, f);
}

/*
  Define a static module
  module: Module name
 */
module_t *define_c_static_mod(char *module_name)
{
  module_t *mod;
  sw_on_key_node_t *keyval;
  tagged_t mod_atm;    /* Def. of predicate with module name prepended */
  tagged_t key;
  intmach_t current_mem = total_mem_count;

  mod_atm = GET_ATOM(module_name);
  key = mod_atm;

  Wait_Acquire_slock(prolog_modules_l);

  keyval = (sw_on_key_node_t *)incore_gethash(prolog_modules, key);

  if (keyval->key)
    mod = keyval->value.mod;
  else {
    mod = new_module(mod_atm);
    add_module(modules_location,keyval,key,mod);
  }

  Release_slock(prolog_modules_l);

  mod->properties.is_static = TRUE;

  INC_MEM_PROG(total_mem_count - current_mem);
  return mod;
}

/*---------------------------------------------------------------
  GENERAL INITIALIZATION
  ------------------------------------------------------------  */

static void deffunction(char *atom,
                        int arity,
                        void *proc,
                        int funcno)
{
  tagged_t k = deffunctor(atom,arity);
  sw_on_key_node_t *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.proc = builtintab[funcno] = (void *)proc;
}

static void deffunction_nobtin(char *atom,
                               int arity,
                               void *proc)
{
  tagged_t k = deffunctor(atom,arity);
  sw_on_key_node_t *node = incore_gethash(switch_on_function,k);

  node->key = k;
  node->value.proc = proc;
}

/* Initializations that need to be made once only. */

void init_locks(void){
#if defined(USE_THREADS)
  Init_slock(prolog_predicates_l);
  Init_slock(prolog_modules_l);
#if defined(DEBUG)
  Init_slock(ops_counter_l);
#endif

  Init_lock(stream_list_l);
  Init_slock(mem_mng_l);
  Init_slock(worker_id_pool_l);
  Init_slock(atom_id_l);
  Init_slock(wam_list_l);

#if defined(ANDPARALLEL)
  Init_slock(stackset_expansion_l);
  Init_slock(wam_circular_list_l);
  Init_slock(nagents_l);
  Init_slock(npargoals_l);
  Init_slock(npargoalstaken_l);
  Init_slock(nlocalbacktr_l);
  Init_slock(nrembacktr_top_l);
  Init_slock(nrembacktr_trapped_l);
#endif

#if defined(PARBACK)
  Init_slock(wam_circular_list_l);
  Init_slock(nagents_l);
#endif
#endif
}

void init_profile(void) {
#if defined(PROFILE)
  if (profile_eng) {
    void (*profile_init)(void) =
      (void (*)(void))dlsym(RTLD_DEFAULT, "profile_init");
    void (*profile_enter_call_)(void) =
      (void (*)(void))dlsym(RTLD_DEFAULT, "profile_enter_call_");
    profile_rcc = TRUE;
    if (profile_init) profile_init();
    if (profile_enter_call_) profile_enter_call_();
  }
#endif
}

CVOID__PROTO(finish_profile) {
#if defined(PROFILE)
  if (profile_eng) {
    void (*profile__stop_exit_)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile__stop_exit");
    void (*profile_dump_)(FILE *) = (void (*)(FILE *))dlsym(RTLD_DEFAULT, "profile_dump");
    if (profile__stop_exit_) profile__stop_exit_();
    if (profile_dump_) profile_dump_(stdout);
  }
#endif
}

extern char *ciaoroot_directory;
extern char *c_headers_directory;

tagged_t atm_var, atm_attv, atm_float, atm_int, atm_str, atm_atm, atm_lst;

/* --------------------------------------------------------------------------- */
/* (prototypes for registering) */
/* bc_aux.h */
CBOOL__PROTO(metachoice);
CBOOL__PROTO(metacut);
/* term_basic.c */
CBOOL__PROTO(prolog_copy_term);
CBOOL__PROTO(prolog_copy_term_nat);
CBOOL__PROTO(prolog_cyclic_term);
CBOOL__PROTO(prolog_unifiable);
CBOOL__PROTO(prolog_unifyOC);
/* internals.c */
CBOOL__PROTO(prolog_global_vars_set_root);
CBOOL__PROTO(prolog_global_vars_get_root);
CBOOL__PROTO(prompt);
CBOOL__PROTO(unknown);
CBOOL__PROTO(setarg);
CBOOL__PROTO(undo);
CBOOL__PROTO(frozen);
CBOOL__PROTO(defrost);
CBOOL__PROTO(compiling);
CBOOL__PROTO(ferror_flag);
CBOOL__PROTO(quiet_flag);
CBOOL__PROTO(prolog_radix);
CBOOL__PROTO(constraint_list);
CBOOL__PROTO(prolog_eq);
CBOOL__PROTO(prolog_interpreted_clause);
CBOOL__PROTO(prolog_erase_atom);
CBOOL__PROTO(prolog_show_nodes);
CBOOL__PROTO(prolog_show_all_nodes);
CBOOL__PROTO(start_node);
/* stream_basic.c */
CBOOL__PROTO(prolog_print_emulator_version);
CBOOL__PROTO(prolog_force_interactive);
CBOOL__PROTO(prolog_sourcepath);
CBOOL__PROTO(prolog_open);
CBOOL__PROTO(prolog_close);
CBOOL__PROTO(prolog_pipe);
CBOOL__PROTO(prolog_current_input);
CBOOL__PROTO(prolog_set_input);
CBOOL__PROTO(prolog_current_output);
CBOOL__PROTO(prolog_set_output);
CBOOL__PROTO(prolog_get_stream);
CBOOL__PROTO(prolog_replace_stream);
CBOOL__PROTO(prolog_stream_code);
CBOOL__PROTO(character_count);
CBOOL__PROTO(line_position);
CBOOL__PROTO(line_count);
CBOOL__PROTO(current_stream);
/* io_basic.c */
CBOOL__PROTO(flush_output);
CBOOL__PROTO(flush_output1);
CBOOL__PROTO(code_class);
CBOOL__PROTO(rune_class);
CBOOL__PROTO(getct);
CBOOL__PROTO(getct1);
CBOOL__PROTO(get);
CBOOL__PROTO(get2);
CBOOL__PROTO(get1);
CBOOL__PROTO(get12);
CBOOL__PROTO(peek);
CBOOL__PROTO(peek2);
CBOOL__PROTO(nl);
CBOOL__PROTO(nl1);
CBOOL__PROTO(put);
CBOOL__PROTO(put2);
CBOOL__PROTO(tab);
CBOOL__PROTO(tab2);
CBOOL__PROTO(skip);
CBOOL__PROTO(skip2);
CBOOL__PROTO(skip_line);
CBOOL__PROTO(skip_line1);
CBOOL__PROTO(get_byte1);
CBOOL__PROTO(get_byte2);
CBOOL__PROTO(peek_byte1);
CBOOL__PROTO(peek_byte2);
CBOOL__PROTO(put_byte1);
CBOOL__PROTO(put_byte2);
CBOOL__PROTO(at_end_of_stream0);
CBOOL__PROTO(at_end_of_stream1);
CBOOL__PROTO(prolog_display);
CBOOL__PROTO(prolog_display2);
CBOOL__PROTO(prolog_displayq);
CBOOL__PROTO(prolog_displayq2);
CBOOL__PROTO(prolog_clearerr);
CBOOL__PROTO(prolog_fast_read_in_c);
CBOOL__PROTO(prolog_fast_write_in_c);
CBOOL__PROTO(prolog_format_print_float);
CBOOL__PROTO(prolog_format_print_integer);
CBOOL__PROTO(raw_copy_stdout);
CBOOL__PROTO(prolog_set_unbuf);
CBOOL__PROTO(prolog_input_wait);
/* arithmetic.c */
CBOOL__PROTO(bu2_numeq, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numge, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numle, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_numne, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu1_add1, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_float, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_integer, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_minus, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_not, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_plus, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sub1, tagged_t, tagged_t x0);
CFUN__PROTO(fu2_and, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_fdivide, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_gcd, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_idivide, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_lsh, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_minus, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_rem, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_mod, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_or, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_plus, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_rsh, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_times, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_xor, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu1_abs, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sign, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_intpart, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_fractpart, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_floor, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_round, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_ceil, tagged_t, tagged_t x0);
CFUN__PROTO(fu2_pow, tagged_t, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu1_exp, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_log, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sqrt, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_sin, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_cos, tagged_t, tagged_t x0);
CFUN__PROTO(fu1_atan, tagged_t, tagged_t x0);
/* term_compare.c */
CBOOL__PROTO(bu2_lexeq, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexge, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexgt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexle, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexlt, tagged_t x0, tagged_t x1);
CBOOL__PROTO(bu2_lexne, tagged_t x0, tagged_t x1);
CFUN__PROTO(fu2_compare, tagged_t, tagged_t x1, tagged_t x2);
/* term_typing.c */
CBOOL__PROTO(bu1_atom, tagged_t x0);
CBOOL__PROTO(bu1_atomic, tagged_t x0);
CBOOL__PROTO(bu1_float, tagged_t x0);
CBOOL__PROTO(bu1_if, tagged_t x0);
CBOOL__PROTO(bu1_integer, tagged_t x0);
CBOOL__PROTO(bu1_nonvar, tagged_t x0);
CBOOL__PROTO(bu1_number, tagged_t x0);
CBOOL__PROTO(bu1_var, tagged_t x0);
CFUN__PROTO(fu1_type, tagged_t, tagged_t t0);
CBOOL__PROTO(cground);
/* term_basic.c */
CFUN__PROTO(fu2_arg, tagged_t, tagged_t number, tagged_t complex);
CBOOL__PROTO(bu3_functor, tagged_t term, tagged_t name, tagged_t arity);
CBOOL__PROTO(bu2_univ, tagged_t term, tagged_t list);
/* debugger_support.c */
CBOOL__PROTO(retry_cut);
CBOOL__PROTO(debugger_state);
CBOOL__PROTO(debugger_mode);
CBOOL__PROTO(spypoint);
/* terms_check.c */
CBOOL__PROTO(cinstance);
/* atomic_basic.c */ 
CBOOL__PROTO(prolog_atom_codes);
CBOOL__PROTO(prolog_atom_length);
CBOOL__PROTO(prolog_sub_atom);
CBOOL__PROTO(prolog_atom_concat);
CBOOL__PROTO(nd_atom_concat); /* TODO: .pl decl should have a pair */
CBOOL__PROTO(prolog_name);
CBOOL__PROTO(prolog_number_codes_2);
CBOOL__PROTO(prolog_number_codes_3);
/* attributes.c */
CBOOL__PROTO(bu1_detach_attribute, tagged_t x);
CBOOL__PROTO(bu2_attach_attribute, tagged_t var, tagged_t constr);
CBOOL__PROTO(bu2_update_attribute, tagged_t x, tagged_t constr);
CFUN__PROTO(fu1_get_attribute, tagged_t, tagged_t x);
#if defined(USE_FAST_MULTIATTR)
CBOOL__PROTO(get_attr__3);
CBOOL__PROTO(put_attr__3);
CBOOL__PROTO(del_attr__2);
#endif
/* concurrency.c */
CBOOL__PROTO(prolog_eng_call);
CBOOL__PROTO(prolog_eng_backtrack);
CBOOL__PROTO(prolog_eng_cut);
CBOOL__PROTO(prolog_eng_release);
CBOOL__PROTO(prolog_eng_wait);
CBOOL__PROTO(prolog_eng_kill);
CBOOL__PROTO(prolog_eng_killothers);
CBOOL__PROTO(prolog_eng_status);
CBOOL__PROTO(prolog_eng_self);
CBOOL__PROTO(prolog_lock_atom);
CBOOL__PROTO(prolog_unlock_atom);
CBOOL__PROTO(prolog_lock_atom_state);
CBOOL__PROTO(prolog_unlock_predicate);
/* system.c */
CBOOL__PROTO(prolog_using_windows);
CBOOL__PROTO(prolog_exec);
CBOOL__PROTO(prolog_wait);
CBOOL__PROTO(prolog_kill);
CBOOL__PROTO(prolog_unix_cd);
CBOOL__PROTO(prolog_exec_shell);
CBOOL__PROTO(prolog_fd_dup);
CBOOL__PROTO(prolog_fd_close);
CBOOL__PROTO(prolog_unix_argv);
CBOOL__PROTO(prolog_unix_exit);
CBOOL__PROTO(prolog_unix_mktemp);
CBOOL__PROTO(prolog_unix_access);
CBOOL__PROTO(prolog_directory_files);
CBOOL__PROTO(prolog_file_properties);
CBOOL__PROTO(prolog_touch);
CBOOL__PROTO(prolog_unix_chmod);
CBOOL__PROTO(prolog_unix_umask);
CBOOL__PROTO(prolog_unix_delete);
CBOOL__PROTO(prolog_unix_rename);
CBOOL__PROTO(prolog_unix_mkdir);
CBOOL__PROTO(prolog_unix_rmdir);
CBOOL__PROTO(prolog_current_host);
CBOOL__PROTO(prolog_c_environs);
CBOOL__PROTO(prolog_c_get_env);
CBOOL__PROTO(prolog_c_set_env);
CBOOL__PROTO(prolog_c_del_env);
CBOOL__PROTO(prolog_c_current_env);
CBOOL__PROTO(prolog_c_errno);
CBOOL__PROTO(prolog_c_strerror);
CBOOL__PROTO(prolog_c_copy_file);
CBOOL__PROTO(prolog_c_winpath);
CBOOL__PROTO(prolog_c_posixpath);
CBOOL__PROTO(prolog_c_winfile);
CBOOL__PROTO(prolog_c_posixfile);
CBOOL__PROTO(prolog_pause);
CBOOL__PROTO(prolog_getpid);
CBOOL__PROTO(prolog_getuid);
CBOOL__PROTO(prolog_getgid);
CBOOL__PROTO(prolog_getpwnam);
CBOOL__PROTO(prolog_getgrnam);
CBOOL__PROTO(prolog_get_numcores);
CBOOL__PROTO(prolog_find_file);
CBOOL__PROTO(prolog_path_is_absolute);
CBOOL__PROTO(prolog_expand_file_name);
CBOOL__PROTO(prolog_extract_paths);
CBOOL__PROTO(prolog_getarch);
CBOOL__PROTO(prolog_getos);
CBOOL__PROTO(prolog_eng_debug_level);
CBOOL__PROTO(prolog_eng_is_sharedlib);
CBOOL__PROTO(prolog_get_ciao_ext);
CBOOL__PROTO(prolog_get_exec_ext);
CBOOL__PROTO(prolog_get_so_ext);
CBOOL__PROTO(prolog_version);
CBOOL__PROTO(prolog_get_foreign_opts_cc);
CBOOL__PROTO(prolog_get_foreign_opts_ld);
CBOOL__PROTO(prolog_get_foreign_opts_ccshared);
CBOOL__PROTO(prolog_get_foreign_opts_ldshared);
CBOOL__PROTO(prolog_current_executable);
CBOOL__PROTO(prolog_now);
CBOOL__PROTO(prolog_datime);
/* timing.c */
CBOOL__PROTO(prolog_runtime);
CBOOL__PROTO(prolog_usertime);
CBOOL__PROTO(prolog_systemtime);
CBOOL__PROTO(prolog_walltime);
CBOOL__PROTO(prolog_walltick);
CBOOL__PROTO(prolog_usertick);
CBOOL__PROTO(prolog_systemtick);
CBOOL__PROTO(prolog_runtick);
CBOOL__PROTO(prolog_wallclockfreq);
CBOOL__PROTO(prolog_userclockfreq);
CBOOL__PROTO(prolog_systemclockfreq);
CBOOL__PROTO(prolog_runclockfreq);
/* modload.c */
CBOOL__PROTO(prolog_dynlink);
CBOOL__PROTO(prolog_dynunlink);

/* --------------------------------------------------------------------------- */

static void define_functions(void)
{
   /* Note: the size of the hash table (64) needs to be expanded when
      new functions are added */
  switch_on_function = new_switch_on_key(64,NULL);

  deffunction("-",1,(void *)fu1_minus,0);
  deffunction("+",1,(void *)fu1_plus,1);
  deffunction("--",1,(void *)fu1_sub1,2); /* shorthand for 'SUB1 FUNCTION' */
  deffunction("++",1,(void *)fu1_add1,3); /* shorthand for 'ADD1 FUNCTION' */
  deffunction("integer",1,(void *)fu1_integer,4);
  deffunction("truncate",1,(void *)fu1_integer,4); /* alias of integer/1 (ISO)*/
  deffunction("float",1,(void *)fu1_float,5);
  deffunction("\\",1,(void *)fu1_not,6);
  deffunction("+",2,(void *)fu2_plus,7);
  deffunction("-",2,(void *)fu2_minus,8);
  deffunction("*",2,(void *)fu2_times,9);
  deffunction("/",2,(void *)fu2_fdivide,10);
  deffunction("//",2,(void *)fu2_idivide,11);
  deffunction("rem",2,(void *)fu2_rem,12); /* was "mod" (ISO) */
  deffunction("#",2,(void *)fu2_xor,13); /* was "^" */
  deffunction("/\\",2,(void *)fu2_and,14);
  deffunction("\\/",2,(void *)fu2_or,15);
  deffunction("<<",2,(void *)fu2_lsh,16);
  deffunction(">>",2,(void *)fu2_rsh,17);
  deffunction("mod",2,(void *)fu2_mod,18); /* (ISO) */
  deffunction("abs",1,(void *)fu1_abs,19); /* (ISO) */
  deffunction("sign",1,(void *)fu1_sign,49); /* (ISO) */
  deffunction_nobtin("gcd",2,(void *)fu2_gcd);

  /* all of these ISO */
  deffunction("float_integer_part",1,(void *)fu1_intpart,50);
  deffunction("float_fractional_part",1,(void *)fu1_fractpart,51);
  deffunction("floor",1,(void *)fu1_floor,52);
  deffunction("round",1,(void *)fu1_round,53);
  deffunction("ceiling",1,(void *)fu1_ceil,54);
  deffunction("**",2,(void *)fu2_pow,55);
  deffunction_nobtin("exp",1,(void *)fu1_exp);
  deffunction_nobtin("log",1,(void *)fu1_log);
  deffunction_nobtin("sqrt",1,(void *)fu1_sqrt);
  deffunction_nobtin("sin",1,(void *)fu1_sin);
  deffunction_nobtin("cos",1,(void *)fu1_cos);
  deffunction_nobtin("atan",1,(void *)fu1_atan);

  /* 41 & 42 are 68 & 79 in SICStus 2.1 */
  deffunction("ARG FUNCTION",2,(void *)fu2_arg,41);
  deffunction("COMPARE FUNCTION",2,(void *)fu2_compare,42);
}

/* The ...STKSIZE constants may be overridden by env. variables.
   This macro first looks for one, and if not found, uses the default. */
#define GETENV(VALUE,WORK,STRING,VAR) \
  if ((WORK = getenv(STRING))) \
    VALUE = atoi(WORK); \
  else \
    VALUE = VAR;

void init_once(void)
{
  int i;
#if defined(ATOMGC)
  int j;
#endif
  char *cp;

  /* Init time variables */
  reset_timing();

#if defined(USE_THREADS)
  INIT_THREADS;
#endif

  GETENV(i,cp,"ATMTABSIZE",ATMTABSIZE);
  atmtab = checkalloc_ARRAY(sw_on_key_node_t *, i);

  ciao_atoms = new_switch_on_key(2*i,NULL);

#if defined(ATOMGC)
  for (j=0; j < i; j++)
    atmtab[j] = NULL;
#endif

  /* Predicate and module database initialization */
  prolog_predicates = new_switch_on_key(2,NULL);
  prolog_modules = new_switch_on_key(2,NULL);

  define_functions();                  /* Uses builtintab up to number 17 */

  builtintab[20] = (void *)bu1_atom;
  builtintab[21] = (void *)bu1_atomic;
  builtintab[22] = (void *)bu1_float;
  builtintab[23] = (void *)bu1_integer;
  builtintab[24] = (void *)bu1_nonvar;
  builtintab[25] = (void *)bu1_number;
  builtintab[26] = (void *)bu1_var;
  builtintab[27] = (void *)bu2_lexeq;
  builtintab[28] = (void *)bu2_lexne;
  builtintab[29] = (void *)bu2_lexlt;
  builtintab[30] = (void *)bu2_lexge;
  builtintab[31] = (void *)bu2_lexgt;
  builtintab[32] = (void *)bu2_lexle;
  builtintab[33] = (void *)bu2_numeq;
  builtintab[34] = (void *)bu2_numne;
  builtintab[35] = (void *)bu2_numlt;
  builtintab[36] = (void *)bu2_numge;
  builtintab[37] = (void *)bu2_numgt;
  builtintab[38] = (void *)bu2_numle;
  builtintab[39] = (void *)bu1_if;
  builtintab[40] = (void *)bu2_univ;
  /* builtintab[41] = (void *)bu3_arg; */
  /* builtintab[42] = (void *)bu3_compare; */
  builtintab[43] = (void *)bu3_functor;

  atm_var   = GET_ATOM("var");
  atm_attv  = GET_ATOM("attv");
  atm_float = GET_ATOM("float");
  atm_int   = GET_ATOM("integer");
  atm_str   = GET_ATOM("structure");
  atm_atm   = GET_ATOM("atom");
  atm_lst   = GET_ATOM("list"); 
    
  builtintab[44] = (void *)fu1_type;                         
  builtintab[45] = (void *)fu1_get_attribute;
  builtintab[46] = (void *)bu2_attach_attribute;
  builtintab[47] = (void *)bu2_update_attribute;
  builtintab[48] = (void *)bu1_detach_attribute;
  
#if defined(MARKERS)
  atom_success=GET_ATOM("success");
  atom_failure=GET_ATOM("failure");
#endif

  atom_share=GET_ATOM("share");
  atom_noshare=GET_ATOM("noshare");
  atom_read=GET_ATOM("read");
  atom_write=GET_ATOM("write");
  atom_append=GET_ATOM("append");
  atom_socket=GET_ATOM("socket");
  atom_symlink=GET_ATOM("symlink");
  atom_regular=GET_ATOM("regular");
  atom_directory=GET_ATOM("directory");
  atom_fifo=GET_ATOM("fifo");
  atom_stdout=GET_ATOM("stdout");
  atom_unknown=GET_ATOM("unknown");
  atom_prolog=GET_ATOM("prolog");
  atom_lessthan=GET_ATOM("<");
  atom_greaterthan=GET_ATOM(">");
  atom_equal=GET_ATOM("=");
  atom_list = GET_ATOM(".");
  atom_nil = GET_ATOM("[]");
  atom_on = GET_ATOM("on");
  atom_off = GET_ATOM("off");
  atom_error = GET_ATOM("error");
  atom_trace = GET_ATOM("trace");
  atom_debug = GET_ATOM("debug");
  atom_fail = GET_ATOM("fail");
  atom_all = GET_ATOM("all");
  atom_terse = GET_ATOM("terse");
  atom_verbose = GET_ATOM("verbose");
  atom_compiled = GET_ATOM("compiled");
  atom_interpreted = GET_ATOM("interpreted");
  atom_builtin = GET_ATOM("built_in");
  atom_true = GET_ATOM("true");
  atom_false = GET_ATOM("false");
  atom_retry_hook = GET_ATOM("$$retry_hook");

  atom_unprofiled = GET_ATOM("unprofiled");
  atom_profiled = GET_ATOM("profiled");

  /* atom_public = GET_ATOM("public"); */
  atom_concurrent = GET_ATOM("concurrent");
  atom_wait = GET_ATOM("wait");
  atom_dynamic = GET_ATOM("dynamic");
  atom_multifile = GET_ATOM("multifile");
  atom_user = GET_ATOM("user");
  atom_att = GET_ATOM("att");

  atom_block = GET_ATOM("block");
  atom_no_block = GET_ATOM("no_block");


  atom_self = GET_ATOM("self");
  atom_create = GET_ATOM("create");

#if defined(GAUGE)
  atom_counter = GET_ATOM("counter");
#endif

  atom_default_ciaoroot = GET_ATOM(ciaoroot_directory);
  atom_default_c_headers_dir = GET_ATOM(c_headers_directory);

  init_gc();
  current_unknown = atom_error;
/*   current_leash_mode = MakeSmall(0xf); */
/*   current_maxdepth = MakeSmall(100000); */
/*   current_printdepth = MakeSmall(10); */
  current_compiling = atom_unprofiled;
  current_ferror_flag = atom_on;
  current_quiet_flag = atom_off;
  
  init_streams();

  functor_neck = deffunctor(":-",2);
  functor_lst = deffunctor(".",2);
  functor_cut = deffunctor("!",0);
  functor_minus = deffunctor("-",2);
  functor_slash = deffunctor("/",2);
  functor_and = deffunctor(",",2);
  functor_functor = deffunctor("functor",1);
  functor_tagged = deffunctor("tagged",1);
  functor_emul_entry = deffunctor("emul_entry",1);
  functor_builtin = deffunctor("builtin",1);
  functor_Dref = deffunctor("$ref",2);
  functor_Dstream = deffunctor("$stream",2);
  functor_Dlock = deffunctor("$lock",2);
  functor_Dhandler = deffunctor("$goal_info",1);
  functor_Dsetarg = deffunctor("internals:$setarg",4);
  functor_large = deffunctor("large",2);
  functor_long = deffunctor("long",1);


  functor_active = deffunctor("active", 4);
  functor_pending = deffunctor("pending", 4);
  functor_failed = deffunctor("failed", 3);
  functor_available = deffunctor("available", 1);

// #if defined(INTERNAL_CALLING)
//   address_internal_call = define_builtin("user:internal_call",ENTER_UNDEFINED,0);
// #endif

  address_interpret_goal = define_builtin("basiccontrol:interpret_goal",ENTER_UNDEFINED,2);
  address_interpret_compiled_goal = define_builtin("basiccontrol:interpret_compiled_goal",ENTER_UNDEFINED,2);
  address_undefined_goal = define_builtin("basiccontrol:undefined_goal",ENTER_UNDEFINED,1);
  address_trace = define_builtin("basiccontrol:debug_goal",ENTER_UNDEFINED,1);
  address_help = define_builtin("internals:control_c_handler",ENTER_UNDEFINED,0);
  address_restart = define_builtin("internals:reboot",ENTER_UNDEFINED,0);
  (void) define_builtin("$geler",BUILTIN_GELER,2);
  (void) define_builtin("internals:$instance",BUILTIN_INSTANCE,3);
  (void) define_builtin("internals:dif",BUILTIN_DIF,2);
  (void) define_builtin("internals:$exit",BUILTIN_ABORT,1);
  address_call = define_builtin("hiord_rt:call",BUILTIN_CALL,1);
  (void) define_builtin("hiord_rt:SYSCALL",BUILTIN_SYSCALL,1);
  (void) define_builtin("hiord_rt:$nodebug_call",BUILTIN_NODEBUGCALL,1);
  address_true = define_builtin("basiccontrol:true",BUILTIN_TRUE,0);
  address_fail = define_builtin("basiccontrol:fail",BUILTIN_FAIL,0);
  address_error = define_builtin("internals:error",ENTER_UNDEFINED,5);

 /* Support for attributed variables */
  address_pending_unifications = 
    define_builtin("internals:pending_unifications",    ENTER_UNDEFINED,1);
  address_uvc = define_builtin("internals:uvc",         ENTER_UNDEFINED,2);
  address_ucc = define_builtin("internals:ucc",         ENTER_UNDEFINED,2);

  (void) define_builtin("internals:$current_instance",BUILTIN_CURRENT_INSTANCE,5);
  (void) define_builtin("internals:$compile_term",BUILTIN_COMPILE_TERM,2);
                              /* initial.c */
  
  define_c_mod_predicate("internals","$atom_mode",2,prolog_atom_mode);
  define_c_mod_predicate("internals","ciao_root",1,prolog_ciao_root);
  define_c_mod_predicate("system_info","ciao_c_headers_dir",1,prolog_ciao_c_headers_dir);

  /* stream_basic.c */
  
  define_c_mod_predicate("internals","$force_interactive",0,prolog_force_interactive);
  define_c_mod_predicate("stream_basic","stream_code",2,prolog_stream_code);
  define_c_mod_predicate("internals","$bootversion",0,prolog_print_emulator_version);
  define_c_mod_predicate("internals","$open",3,prolog_open);
  define_c_mod_predicate("stream_basic","close",1,prolog_close); 
  define_c_mod_predicate("stream_basic","character_count",2,character_count);
  define_c_mod_predicate("stream_basic","line_position",2,line_position);
  define_c_mod_predicate("stream_basic","line_count",2,line_count);
  define_c_mod_predicate("stream_basic","current_input",1,prolog_current_input);
  define_c_mod_predicate("stream_basic","set_input",1,prolog_set_input);
  define_c_mod_predicate("stream_basic","current_output",1,prolog_current_output);
  define_c_mod_predicate("stream_basic","set_output",1,prolog_set_output);
  define_c_mod_predicate("stream_basic","pipe",2,prolog_pipe);

  define_c_mod_predicate("io_alias_redirection", "replace_stream",2, prolog_replace_stream);
  define_c_mod_predicate("io_alias_redirection", "get_stream",2, prolog_get_stream);

                              /* dynamic_rt.c */

  define_c_mod_predicate("internals","$purge",1,prolog_purge);
  define_c_mod_predicate("internals","$erase",1,prolog_erase);
  define_c_mod_predicate("internals","$ptr_ref",2,prolog_ptr_ref);
  define_c_mod_predicate("internals","$inserta",2,inserta);
  define_c_mod_predicate("internals","$insertz",2,insertz);
  define_c_mod_predicate("internals","$make_bytecode_object",4,make_bytecode_object);

                                /* term_typing.c */

  define_c_mod_predicate("term_typing","ground",1,cground);

                                /* terms_check.c */

  define_c_mod_predicate("terms_check","$instance",2,cinstance);

                                /* atomic_basic.c */

  define_c_mod_predicate("atomic_basic","name",2,prolog_name);
  define_c_mod_predicate("atomic_basic","atom_codes",2,prolog_atom_codes);
  define_c_mod_predicate("atomic_basic","number_codes",2,prolog_number_codes_2);
  define_c_mod_predicate("atomic_basic","number_codes",3,prolog_number_codes_3);
  define_c_mod_predicate("atomic_basic","atom_length",2,prolog_atom_length);
  define_c_mod_predicate("atomic_basic","sub_atom",4,prolog_sub_atom);
  define_c_mod_predicate("atomic_basic","atom_concat",3,prolog_atom_concat);

                                /* term_basic.c */

  define_c_mod_predicate("term_basic","copy_term",2,prolog_copy_term);
  define_c_mod_predicate("term_basic","copy_term_nat",2,prolog_copy_term_nat);
  define_c_mod_predicate("term_basic","cyclic_term",1,prolog_cyclic_term);
  define_c_mod_predicate("terms_check","unifiable",3,prolog_unifiable);
  define_c_mod_predicate("iso_misc","unify_with_occurs_check",2,prolog_unifyOC);

                                /* internals.c */
  define_c_mod_predicate("internals","$abolish",1,prolog_abolish); 
  define_c_mod_predicate("internals","$define_predicate",2,define_predicate);
  define_c_mod_predicate("internals","$erase_clause",1,erase_clause);
  define_c_mod_predicate("internals","$clause_number",2,clause_number);
  define_c_mod_predicate("internals","$compiled_clause",4,compiled_clause);
  define_c_mod_predicate("internals","$empty_gcdef_bin",0,empty_gcdef_bin);
  define_c_mod_predicate("internals","$set_property",2,set_property);
  define_c_mod_predicate("internals","$global_vars_get_root", 1, prolog_global_vars_get_root);
  define_c_mod_predicate("internals","$global_vars_set_root", 1, prolog_global_vars_set_root);
#if defined(ATOMGC)
  define_c_mod_predicate("internals","$erase_atom", 1, prolog_erase_atom);
#endif
  define_c_mod_predicate("internals","$prompt",2,prompt);
  define_c_mod_predicate("internals","$frozen",2,frozen);
  define_c_mod_predicate("internals","$defrost",2,defrost);
  define_c_mod_predicate("internals","$setarg",4,setarg);
  define_c_mod_predicate("internals","$undo_goal",1,undo);
  define_c_mod_predicate("internals","$unknown",2,unknown);
  define_c_mod_predicate("internals","$compiling",2,compiling);
  define_c_mod_predicate("internals","$ferror_flag",2,ferror_flag);
  define_c_mod_predicate("internals","$quiet_flag",2,quiet_flag);

  define_c_mod_predicate("internals","$prolog_radix",2,prolog_radix);
  define_c_mod_predicate("internals","$constraint_list",2,constraint_list);
  define_c_mod_predicate("internals","$eq",2,prolog_eq);
  define_c_mod_predicate("internals","$interpreted_clause",2,prolog_interpreted_clause);
  define_c_mod_predicate("internals","$show_nodes",2,prolog_show_nodes);
  define_c_mod_predicate("internals","$show_all_nodes",0,prolog_show_all_nodes);
  define_c_mod_predicate("internals","$start_node",1,start_node);

  /* io_basic.c */
  
  define_c_mod_predicate("io_basic","code_class",2,code_class);
  define_c_mod_predicate("stream_basic","flush_output",0,flush_output);
  define_c_mod_predicate("stream_basic","flush_output",1,flush_output1);
  address_getct = define_c_mod_predicate("io_basic","getct",2,getct);
  address_getct1 = define_c_mod_predicate("io_basic","getct1",2,getct1);
  address_get = define_c_mod_predicate("io_basic","get_code",1,get);
  address_get2 = define_c_mod_predicate("io_basic","get_code",2,get2);
  address_get1 = define_c_mod_predicate("io_basic","get1_code",1,get1);
  address_get12 = define_c_mod_predicate("io_basic","get1_code",2,get12);
  address_peek = define_c_mod_predicate("io_basic","peek_code",1,peek);
  address_peek2 = define_c_mod_predicate("io_basic","peek_code",2,peek2);
  define_c_mod_predicate("io_basic","nl",0,nl);
  define_c_mod_predicate("io_basic","nl",1,nl1);
  define_c_mod_predicate("io_basic","put_code",1,put);
  define_c_mod_predicate("io_basic","put_code",2,put2);
  define_c_mod_predicate("io_basic","tab",1,tab);
  define_c_mod_predicate("io_basic","tab",2,tab2);
  address_skip = define_c_mod_predicate("io_basic","skip_code",1,skip);
  address_skip2 = define_c_mod_predicate("io_basic","skip_code",2,skip2);
  address_skip_line = define_c_mod_predicate("io_basic","skip_line",0,skip_line);
  address_skip_line1 = define_c_mod_predicate("io_basic","skip_line",1,skip_line1);
  address_get_byte1 = define_c_mod_predicate("io_basic","get_byte",1,get_byte1);
  address_get_byte2 = define_c_mod_predicate("io_basic","get_byte",2,get_byte2);
  address_peek_byte1 = define_c_mod_predicate("io_basic","peek_byte",1,peek_byte1);
  address_peek_byte2 = define_c_mod_predicate("io_basic","peek_byte",2,peek_byte2);
  define_c_mod_predicate("io_basic","put_byte",1,put_byte1);
  define_c_mod_predicate("io_basic","put_byte",2,put_byte2);
  define_c_mod_predicate("io_basic","at_end_of_stream",0,at_end_of_stream0);
  define_c_mod_predicate("io_basic","at_end_of_stream",1,at_end_of_stream1);
  define_c_mod_predicate("io_basic","display",1,prolog_display);
  define_c_mod_predicate("io_basic","display",2,prolog_display2);
  define_c_mod_predicate("io_basic","displayq",1,prolog_displayq);
  define_c_mod_predicate("io_basic","displayq",2,prolog_displayq2);
  define_c_mod_predicate("stream_basic","clearerr",1,prolog_clearerr);
  define_c_mod_predicate("fastrw","fast_read",1,prolog_fast_read_in_c);
  define_c_mod_predicate("fastrw","fast_write",1,prolog_fast_write_in_c);
  define_c_mod_predicate("compressed_bytecode","copyLZ",1,raw_copy_stdout); /* TODO: remove on next bootstrap promotion */
  define_c_mod_predicate("io_basic","$raw_copy_stdout",1,raw_copy_stdout);
  define_c_mod_predicate("io_basic","$set_unbuf",1,prolog_set_unbuf);
  define_c_mod_predicate("io_basic","$input_wait",3,prolog_input_wait);
  
  define_c_mod_predicate("internals","$format_print_float",3,prolog_format_print_float);
  define_c_mod_predicate("internals","$format_print_integer",3,prolog_format_print_integer);

                                /* bc_aux.h */
  
  define_c_mod_predicate("basiccontrol","$metachoice",1,metachoice);
  define_c_mod_predicate("basiccontrol","$metacut",1,metacut);
  define_c_mod_predicate("internals","$ddt",1,set_trace_calls);

                                /* qread.c */

  define_c_mod_predicate("internals","$qread",2,prolog_qread);
  define_c_mod_predicate("internals","$push_qlinfo",0,push_qlinfo);
  define_c_mod_predicate("internals","$pop_qlinfo",0,pop_qlinfo);

                                /* debugger_support.c */

  define_c_mod_predicate("debugger_support","$retry_cut",2,retry_cut);
  define_c_mod_predicate("debugger_support","$spypoint",3,spypoint);
  define_c_mod_predicate("debugger_support","$debugger_state",2,debugger_state);
  define_c_mod_predicate("debugger_support","$debugger_mode",0,debugger_mode);

  /* system.c */
  define_c_mod_predicate("system","using_windows",0,prolog_using_windows);
  define_c_mod_predicate("system","working_directory",2,prolog_unix_cd);
  define_c_mod_predicate("system","pause", 1, prolog_pause);
  define_c_mod_predicate("system","$exec_shell",2,prolog_exec_shell);
  define_c_mod_predicate("system","fd_dup",2,prolog_fd_dup);
  define_c_mod_predicate("system","fd_close",1,prolog_fd_close);
  define_c_mod_predicate("internals","$exec",9,prolog_exec);
  define_c_mod_predicate("system","wait",2,prolog_wait);
  define_c_mod_predicate("system","kill",2,prolog_kill);
  define_c_mod_predicate("internals","$unix_argv",1,prolog_unix_argv);
  define_c_mod_predicate("system","mktemp",2,prolog_unix_mktemp);
  define_c_mod_predicate("system","file_exists",2,prolog_unix_access);
  define_c_mod_predicate("system","directory_files",2,prolog_directory_files);
  define_c_mod_predicate("system","file_properties",6,prolog_file_properties);
  define_c_mod_predicate("system","touch",1,prolog_touch);
  define_c_mod_predicate("system","chmod",2,prolog_unix_chmod);
  define_c_mod_predicate("system","umask",2,prolog_unix_umask);
  define_c_mod_predicate("system","delete_file",1,prolog_unix_delete);
  define_c_mod_predicate("system","rename_file",2,prolog_unix_rename);
  define_c_mod_predicate("system","make_directory",2,prolog_unix_mkdir);
  define_c_mod_predicate("system","delete_directory",1,prolog_unix_rmdir);  
  define_c_mod_predicate("system","c_errno",1,prolog_c_errno);
  define_c_mod_predicate("system","c_strerror",1,prolog_c_strerror);
  define_c_mod_predicate("system","c_copy_file",3,prolog_c_copy_file);
  define_c_mod_predicate("system","c_winpath",2,prolog_c_winpath);
  define_c_mod_predicate("system","c_posixpath",2,prolog_c_posixpath);
  define_c_mod_predicate("system","c_winfile",2,prolog_c_winfile);
  define_c_mod_predicate("system","c_posixfile",2,prolog_c_posixfile);
  define_c_mod_predicate("system","current_host",1,prolog_current_host);
  define_c_mod_predicate("system","c_get_env", 2,prolog_c_get_env);
  define_c_mod_predicate("system","c_set_env", 2,prolog_c_set_env);
  define_c_mod_predicate("system","c_del_env", 1,prolog_c_del_env);
  define_c_mod_predicate("system","c_current_env", 3,prolog_c_current_env);
  define_c_mod_predicate("system","extract_paths", 2,prolog_extract_paths);

  define_c_mod_predicate("system","get_pid", 1, prolog_getpid);
  define_c_mod_predicate("system","get_uid", 1, prolog_getuid);
  define_c_mod_predicate("system","get_gid", 1, prolog_getgid);
  define_c_mod_predicate("system","get_pwnam", 1, prolog_getpwnam);
  define_c_mod_predicate("system","get_grnam", 1, prolog_getgrnam);
  define_c_mod_predicate("system","get_numcores", 1, prolog_get_numcores);
  define_c_mod_predicate("system_info","get_arch", 1, prolog_getarch);
  define_c_mod_predicate("system_info","get_os", 1, prolog_getos);
  define_c_mod_predicate("system_info","eng_debug_level", 1, prolog_eng_debug_level);
  define_c_mod_predicate("system_info","eng_is_sharedlib", 0, prolog_eng_is_sharedlib);
  define_c_mod_predicate("system_info","get_ciao_ext", 1, prolog_get_ciao_ext);
  define_c_mod_predicate("system_info","get_exec_ext", 1, prolog_get_exec_ext);
  define_c_mod_predicate("system_info","get_so_ext", 1, prolog_get_so_ext);
  define_c_mod_predicate("foreign_compilation","foreign_opts_cc", 1, prolog_get_foreign_opts_cc);
  define_c_mod_predicate("foreign_compilation","foreign_opts_ld", 1, prolog_get_foreign_opts_ld);
  define_c_mod_predicate("foreign_compilation","foreign_opts_ccshared", 1, prolog_get_foreign_opts_ccshared);
  define_c_mod_predicate("foreign_compilation","foreign_opts_ldshared", 1, prolog_get_foreign_opts_ldshared);
  define_c_mod_predicate("internals","$ciao_version", 7, prolog_version);

  define_c_mod_predicate("internals","$find_file",8,prolog_find_file); 
  define_c_mod_predicate("internals","$path_is_absolute",1,prolog_path_is_absolute); 
  define_c_mod_predicate("internals","$expand_file_name",3,prolog_expand_file_name); 

  define_c_mod_predicate("system","current_executable",1, prolog_current_executable);

  define_c_mod_predicate("system","now",1,prolog_now);
  define_c_mod_predicate("system","datime",9,prolog_datime);  

  /* modload.c */
  define_c_mod_predicate("internals","dynlink", 2, prolog_dynlink);
  define_c_mod_predicate("internals","dynunlink", 1, prolog_dynunlink); 

  /* timing.c */
  define_c_mod_predicate("internals","$runtime",1,prolog_runtime);
  define_c_mod_predicate("internals","$usertime",1,prolog_usertime);
  define_c_mod_predicate("internals","$systemtime",1,prolog_systemtime);  
  define_c_mod_predicate("internals","$walltime",1,prolog_walltime);

  /* clock/cpu ticks */  
  define_c_mod_predicate("internals","$runtick",1,prolog_runtick);
  define_c_mod_predicate("internals","$usertick",1,prolog_usertick);
  define_c_mod_predicate("internals","$systemtick",1,prolog_systemtick);
  define_c_mod_predicate("internals","$walltick",1,prolog_walltick);

  /* clock frequency */  
  define_c_mod_predicate("internals","$runclockfreq",1,prolog_runclockfreq);
  define_c_mod_predicate("internals","$userclockfreq",1,prolog_userclockfreq);
  define_c_mod_predicate("internals","$systemclockfreq",1,prolog_systemclockfreq);
  define_c_mod_predicate("internals","$wallclockfreq",1,prolog_wallclockfreq);

  /* eng_alloc.c */
  define_c_mod_predicate("runtime_control","statistics",0,statistics);
  define_c_mod_predicate("internals","$program_usage",1,program_usage);
  define_c_mod_predicate("internals","$internal_symbol_usage",1,internal_symbol_usage);
  define_c_mod_predicate("internals","$total_usage",1,total_usage);

  /* eng_gc.c */
  define_c_mod_predicate("internals","$termheap_usage",1,termheap_usage);
  define_c_mod_predicate("internals","$envstack_usage",1,envstack_usage);
  define_c_mod_predicate("internals","$trail_usage",1,trail_usage);
  define_c_mod_predicate("internals","$choice_usage",1,choice_usage);
  define_c_mod_predicate("internals","$stack_shift_usage",1,stack_shift_usage);
  define_c_mod_predicate("internals","$gc_mode",2,gc_mode);
  define_c_mod_predicate("internals","$gc_trace",2,gc_trace);
  define_c_mod_predicate("internals","$gc_margin",2,gc_margin);
  define_c_mod_predicate("internals","$gc_usage",1,gc_usage);
  define_c_mod_predicate("runtime_control","garbage_collect",0,gc_start);

  /* rt_exp.c */
  /* runtime_control.c */
  define_c_mod_predicate("basiccontrol","repeat",0,prolog_repeat);
  define_c_mod_predicate("runtime_control","current_atom",1,current_atom);
  define_c_mod_predicate("stream_basic","current_stream",3,current_stream);
  define_c_mod_predicate("internals","$current_predicate",2,current_predicate);
  define_c_mod_predicate("internals","$predicate_property",3,predicate_property);
  define_c_mod_predicate("internals","$current_clauses",2,current_clauses);
  define_c_mod_predicate("internals","$module_is_static",1,module_is_static);
  define_c_mod_predicate("internals","$first_instance",2,first_instance);
  define_c_mod_predicate("internals","$close_predicate",1,close_predicate);
  define_c_mod_predicate("internals","$open_predicate",1,open_predicate);
  define_c_mod_predicate("runtime_control", "new_atom", 1, prolog_new_atom);
  // (experimental)
  define_c_mod_predicate("internals", "$yield", 0, prolog_yield);

#if defined(GAUGE)
  /* gauge.c */
  define_c_mod_predicate("internals","$emulated_clause_counters",4,emulated_clause_counters);
  define_c_mod_predicate("internals","$counter_values",3,counter_values);
  define_c_mod_predicate("internals","$reset_counters",2,reset_counters);
#endif

#if defined(USE_FAST_MULTIATTR)
  /* attributes.c */
  define_c_mod_predicate("attr_rt","get_attr",3,get_attr__3);
  define_c_mod_predicate("attr_rt","put_attr",3,put_attr__3);
  define_c_mod_predicate("attr_rt","del_attr",2,del_attr__2);
#endif

  /* concurrency.c */
  define_c_mod_predicate("concurrency","$eng_call",6,prolog_eng_call);
  define_c_mod_predicate("concurrency","$eng_backtrack",2,prolog_eng_backtrack);
  define_c_mod_predicate("concurrency","$eng_cut",1,prolog_eng_cut);
  define_c_mod_predicate("concurrency","$eng_release",1,prolog_eng_release);
  define_c_mod_predicate("concurrency","$eng_wait",1,prolog_eng_wait);
  define_c_mod_predicate("concurrency","$eng_kill",1,prolog_eng_kill);
  define_c_mod_predicate("concurrency","$eng_killothers",0,prolog_eng_killothers);
  define_c_mod_predicate("concurrency","$eng_status",0,prolog_eng_status);
  define_c_mod_predicate("concurrency","$eng_self",2,prolog_eng_self);
  define_c_mod_predicate("concurrency","lock_atom",1,prolog_lock_atom);
  define_c_mod_predicate("concurrency","unlock_atom",1,prolog_unlock_atom);
  define_c_mod_predicate("concurrency","atom_lock_state",2,prolog_lock_atom_state);
  define_c_mod_predicate("internals","$unlock_predicate",1,prolog_unlock_predicate);

  address_nd_repeat = def_retry_c(nd_repeat,0);
  address_nd_current_atom = def_retry_c(nd_current_atom,2);
  address_nd_current_stream = def_retry_c(nd_current_stream,4);
  address_nd_current_predicate = def_retry_c(nd_current_predicate,4);
  address_nd_predicate_property = def_retry_c(nd_predicate_property,5);
  address_nd_atom_concat = def_retry_c(nd_atom_concat,4);
#if defined(TABLING)
  address_nd_fake_choicept = def_retry_c(nd_fake_choicept,0);
#endif
#if defined(PARBACK)
  address_nd_suspension_point = def_retry_c(nd_suspension_point,1);
  {
    bcp_t P;
    restart_point_insn = (bcp_t)checkalloc_ARRAY(char, FTYPE_size(f_o));
    P = restart_point_insn;
    EMIT_o(RESTART_POINT);
  }
#endif
  address_nd_yield = def_retry_c(nd_yield,0);

#include "eng_static_mod.c"

#if defined(MARKERS)
  init_markercode();
#endif
}

void compute_cwd(void);

void glb_init_each_time(void)
{
  address_interpret_c_goal = address_interpret_goal;

  current_radix = MakeSmall(10);
  /*current_breaklevel = TaggedZero;*/
  current_prompt = GET_ATOM("|: ");
  enable_conditions();
  compute_cwd();
}

CVOID__PROTO(local_init_each_time) {
  /* Debugger state globals moved to per-thread variables because of
     reallocations of the atoms they point to when expanding the heap
     --- this caused the program to break, since a C variable could be
     pointing to a thread's heap and be reallocated by another thread
     with wrong displacements. */

  /* Initialize debugger variables */

  Current_Debugger_State = atom_nil;
  Current_Debugger_Mode = atom_off;

  /* Worker expansion caused by compiling big clauses */

  Expanded_Worker = NULL;

  /* Initialize garbage collection ciao_stats */

  Gc_Total_Grey = 0;

  /* Initialize some top registers */
  w->heap_top = Heap_Start;
  w->trail_top = Trail_Start;

#if defined(USE_GLOBAL_VARS)
  /* Initialize heap region for global variables (make sure this is
     created after heap pointers are set and before the initial
     choicepoint) */
#define MAX_GLOBALS 32 /* currently it cannot be more than 255 */
  {
    tagged_t *ptr, *ptr2;
    int i;
    tagged_t functor_Dglb = deffunctor("$glb",MAX_GLOBALS);
    
    ptr2 = ptr = w->heap_top;
    HeapPush(ptr,functor_Dglb);
    for (i = 0; i < MAX_GLOBALS; i++) {
      HeapPush(ptr, MakeSmall(0));
    }
    w->heap_top = ptr;
    GLOBAL_VARS_ROOT = Tagp(STR,ptr2);
  }
#endif

  /* Setup initial frame */
  w->frame = (frame_t *)Stack_Start;
  w->local_top = (frame_t *)Offset(w->frame,EToY0);
  w->frame->next_insn = NULL;   
  w->frame->frame = NULL;

  /* Setup initial choicepoint */
  choice_t *b = InitialChoice;
  w->choice = b;                            
  b->frame = w->frame;

  b->next_alt = termcode;

  b->local_top = w->local_top;
  b->heap_top = w->heap_top;
  CHPTFLG(b->flags = 0);
  b->trail_top = w->trail_top;
  b->next_insn = exitcode;
  b->x[0] = atom_nil;

  ChoiceptMarkPure(b);
  ChoiceptMarkStatic(b);
  ChoiceptMarkNoCVA(b);
                                
  NewShadowregs(w->heap_top);
  SetDeep();

  VALUETRAIL__INIT;
  TopConcChpt = b; /* Initialize concurrent topmost choicepoint */

  w->next_insn = bootcode;
  w->misc->exit_code = 0;
  Stop_This_Goal(Arg) = FALSE;
  UnsetEvent();

  w->liveinfo = NULL;

#if defined(DEBUG)
  if (debug_threads)
    fprintf(stderr, "*** %" PRIdm " (%" PRIdm ") Initializing WAM %p: node = %p, trail = %p, frame = %p\n",
            (intmach_t)Thread_Id, (intmach_t)GET_INC_COUNTER, Arg,
            b, w->trail_top, w->frame);
#endif
  init_streams_each_time(Arg);       /* set misc. variables, handle signals */
  control_c_normal(Arg);                               /* For threads also? */
}


/* Init. at boot and after abort. */

CVOID__PROTO(init_each_time)
{
  glb_init_each_time();
  local_init_each_time(Arg);
}

CVOID__PROTO(init_streams_each_time)
{
  Input_Stream_Ptr = stream_user_input;
  Output_Stream_Ptr = stream_user_output;
  Error_Stream_Ptr = stream_user_error;
}

/* --------------------------------------------------------------------------- */
/*  Allocation of principal WAM areas. */

#if defined(Solaris)||defined(LINUX)||defined(EMSCRIPTEN)||defined(DARWIN)||defined(BSD)
#include <string.h>
#else
#include <memory.h>
#endif
#include <unistd.h>

 /* # bytes used by the Prolog program & database code.  Probably not
    accurately measured (patched here and there) (MCL).  */
intmach_t mem_prog_count = 0;                                     /* Shared */

 /* Number of predicates asserted */
intmach_t num_of_predicates = 0;                                  /* Shared */

 /* Creates the wam structure, allocates its areas and initializes them.
    This returns an empty, fresh wam.  We do not add it here to the task
    state list; it needs its own thread, which we have after startwam() */
worker_t *create_and_init_wam(void) {
  worker_t *w;
  /*intmach_t saved_program_count = mem_prog_count;  */

  Arg = create_wam_storage();                         /* Just create *Arg */
  create_wam_areas(Arg);                      /* Make room for the stacks */
  local_init_each_time(Arg);                               /* Local areas */
  return Arg;
}

/* Available workers are queued here */

worker_t *wam_list = NULL;
SLOCK    wam_list_l;

worker_t *free_wam(void) {
  worker_t *free_wam;

  Wait_Acquire_slock(wam_list_l);
  if (wam_list) {
    free_wam = wam_list;
    wam_list = Next_Worker(free_wam);
    Release_slock(wam_list_l);
    Next_Worker(free_wam) = NULL;
  } else {
    Release_slock(wam_list_l);
    free_wam = create_and_init_wam();
  }
  return free_wam;
}

CVOID__PROTO(release_wam)
{
  local_init_each_time(Arg);
  Wait_Acquire_slock(wam_list_l);
  Next_Worker(Arg) = wam_list;
  wam_list = Arg;
  Release_slock(wam_list_l);
}


#if defined(ANDPARALLEL)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;
bool_t unwinding_done = FALSE;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif

#if defined(PARBACK)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif

/* TODO: a global variable here is wrong if a clause is asserted in one worker and consulted in other */
int reg_bank_size = XREGBANKSIZE; /* Shared? Strange use in compile_term_aux */

worker_t *create_wam_storage(void) {
  worker_t *w;

  w = checkalloc_FLEXIBLE(worker_t, tagged_t, reg_bank_size);
  w->misc = checkalloc_TYPE(misc_info_t);
  w->streams = checkalloc_TYPE(io_streams_t);
  w->debugger_info = checkalloc_TYPE(debugger_state_t);

  return w;
}

CVOID__PROTO(create_wam_areas)
{
  int i, j;
  char *cp;

  Atom_Buffer_Length = STATICMAXATOM;
  Atom_Buffer = checkalloc_ARRAY(char, Atom_Buffer_Length);

#if defined(ANDPARALLEL)
  /* Initializing pointers and locks */
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Goal_Cache = NULL;
  Goal_Cache = NULL;
  Dep_Id = NULL;
  Dep_Size = 0;
  Event_Queue_Start = NULL;
  Event_Queue_Top = NULL;
  Last_Parallel_Exec = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Cancel_Goal_Exec = FALSE;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;
  Mode = FORWARD_EXEC;

  Init_slock(Goal_List_Lock);
  Init_slock(Event_Queue_Lock);
  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);

#if defined(VISANDOR)
  Pcall_Level(w) = 0;
  FirstEvent(w)  = checkalloc_ARRAY(visandor_event_t, maxevents);
  NextEvent(w)   = FirstEvent(w);
  LastEvent(w)   = &((FirstEvent(w))[maxevents]);
#endif

#endif

#if defined(PARBACK)
  /* Initializing pointers and locks */
  Act_PF = NULL;
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Cancel_Goal_List_Start = NULL;
  Cancel_Goal_List_Top = NULL;
  Back_Goal_List_Start = NULL;
  Back_Goal_List_Top = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Goal_To_Cancel = NULL;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;

  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);
#endif

  /* heap pointer is first free cell, grows ++ */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  Heap_Start = checkalloc_ARRAY(tagged_t, i);
  Heap_End =  HeapOffset(Heap_Start,i);
  UnsetEvent();

  /* stack pointer is first free cell, grows ++ */
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  Stack_Start  = checkalloc_ARRAY(tagged_t, i);
  Stack_End =  StackOffset(Stack_Start,i);

  /* trail pointer is first free cell, grows ++ */
  /* choice pointer is last busy cell, grows -- */
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  Choice_End = Trail_Start = checkalloc_ARRAY(tagged_t, i);
  Choice_Start = Trail_End = TrailOffset(Trail_Start, i);
#if defined(USE_TAGGED_CHOICE_START)
  /*  Do not touch the (tagged_t) type casting! Or the emulator will break! */
  Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
}

/* Cleanup after abort: shrink stacks to initial sizes. */
CVOID__PROTO(reinitialize_wam_areas)
{
  int i, j;
  char *cp;

  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  if ((j=HeapDifference(Heap_Start,Heap_End)) != i) {
    Heap_Start = checkrealloc_ARRAY(tagged_t, j, i, Heap_Start);
    Heap_End = HeapOffset(Heap_Start,i);
  }
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  if ((j=StackDifference(Stack_Start,Stack_End)) != i) {
    Stack_Start = checkrealloc_ARRAY(tagged_t, j, i, Stack_Start);
    Stack_End = StackOffset(Stack_Start,i);
  }
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  if ((j=TrailDifference(Trail_Start,Trail_End)) != i) {
    Choice_End = Trail_Start = checkrealloc_ARRAY(tagged_t, j, i, Trail_Start);
    Choice_Start = Trail_End = TrailOffset(Trail_Start,i);
#if defined(USE_TAGGED_CHOICE_START)
    /*  Do not touch the (tagged_t) type casting! Or the emulator will break! */
    Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
  }

  /* Create an expandable char array for loading po files */ 

  if (Atom_Buffer_Length != STATICMAXATOM) {
    Atom_Buffer = checkrealloc_ARRAY(char, 
                                     Atom_Buffer_Length,
                                     STATICMAXATOM,
                                     Atom_Buffer);
    Atom_Buffer_Length = STATICMAXATOM;
  }

  UnsetEvent();
}

/*static char *mem_start; */  /* beginning of our virtual memory -- Shared */

/*  mem_start wrongly calculated, and mem_prog_count set to zero only once */
/*
void mem_prog_reset(void) {
    mem_start = (char *)(&end);
#if SMALLPTR_BASE
  if (mem_start < (char *)SMALLPTR_BASE)
    mem_start = (char *)SMALLPTR_BASE;
#endif

  mem_prog_count = 0;
}
*/

/* program_usage: [sizeof_used_space, 0] */
CBOOL__PROTO(program_usage)
{
  tagged_t x;

  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,IntvalToTagged(mem_prog_count),x);
  CBOOL__LASTUNIFY(X(0),x);
}

/* internal_symbol_usage: [number_atoms_funcs_preds, number_pred_defs] */
CBOOL__PROTO(internal_symbol_usage)
{
  tagged_t x;

  MakeLST(x,IntvalToTagged(num_of_predicates),atom_nil);
  MakeLST(x,IntvalToTagged(ciao_atoms->count),x);
  CBOOL__LASTUNIFY(X(0),x);
}


/* total_usage: [total_space, 0].  Changed to use total_mem_count (MCL) */
CBOOL__PROTO(total_usage)
{
  tagged_t x;
  intmach_t n;

  n = total_mem_count;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,IntvalToTagged(n),x);
  CBOOL__LASTUNIFY(X(0),x);
}


CBOOL__PROTO(statistics)
{
  stream_node_t *s = Output_Stream_Ptr;
  intmach_t used, free;
  frame_t *newa;

#if !defined(EMSCRIPTEN)
  inttime_t runtick0;
  inttime_t usertick0 = usertick();
  inttime_t systemtick0 = systemtick();
  runtick0=usertick0;
#endif
  inttime_t walltick0 = walltick();

  StreamPrintf(s,
             "memory used (total)    %10" PRIdm " bytes\n",
             total_mem_count);
  StreamPrintf(s, 
             "   program space (including reserved for atoms): %" PRIdm " bytes\n", 
             mem_prog_count);

  StreamPrintf(s,
             "   number of atoms and functor/predicate names: %" PRIdm "\n", 
             ciao_atoms->count);
  StreamPrintf(s,
             "   number of predicate definitions: %" PRIdm "\n", 
             num_of_predicates);

  used = HeapCharDifference(Heap_Start,w->heap_top);
  free = HeapCharDifference(w->heap_top,Heap_End);
  StreamPrintf(s, 
             "   global stack   %10" PRIdm " bytes:%" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  GetFrameTop(newa,w->choice,G->frame);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  StreamPrintf(s,
             "   local stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->choice)/2;
  StreamPrintf(s,
             "   trail stack    %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n",
             used+free, used, free);

  used = ChoiceCharDifference(Choice_Start,w->choice);
  free = ChoiceCharDifference(w->choice,w->trail_top)/2;
  StreamPrintf(s,
             "   control stack  %10" PRIdm " bytes:%10" PRIdm " in use,%10" PRIdm " free\n\n",
             used+free, used, free);

  StreamPrintf(s,
             " %10.6f sec. for %" PRIdm " global, %" PRIdm " local, and %" PRIdm " control space overflows\n",
             ((flt64_t)ciao_stats.ss_tick)/RunClockFreq(ciao_stats),
             ciao_stats.ss_global,
             ciao_stats.ss_local, ciao_stats.ss_control);
  StreamPrintf(s,
             " %10.6f sec. for %" PRIdm " garbage collections which collected %" PRIdm " bytes\n\n",
             ((flt64_t)ciao_stats.gc_tick)/RunClockFreq(ciao_stats),
             ciao_stats.gc_count,
             (intmach_t)ciao_stats.gc_acc);

#if !defined(EMSCRIPTEN) /* not supported by emscripten */
  StreamPrintf(s,
             " runtime:    %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(runtick0-ciao_stats.starttick)/RunClockFreq(ciao_stats),
             runtick0-ciao_stats.starttick,
             RunClockFreq(ciao_stats));
  StreamPrintf(s,
             " usertime:   %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(usertick0-ciao_stats.startusertick)/ciao_stats.userclockfreq,
             usertick0-ciao_stats.startusertick,
             ciao_stats.userclockfreq);
  StreamPrintf(s,
             " systemtime: %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n",
             (flt64_t)(systemtick0-ciao_stats.startsystemtick)/ciao_stats.systemclockfreq,
             systemtick0-ciao_stats.startsystemtick,
             ciao_stats.systemclockfreq);
#endif
  StreamPrintf(s,
             " walltime:   %10.6f sec. %12" PRId64 " ticks at %12" PRId64 " Hz\n\n",
             (flt64_t)(walltick0-ciao_stats.startwalltick)/ciao_stats.wallclockfreq,
             walltick0-ciao_stats.startwalltick,
             ciao_stats.wallclockfreq);

  return TRUE;
}
