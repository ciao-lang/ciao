/*
 *  profiler.h
 */

#ifndef _CIAO_PROFILER_H
#define _CIAO_PROFILER_H

#include <ciao/timing.h>
#include "hashtab.h"

/* Uncomment this if you like to turn off the time profiling */
#define PROFILE__PROFTIME

/* Decide which kind of functions you like to use to measure the time */
#define PROFILE__USE_HRTIME
/* #define PROFILE__USE_USERTIME */
/* #define PROFILE__USE_WALLTIME */

#define MAXSTRNUMBERSIZE 64

#define ENTER_CALL 0
#define ENTER_REDO 1

#define LEAVE_EXIT 0
#define LEAVE_FAIL 1

#if defined(PROFILE__USE_HRTIME)
#include "hrtime.h"
# define PROFILE__GET_TICK() (hrtime())
# define PROFILE__GET_FREQ  (hrfreq())
#else /* USERTIME or WALLTIME */
extern inttime_t (**proftick)(void);
extern inttime_t *profclockfreq;
# define PROFILE__GET_TICK() ((*proftick)())
# define PROFILE__GET_FREQ (*profclockfreq)
#endif

typedef struct edge_cc_ edge_cc_t;

typedef struct prof_frame_ prof_frame_t;

CBOOL__PROTO(prolog_profile_dump);

CVOID__PROTO(profile__hook_fail_);
CVOID__PROTO(profile__hook_redo_);
CVOID__PROTO(profile__hook_call_, definition_t *);
CVOID__PROTO(profile__hook_cut_);
CVOID__PROTO(profile__hook_proceed_);
CVOID__PROTO(profile__hook_neck_proceed_);

void profile_dump(prof_frame_t *frame, FILE *);
char *functor_name(char *buffer, definition_t *functor);
bool_t functor_have_overhead(prof_frame_t *frame, definition_t *functor);
const char * predicate_type(int t);
void show_func_point(FILE * streamfile, char *label, int call_point,
		     unsigned int choice, definition_t *functor);
void show_func_point2(FILE * streamfile, char *label, int call_point,
		      unsigned int choice, definition_t *functor0, definition_t *functor1);
void profile_init(void);
void profile_reset(prof_frame_t *frame);

#define TOTAL(P,C) ((P)->C[0][0]+(P)->C[0][1]+(P)->C[1][0]+(P)->C[1][1])

struct edge_cc_ {
  definition_t *functor[2]; /* functor[0]: current <- functor[1]: previous */
  unsigned int entry_port;
  inttime_t entry_time;
  unsigned long int counts[2][2]; /* [INPUT][OUTPUT] */
  inttime_t times[2][2]; /* [INPUT][OUTPUT] */
#if defined(PROFILE)
  ht_tab * cc_item_table;
  bool_t hooks;
  unsigned long int entry_cuts;
  unsigned long int entry_scuts;
  unsigned long int cuts[2][2];   /* EMM -- Cuts that removes choicepoints */
  unsigned long int scuts[2][2];  /* EMM -- Cuts that don't have effects */
#endif
};

typedef struct profile_currents_ profile_currents_t;

struct profile_currents_ {
  inttime_t entry_time;
  unsigned long int counts[2][2]; /* [INPUT][OUTPUT] */
  inttime_t times[2][2];           /* [INPUT][OUTPUT] */
  unsigned long int skips;  /* EMM -- Skips over a node in case it is cutted */
};

#define PROFILE__RESET_PORTS(var)		\
  {						\
    var[0][0]=var[0][1]=var[1][0]=var[1][1]=0;	\
  }

#define PROFILE__RESET_CC_COMMON(cc)			\
  {							\
    (cc).entry_time=0;					\
    PROFILE__RESET_PORTS((cc).counts);			\
    PROFILE__RESET_PORTS((cc).times);			\
  }

#if defined(PROFILE)

typedef struct cc_item_ cc_item_t;
cc_item_t *get_cc_item(struct ht_tab *ht, definition_t *functor);

struct cc_item_ {
  definition_t *functor;
  profile_currents_t prof;
};

extern unsigned int last_entry_port;

#define PROFILE__RESET_PROF(prof)		\
  {						\
    (prof).skips=0;				\
    PROFILE__RESET_PORTS((prof).counts);	\
    PROFILE__RESET_PORTS((prof).times);		\
  }

extern bool_t hooks_enabled;

void disable_hooks(void);
void enable_hooks(void);

int profile_hooks;

#define DISABLE_HOOKS {if(hooks_enabled){disable_hooks();last_cci=NULL;}}
#define ENABLE_HOOKS(f) {if(!hooks_enabled){enable_hooks();last_cci=get_cc_item(active_ecc->cc_item_table, f);}}

#define PROFILE__RESET_CC(cc)		\
  {					\
    PROFILE__RESET_CC_COMMON(cc);	\
    PROFILE__RESET_PORTS((cc).cuts);	\
    PROFILE__RESET_PORTS((cc).scuts);	\
    (cc).entry_cuts=			\
      (cc).entry_scuts=0;		\
  }

#else

#define PROFILE__RESET_CC(cc) PROFILE__RESET_CC_COMMON(cc)

#endif

struct prof_frame_ {
  ht_tab *node_table_cc;
  ht_tab *edge_table_cc;
  edge_cc_t *active_ecc;
#if defined(PROFILE)
  cc_item_t *last_cci;
#endif
};

#define MAX_PROF_FRAMES 32

extern prof_frame_t prof_frames[MAX_PROF_FRAMES];
extern prof_frame_t *active_frame;

#define last_cci        (active_frame->last_cci)
#define active_ecc      (active_frame->active_ecc)

edge_cc_t *get_edge_cc(struct ht_tab *cct, definition_t *functor[2]);
edge_cc_t *add_edge_cc(struct ht_tab *cct, definition_t *functor[2]);

extern definition_t *cc_call;
extern definition_t *cc_call_nf;
extern definition_t *cc_call_ncnf;
extern definition_t *cc_redo;
extern definition_t *cc_redo_1;
extern definition_t *cc_redo_1_nf;
extern definition_t *cc_redo_2;
extern definition_t *cc_exit;
extern definition_t *cc_exit_nc;
extern definition_t *cc_exit_ncnf;
extern definition_t *cc_fail;
extern definition_t *cc_fail_nc;
extern definition_t *cc_fail_ncnf;
extern definition_t *cc_fail_1;
extern definition_t *cc_fail_2;
extern definition_t *profile_leave_fail;
extern definition_t *profile_leave_exit;
extern definition_t *profile_enter_call;
extern definition_t *profile_enter_redo_1;
extern definition_t *cost_center;
extern definition_t *cost_center_4;
extern definition_t *cost_center_nc;
extern definition_t *cost_center_ncnf;

extern inttime_t tick_last_addition;
extern inttime_t tick_ini_profiling;
extern inttime_t tick_start;
#if defined(PROFILE__PROFTIME)
extern inttime_t tick_profiling;
extern inttime_t tick_end_profiling;
#endif

#define PROFILE__FUNCTOR_PROF(functor)     (&functor->prof)
#define PROFILE__INCR(functor, member)     {functor->prof.member++;}
#define PROFILE__INCRN(functor, member, n) {functor->prof.member+=n;}

#if defined(PROFILE__TRACER)

int profile_trace;

# define ShowFuncPoint(filestream, label, call_point, choice, functor)	\
  {if(profile_trace) show_func_point((filestream), (label), (call_point), (choice), (functor));}
# define ShowFuncPoint2(filestream, label, call_point, choice, functor0, functor1) \
  {if(profile_trace) show_func_point2((filestream), (label), (call_point), (choice), (functor0), (functor1));}
# define ShowSkipNodes(filestream, skips)				\
  {if(profile_trace) fprintf(filestream, "cut skipped %d nodes \n", skips);}
# define ShowClauseNumber(filestream)					\
  {if(profile_trace && w->next_alt) fprintf(filestream, "\tclause %d \n", w->next_alt->number);}
# define ShowNoMoreAlts(filestream)				\
  {if(profile_trace) fprintf(filestream, "No more alts\n");}

#else

# define ShowFuncPoint(filestream, label, call_point, choice, functor)
# define ShowFuncPoint2(filestream, label, call_point, choice, functor0, functor1)
# define ShowSkipNodes(filestream, skips)
# define ShowClauseNumber(filestream)
# define ShowNoMoreAlts(filestream)

#endif

#define GET_DEFINITION(NAME, ARITY)					\
  insert_definition(predicates_location,init_atom_check((NAME)),(ARITY),TRUE)
/*
  example:
  
  definition_t *address_mypred;
  address_mypred = GET_DEFINITION("profilermodule:mypred", 3);
  
*/

# define PROFILE__TIME_FLUSH \
{ \
  PROFILE__TIME_CARRY \
  tick_ini_profiling = tick_end_profiling; \
}

#define PROFILE__TIME_INI \
{ \
  inttime_t proftick0 = PROFILE__GET_TICK(); \
  PROFILE__TIME_CARRY \
  tick_ini_profiling = proftick0; \
}

#if defined(PROFILE__PROFTIME)

# define PROFILE__TIME_END \
{ \
  tick_end_profiling = PROFILE__GET_TICK(); \
}

# define PROFILE__TIME_CARRY \
{ \
  inttime_t profdiff0 = tick_end_profiling - tick_ini_profiling; \
  tick_profiling += profdiff0; \
  tick_last_addition += profdiff0; \
}

#else

# define PROFILE__TIME_END
# define PROFILE__TIME_CARRY
# define PROFILE__TIME_FLUSH \
{ \
  PROFILE__TIME_CARRY \
  tick_ini_profiling = 0; \
}

#endif

#endif /* _CIAO_PROFILER_H */
