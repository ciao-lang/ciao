#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ciao/datadefs.h>
#include <ciao/global_defs.h>
#include <ciao/profile_hooks.h>
#include <ciao/alloc.h>
#include <ciao/wam_alloc.h>
#include <ciao/support_macros.h>
#include <ciao/support.h>
#include <ciao/eng_dbg.h>
#include <ciao/timing.h>
#include <ciao/float_tostr.h>
#include "profiler.h"

static int compare_counts(const void *arg1, const void *arg2);
static int compare_ticks(const void *arg1, const void *arg2);
static int compare_cci_counts(const void *arg1, const void *arg2);
static int compare_cci_ticks(const void *arg1, const void *arg2);
static int compare_ccw_calls(const void *arg1, const void *arg2);
static int compare_ccw_ticks(const void *arg1, const void *arg2);

#if defined(PROFILE__TRACER)
bool_t profile_trace=FALSE;
#endif

#if defined(PROFILE)
bool_t profile_hooks=TRUE;
#endif

prof_frame_t prof_frames[MAX_PROF_FRAMES];
prof_frame_t *active_frame=NULL;

definition_t *cc_call=NULL;
definition_t *cc_call_nf=NULL;
definition_t *cc_call_ncnf=NULL;
definition_t *cc_redo=NULL;
definition_t *cc_redo_1=NULL;
definition_t *cc_redo_1_nf=NULL;
definition_t *cc_redo_2=NULL;
definition_t *cc_exit=NULL;
definition_t *cc_exit_nc=NULL;
definition_t *cc_exit_ncnf=NULL;
definition_t *cc_fail=NULL;
definition_t *cc_fail_nc=NULL;
definition_t *cc_fail_1=NULL;
definition_t *cc_fail_2=NULL;
definition_t *profile_leave_fail=NULL;
definition_t *profile_leave_exit=NULL;
definition_t *profile_enter_call=NULL;
definition_t *profile_enter_redo_1=NULL;
definition_t *cost_center=NULL;
definition_t *cost_center_4=NULL;
definition_t *cost_center_nc=NULL;
definition_t *cost_center_ncnf=NULL;
unsigned int last_entry_port=ENTER_CALL;

inttime_t tick_last_addition=0;
inttime_t tick_ini_profiling=0;
inttime_t tick_start=0;
# if defined(PROFILE__PROFTIME)
inttime_t tick_profiling=0; /* time (in ticks) doing profiling (overhead) */
inttime_t tick_end_profiling=0;
# endif

# if defined(PROFILE__USE_HRTIME)
/* nothing to do */
#elif defined(PROFILE__USE_USERTIME)
inttime_t (**proftick)(void)=&usertick;
inttime_t *profclockfreq=&ciao_statistics.userclockfreq;
#else
inttime_t (**proftick)(void)=&walltick;
inttime_t *profclockfreq=&ciao_statistics.wallclockfreq;
# endif

inttime_t profile_freq=0;

/* run time hooks */

typedef struct edge_cc_wrap_ edge_cc_wrap_t;

struct edge_cc_wrap_ {
  edge_cc_t *ecc;
#if defined(PROFILE)
  long int realsize;
  long int realsize_oh;
  profile_currents_t total;
  profile_currents_t overhead;
#endif
};

static bool_t functor_is_instrumenter(definition_t *functor)
{
  return
    functor==NULL                 || /* residual cc or first call */
    functor==cc_call              ||
    functor==cc_call_nf           ||
    functor==cc_call_ncnf         ||
    functor==cc_redo              ||
    functor==cc_redo_1            ||
    functor==cc_redo_1_nf         ||
    functor==cc_redo_2            ||
    functor==cc_exit              ||
    functor==cc_exit_nc           ||
    functor==cc_exit_ncnf         ||
    functor==cc_fail              ||
    functor==cc_fail_nc           ||
    functor==cc_fail_1            ||
    functor==cc_fail_2            ||
    functor==profile_leave_fail   ||
    functor==profile_leave_exit   ||
    functor==profile_enter_call   ||
    functor==profile_enter_redo_1 ||
    functor==cost_center          ||
    functor==cost_center_4        ||
    functor==cost_center_nc       ||
    functor==cost_center_ncnf;
}

static bool_t functor_is_cost_center(prof_frame_t *frame, definition_t *functor)
{
  static definition_t *d[1];
  *d=functor;
  return ht_exists(frame->node_table_cc, (ub1 *)(d), sizeof(*d));
}

bool_t functor_have_overhead(prof_frame_t *frame, definition_t *functor)
{
  return functor_is_instrumenter(functor)||functor_is_cost_center(frame, functor);
}

#if defined(PROFILE)

bool_t hooks_enabled=FALSE;

void disable_hooks(void) {
  if (hooks_enabled) {
    profile__hook_redo=profile__hook_noop;
    profile__hook_cut=profile__hook_noop;
    profile__hook_call=profile__hook_call_noop;
# if defined(PROFILE__TRACER)
    profile__hook_fail=profile__hook_noop;
    profile__hook_proceed=profile__hook_noop;
    profile__hook_neck_proceed=profile__hook_noop;
# endif
    hooks_enabled=FALSE;
  }
}

void enable_hooks(void) {
  if (!hooks_enabled) {
    profile__hook_redo=profile__hook_redo_;
    profile__hook_cut=profile__hook_cut_;
    profile__hook_call=profile__hook_call_;
# if defined(PROFILE__TRACER)
    profile__hook_fail=profile__hook_fail_;
    profile__hook_proceed=profile__hook_proceed_;
    profile__hook_neck_proceed=profile__hook_neck_proceed_;
# endif
    hooks_enabled=TRUE;
  }
}

typedef struct definition_wrap_ definition_wrap_t;

struct definition_wrap_ {
  definition_t *functor;
  profile_currents_t *prof;
};

CVOID__PROTO(profile__hook_redo_)
{
  FILE * streamfile;
  inttime_t profdiff;
  definition_t *f;
  PROFILE__TIME_INI;
  streamfile=Output_Stream_Ptr->streamfile;
  if (w->node!=InitialNode) {
    f=w->next_node->functor;
  } else {
    f=NULL;
    /*       f=profile_leave_exit; */
  }
  ShowFuncPoint(streamfile,  "+retr", 0, 0, f);
  ShowFuncPoint2(streamfile, " retr", 0, 0, active_ecc->functor[0],active_ecc->functor[1]);
  last_cci->prof.counts[last_entry_port][LEAVE_FAIL]++;
  profdiff=tick_ini_profiling - tick_last_addition;
  last_cci->prof.times[last_entry_port][LEAVE_FAIL]+=profdiff;
  if (active_ecc && functor_have_overhead(active_frame, last_cci->functor)) {
    active_ecc->entry_time += profdiff; /* offset with profdiff */
  }
  ShowClauseNumber(streamfile);
  last_entry_port=ENTER_REDO;
  last_cci=get_cc_item(active_ecc->cc_item_table, f);
  tick_last_addition=tick_ini_profiling;
  PROFILE__TIME_END;
}

CVOID__PROTO(profile__hook_cut_)
{
/*   show_nodes(w, w->node, w->next_node); */
  node_t *cp_younger=w->node;
  node_t *cp_older=w->next_node;
  int i=0;
  try_node_t *next_alt;
  if (cp_younger->next_alt)
    next_alt = cp_younger->next_alt;
  else
    next_alt = w->next_alt;
  cp_younger = ChoiceCharOffset(cp_younger, -next_alt->node_offset);
  while(ChoiceYounger(cp_younger, cp_older)) {
    get_cc_item(active_ecc->cc_item_table,cp_younger->functor)->prof.skips++;
    cp_younger = ChoiceCharOffset(cp_younger, -cp_younger->next_alt->node_offset);
  };
  if (!ChoiceYounger(cp_older, cp_younger)) {
    get_cc_item(active_ecc->cc_item_table,cp_younger->functor)->prof.skips++;
    active_ecc->entry_cuts++;
  }
  else
    active_ecc->entry_scuts++;
}

#if defined(PROFILE__TRACER)
CVOID__PROTO(profile__hook_fail_)
{
  FILE * streamfile;
  PROFILE__TIME_INI;
  streamfile=Output_Stream_Ptr->streamfile;
  ShowFuncPoint(Output_Stream_Ptr->streamfile, "-fail", 0, 0, last_cci->functor);
  ShowFuncPoint2(Output_Stream_Ptr->streamfile, " fail", 0, 0,
		 active_ecc->functor[0], active_ecc->functor[1]);
  PROFILE__TIME_END;
}

CVOID__PROTO(profile__hook_proceed_)
{
  PROFILE__TIME_INI;
  ShowFuncPoint(Output_Stream_Ptr->streamfile,"-proc",0,0,w->node->functor);
  ShowFuncPoint2(Output_Stream_Ptr->streamfile," proc",0,0,
		 active_ecc->functor[0], active_ecc->functor[1]);
  PROFILE__TIME_END;
}

CVOID__PROTO(profile__hook_neck_proceed_)
{
  PROFILE__TIME_INI;
  ShowFuncPoint(Output_Stream_Ptr->streamfile,"-nprc",0,0,w->node->functor);
  ShowFuncPoint2(Output_Stream_Ptr->streamfile," nprc",0,0,
		 active_ecc->functor[0], active_ecc->functor[1]);
  PROFILE__TIME_END;
}
#endif

/* This is executed just before to call the functor (1) */
CVOID__PROTO(profile__hook_call_, definition_t *functor) {
  FILE * streamfile;
  inttime_t profdiff;
  PROFILE__TIME_INI;
  profdiff=tick_ini_profiling - tick_last_addition;
  streamfile=Output_Stream_Ptr->streamfile;
  ShowFuncPoint(streamfile,"-exit",0,0,last_cci->functor);
  ShowFuncPoint2(streamfile," exit",0,0,
		 active_ecc->functor[0], active_ecc->functor[1]);
  ShowFuncPoint(streamfile,"+call",0,0,functor);
  ShowFuncPoint2(streamfile," call",0,0,
		 active_ecc->functor[0],active_ecc->functor[1]);
  last_cci->prof.counts[last_entry_port][LEAVE_EXIT]++;
  last_cci->prof.times[last_entry_port][LEAVE_EXIT]+=profdiff;
  /*     if (functor==cc_call||functor_is_instrumenter(last_cci->functor)) { */
  if (functor_have_overhead(active_frame, last_cci->functor)) {
    active_ecc->entry_time += profdiff; /* offset with profdiff */
  }
  last_cci=get_cc_item(active_ecc->cc_item_table,functor);
  last_entry_port=ENTER_CALL;
  tick_last_addition=tick_ini_profiling;
  PROFILE__TIME_END;
}

cc_item_t *add_cc_item(struct ht_tab *ht, definition_t *functor)
{
  cc_item_t *e;
  e=(cc_item_t *)checkalloc(sizeof(cc_item_t));
  e->functor=functor;
  PROFILE__RESET_PROF(e->prof);
  ht_add(ht,(ub1 *)(&(e->functor)),sizeof(functor),e);
  return e;
}

cc_item_t *get_cc_item(struct ht_tab *ht, definition_t *functor)
{
  if (ht_find(ht,(ub1 *)(&functor),sizeof(functor)))
    return (cc_item_t *)ht_stuff(ht);
  else
    return add_cc_item(ht,functor);  
}

#endif

void add_node_cc(struct ht_tab *vl, definition_t *functor)
{
  if (!ht_exists(vl,(ub1 *)(&functor), sizeof(functor))) {
    definition_t **d=(definition_t **)checkalloc(sizeof(definition_t *));
    *d=functor;
    ht_add(vl, (ub1 *)(d), sizeof(*d), d);
  }
}

edge_cc_t *add_edge_cc(struct ht_tab *cct, definition_t *functor[2])
{
  edge_cc_t *r;
  int i, j;
  r=(edge_cc_t *)checkalloc(sizeof(edge_cc_t));
  r->functor[0]=functor[0];
  r->functor[1]=functor[1];
  for (i=0;i<2;i++)
    for (j=0;j<2;j++)
      r->counts[i][j]=r->times[i][j]=0;
  r->entry_time=0;
#if defined(PROFILE)
  r->entry_cuts=0;
  r->entry_scuts=0;
  PROFILE__RESET_PORTS(r->cuts);
  PROFILE__RESET_PORTS(r->scuts);
  r->hooks=(profile_rcc||(functor[0]!=NULL));
  r->cc_item_table=ht_create(8);
#endif
  ht_add(cct,(ub1 *)(&r->functor),2*sizeof(functor),r);
  return r;
}

edge_cc_t *get_edge_cc(struct ht_tab *cct, definition_t *functor[2])
{
  if (ht_find(cct,(ub1 *)(functor),2*sizeof(functor)))
    return (edge_cc_t *)ht_stuff(cct);
  else
    return add_edge_cc(cct, functor);
}

void empty_hashtable(struct ht_tab * tab, int size)
{
  while (ht_first(tab)) {
    checkdealloc((tagged_t *)ht_stuff(tab),size);
    ht_del(tab);
  }
}

void empty_edge_table_cc(struct ht_tab *cct)
{
  edge_cc_t *r;
  while (ht_first(cct)) {
    r=(edge_cc_t *)ht_stuff(cct);
#if defined(PROFILE)
    empty_hashtable(r->cc_item_table, sizeof(cc_item_t));
    ht_destroy(r->cc_item_table);
#endif
    checkdealloc((tagged_t *)r,sizeof(edge_cc_t));
    ht_del(cct);
  }
}

#if defined(PROFILE)
void reset_cc_item_table(struct ht_tab *cc_item_table)
{
  cc_item_t *r;
  if (ht_first(cc_item_table)) do {
      r=(cc_item_t *)ht_stuff(cc_item_table);
      PROFILE__RESET_PROF(r->prof);
    }
    while (ht_next(cc_item_table));
}
#endif

void reset_edge_table_cc(struct ht_tab *cct)
{
  edge_cc_t *r;
  int i, j;
  if (ht_first(cct)) do {
      r=(edge_cc_t *)ht_stuff(cct);
      PROFILE__RESET_CC(*r);
#if defined(PROFILE)
      reset_cc_item_table(r->cc_item_table);
#endif
    }
    while (ht_next(cct));
}

void profile_reset(prof_frame_t *frame)
{
  reset_edge_table_cc(frame->edge_table_cc);
  empty_hashtable(frame->node_table_cc, sizeof(definition_t *));
#if defined(PROFILE__PROFTIME)
  tick_profiling=0;
#endif
  tick_last_addition=PROFILE__GET_TICK();
  tick_start=tick_last_addition;
}

void profile_init(void)
{
  int i;
  if (active_frame) {
    for (active_frame=prof_frames; active_frame<prof_frames+MAX_PROF_FRAMES;
	 active_frame++)
      profile_reset(active_frame);
  } else {
    for (active_frame=prof_frames; active_frame<prof_frames+MAX_PROF_FRAMES;
	 active_frame++) {
      active_ecc=NULL;
#if defined(PROFILE)
      last_cci=NULL;
#endif
      active_frame->edge_table_cc=ht_create(8);
      active_frame->node_table_cc=ht_create(8);
    }
  }
  profile_freq=PROFILE__GET_FREQ; /* Assume constant frequency (not
				     true in some laptops) */
  active_frame=prof_frames - 1;
  cc_call=GET_DEFINITION("profiler_rt:cc_call", 5);
  cc_call_nf=GET_DEFINITION("profiler_rt:cc_call_nf", 5);
  cc_call_ncnf=GET_DEFINITION("profiler_rt:cc_call_ncnf", 4);
  cc_redo=GET_DEFINITION("profiler_rt:cc_redo", 4);
  cc_redo_1=GET_DEFINITION("profiler_rt:cc_redo_1", 3);
  cc_redo_1_nf=GET_DEFINITION("profiler_rt:cc_redo_1_nf", 2);
  cc_redo_2=GET_DEFINITION("profiler_rt:cc_redo_2", 1);
  cc_exit=GET_DEFINITION("profiler_rt:cc_exit", 3);
  cc_exit_nc=GET_DEFINITION("profiler_rt:cc_exit_nc", 2);
  cc_exit_ncnf=GET_DEFINITION("profiler_rt:cc_exit_ncnf", 1);
  cc_fail=GET_DEFINITION("profiler_rt:cc_fail", 2);
  cc_fail_nc=GET_DEFINITION("profiler_rt:cc_fail_nc", 1);
  cc_fail_1=GET_DEFINITION("profiler_rt:cc_fail_1", 1);
  cc_fail_2=GET_DEFINITION("profiler_rt:cc_fail_2", 1);
  cost_center=GET_DEFINITION("profiler_cc:cost_center", 2);
  cost_center_4=GET_DEFINITION("profiler_cc:cost_center", 4);
  cost_center_nc=GET_DEFINITION("profiler_cc:cost_center_nc", 4);
  cost_center_ncnf=GET_DEFINITION("profiler_rt:cost_center_ncnf", 4);
  profile_leave_fail=GET_DEFINITION("profiler_utils:profile_leave_fail", 0);
  profile_leave_exit=GET_DEFINITION("profiler_utils:profile_leave_exit", 0);
  profile_enter_call=GET_DEFINITION("profiler_utils:profile_enter_call", 0);
  profile_enter_redo_1=GET_DEFINITION("profiler_utils:profile_enter_redo_1", 0);
}

static void show_func_point_info(
				 FILE * streamfile,
				 definition_t *functor)
{
  if (functor) {
    fprintf(streamfile, "%-6s ", predicate_type(functor->predtyp));
    if (IsString(functor->printname))
      fprintf(streamfile, "%s/", GetString(functor->printname));
    else
      fprintf(streamfile, "*** Unknown term ***/");
    fprintf(streamfile, "%d", functor->arity);
  }
  else
    fprintf(streamfile, "*** Null functor ***");
}

void show_func_point(
		     FILE * streamfile,
		     char *label,
		     int call_point,
		     unsigned int choice,
		     definition_t *functor)
{
  fprintf(streamfile, "%s %d-%d ", label, call_point, choice);
  show_func_point_info(streamfile, functor);
  fprintf(streamfile, "\n");
}

void show_func_point2(
		     FILE * streamfile,
		     char *label,
		     int call_point,
		     unsigned int choice,
		     definition_t *functor0,
		     definition_t *functor1)
{
  fprintf(streamfile, "%s %d-%d ", label, call_point, choice);
  show_func_point_info(streamfile, functor0);
  fprintf(streamfile, "<-");
  show_func_point_info(streamfile, functor1);
  fprintf(streamfile, "\n");
}

#define FORMAT_SEPARATOR						\
  "\n+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+"
#define FORMAT_SEPARATOR_2						\
  "\n+================================================================================================================================================================================================================================+"
#define FORMAT_TITLE							\
  "\n| %-79s |         |    CallExits     |    CallFails     |    RedoExits     |    RedoFails     |      Totals      |      (%%)       |      Time(ms)      |"
#define FORMAT_TITLE_2							\
  "\n| %-79s |  Skips  |Counts |   Ticks  |Counts |   Ticks  |Counts |   Ticks  |Counts |   Ticks  |Counts |  Ticks   | Counts | Ticks |                    |"

#define FORMAT_TITLE_PRED   "Predicates                                                               |"
#define FORMAT_TITLE_PRED_2 "Name                                                                     | Type"
#define FORMAT_TITLE_EDGE   "Edges"
#define FORMAT_TITLE_EDGE_2 "Called CC                     | Caller CC                     |H| Cuts  | SCuts"

#define FORMAT_TITLE_SEPARATOR						\
  "\n+---------------------------------------------------------------------------------+---------+-------+----------+-------+----------+-------+----------+-------+----------+-------+----------+--------+-------+--------------------+"
#define FORMAT_ITEM							\
  "%c(\n %-81s,i(%7ld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld/*%6.2f%%|%6.2f%%|%20s*/))"
#define FORMAT_CC_TOTAL_PORTS						\
  "\n %-81s,i(%7ld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld/*%6.2f%%|%6.2f%%|%20s*/),"
#define FORMAT_TOTAL							\
  "\n|Total */t(                                                              %9ld,i(%7ld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld,%7ld,%10lld/*%6.2f%%|%6.2f%%|%20s*/))"
#define FORMAT_OVERHEAD							\
  "\n|CC Overhead (%6.2f%%)                                                                                                                                                                                                           |"

#define NULL_VALUE(obj, member, value) (((obj)==NULL)?(value):(obj)->member)

char *functor_name(char *buffer, definition_t *functor) {
  if (functor)
    sprintf(buffer, "'%s'/%d", GetString(functor->printname), functor->arity);
  else
    sprintf(buffer, "'$$remainder_cc'/0");
  return buffer;
}

#if defined(PROFILE)

void print_profile_item(FILE * streamfile,
			definition_t *functor,
			char id,
			char *buffer,
			profile_currents_t *prof,
			long int counts, inttime_t times,
			long int total_counts, inttime_t total_times)
{
  char buffer_time[MAXSTRNUMBERSIZE];
  int arity, i, j;
  
  float_to_string(buffer_time,
		  ENG_FLT_SIGNIF,
		  'p',
		  ((double)times)*1.0e3/profile_freq,
		  10);
  fprintf(streamfile,
	  FORMAT_ITEM,
	  id,
	  buffer,
	  NULL_VALUE(prof, skips, 0),
	  NULL_VALUE(prof, counts[0][0], 0), NULL_VALUE(prof, times[0][0], 0),
	  NULL_VALUE(prof, counts[0][1], 0), NULL_VALUE(prof, times[0][1], 0),
	  NULL_VALUE(prof, counts[1][0], 0), NULL_VALUE(prof, times[1][0], 0),
	  NULL_VALUE(prof, counts[1][1], 0), NULL_VALUE(prof, times[1][1], 0),
	  counts,times,
	  (((double)counts)*100.0)/total_counts,(((double)times)*100.0)/total_times,
	  buffer_time);
}

void print_profile_edge(FILE * streamfile,
			edge_cc_wrap_t * edge_ccw,
			long int total_counts,
			inttime_t total_times)
{
  char buffer[2*(STATICMAXATOM+MAXSTRNUMBERSIZE+1)],
    buffer2[STATICMAXATOM+MAXSTRNUMBERSIZE+1],
    buffer3[STATICMAXATOM+MAXSTRNUMBERSIZE+1];
  inttime_t times;
  sprintf(buffer, "%-31s,%-31s,%d,%7ld,%7ld",
	  functor_name(buffer2, edge_ccw->ecc->functor[0]),
	  functor_name(buffer3, edge_ccw->ecc->functor[1]),
	  edge_ccw->ecc->hooks,
	  TOTAL(edge_ccw->ecc, cuts),
	  TOTAL(edge_ccw->ecc, scuts));
  if (edge_ccw->ecc->hooks)
    times=TOTAL(&(edge_ccw->total),times);
  else
    times=TOTAL(edge_ccw->ecc, times);
  print_profile_item(streamfile,
		     edge_ccw->ecc->functor[0],
		     'e',
		     buffer,
		     &(edge_ccw->total),
		     TOTAL(&(edge_ccw->total),counts),
		     times,
		     total_counts,
		     total_times);
}

void print_profile_pred(FILE *streamfile,
			definition_t *functor,
			profile_currents_t *prof,
			long int total_counts,
			inttime_t total_times)
{
  const char *predtype;
  char buffer[2*(STATICMAXATOM+MAXSTRNUMBERSIZE+1)],
    buffer2[STATICMAXATOM+MAXSTRNUMBERSIZE+1];

  if (functor)
    predtype=predicate_type(functor->predtyp);
  else
    predtype="remain";
  sprintf(buffer, "%-74s,%-6s", functor_name(buffer2, functor), predtype);
  print_profile_item(streamfile,
		     functor,
		     'p',
		     buffer,
		     prof,
		     TOTAL(prof,counts),
		     TOTAL(prof,times),
		     total_counts,
		     total_times);
}
#endif

void print_profile_total(FILE * streamfile,
			 profile_currents_t *total,
			 long int realsize)
{
  char buffer_time[MAXSTRNUMBERSIZE];
  inttime_t t=TOTAL(total,times);
  float_to_string(buffer_time
		  ,ENG_FLT_SIGNIF
		  ,'p'
		  ,(((double)t)/profile_freq)*1.0e3
		  ,10);
  fprintf(streamfile,
	  FORMAT_TOTAL,
	  realsize,
	  total->skips,
	  total->counts[0][0],total->times[0][0],
	  total->counts[0][1],total->times[0][1],
	  total->counts[1][0],total->times[1][0],
	  total->counts[1][1],total->times[1][1],
	  TOTAL(total,counts),t,
	  100.0,
	  100.0,
	  buffer_time);
}

void profile_ccw_dump(prof_frame_t *frame, FILE * streamfile, edge_cc_wrap_t *eccw, int index, long int counts, inttime_t times)
{
  int (*compare_cci)(const void *, const void *);
  long int i, j;
  char buffer[2*(STATICMAXATOM+MAXSTRNUMBERSIZE+1)+2+2*MAXSTRNUMBERSIZE],
    buffer2[STATICMAXATOM+MAXSTRNUMBERSIZE+1],
    buffer3[STATICMAXATOM+MAXSTRNUMBERSIZE+1];
  char buffer_time[MAXSTRNUMBERSIZE];
  sprintf(buffer, "%-31s,%-31s,%d,%7ld,%7ld",
	  functor_name(buffer2, eccw->ecc->functor[0]),
	  functor_name(buffer3, eccw->ecc->functor[1]),
#if defined(PROFILE)
	  eccw->ecc->hooks,
	  TOTAL(eccw->ecc,cuts),
	  TOTAL(eccw->ecc,scuts)
#else
	  0,0l,0l
#endif
	  );
  float_to_string(buffer_time,
		  ENG_FLT_SIGNIF,
		  'p',
		  ((double)(TOTAL(eccw->ecc, times))/profile_freq)*1.0e3,
		  10);
#if defined(PROFILE)
  if (profile_hooks) {
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_SEPARATOR_2);
    fprintf(streamfile, "*/");
  }
#endif
  fprintf(streamfile, "c(");
  fprintf(streamfile,
	  FORMAT_CC_TOTAL_PORTS,
	  buffer,
#if defined(PROFILE)
	  eccw->total.skips,
#else
	  0l,
#endif
	  eccw->ecc->counts[ENTER_CALL][LEAVE_EXIT],eccw->ecc->times[ENTER_CALL][LEAVE_EXIT],
	  eccw->ecc->counts[ENTER_CALL][LEAVE_FAIL],eccw->ecc->times[ENTER_CALL][LEAVE_FAIL],
	  eccw->ecc->counts[ENTER_REDO][LEAVE_EXIT],eccw->ecc->times[ENTER_REDO][LEAVE_EXIT],
	  eccw->ecc->counts[ENTER_REDO][LEAVE_FAIL],eccw->ecc->times[ENTER_REDO][LEAVE_FAIL],
	  TOTAL(eccw->ecc,counts),TOTAL(eccw->ecc,times),
	  (double)TOTAL(eccw->ecc,counts)/counts*100.0,(double)TOTAL(eccw->ecc,times)/times*100.0,
	  buffer_time);
#if defined(PROFILE)
  if (profile_hooks) {
    struct ht_tab *t;
    /* first calculate the totals */
    /* Make a table with room for them */
    cc_item_t **cci_table, **cci_table_oh, *e;
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_SEPARATOR);
    fprintf(streamfile, FORMAT_TITLE, FORMAT_TITLE_PRED);
    fprintf(streamfile, FORMAT_TITLE_2, FORMAT_TITLE_PRED_2);
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    fprintf(streamfile, "*/d([");
    t=eccw->ecc->cc_item_table;
    cci_table=(cc_item_t **)checkalloc(eccw->realsize * sizeof(cc_item_t *));
    cci_table_oh=(cc_item_t **)checkalloc((eccw->realsize_oh) * sizeof(cc_item_t *));
    i=0;
    j=0;
    if (ht_first(t)) do {
	e=(cc_item_t *)ht_stuff(t);
	if (TOTAL(&e->prof,counts)) {
	  if (functor_have_overhead(frame, e->functor))
	    cci_table_oh[j++]=e;
	  else
	    cci_table[i++]=e;
	}
      } while (ht_next(t));
    compare_cci=compare_cci_ticks;
    qsort(cci_table, eccw->realsize, sizeof(cc_item_t *), compare_cci);
    qsort(cci_table_oh, eccw->realsize_oh, sizeof(cc_item_t *), compare_cci);
    i=eccw->realsize;
    while (i>0){
      if (i != eccw->realsize) fprintf(streamfile, ",");
      e=cci_table[--i];
      print_profile_pred(streamfile, e->functor, PROFILE__FUNCTOR_PROF(e),
			 TOTAL(&eccw->total,counts), TOTAL(&eccw->total,times));
    }
    fprintf(streamfile, "],/*");
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    print_profile_total(streamfile, &(eccw->total), eccw->realsize);
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_SEPARATOR);
    fprintf(streamfile, FORMAT_OVERHEAD,
	    ((double)TOTAL(&eccw->overhead,times))/(TOTAL(&eccw->total,times) +
						    TOTAL(&eccw->overhead,times))*100.0);
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    fprintf(streamfile, "*/,[");
    j=eccw->realsize_oh;
    while (j>0){
      if (j != eccw->realsize_oh) fprintf(streamfile, ",");
      e=cci_table_oh[--j];
      print_profile_pred(streamfile, e->functor, PROFILE__FUNCTOR_PROF(e),
			 TOTAL(&eccw->overhead,counts), TOTAL(&eccw->overhead,times));
    }
    fprintf(streamfile, "],/*", index);
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    print_profile_total(streamfile, &(eccw->overhead), eccw->realsize_oh);
    fprintf(streamfile, ")");
    checkdealloc((tagged_t *)cci_table_oh, eccw->realsize_oh*sizeof(cc_item_t *));
    checkdealloc((tagged_t *)cci_table, eccw->realsize*sizeof(cc_item_t *));
  } else
#endif
    fprintf(streamfile, "_");
  fprintf(streamfile, ")");
}

#if defined(PROFILE)
static int compare_cci_counts(const void *arg1, const void *arg2)
{
  cc_item_t **cce1, **cce2;
  
  cce1=(cc_item_t **)arg1;
  cce2=(cc_item_t **)arg2;

  if (TOTAL(&(*cce1)->prof,counts) > TOTAL(&(*cce2)->prof,counts))
    return (1);
  if (TOTAL(&(*cce1)->prof,counts) < TOTAL(&(*cce2)->prof,counts))
    return (-1);
  return (0);
}

static int compare_cci_ticks(const void *arg1, const void *arg2)
{
  cc_item_t **cce1, **cce2;
  
  cce1=(cc_item_t **)arg1;
  cce2=(cc_item_t **)arg2;

  if (TOTAL(&(*cce1)->prof,times) > TOTAL(&(*cce2)->prof,times))
    return (1);
  if (TOTAL(&(*cce1)->prof,times) < TOTAL(&(*cce2)->prof,times))
    return (-1);
  return (0);
}
#endif

void profile_edge_cc_dump(prof_frame_t *frame, FILE * streamfile)
{
  char buffer[128];
  edge_cc_wrap_t *c;
  edge_cc_t *ecc;
  int (*compare_eccw)(const void *, const void *);
  edge_cc_t total_ecc;
  long int i, j, k;
  char *predtype;
  int arity;
  long int realsize=0;
  edge_cc_wrap_t *eccw_table;
  inttime_t total_times=0;
#if defined(PROFILE)
  profile_currents_t total;
  unsigned long int cuts, scuts;
  PROFILE__RESET_PROF(total);
#endif
  PROFILE__RESET_CC(total_ecc);
  eccw_table=(edge_cc_wrap_t *)checkalloc(ht_count(frame->edge_table_cc)
					  * sizeof(edge_cc_wrap_t));
  if (ht_first(frame->edge_table_cc)) do {
      ecc=(edge_cc_t *)ht_stuff(frame->edge_table_cc);
      inttime_t ecc_total_times=TOTAL(ecc,times);
      total_times+=ecc_total_times;
#if defined(PROFILE)
      cuts+=TOTAL(ecc,cuts);
      scuts+=TOTAL(ecc,scuts);
      /* first calculate the totals */
      eccw_table[realsize].realsize=0;
      eccw_table[realsize].realsize_oh=0;
      PROFILE__RESET_PROF(eccw_table[realsize].total);
      PROFILE__RESET_PROF(eccw_table[realsize].overhead);
      if (profile_hooks) {
	struct ht_tab *t;
	t=ecc->cc_item_table;
	if (ht_first(t)) do {
	    cc_item_t *e;
	    e=(cc_item_t *)ht_stuff(t);
	    if (TOTAL(&(e->prof),counts)) {
	      if (functor_have_overhead(frame, e->functor)) {
		eccw_table[realsize].realsize_oh++;
		for (i=0;i<2;i++)
		  for (j=0;j<2;j++) {
		    eccw_table[realsize].overhead.counts[i][j] += e->prof.counts[i][j];
		    eccw_table[realsize].overhead.times[i][j] += e->prof.times[i][j];
		  }
		eccw_table[realsize].overhead.skips += e->prof.skips;
	      }
	      else
		{
		  eccw_table[realsize].realsize++;
		  for (i=0;i<2;i++)
		    for (j=0;j<2;j++) {
		      eccw_table[realsize].total.counts[i][j] += e->prof.counts[i][j];
		      eccw_table[realsize].total.times[i][j] += e->prof.times[i][j];
		    }
		  eccw_table[realsize].total.skips += e->prof.skips;
		}
	    }
	  } while (ht_next(t));
	if (ecc->hooks) {
	  if (TOTAL(&eccw_table[realsize].total,times)!=ecc_total_times)
	    fprintf(streamfile, "/*** Problems when sumarizing counts %lld!=%lld ***/\n",
		    TOTAL(&eccw_table[realsize].total,times), ecc_total_times);
	}
      }
#endif
      if (TOTAL(ecc,counts) != 0) {
	eccw_table[realsize].ecc=ecc;
	realsize++;
      }
    } while (ht_next(frame->edge_table_cc));
  if (realsize<=1) {
    fprintf(streamfile, "/* NOTE: No cost centers has been specified */");
  };
  /*   else */
  {
    char buffer_time[MAXSTRNUMBERSIZE];
    compare_eccw=compare_ccw_ticks;
    qsort(eccw_table, realsize, sizeof(edge_cc_wrap_t), compare_eccw);
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_SEPARATOR_2);
    fprintf(streamfile, "*/cr([/*\n");
    fprintf(streamfile, " Cost center based profiling report: ");
#if defined(PROFILE)
    if (!profile_hooks)
#endif
      {
	fprintf(streamfile, FORMAT_SEPARATOR);
	fprintf(streamfile, FORMAT_TITLE, FORMAT_TITLE_EDGE);
	fprintf(streamfile, FORMAT_TITLE_2, FORMAT_TITLE_EDGE_2);
	fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
      }
    fprintf(streamfile, "*/");
    i=realsize;
    while (i>0) {
      --i;
#if defined(PROFILE)
      if (profile_hooks) {
	for (j=0;j<2;j++)
	  for (k=0;k<2;k++) {
	    total.counts[j][k] += eccw_table[i].total.counts[j][k];
	    total.times[j][k] += eccw_table[i].total.times[j][k];
	  }
	total.skips += eccw_table[i].total.skips;
      }
#endif
      for (j=0;j<2;j++)
	for (k=0;k<2;k++) {
	  total_ecc.counts[j][k] += eccw_table[i].ecc->counts[j][k];
	  total_ecc.times[j][k] += eccw_table[i].ecc->times[j][k];
	}
    };
    i=realsize;
    while (i>0) {
      if (i != realsize)
	fprintf(streamfile, ",");
      --i;
      profile_ccw_dump(frame, streamfile, eccw_table+i, realsize-i,
		       TOTAL(&total_ecc, counts), total_times);
    }
    fprintf(streamfile, "],/*");
    fprintf(streamfile, FORMAT_SEPARATOR_2);
    float_to_string(buffer_time,
		    ENG_FLT_SIGNIF,
		    'p',
		    ((double)total_times/profile_freq)*1.0e3,
		    10);
    sprintf(buffer, "Total Edge Cost Center Counters          */tc(%7ld,%7ld",
#if defined(PROFILE)
	    cuts,
	    scuts
#else
	    0l,0l
#endif
	    );
    fprintf(streamfile, FORMAT_CC_TOTAL_PORTS,
	    buffer,
#if defined(PROFILE)
	    total.skips,
#else
	    0l,
#endif
	    total_ecc.counts[ENTER_CALL][LEAVE_EXIT],total_ecc.times[ENTER_CALL][LEAVE_EXIT],
	    total_ecc.counts[ENTER_CALL][LEAVE_FAIL],total_ecc.times[ENTER_CALL][LEAVE_FAIL],
	    total_ecc.counts[ENTER_REDO][LEAVE_EXIT],total_ecc.times[ENTER_REDO][LEAVE_EXIT],
	    total_ecc.counts[ENTER_REDO][LEAVE_FAIL],total_ecc.times[ENTER_REDO][LEAVE_FAIL],
	    TOTAL(&total_ecc,counts),total_times, 100.0, 100.0,
	    buffer_time);
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_SEPARATOR);
#if defined(PROFILE)
    if (profile_hooks) {
      fprintf(streamfile, FORMAT_TITLE,   FORMAT_TITLE_EDGE);
      fprintf(streamfile, FORMAT_TITLE_2, FORMAT_TITLE_EDGE_2);
      fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    }
#endif
    fprintf(streamfile, "*/");
#if defined(PROFILE)
    if (profile_hooks) {
      fprintf(streamfile, "[");
      i=realsize;
      while (i>0) {
	if (i != realsize) fprintf(streamfile, ",");
	c=&eccw_table[--i];
	print_profile_edge(streamfile, c, TOTAL(&total,counts), total_times);
      }
      fprintf(streamfile, "],");
    } else
#endif
      fprintf(streamfile, "_,");
#if defined(PROFILE)
    if (profile_hooks) {
      char buffer_time[MAXSTRNUMBERSIZE];
      fprintf(streamfile, "/*");
      fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
      float_to_string(buffer_time
		      ,ENG_FLT_SIGNIF
		      ,'p'
		      ,(((double)total_times)/profile_freq)*1.0e3
		      ,10);
  fprintf(streamfile,
	  FORMAT_TOTAL,
	  realsize,
#if defined(PROFILE)
	  total.skips,
#else
	  0l,
#endif
	  total.counts[0][0],total.times[0][0],
	  total.counts[0][1],total.times[0][1],
	  total.counts[1][0],total.times[1][0],
	  total.counts[1][1],total.times[1][1],
	  TOTAL(&total,counts),total_times,
	  100.0,
	  100.0,
	  buffer_time);
      fprintf(streamfile, "/*");
      fprintf(streamfile, FORMAT_SEPARATOR_2);
      fprintf(streamfile, "*/");
    } else
#endif
      fprintf(streamfile, "_");
    fprintf(streamfile, ")");
  }
  checkdealloc((tagged_t *)eccw_table, ht_count(frame->edge_table_cc)
	       * sizeof(edge_cc_wrap_t));
  fprintf(streamfile, ")");
}

static int compare_ccw_calls(const void *arg1, const void *arg2)
{
  edge_cc_wrap_t *eccw1, *eccw2;
  
  eccw1=(edge_cc_wrap_t *)arg1;
  eccw2=(edge_cc_wrap_t *)arg2;
  if (TOTAL(eccw1->ecc,counts) > TOTAL(eccw2->ecc,counts))
    return (1);
  if (TOTAL(eccw1->ecc,counts) < TOTAL(eccw2->ecc,counts))
    return (-1);
  return (0);
}

static int compare_ccw_ticks(const void *arg1, const void *arg2)
{
  edge_cc_wrap_t *eccw1, *eccw2;
  
  eccw1=(edge_cc_wrap_t *)arg1;
  eccw2=(edge_cc_wrap_t *)arg2;

  if (TOTAL(eccw1->ecc,times) > TOTAL(eccw2->ecc,times))
    return (1);
  if (TOTAL(eccw1->ecc,times) < TOTAL(eccw2->ecc,times))
    return (-1);
  return (0);
}

void profile_flat_dump(prof_frame_t *frame, FILE * streamfile)
{
  int j, k, l, n;
  long int i, realsize=0, realsize_oh=0;
  sw_on_key_t *table=(sw_on_key_t *)*predicates_location;
  sw_on_key_node_t *keyval;
  definition_t *d;
  int (*compare_func)(const void *, const void *);
  profile_currents_t *prof;
  fprintf(streamfile, "fr(");
#if defined(PROFILE)
  definition_wrap_t *pred_table, *pred_table_oh;
  profile_currents_t total, overhead;
  inttime_t tick_not_profiling=0;
  if (profile_hooks) {
    PROFILE__RESET_PROF(total);
    PROFILE__RESET_PROF(overhead);
/*     Please consider the next definitions: */
/*     Counts[][]=Number of times an input-output port is reached */
/*     Skips     =Number of nodes removed with a cut. */
/*     Cuts      =Number of cuts that remove nodes. */
/*     SCuts     =Number of cuts that do not remove nodes. */
    fprintf(streamfile, "/*\n Flat profiling report:");
    fprintf(streamfile, FORMAT_SEPARATOR_2);
    n=SwitchSize(*predicates_location);
    prof=(profile_currents_t *)checkalloc(n*sizeof(profile_currents_t));
    for (j=n-1;j>=0;--j) {
      /* Find how many preds. we have called */
      keyval=&table->tab.asnode[j];
      PROFILE__RESET_PROF(prof[j]);
      if (d=keyval->value.def)
	{
	  if (ht_first(frame->edge_table_cc)) do {
	      edge_cc_t *cc=(edge_cc_t *)ht_stuff(frame->edge_table_cc);
	      if (cc->hooks) {
		struct ht_tab *t=cc->cc_item_table;
		if (ht_find(t, (ub1 *)(&d), sizeof(d))) {
		  cc_item_t *cci=(cc_item_t *)ht_stuff(t);
		  for (k=0;k<2;k++)
		    for (l=0;l<2;l++) {
		      prof[j].counts[k][l] += cci->prof.counts[k][l];
		      prof[j].times[k][l]  += cci->prof.times[k][l];
		    }
		  prof[j].skips+=cci->prof.skips;
		}
	      }
	    } while (ht_next(frame->edge_table_cc));
	  if (TOTAL(prof+j,times)) {
	    if (functor_have_overhead(frame, d)) {
	      realsize_oh++;
	      for (k=0;k<2;k++)
		for (l=0;l<2;l++) {
		  overhead.counts[k][l]+=prof[j].counts[k][l];
		  overhead.times[k][l]+=prof[j].times[k][l];
		}
	      overhead.skips+=prof[j].skips;
	    }
	    else {
	      realsize++;
	      for (k=0;k<2;k++)
		for (l=0;l<2;l++) {
		  total.counts[k][l]+=prof[j].counts[k][l];
		  total.times[k][l]+=prof[j].times[k][l];
		}
	      total.skips+=prof[j].skips;
	    }
	  }
	}
    }
    if (ht_first(frame->edge_table_cc)) do {
	edge_cc_t *cc=(edge_cc_t *)ht_stuff(frame->edge_table_cc);
	if (!cc->hooks) {
	  for (k=0;k<2;k++)
	    for (l=0;l<2;l++)
	      tick_not_profiling+=cc->times[k][l];
	}
      } while (ht_next(frame->edge_table_cc));
    /* using the time conservation law, we can obtain the tot_ticks:
       (valid also when time profiling is off)*/
#if defined(PROFILE__PROFTIME)
    if (tick_profiling + tick_not_profiling + TOTAL(&total,times)
	+ TOTAL(&overhead,times)!=tick_last_addition - tick_start) {
      fprintf(streamfile, "\n Verification Error %lld != %lld",
	      tick_profiling + tick_not_profiling + TOTAL(&total,times)
	      + TOTAL(&overhead,times), tick_last_addition - tick_start);
      fprintf(streamfile, "\n   %lld+%lld+%lld+%lld != %lld-%lld",
	      tick_profiling, tick_not_profiling, TOTAL(&total,times),
	      TOTAL(&overhead,times), tick_last_addition, tick_start);
    }
#else
    /*   tot_ticks=tick_last_addition - tick_start + 1; */
    if (TOTAL(&total,times) + TOTAL(&overhead,times)
	!= tick_last_addition - tick_start) {
      fprintf(streamfile, "\n Verification Error %lld != %lld",
	      TOTAL(&total,times) + TOTAL(&overhead,times),
	      tick_last_addition - tick_start);
      fprintf(streamfile, "\n   %lld+%lld != %lld-%lld",
	      TOTAL(&total,times) + TOTAL(&overhead,times),
	      tick_last_addition, tick_start);
    }
#endif
    
    pred_table=                  /* Make a table with room for them */
      (definition_wrap_t *)checkalloc(realsize*sizeof(definition_wrap_t));
    pred_table_oh=
      (definition_wrap_t *)checkalloc(realsize_oh*sizeof(definition_wrap_t));
    
    realsize=0;
    realsize_oh=0;
    for (j=n-1;j>=0;--j) {
      keyval=&table->tab.asnode[j];
      if ((d=keyval->value.def) && TOTAL(prof+j,times)) {
	if (functor_have_overhead(frame, d)) {
	  pred_table_oh[realsize_oh].prof=prof+j;
	  pred_table_oh[realsize_oh++].functor=d;
	}
	else {
	  pred_table[realsize].prof=prof+j;
	  pred_table[realsize++].functor=d;
	}
      }
    }
    compare_func=compare_ticks;
    qsort(pred_table, realsize, sizeof(definition_wrap_t), compare_func);
    qsort(pred_table_oh, realsize_oh, sizeof(definition_wrap_t), compare_func);
    fprintf(streamfile, FORMAT_TITLE, FORMAT_TITLE_PRED);
    fprintf(streamfile, FORMAT_TITLE_2, FORMAT_TITLE_PRED_2);
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    fprintf(streamfile, "*/");
  }
#endif
  fprintf(streamfile, "[");
#if defined(PROFILE)
  i=realsize;
  while (i>0) {
    if (i != realsize) fprintf(streamfile, ",");
    d=pred_table[--i].functor;
    print_profile_pred(streamfile, d, pred_table[i].prof,
		       TOTAL(&total,counts), TOTAL(&total,times));
  }
#endif
  fprintf(streamfile, "],");
#if defined(PROFILE)
  if (profile_hooks) {
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    print_profile_total(streamfile, &total, realsize);
  } else
#endif
    fprintf(streamfile, "_");
  fprintf(streamfile, ",");
#if defined(PROFILE)
  if (profile_hooks) {
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    i=realsize_oh;
    if (i) {
      fprintf(streamfile, "\n Overhead:");
      fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    }
    else
      fprintf(streamfile, "\n No overhead caused by instrumentation predicates");
    fprintf(streamfile, "*/");
  }
#endif
  fprintf(streamfile, "[");
#if defined(PROFILE)
  if (profile_hooks) {
    while (i>0) {
      if (i != realsize_oh) fprintf(streamfile, ",");
      d=pred_table_oh[--i].functor;
      print_profile_pred(streamfile, d, pred_table_oh[i].prof,
			 TOTAL(&overhead,counts), TOTAL(&overhead,times));
    }
  }
#endif
  fprintf(streamfile, "],");
#if defined(PROFILE)
  if (profile_hooks) {
    fprintf(streamfile, "/*");
    fprintf(streamfile, FORMAT_TITLE_SEPARATOR);
    print_profile_total(streamfile, &overhead, realsize_oh);
  } else
#endif
    fprintf(streamfile, "_");

#if defined(PROFILE__PROFTIME)
  inttime_t total_overhead;
# if defined(PROFILE)
  if (profile_hooks)
    total_overhead=TOTAL(&overhead,times);
  else
# endif
    total_overhead=0;
  fprintf(streamfile, "/*");
  fprintf(streamfile, FORMAT_SEPARATOR_2);
  fprintf(streamfile, "*/,oh(\n %12lld /* %8f ms (%6.2f%%) Executing profiler hooks */",
	  tick_profiling,
	  (double)tick_profiling/profile_freq*1.0e3,
	  (double)tick_profiling/(tick_last_addition - tick_start)*100.0);
# if defined(PROFILE)
  if (profile_hooks) {
    fprintf(streamfile, ",\n %12lld /* %8f ms (%6.2f%%) Executing instrumentation code",
	    total_overhead,
	    ((double)total_overhead/profile_freq)*1.0e3,
	    ((double)total_overhead)/(tick_last_addition - tick_start)*100.0);
    fprintf(streamfile, FORMAT_SEPARATOR);
    fprintf(streamfile, "*/,\n %12lld /* %8f ms (%6.2f%%) Total overhead",
	    tick_profiling + total_overhead,
	    ((double)tick_profiling + total_overhead)/profile_freq*1.0e3,
	    ((double)tick_profiling + total_overhead)/(tick_last_addition - tick_start)*100.0);
    fprintf(streamfile, "*/,\n %12lld /* %8f ms (%6.2f%%) Executing code with WAM hooks turned off",
	    tick_not_profiling,
	    ((double)tick_not_profiling)/profile_freq*1.0e3,
	    ((double)tick_not_profiling)/(tick_last_addition - tick_start)*100.0);
    fprintf(streamfile, "*/,\n %12lld /* %8f ms (%6.2f%%) Executing code with WAM hooks turned on",
	    TOTAL(&total,times),
	    ((double)TOTAL(&total,times))/profile_freq*1.0e3,
	    ((double)TOTAL(&total,times))/(tick_last_addition - tick_start)*100.0);
    fprintf(streamfile, FORMAT_SEPARATOR);
    fprintf(streamfile, "*/");
  } else
# endif
    fprintf(streamfile, ",0,%lld,%lld,0", tick_profiling,
	    tick_last_addition - tick_start - tick_profiling);
  fprintf(streamfile, ",\n %12lld /* %8f ms (%6.2f%%) Total Time */)",
	  tick_last_addition-tick_start,
	  ((double)(tick_last_addition-tick_start))/profile_freq*1.0e3,
	  100.0);
#else
  fprintf(streamfile, ",_");
#endif
  fprintf(streamfile, ")");
#if defined(PROFILE)
  if (profile_hooks) {
    checkdealloc((tagged_t *)pred_table_oh, realsize_oh*sizeof(definition_t *));
    checkdealloc((tagged_t *)pred_table, realsize*sizeof(definition_t *));
    checkdealloc((tagged_t *)prof, realsize*sizeof(profile_currents_t));
  }
#endif
}

static int compare_counts(const void *arg1, const void *arg2)
{
#if defined(PROFILE)
  definition_wrap_t *dw1, *dw2;
  
  dw1=(definition_wrap_t *)arg1;
  dw2=(definition_wrap_t *)arg2;
  
  if (TOTAL(dw1->prof,counts) > TOTAL(dw2->prof,counts))
    return (1);
  if (TOTAL(dw1->prof,counts) < TOTAL(dw2->prof,counts))
    return (-1);
#endif
  return (0);
}

static int compare_ticks(const void *arg1, const void *arg2)
{
#if defined(PROFILE)
  definition_wrap_t *pred1, *pred2;
  
  pred1=(definition_wrap_t *)arg1;
  pred2=(definition_wrap_t *)arg2;
  
  if (TOTAL(pred1->prof,times) > TOTAL(pred2->prof,times))
    return (1);
  if (TOTAL(pred1->prof,times) < TOTAL(pred2->prof,times))
    return (-1);
#endif
  return (0);
}

const char * predicate_type(int t)
{
  switch (t) {
  case ENTER_COMPACTCODE:          return "compac";
  case ENTER_COMPACTCODE_INDEXED:  return "compid";
  case ENTER_PROFILEDCODE:         return "emul";
  case ENTER_PROFILEDCODE_INDEXED: return "emulid";
  case ENTER_FASTCODE:             return "fast";
  case ENTER_FASTCODE_INDEXED:     return "fastid";
  case ENTER_UNDEFINED:            return "undef";
  case ENTER_C:                    return "c";
  case ENTER_INTERPRETED:          return "interp";
  case BUILTIN_ABORT:              return "buabor";
  case BUILTIN_APPLY:              return "butapp";
  case BUILTIN_CALL:               return "bucall";
  case BUILTIN_SYSCALL:            return "buscll";
  case BUILTIN_NODEBUGCALL:        return "bundcl";
  case BUILTIN_TRUE:               return "butrue";
  case BUILTIN_FAIL:               return "bufail";
  case BUILTIN_CURRENT_INSTANCE:   return "bucins";
  case BUILTIN_RESTORE:            return "burest";
  case BUILTIN_COMPILE_TERM:       return "bucomp";
  case BUILTIN_GELER:              return "bugele";
  case BUILTIN_INSTANCE:           return "buinst";
  case BUILTIN_DIF:                return "builtd";
  default:                         return "other";
  }
}

void profile_dump(prof_frame_t *frame, FILE * streamfile)
{
  fprintf(streamfile, "p(%lld", profile_freq);
  if (frame->edge_table_cc==NULL)
    fprintf(streamfile, ",_,_)\n");
  else {
    fprintf(streamfile, ",");
    profile_flat_dump(frame, streamfile);
    fprintf(streamfile, ",");
    profile_edge_cc_dump(frame, streamfile);
    fprintf(streamfile, ")\n");
  }
}
