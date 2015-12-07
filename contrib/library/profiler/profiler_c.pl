:- module(profiler_c, [], [assertions, nativeprops, foreign_interface]).

:- foreign_inline("
#include <stdio.h>
#include <ciao_prolog.h>
#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/profile_hooks.h>
#include <ciao/timing.h>
#include <ciao/eng_dbg.h>
#include <ciao/initial.h>
#include \"profiler.h\"
").

:- use_foreign_source('../hashtable/hashtab').
:- use_foreign_source('../hashtable/lookupa').
:- use_foreign_source('../hashtable/recycle').
:- extra_compiler_opts('-I../hashtable').
:- use_foreign_source('../hrtime/hrtime').
:- extra_compiler_opts('-I../hrtime').


:- use_foreign_source(profiler).

/* for profiler_rt.pl */

:- initialization(profile_init).



:- export(get_profile_active/1).
:- true pred get_profile_active(go(Active)) :: int
	+ (foreign_low(prolog_get_profile_active)) #
 
	"Unifies Active with 1 if the profiler is turned on, or with 0 otherwise"  -->

"
CBOOL__PROTO(prolog_get_profile_active)
{
  return cunify(Arg,MakeInteger(Arg,profile),X(0));
}
".

:- export(add_node_cc/2).
:- true pred add_node_cc(in(Name), in(Arity)) :: atm * int
	+ (foreign_low(prolog_add_node_cc), not_fails) -->
"
void add_node_cc(struct ht_tab *vl, definition_t *functor);

CBOOL__PROTO(prolog_add_node_cc)
{
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  add_node_cc(active_frame[1].node_table_cc, f);
  return TRUE;
}
".

:- export(profile_init/0).
:- true pred profile_init + (foreign_low(prolog_profile_init)) -->

"
CBOOL__PROTO(prolog_profile_init)
{
  profile_init();
  return TRUE;
}
".

:- export(cc_call/5).
:- true pred cc_call(in(Name), in(Arity), in(Hooks), go(PrevECC), go(CutTo))
	:: atm * int * int * int * int + ( foreign_low(prolog_cc_call),
	    not_fails ) -->
"
CBOOL__PROTO(prolog_cc_call_ncnf);

CBOOL__PROTO(prolog_cc_call)
{
  Unify_constant(ChoiceToInt(w->node),X(4));
  return prolog_cc_call_ncnf(Arg);
}
".

:- export(cc_redo_1/3).
:- true pred cc_redo_1(in(ChPt0), in(ChPt1), in(CutTo)) :: int * int * int
	+ (foreign_low(prolog_cc_redo_1), not_fails) -->
"
CBOOL__PROTO(prolog_cc_redo_1)
{
  if (profile) {
    PROFILE__TIME_INI;
    node_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromInt(X(0));
    n2=ChoiceFromInt(X(1));
    if (n1==n2) {
      DEREF(X(2),X(2));
      w->node=ChoiceFromInt(X(2));
      SetShadowregs(w->node);
    }
    PROFILE__TIME_END;
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- export(cc_redo_1_nf/2).
:- true pred cc_redo_1_nf(in(ChPt1), in(CutTo)) :: int * int
	+ (foreign_low(prolog_cc_redo_1_nf), not_fails) -->
"
CBOOL__PROTO(prolog_cc_redo_1_nf)
{
  if (profile) {
    PROFILE__TIME_INI;
    node_t *n1, *n2;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    n1=ChoiceFromInt(X(1));
    n2=ChoiceFromInt(X(0));
    if (n1==n2) {
      w->node=n1;
      SetShadowregs(w->node);
    }
    PROFILE__TIME_END;
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- export(cc_redo_2/1).
:- true pred cc_redo_2(in(ActiveCC)) :: int
	+ (foreign_low(prolog_cc_redo_2), fails) -->
"
CBOOL__PROTO(prolog_cc_redo_2)
{
  if (profile) {
    inttime_t d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_port=ENTER_REDO;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-redo\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}
".

:- export(cc_exit/3).
:- true pred cc_exit(in(PrevECC), go(ActiveCC), go(ChPt))
	:: int * int * int
	+ (foreign_low(prolog_cc_exit), not_fails) -->
"
CVOID__PROTO(prolog_cc_exit_common)
{
  inttime_t d;
  DEREF(X(0),X(0));
  ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-exit\",0,0,
    active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][LEAVE_EXIT]++;
  active_ecc->times[active_ecc->entry_port][LEAVE_EXIT]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
#if defined(PROFILE)
  active_ecc->cuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_cuts;
  active_ecc->scuts[active_ecc->entry_port][LEAVE_EXIT]+=active_ecc->entry_scuts;
  active_ecc->entry_cuts=0;
  active_ecc->entry_scuts=0;
#endif
  active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
  if (profile_hooks) {
    if (active_ecc->hooks)
      ENABLE_HOOKS(active_ecc->functor[0])
    else
      DISABLE_HOOKS
  }
#endif
  active_ecc->entry_time+=d;
  tick_last_addition=tick_ini_profiling;
}

CBOOL__PROTO(prolog_cc_exit)
{
  Unify_constant(ChoiceToInt(w->node),X(2));
  if (profile) {
    bool_t r;
    edge_cc_t *cc;
    PROFILE__TIME_INI;
    cc=active_ecc;
    prolog_cc_exit_common(Arg);
    r=cunify(Arg,PointerToTerm(cc),X(1));
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}
".

:- export(cc_fail_1/1).
:- true pred cc_fail_1(go(ChPt)) :: int
	+ (foreign_low(prolog_cc_fail_1), not_fails) -->
"
CBOOL__PROTO(prolog_cc_fail_1)
{
  if (profile) {
    Unify_constant(ChoiceToInt(w->node),X(0));
  } else {
    w->node=w->next_node; /* DOCUT */
  }
  return TRUE;
}
".

:- export(cc_fail_2/1).
:- true pred cc_fail_2(in(PrevECC)) :: int
	+ (foreign_low(prolog_cc_fail_2), fails) -->
"
CBOOL__PROTO(prolog_cc_fail_2)
{
  if (profile) {
    inttime_t d;
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-fail\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->counts[active_ecc->entry_port][LEAVE_FAIL]++;
    active_ecc->times[active_ecc->entry_port][LEAVE_FAIL]+=d-active_ecc->entry_time;
    active_ecc->entry_time=0;
#if defined(PROFILE)
    active_ecc->cuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_cuts;
    active_ecc->scuts[active_ecc->entry_port][LEAVE_FAIL]+=active_ecc->entry_scuts;
    active_ecc->entry_cuts=0;
    active_ecc->entry_scuts=0;
#endif
    active_ecc=(edge_cc_t *)TermToPointer(X(0));
#if defined(PROFILE)
    if (profile_hooks) {
      if (active_ecc->hooks)
        ENABLE_HOOKS(active_ecc->functor[0])
      else
        DISABLE_HOOKS
    }
#endif
    active_ecc->entry_time+=d;
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
  }
  return FALSE;
}
".


:- export(cc_exit_nc/2).
:- true pred cc_exit_nc(in(PrevECC), in(CutTo)) :: int * int
	+ (foreign_low(prolog_cc_exit_nc), not_fails) -->
"
CBOOL__PROTO(prolog_cc_exit_nc)
{
  if (profile) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    DEREF(X(1),X(1));
    w->node=ChoiceFromInt(X(1));
    SetShadowregs(w->node);
    PROFILE__TIME_END;
  }
  return TRUE;
}
".

:- export(cc_call_ncnf/4).
:- true pred cc_call_ncnf(in(Name), in(Arity), in(Hooks), go(PrevECC))
	:: atm * int * int * int
	+ (foreign_low(prolog_cc_call_ncnf), not_fails) -->
"
CBOOL__PROTO(prolog_cc_call_ncnf)
{
  if (profile) {
    bool_t r;
    inttime_t d;
    definition_t *f[2];
    PROFILE__TIME_INI;
    DEREF(X(0),X(0));
    DEREF(X(1),X(1));
    f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
    f[1]=active_ecc->functor[0];
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
    active_ecc->entry_time-=d;
    r=cunify(Arg,PointerToTerm(active_ecc),X(3));
    active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
#if defined(PROFILE)
    DEREF(X(2),X(2));
    active_ecc->hooks=GetInteger(X(2));
    if (profile_hooks) {
      if (active_ecc->hooks) {
        ENABLE_HOOKS(f[0]);
      }
      else {
	DISABLE_HOOKS;
      }
    }
#endif
    active_ecc->entry_port=ENTER_CALL;
    active_ecc->entry_time+=d;
    ShowFuncPoint2(Output_Stream_Ptr->streamfile,\"cc-call\",0,0,
      active_ecc->functor[0],active_ecc->functor[1]);
    tick_last_addition=tick_ini_profiling;
    PROFILE__TIME_END;
    return r;
  }
  else
    return TRUE;
}
".

:- export(cc_exit_ncnf/1).
:- true pred cc_exit_ncnf(in(PrevECC)) :: int
	+ (foreign_low(prolog_cc_exit_ncnf), not_fails) -->
"
CBOOL__PROTO(prolog_cc_exit_ncnf)
{
  if (profile) {
    PROFILE__TIME_INI;
    prolog_cc_exit_common(Arg);
    PROFILE__TIME_END;
  }
  return TRUE;
}
".

:- export(cc_call_nf/5).
:- true pred cc_call_nf(in(Name), in(Arity), in(Hooks), go(PrevECC), go(CutTo))
	:: atm * int * int * int * int
	+ (foreign_low(prolog_cc_call_nf), not_fails) -->
"
CBOOL__PROTO(prolog_cc_call_nf)
{
  Unify_constant(ChoiceToInt(w->node),X(4));
  return prolog_cc_call_ncnf(Arg);
}
".


/* for profiler_utils_native */

:- export(dump_node_table_cc/0).
:- true pred dump_node_table_cc + (foreign_low(prolog_dump_node_table_cc))
	--> "
CBOOL__PROTO(prolog_dump_node_table_cc)
{
  if (ht_first(active_frame[1].node_table_cc)) do {
      definition_t *r;
      char buffer[STATICMAXATOM+MAXSTRNUMBERSIZE+1];
      r=*(definition_t **)ht_stuff(active_frame[1].node_table_cc);
      fprintf(Output_Stream_Ptr->streamfile, \"%s\\n\", functor_name(buffer, r));
    } while (ht_next(active_frame[1].node_table_cc));
  return TRUE;
}
".


:- export(have_overhead/2).
:- true pred have_overhead(in(Name), in(Arity)) :: atm * int +
	(foreign_low(prolog_have_overhead)) -->
"
CBOOL__PROTO(prolog_have_overhead)
{
  bool_t result;
  definition_t *f;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  result=functor_have_overhead(active_frame+1,f);
  return result;
}
".

:- export(profile_dump/0).
:- true pred profile_dump + (foreign_low(prolog_profile_dump)) # "Show the
   information collected by the profiler." -->

"
CBOOL__PROTO(prolog_profile_dump)
{
  profile_dump(active_frame+1, Output_Stream_Ptr->streamfile);
  return TRUE;
}

".

:- export(do_profile_reset/0).
:- true pred do_profile_reset + (foreign_low(prolog_profile_reset)) #
"Restart the profiler.  This option erases previously collected
   information." -->

"
CBOOL__PROTO(prolog_profile_reset)
{
  profile_reset(active_frame+1);
  return TRUE;
}
".

:- export(get_trace_active/1).
:- true pred get_trace_active(go(Active)) :: int +
	(foreign_low(prolog_get_trace_active)) #

	"Return 1 if the trace is active, or 0 otherwise." -->

"
CBOOL__PROTO(prolog_get_trace_active)
{
#if defined(PROFILE__TRACER)
  return cunify(Arg,MakeSmall(profile_trace),X(0));
#else
  return cunify(Arg,MakeSmall(0),X(0));
#endif
}
".

:- export(set_trace_active/1).
:- true pred set_trace_active(in(Active)) :: int +
	(foreign_low(prolog_set_trace_active)) #

	"If @var{Active} is 0, turn off the trace, otherwise it is turned on." 
--> 

"
CBOOL__PROTO(prolog_set_trace_active)
{
#if defined(PROFILE__TRACER)
  ERR__FUNCTOR(\"set_trace_active\", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_trace=GetInteger(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}
".


:- export(get_hooks_active/1).
:- true pred get_hooks_active(go(Active)) :: int +
	(foreign_low(prolog_get_hooks_active)) #

	"Return 1 if the hooks are active, or 0 otherwise." -->

"
CBOOL__PROTO(prolog_get_hooks_active)
{
#if defined(PROFILE)
  return cunify(Arg,MakeSmall(profile_hooks),X(0));
#else
  return cunify(Arg,MakeSmall(0),X(0));
#endif
}
".

:- export(set_hooks_active/1).
:- true pred set_hooks_active(in(Active)) :: int +
	(foreign_low(prolog_set_hooks_active)) #

	"If Active is 0, turn off the hooks, otherwise turn them on." -->

"
CBOOL__PROTO(prolog_set_hooks_active)
{
#if defined(PROFILE)
  ERR__FUNCTOR(\"set_hooks_active\", 1);
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_hooks=GetInteger(X(0));
  return TRUE;
#else
  return FALSE;
#endif
}
".

:- export(using_timestamp/1).
:- true pred using_timestamp(go(UsingTimeStamp)) :: int +
	(foreign_low(prolog_using_timestamp)) #

"Unifies @var{UsingTimeStamp} with 1 if the profile is using timestamp
to measure the time, otherwise, unifies it with 0." -->

"
CBOOL__PROTO(prolog_using_timestamp)
{
  int using_timestamp;
#if defined(USE_TIMESTAMP)
  using_timestamp=1;
#else
  using_timestamp=0;
#endif
  return cunify(Arg,MakeSmall(using_timestamp),X(0));
}
".

:- export(total_time/1).
:- true pred total_time(go(TotalTime)) :: num +
	(foreign(total_time), returns(TotalTime)) -->

"
double total_time(void)
{
  return (double)(tick_last_addition - tick_start);
}
".

:- export(cost_center_edge_counts/7).
:- true pred cost_center_edge_counts(Name0, Arity0, Name, Arity, Enter,
	    Leave, Counts) :: atm * int * atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_edge_counts)) -->

"
CBOOL__PROTO(prolog_cost_center_edge_counts)
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), GetInteger(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=GetInteger(X(4));
  leave=GetInteger(X(5));
  return cunify(Arg,MakeSmall(ecc->counts[enter][leave]),X(6));
}
".

:- export(cost_center_edge_ticks/7).
:- true pred cost_center_edge_ticks(Name0, Arity0, Name, Arity, Enter,
	    Leave, Ticks) :: atm * int * atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_edge_ticks)) -->

"
CBOOL__PROTO(prolog_cost_center_edge_ticks)
{
  edge_cc_t *ecc;
  definition_t *f[2];
  int enter, leave;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));

  f[0]=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  f[1]=GET_DEFINITION(GetString(X(2)), GetInteger(X(3)));
  ecc=get_edge_cc(active_frame->edge_table_cc, f);

  enter=GetInteger(X(4));
  leave=GetInteger(X(5));
  return cunify(Arg,MakeFloat(Arg,ecc->times[enter][leave]),X(6));
}
".


:- export(cost_center_node_counts/5).
:- true pred cost_center_node_counts(Name, Arity, Enter,
	    Leave, Counts) :: atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_node_counts)) -->

"
CBOOL__PROTO(prolog_cost_center_node_counts)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  enter=GetInteger(X(2));
  leave=GetInteger(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        counts+=ecc->counts[enter][leave];
      }
    }
  while (ht_next(cct));
  return cunify(Arg,MakeSmall(counts),X(4));
}
".

:- export(cost_center_node_ticks/5).
:- true pred cost_center_node_ticks(Name, Arity, Enter,
	    Leave, Ticks) :: atm* int * int * int * int
	+ (foreign_low(prolog_cost_center_node_ticks)) -->

"
CBOOL__PROTO(prolog_cost_center_node_ticks)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  inttime_t times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  f=GET_DEFINITION(GetString(X(0)), GetInteger(X(1)));
  enter=GetInteger(X(2));
  leave=GetInteger(X(3));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      if (ecc->functor[0]==f) {
        times+=ecc->times[enter][leave];
      }
    }
  while (ht_next(cct));
  return cunify(Arg,MakeFloat(Arg,times),X(4));
}
".

:- export(cost_center_global_counts/3).
:- true pred cost_center_global_counts(Enter, Leave, Counts) :: int * int * int
	+ (foreign_low(prolog_cost_center_global_counts)) -->

"
CBOOL__PROTO(prolog_cost_center_global_counts)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  unsigned long int counts = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=GetInteger(X(0));
  leave=GetInteger(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      counts+=ecc->counts[enter][leave];
    }
  while (ht_next(cct));
  return cunify(Arg,MakeSmall(counts),X(2));
}
".

:- export(cost_center_global_ticks/3).
:- true pred cost_center_global_ticks(Enter, Leave, Counts) :: int * int * int
	+ (foreign_low(prolog_cost_center_global_ticks)) -->

"
CBOOL__PROTO(prolog_cost_center_global_ticks)
{
  edge_cc_t *ecc;
  ht_tab *cct = active_frame->edge_table_cc;
  definition_t *f;
  int enter, leave;
  inttime_t times = 0;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  enter=GetInteger(X(0));
  leave=GetInteger(X(1));
  
  if (ht_first(cct)) do {
      ecc=(edge_cc_t *)ht_stuff(cct);
      times+=ecc->times[enter][leave];
    }
  while (ht_next(cct));
  return cunify(Arg,MakeFloat(Arg,times),X(2));
}
".


/* for  profiler_utils_base.pl */

:- foreign_inline("

static void profile_enter(int entry_port, definition_t *functor) {
  definition_t *f[2]={NULL, NULL};
  inttime_t d;
  if (active_frame<prof_frames) {
    tick_start+=tick_ini_profiling-tick_last_addition;
    stop_on_pred_calls=TRUE;
    profile=TRUE;
    d=tick_ini_profiling-tick_profiling;
  } else {
#if defined(PROFILE)
    if (profile_hooks && active_ecc->hooks) {
      d=tick_last_addition-tick_profiling;
      tick_profiling+=tick_ini_profiling-tick_last_addition;
    } else
#endif
      d=tick_ini_profiling-tick_profiling;
  }
  active_frame++;
  active_ecc=get_edge_cc(active_frame->edge_table_cc, f);
  active_ecc->entry_port=ENTER_CALL;
#if defined(PROFILE)
  active_ecc->hooks=profile_rcc; /* Don't accumulate costs in rcc */
  if (profile_hooks) {
    if (active_ecc->hooks)
      ENABLE_HOOKS(functor)
    else
      DISABLE_HOOKS;
  }
#endif
  tick_last_addition=tick_ini_profiling;
  active_ecc->entry_time=d;
}

static void profile_leave(int output_port) {
  inttime_t d;
#if defined(PROFILE)
  if (profile_hooks && active_ecc->hooks) {
    d=tick_last_addition-tick_profiling;
    tick_profiling+=tick_ini_profiling-tick_last_addition;
  } else
#endif
    d=tick_ini_profiling-tick_profiling;
  active_ecc->counts[active_ecc->entry_port][output_port]++;
  active_ecc->times[active_ecc->entry_port][output_port]+=d-active_ecc->entry_time;
  active_ecc->entry_time=0;
  active_ecc=NULL;
#if defined(PROFILE)
  if (profile_hooks) {
    DISABLE_HOOKS;
  }
#endif
  if (active_frame==prof_frames) {
    stop_on_pred_calls=trace_calls;
    profile=FALSE;
  }
  tick_last_addition=tick_ini_profiling;
  active_frame--;
}
").

:- export(profile_enter_call/0).
:- true pred profile_enter_call
	+ (foreign_low(prolog_profile_enter_call), not_fails)
#
	"Turn on the profiler." -->

"
void profile_enter_call_(void)
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_CALL, profile_enter_call);
  PROFILE__TIME_END;
}

CBOOL__PROTO(prolog_profile_enter_call)
{
  profile_enter_call_();
  return TRUE;
}
".

:- export(profile_enter_redo_1/0).
:- true pred profile_enter_redo_1
	+ (foreign_low(prolog_profile_enter_redo_1), fails)
#
	"Turn on the profiler." -->
"
CBOOL__PROTO(prolog_profile_enter_redo_1)
{
  PROFILE__TIME_INI;
  profile_enter(ENTER_REDO, profile_enter_redo_1);
  PROFILE__TIME_END;
  return FALSE;
}
".

:- export(profile_leave_exit/0).
:- true pred profile_leave_exit
	+ (foreign_low(prolog_profile_leave_exit), not_fails)
# "Turn off the profiler." -->
"
void profile_leave_exit_(void)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
}

CBOOL__PROTO(prolog_profile_leave_exit)
{
  profile_leave_exit_();
  return TRUE;
}
".

:- export(profile_leave_fail_1/0).
:- true pred profile_leave_fail_1
	+ (foreign_low(prolog_profile_leave_fail_1), fails)
# "Turn off the profiler." -->
"
CBOOL__PROTO(prolog_profile_leave_fail_1)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_FAIL);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return FALSE;
}
".

:- export(profile_leave_error/0).
:- true pred profile_leave_error + (foreign_low(prolog_profile_leave_error)) #
	"Turn off the profiler." -->

"
CBOOL__PROTO(prolog_profile_leave_error)
{
  PROFILE__TIME_INI;
  profile_leave(LEAVE_EXIT);
  PROFILE__TIME_END;
  PROFILE__TIME_FLUSH;
  return TRUE;
}
".
