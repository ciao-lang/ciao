#include <ciao/eng.h>
#if !defined(OPTIM_COMP) && defined(ABSMACH_OPT__profilecc)
#define __USE_GNU
# include <dlfcn.h>
#endif

/* --------------------------------------------------------------------------- */
/* Get profiling options */

#if defined(ABSMACH_OPT__profilecc) || defined(ABSMACH_OPT__profile_calls)
#include <string.h>

bool_t profile = FALSE; /* enable profiler hooks in wamloop -- Shared */
#if defined(ABSMACH_OPT__profile_calls)
bool_t profile__roughtime = FALSE; /* include (rough) time in profile -- Shared */
#endif

/* (done for each option) */
bool_t profile__get_opt(const char *arg) {
  if (strcmp(arg, "--profile-ncalls") == 0) { /* Simple profile */
    profile = TRUE;
    return TRUE;
  } else if (strcmp(arg, "--profile-roughtime") == 0) { /* Include time */
    profile = TRUE;
#if defined(ABSMACH_OPT__profile_calls)
    profile__roughtime = TRUE;
#endif
    return TRUE;
  }
  return FALSE;
}
#endif

/* --------------------------------------------------------------------------- */
/* profilecc */

#if defined(ABSMACH_OPT__profilecc)
bool_t profile_eng            = FALSE;
bool_t profile_rcc            = FALSE;

CVOID__PROTO(profile__hook_nop) {};
CVOID__PROTO(profile__hook_call_nop, definition_t *f) {};

/* TODO: add macro to define typedefs from prototypes, use ## */
CVOID__PROTO((*profile__hook_fail)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_redo)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_cut)) = profile__hook_nop;
CVOID__PROTO((*profile__hook_call), definition_t *f) = profile__hook_call_nop;
#if defined(PROFILE__TRACER)
CVOID__PROTO((*profile__hook_proceed)) = profile__hook_nop;
#endif

void init_profilecc(void) {
  if (profile) { /* simulate with PROFILECC */
    profile_eng = TRUE; /* annotate it */
    profile = FALSE; /* disable profiling hooks, enabled later */
    void (*profile_init)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile_init");
    void (*profile_enter_call_)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile_enter_call_");
    profile_rcc = TRUE;
    if (profile_init) profile_init();
    if (profile_enter_call_) profile_enter_call_();
  }
}

CVOID__PROTO(finish_profilecc) {
  if (profile_eng) {
    void (*profile__stop_exit_)(void) = (void (*)(void))dlsym(RTLD_DEFAULT, "profile__stop_exit");
    void (*profile_dump_)(FILE *) = (void (*)(FILE *))dlsym(RTLD_DEFAULT, "profile_dump");
    if (profile__stop_exit_) profile__stop_exit_();
    if (profile_dump_) profile_dump_(stdout);
  }
}
#endif

/* --------------------------------------------------------------------------- */

#if defined(ABSMACH_OPT__profilecc) || defined(ABSMACH_OPT__profile_calls)
#if defined(OPTIM_COMP)
const char *predicate_type(int t) {
  switch (t) {
  case ENTER_COMPACTCODE: return "Emul  " ;
  case ENTER_UNDEFINED: return "Undef " ;
  case ENTER_CBOOL: return "Cbool " ;
  case ENTER_CINSNP: return "Cinsnp" ;
  case ENTER_CVOID: return "Cvoid" ;
  case ENTER_INTERPRETED: return "Interp" ;
  default: return "Other " ;
  }
}
#else
const char *predicate_type(int t) {
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
#endif
#endif

/* --------------------------------------------------------------------------- */
/* naive profile_calls */

#if defined(ABSMACH_OPT__profile_calls)

#include <stdlib.h>
#include <math.h>

static intmach_t compare_times(const void *arg1, const void *arg2);

flt64_t time_last_addition;
definition_t *last_called_predicate = NULL;

void add_to_profiling(definition_t *functor) {
  flt64_t time_now;

  functor->number_of_calls++;

  if (profile__roughtime) {
    time_now = usertime();
    if (last_called_predicate) {
      last_called_predicate->time_spent += (uintmach_t)((time_now - time_last_addition)*1e6);
    }
    time_last_addition = time_now;
    last_called_predicate = functor;
  }
}

void dump_profile(void) {
  hashtab_t *table;
  hashtab_node_t *keyval;
  intmach_t j;
  intmach_t realsize;
  intmach_t tot_reductions;
  definition_t **pred_table, *d;
  uintmach_t total_time = 1;  /* microsecs; avoid dividing by zero */
  FILE *out;
  const char *out_filename = "/tmp/ciao__profile.txt";

  out = fopen(out_filename, "w");
  if (out == NULL) {
    fprintf(stderr, "{error: cannot open profile file %s}\n", out_filename);
    abort();
  }

  fprintf(out, "\n\nProfile information:\n");

  table = predicates_location;
  
  realsize = 0;
  tot_reductions = 0;
  j = HASHTAB_SIZE(predicates_location);
  for (--j; j>=0; --j) {           /* Find how many preds. we have called */
    keyval = &table->node[j];
    d = (definition_t *)keyval->value.as_ptr;
    if (d != NULL &&
        d -> predtyp != ENTER_UNDEFINED &&
        d->number_of_calls) {
      realsize++;
      tot_reductions += d->number_of_calls;
      total_time += d->time_spent;
    }
  }

  fprintf(out, "%d predicates called, %d calls made, %.2f secs. accumulated time\n", 
          realsize, 
          tot_reductions,
          (flt64_t)total_time/1e6);
  /* Make a table with room for them */
  pred_table = checkalloc_ARRAY(definition_t *, realsize);

  realsize = 0;
  j = HASHTAB_SIZE(predicates_location);
  for (--j; j>=0; --j) {
    keyval = &table->node[j];
    d = (definition_t *)keyval->value.as_ptr;
    if (d != NULL &&
        d -> predtyp != ENTER_UNDEFINED &&
        d->number_of_calls) {
      pred_table[realsize++] = d;
    }
  }

  /* Sort the table */
  qsort(pred_table, realsize, sizeof(definition_t *), compare_times);

  /* Print the table */
  fprintf(out, "Calls \t\t Time (rough) \t\t Type    Spec\n");
  fprintf(out, "===== \t\t ============ \t\t ====    ====\n");

  for (j = realsize - 1; j >= 0; j--) {
    d = pred_table[j];
    fprintf(out, "%ld (%.2f%%) \t %f (%.2f%%) \t %s  %s/%d\n",
            (long)d->number_of_calls,
            (flt64_t)(d->number_of_calls)*100.0/(flt64_t)tot_reductions,
            ((flt64_t)d->time_spent)/1.0e6,
            (flt64_t)(d->time_spent)*100.0/(flt64_t)total_time,
            predicate_type(d->predtyp),
            GetString(FuncName(d)),
            (int)FuncArity(d));
  }

  checkdealloc_ARRAY(definition_t *, realsize, pred_table);

  fclose(out);
  TRACE_PRINTF("{profile: dump saved in %s}\n", out_filename);
}

static intmach_t compare_times(const void *arg1, const void *arg2) {
  definition_t *pred1, *pred2;
  
  pred1 = *(definition_t **)arg1;
  pred2 = *(definition_t **)arg2;
  
  if (pred1->number_of_calls > pred2->number_of_calls) {
    return 1;
  } else if (pred1->number_of_calls < pred2->number_of_calls) {
    return -1;
  } else {
    return 0;
  }
}

#endif
