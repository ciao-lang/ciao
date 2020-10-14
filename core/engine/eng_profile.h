/*
 *  eng_profile.h
 *
 *  Profiler support
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_PROFILE_H
#define _CIAO_ENG_PROFILE_H

#if defined(OPTIM_COMP) && defined(ABSMACH_OPT__profile_calls)
extern bool_t profile;
extern bool_t prof_include_time;

void dump_profile(void);
void add_to_profiling(definition_t *functor);

bool_t profile__get_opt(const char *arg);
#endif

#if !defined(OPTIM_COMP)

extern bool_t profile;
extern bool_t profile_eng;
extern bool_t profile_rcc;

/* Uncomment this line to use the profiler as a tracer */
/* #define PROFILE__TRACER 1 */

#if defined(PROFILE)

CVOID__PROTO(profile__hook_nop);
CVOID__PROTO(profile__hook_call_nop, definition_t *f);

/* TODO: create a struct of function pointers (JF) */
extern CVOID__PROTO((*profile__hook_redo));
extern CVOID__PROTO((*profile__hook_cut));
extern CVOID__PROTO((*profile__hook_call), definition_t *functor);
#if defined(PROFILE__TRACER)
extern CVOID__PROTO((*profile__hook_fail));
extern CVOID__PROTO((*profile__hook_proceed));
extern CVOID__PROTO((*profile__hook_neck_proceed));
#endif

#define PROFILE__HOOK_REDO          CVOID__CALL(profile__hook_redo)
#define PROFILE__HOOK_CUT           CVOID__CALL(profile__hook_cut)
#define PROFILE__HOOK_CALL(FUNCTOR) CVOID__CALL(profile__hook_call, (FUNCTOR))
#if defined(PROFILE__TRACER)
#define PROFILE__HOOK_FAIL          CVOID__CALL(profile__hook_fail)
#define PROFILE__HOOK_PROCEED       CVOID__CALL(profile__hook_proceed)
#define PROFILE__HOOK_NECK_PROCEED  CVOID__CALL(profile__hook_neck_proceed)
#else
#define PROFILE__HOOK_FAIL
#define PROFILE__HOOK_PROCEED
#define PROFILE__HOOK_NECK_PROCEED
#endif

#else

#define PROFILE__HOOK_REDO
#define PROFILE__HOOK_CUT
#define PROFILE__HOOK_CALL(FUNCTOR)
#define PROFILE__HOOK_FAIL
#define PROFILE__HOOK_PROCEED
#define PROFILE__HOOK_NECK_PROCEED

#endif /* PROFILE */

#endif /* !defined(OPTIM_COMP) */

#endif /* _CIAO_ENG_PROFILE_H */
