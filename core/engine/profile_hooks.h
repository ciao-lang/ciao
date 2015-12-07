/*
 *  profile_hooks.h
 *
 *  Hooks for profiling of user programs.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_PROFILE_HOOKS_H
#define _CIAO_PROFILE_HOOKS_H

extern bool_t profile, profile_eng, profile_rcc;

#if defined(PROFILE)

/* Uncomment this line to use the profiler as a tracer */
/* #define PROFILE__TRACER */

#define CALL_STACK_INITIAL_SIZE 16384

CVOID__PROTO(profile__hook_noop);
CVOID__PROTO(profile__hook_call_noop, definition_t *f);

/* TODO: add typedefs */
extern void (*profile__hook_redo)(worker_t *w);
extern void (*profile__hook_cut)(worker_t *w);
extern void (*profile__hook_call)(worker_t *w, definition_t *functor);

#define PROFILE__HOOK_REDO                 {profile__hook_redo(w);}
#define PROFILE__HOOK_CUT                  {profile__hook_cut(w);}
#define PROFILE__HOOK_CALL(w,functor)      {profile__hook_call(w, functor);}
#define PROFILE__HOOK_METACUT              PROFILE__HOOK_CUT
#define PROFILE__HOOK_CIAOCUT              PROFILE__HOOK_CUT

# if defined(PROFILE__TRACER)
/* TODO: add typedefs */
extern void (*profile__hook_fail)(worker_t *w);
extern void (*profile__hook_proceed)(worker_t *w);
extern void (*profile__hook_neck_proceed)(worker_t *w);
#define PROFILE__HOOK_FAIL                 {profile__hook_fail(w);}
#define PROFILE__HOOK_PROCEED              {profile__hook_proceed(w);}
#define PROFILE__HOOK_NECK_PROCEED         {profile__hook_neck_proceed(w);}
# else
#define PROFILE__HOOK_FAIL
#define PROFILE__HOOK_PROCEED
#define PROFILE__HOOK_NECK_PROCEED
# endif

#else

#define PROFILE__HOOK_FAIL
#define PROFILE__HOOK_REDO
#define PROFILE__HOOK_CALL(w,functor)
#define PROFILE__HOOK_CUT
#define PROFILE__HOOK_METACUT
#define PROFILE__HOOK_CIAOCUT
#define PROFILE__HOOK_PROCEED
#define PROFILE__HOOK_NECK_PROCEED

#endif /* PROFILE */

#endif /* _CIAO_PROFILE_HOOKS_H */
