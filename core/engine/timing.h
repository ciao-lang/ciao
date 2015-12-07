/*
 *  timing.h
 *
 *  Metering primitives.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_TIMING_H
#define _CIAO_TIMING_H

/* This is defined here to let it can be changed to another functions */

# define TICK_FUNCTION usertick
# define GET_CLOCKFREQ(X) (X.userclockfreq)
# define BASE_RUNTICK (TICK_FUNCTION())

flt64_t usertime(void);
flt64_t walltime(void);
void init_statistics(void);
void reset_statistics(void);

extern inttime_t (*usertick)(void);
extern inttime_t (*systemtick)(void);
extern inttime_t (*walltick)(void);

CBOOL__PROTO(prolog_runtime);
CBOOL__PROTO(prolog_usertime);
CBOOL__PROTO(prolog_systemtime);
CBOOL__PROTO(prolog_walltime);

CBOOL__PROTO(prolog_time);
CBOOL__PROTO(prolog_datime);

CBOOL__PROTO(prolog_walltick);
CBOOL__PROTO(prolog_usertick);
CBOOL__PROTO(prolog_systemtick);
CBOOL__PROTO(prolog_runtick);

CBOOL__PROTO(prolog_wallclockfreq);
CBOOL__PROTO(prolog_userclockfreq);
CBOOL__PROTO(prolog_systemclockfreq);
CBOOL__PROTO(prolog_runclockfreq);

#endif /* _CIAO_TIMING_H */
