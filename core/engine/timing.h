/*
 *  timing.h
 *
 *  Metering primitives.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_TIMING_H
#define _CIAO_TIMING_H

void init_timing(void);
void reset_timing(void);

flt64_t usertime(void);
flt64_t walltime(void);

inttime_t usertick(void);
inttime_t systemtick(void);
inttime_t walltick(void);
/* By default, use usertick() */
#define RunTickFunc usertick
#define RunClockFreq(STATS) (STATS.userclockfreq)

#endif /* _CIAO_TIMING_H */
