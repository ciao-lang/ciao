/*
 *  timing.c
 *
 *  Metering primitives.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <string.h>

#include <sys/types.h>
#include <time.h>

#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/timing.h>

#if (defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(Win32) || defined(BSD)) \
    && !defined(_WIN32) && !defined(_WIN64)

#include <sys/time.h>
#include <sys/resource.h>

inttime_t internal_usertick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((inttime_t)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

inttime_t internal_systemtick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((inttime_t)rusage.ru_stime.tv_sec) * 1000000 + rusage.ru_stime.tv_usec;
}

static void init_frequency_info(void)
{
  ciao_statistics.userclockfreq = 1000000;
  ciao_statistics.systemclockfreq = 1000000;
}

#elif defined(_WIN32) || defined(_WIN64)

#if !defined(HZ)
#define HZ 1000
#endif

struct tms {
    u_int tms_utime;
    u_int tms_stime;
    u_int tms_cutime;
    u_int tms_cstime;
};

/* Own definition for times() */
clock_t times(struct tms *info)
{
    HANDLE process = GetCurrentProcess();
    FILETIME ctime, xtime, utime, stime;
    int64_t val;
    const int factor = 10000000/CLK_TCK;
    const int bias   = factor/2;

    if (!GetProcessTimes(process, &ctime, &xtime, &stime, &utime)) {
      /* TODO: handle error? */
      info->tms_stime = 0;
      info->tms_utime = 0;
      info->tms_cstime = 0;
      info->tms_cutime = 0;
      return 0;
    }
    val = ((int64_t)stime.dwHighDateTime << 32) + stime.dwLowDateTime;
    info->tms_stime = (u_int)((val+bias) / factor);
    val = ((int64_t)utime.dwHighDateTime << 32) + utime.dwLowDateTime;
    info->tms_utime = (u_int)((val+bias) / factor);

    info->tms_cstime = 0;
    info->tms_cutime = 0;
    return 0;
}

inttime_t internal_usertick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_utime;
}

inttime_t internal_systemtick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void)
{
  ciao_statistics.userclockfreq = HZ;
  ciao_statistics.systemclockfreq = HZ;
}

#else

#include <sys/times.h>
#include <sys/param.h>

inttime_t internal_usertick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_utime;
}

inttime_t internal_systemtick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void)
{
  ciao_statistics.userclockfreq = HZ;
  ciao_statistics.systemclockfreq = HZ;
}
#endif

/* usertick is defined as a pointer to a function to let the
   redefinition on the fly for a better timing measurement function */

inttime_t (*usertick)(void) = internal_usertick_std;
inttime_t (*systemtick)(void) = internal_systemtick_std;

flt64_t usertime(void)
{
  return ((flt64_t)usertick()) / ciao_statistics.userclockfreq;
}

CBOOL__PROTO(prolog_time)
{
  
  time_t timeofday = time(NULL);

  return cunify(Arg,MakeInteger(Arg,timeofday),X(0));
}

#if defined(_WIN32) || defined(_WIN64)
/*
 * The unit of FILETIME is 100-nanoseconds since January 1, 1601, UTC.
 * Returns the 100-nanoseconds ("hekto nanoseconds") since the epoch.
 */
static inline long long filetime_to_hnsec(const FILETIME *ft)
{
  long long winTime = ((long long)ft->dwHighDateTime << 32) + ft->dwLowDateTime;
  /* Windows to Unix Epoch conversion */
  return winTime - 116444736000000000LL;
}

static inline time_t filetime_to_time_t(const FILETIME *ft)
{
  return (time_t)(filetime_to_hnsec(ft) / 10000000);
}

int gettimeofday(struct timeval *tv, void *tz)
{
  FILETIME ft;
  long long hnsec;

  GetSystemTimeAsFileTime(&ft);
  hnsec = filetime_to_hnsec(&ft);
  tv->tv_sec = hnsec / 10000000;
  tv->tv_usec = (hnsec % 10000000) / 10;
  return 0;
}
#endif

/* walltime(?Time): unifies Time with the time in milliseconds elapsed since
  the last call to walltime/1 . The first call returns walltime since the
  start of the execution.  */

/* Shared but locked?  Initialized in init_once() */

inttime_t internal_walltick_std(void)
{
  struct timeval tp;
  gettimeofday(&tp, 0L);
  return (inttime_t)tp.tv_sec*1000000 + ((inttime_t)tp.tv_usec);
}

inttime_t (*walltick)(void) = internal_walltick_std;

void init_statistics(void) {
  init_frequency_info();
  ciao_statistics.wallclockfreq = 1000000;
}

/*
  This function returns the walltime in milliseconds
*/

flt64_t walltime(void)
{
  return ((flt64_t)walltick() * 1000) / ciao_statistics.wallclockfreq;
}

/*
  This function has been modified by Edison Mera to prevents the
  truncation of the microseconds.  Very important in modern
  plattforms where the speed is given in GHz.
 */
static CBOOL__PROTO(generic_time,
		    inttime_t (*time_function)(void),
		    inttime_t starttick,
		    inttime_t *lasttick,
		    inttime_t clockfreq)
{
  /*int st,lt; */
  inttime_t st,lt;
  inttime_t t;
  tagged_t x;
  
  t = time_function();
  st = t - starttick;
  lt = t - *lasttick;
  *lasttick = t;
  /* while ciao not support inttime_t, lt and st must be cast to
    flt64_t */
  MakeLST(x,MakeFloat(Arg,(((flt64_t)lt)*1000)/clockfreq),atom_nil);
  MakeLST(x,MakeFloat(Arg,(((flt64_t)st)*1000)/clockfreq),x);
  return cunify(Arg,x,X(0));
}

/* runtime returns a list of two floats
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the second since the last call to runtime */
CBOOL__PROTO(prolog_runtime)
{
  return generic_time(Arg, 
                      TICK_FUNCTION,
                      ciao_statistics.starttick, 
                      &ciao_statistics.lasttick,
                      GET_CLOCKFREQ(ciao_statistics));
}

CBOOL__PROTO(prolog_usertime)
{
  return generic_time(Arg, 
                      usertick,
                      ciao_statistics.startusertick,
                      &ciao_statistics.lastusertick,
                      ciao_statistics.userclockfreq);
}

CBOOL__PROTO(prolog_systemtime)
{
  return generic_time(Arg, 
                      systemtick, 
                      ciao_statistics.startsystemtick,
                      &ciao_statistics.lastsystemtick,
                      ciao_statistics.systemclockfreq);
}

CBOOL__PROTO(prolog_walltime)
{
  return generic_time(Arg, 
                      walltick, 
                      ciao_statistics.startwalltick, 
                      &ciao_statistics.lastwalltick,
                      ciao_statistics.wallclockfreq);
}

/* New time medition functions */
static CBOOL__PROTO(generic_tick,
		    inttime_t (*time_function)(void),
		    inttime_t starttick,
		    inttime_t *lasttick)
{
  /*int st,lt; */
  inttime_t st,lt;
  inttime_t t;
  tagged_t x;
  
  t = time_function();
  st = t - starttick;
  lt = t - *lasttick;
  *lasttick = t;
  /* while ciao does not support inttime_t, lt and st must be cast to
    flt64_t */
  MakeLST(x,MakeFloat(Arg,(flt64_t)lt),atom_nil);
  MakeLST(x,MakeFloat(Arg,(flt64_t)st),x);
  return cunify(Arg,x,X(0));  
}

CBOOL__PROTO(prolog_walltick)
{
  return generic_tick(Arg, 
                       walltick, 
                       ciao_statistics.startwalltick,
                       &ciao_statistics.lastwalltick);
}

CBOOL__PROTO(prolog_usertick)
{
  return generic_tick(Arg, 
                       usertick, 
                       ciao_statistics.startusertick,
                       &ciao_statistics.lastusertick);
}

CBOOL__PROTO(prolog_systemtick)
{
  return generic_tick(Arg, 
                       systemtick, 
                       ciao_statistics.startsystemtick,
                       &ciao_statistics.lastsystemtick);
}

CBOOL__PROTO(prolog_runtick)
{
  return generic_tick(Arg, 
                       TICK_FUNCTION,
                       ciao_statistics.starttick,
                       &ciao_statistics.lasttick);
}

/* New time medition functions */
inline static CBOOL__PROTO(generic_clockfreq, inttime_t clockfreq)
{
  /* while ciao not support inttime_t, return value must be cast to
    flt64_t */
  return cunify(Arg,MakeFloat(Arg,(flt64_t)clockfreq),X(0));
}

CBOOL__PROTO(prolog_runclockfreq)
{
  return generic_clockfreq(Arg, GET_CLOCKFREQ(ciao_statistics));
}

CBOOL__PROTO(prolog_userclockfreq)
{
  return generic_clockfreq(Arg, ciao_statistics.userclockfreq);
}

CBOOL__PROTO(prolog_systemclockfreq)
{
  return generic_clockfreq(Arg, ciao_statistics.systemclockfreq);
}

CBOOL__PROTO(prolog_wallclockfreq)
{
  return generic_clockfreq(Arg, ciao_statistics.wallclockfreq);
}

void reset_statistics(void)
{
  ciao_statistics.startusertick = usertick();
  ciao_statistics.lastusertick = ciao_statistics.startusertick;
  ciao_statistics.startwalltick = walltick();
  ciao_statistics.lastwalltick = ciao_statistics.startwalltick;
  ciao_statistics.startsystemtick = systemtick();
  ciao_statistics.lastsystemtick = ciao_statistics.startsystemtick;
  ciao_statistics.lasttick = 
    ciao_statistics.starttick = 
    ciao_statistics.startusertick;
}

/* datime(+Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */
/* datime(-Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */
/* datime(-Time,+Year,+Month,+Day,+Hour,+Min,+Sec,-WeekDay,-YearDay) */

CBOOL__PROTO(prolog_datime)
{
  ERR__FUNCTOR("system:datime", 9);
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));
  DEREF(X(6),X(6));
  
  if (IsInteger(X(1))
      && IsInteger(X(2))
      && IsInteger(X(3))
      && IsInteger(X(4))
      && IsInteger(X(5))
      && IsInteger(X(6))) {
    struct tm datime[1];
    time_t inputtime;
    datime->tm_year=GetInteger(X(1))-1900;
    datime->tm_mon =GetInteger(X(2))-1;
    datime->tm_mday=GetInteger(X(3));
    datime->tm_hour=GetInteger(X(4));
    datime->tm_min =GetInteger(X(5));
    datime->tm_sec =GetInteger(X(6));
    inputtime = mktime(datime);
    return(cunify(Arg,MakeInteger(Arg,inputtime),X(0))
	   && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
	   && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));
  } else {
    struct tm *datime;
    time_t inputtime;
    if (IsVar(X(0))) {
      inputtime = time(NULL);
      cunify(Arg,MakeInteger(Arg,inputtime),X(0));
    } else if (IsInteger(X(0))) {
      inputtime = GetInteger(X(0));
    } else {
      BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
    }
    
    datime = localtime(&inputtime);
    
    return(cunify(Arg,MakeSmall((datime->tm_year)+1900),X(1))
	   && cunify(Arg,MakeSmall((datime->tm_mon)+1), X(2))
	   && cunify(Arg,MakeSmall(datime->tm_mday),X(3))
	   && cunify(Arg,MakeSmall(datime->tm_hour),X(4))
	   && cunify(Arg,MakeSmall(datime->tm_min), X(5))
	   && cunify(Arg,MakeSmall(datime->tm_sec), X(6))
	   && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
	   && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));
  }
}

#if defined(ANDPARALLEL) && defined(VISANDOR) && !defined(USCLK_EXISTS) && !defined(NSCLK_EXISTS)
/*
  We realized that perhaps the overhead caused by the call to
  usertime() could be too high to give us accurate timing. So we try to
  compensate it by counting the number of times called so far. 
  The initial compensate time is adjusted for an SUN SPARC.
  Things to do: a built-in predicate to return the compensating time.
*/

uintmach_t count_calls = 0; /* MCL */
float time_each_call = 0.000053;   /* For a SUN IPC */

/* usertime() returns seconds */

float usertime_visandor(void) {
  float time;

#if defined(Solaris)
  time = ((float)gethrtime()/1.0e9);
#else
  struct timeval current_time;

  gettimeofday(&current_time, NULL);
  time = current_time.tv_sec + current_time.tv_usec / 1e6;
#endif

  if (gen_event_file)
    time -= ++count_calls * time_each_call;

  return time;
}
#endif

