/*
 *  timing.c
 *
 *  Metering primitives.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <string.h>
#include <sys/types.h>
#include <time.h>

#include <ciao/eng.h>
#include <ciao/timing.h>

#if (defined(Solaris)||defined(LINUX)||defined(EMSCRIPTEN)||defined(DARWIN)||defined(Win32)||defined(BSD)) \
    && !defined(_WIN32) && !defined(_WIN64)

#include <sys/time.h>
#include <sys/resource.h>

inttime_t usertick(void) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((inttime_t)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

inttime_t systemtick(void) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((inttime_t)rusage.ru_stime.tv_sec) * 1000000 + rusage.ru_stime.tv_usec;
}

static void init_frequency_info(void) {
  ciao_stats.userclockfreq = 1000000;
  ciao_stats.systemclockfreq = 1000000;
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
clock_t times(struct tms *info) {
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

inttime_t usertick(void) {
  struct tms buffer;
  times(&buffer);
  return buffer.tms_utime;
}

inttime_t systemtick(void) {
  struct tms buffer;
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void) {
  ciao_stats.userclockfreq = HZ;
  ciao_stats.systemclockfreq = HZ;
}

#else

#include <sys/times.h>
#include <sys/param.h>

inttime_t usertick(void) {
  struct tms buffer;
  times(&buffer);
  return buffer.tms_utime;
}

inttime_t systemtick(void) {
  struct tms buffer;
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void) {
  ciao_stats.userclockfreq = HZ;
  ciao_stats.systemclockfreq = HZ;
}
#endif

flt64_t usertime(void) {
  return ((flt64_t)usertick()) / ciao_stats.userclockfreq;
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

/* Shared but locked?  Initialized in engine_init() */

inttime_t walltick(void) {
  struct timeval tp;
  gettimeofday(&tp, 0L);
  return (inttime_t)tp.tv_sec*1000000 + ((inttime_t)tp.tv_usec);
}

void init_timing(void) {
  init_frequency_info();
  ciao_stats.wallclockfreq = 1000000;
}

/* Walltime in milliseconds */
flt64_t walltime(void) {
  return ((flt64_t)walltick() * 1000) / ciao_stats.wallclockfreq;
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
                    inttime_t clockfreq) {
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
  tagged_t k,j;
  k=BoxFloat(((flt64_t)lt)*1000/clockfreq);
  j=BoxFloat(((flt64_t)st)*1000/clockfreq);
  MakeLST(x,k,atom_nil);
  MakeLST(x,j,x);
  CBOOL__LASTUNIFY(x,X(0));
}

/* runtime returns a list of two floats
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the second since the last call to runtime */

CBOOL__PROTO(prolog_walltime) {
  CBOOL__LASTCALL(generic_time, walltick, ciao_stats.startwalltick, &ciao_stats.lastwalltick, ciao_stats.wallclockfreq);
}
#if defined(EMSCRIPTEN)
/* getrusage not implemented, rely on gettimeofday */
CBOOL__PROTO(prolog_runtime) { CBOOL__LASTCALL(prolog_walltime); }
CBOOL__PROTO(prolog_usertime) { CBOOL__LASTCALL(prolog_walltime); }
CBOOL__PROTO(prolog_systemtime) { CBOOL__LASTCALL(prolog_walltime); }
#else 
CBOOL__PROTO(prolog_runtime) {
  CBOOL__LASTCALL(generic_time, RunTickFunc, ciao_stats.starttick, &ciao_stats.lasttick, RunClockFreq(ciao_stats));
}
CBOOL__PROTO(prolog_usertime) {
  CBOOL__LASTCALL(generic_time, usertick, ciao_stats.startusertick, &ciao_stats.lastusertick, ciao_stats.userclockfreq);
}
CBOOL__PROTO(prolog_systemtime) {
  CBOOL__LASTCALL(generic_time, systemtick, ciao_stats.startsystemtick, &ciao_stats.lastsystemtick, ciao_stats.systemclockfreq);
}
#endif

/* New time medition functions */
static CBOOL__PROTO(generic_tick,
                    inttime_t (*time_function)(void),
                    inttime_t starttick,
                    inttime_t *lasttick) {
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
  tagged_t k,j;
  k = BoxFloat((flt64_t)lt);
  j = BoxFloat((flt64_t)st);
  MakeLST(x,k,atom_nil);
  MakeLST(x,j,x);
  CBOOL__LASTUNIFY(x,X(0));  
}

CBOOL__PROTO(prolog_walltick) {
  CBOOL__LASTCALL(generic_tick, walltick, ciao_stats.startwalltick, &ciao_stats.lastwalltick);
}

CBOOL__PROTO(prolog_usertick) {
  CBOOL__LASTCALL(generic_tick, usertick, ciao_stats.startusertick, &ciao_stats.lastusertick);
}

CBOOL__PROTO(prolog_systemtick) {
  CBOOL__LASTCALL(generic_tick, systemtick, ciao_stats.startsystemtick, &ciao_stats.lastsystemtick);
}

CBOOL__PROTO(prolog_runtick) {
  CBOOL__LASTCALL(generic_tick, RunTickFunc, ciao_stats.starttick, &ciao_stats.lasttick);
}

/* New time medition functions */
/* while ciao not support inttime_t, return value must be cast to
   flt64_t */
static inline CBOOL__PROTO(generic_clockfreq, inttime_t clockfreq) {
  CBOOL__LASTUNIFY(BoxFloat((flt64_t)clockfreq),X(0));
}

CBOOL__PROTO(prolog_runclockfreq) {
  CBOOL__LASTCALL(generic_clockfreq, RunClockFreq(ciao_stats));
}

CBOOL__PROTO(prolog_userclockfreq) {
  CBOOL__LASTCALL(generic_clockfreq, ciao_stats.userclockfreq);
}

CBOOL__PROTO(prolog_systemclockfreq) {
  CBOOL__LASTCALL(generic_clockfreq, ciao_stats.systemclockfreq);
}

CBOOL__PROTO(prolog_wallclockfreq) {
  CBOOL__LASTCALL(generic_clockfreq, ciao_stats.wallclockfreq);
}

void reset_timing(void) {
  ciao_stats.startusertick = usertick();
  ciao_stats.lastusertick = ciao_stats.startusertick;
  ciao_stats.startwalltick = walltick();
  ciao_stats.lastwalltick = ciao_stats.startwalltick;
  ciao_stats.startsystemtick = systemtick();
  ciao_stats.lastsystemtick = ciao_stats.startsystemtick;
  ciao_stats.lasttick = 
    ciao_stats.starttick = 
    ciao_stats.startusertick;
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

