#ifndef _CIAO_HRTIME_H
#define _CIAO_HRTIME_H

/* #define SERIALIZE_WITH_CPUID */

#if defined(__BORLANDC__)
typedef unsigned __int64 uint64;
typedef __int64 int64;
#else
typedef unsigned long long uint64;
typedef long long int64;
#endif

typedef unsigned long uint32;
typedef long int32;

#if defined(__i386__) || defined(_M_IX86)

# if defined(__BORLANDC__) || defined(_MSC_VER)

# define HRTIME_METHOD "rdtsc_func"

extern uint64 __cdecl hrtime(void);

# else

# define HRTIME_METHOD "rdtsc"

#  if defined(SERIALIZE_WITH_CPUID)
#   define hrtime()					\
  ({							\
    register uint32 x, y;				\
    __asm__ __volatile__("pushl %eax");			\
    __asm__ __volatile__("pushl %ebx");			\
    __asm__ __volatile__("pushl %edx");			\
    __asm__ __volatile__("pushl %ecx");			\
    __asm__ __volatile__("xorl %eax, %eax");		\
    __asm__ __volatile__("cpuid");			\
    __asm__ __volatile__("rdtsc" : "=a"(x), "=d(y)");	\
    __asm__ __volatile__("popl %ecx");			\
    __asm__ __volatile__("popl %edx");			\
    __asm__ __volatile__("popl %ebx");			\
    __asm__ __volatile__("popl %eax");			\
    ((uint64)x|((uint64)y)<<32);			\
  })
#  else
#   define hrtime()					\
  ({							\
    register uint32 x, y;				\
    __asm__ __volatile__("rdtsc" : "=a"(x), "=d"(y));	\
    ((uint64)x|((uint64)y)<<32);			\
  })
#  endif

# endif

extern uint64 hrfreq(void);

#elif defined(__x86_64__)

# define HRTIME_METHOD "rdtsc_64"

#  if defined(SERIALIZE_WITH_CPUID)
#   define hrtime()					\
  ({							\
    register uint32 x, y;				\
    __asm__ __volatile__("cpuid");			\
    __asm__ __volatile__("rdtsc" : "=a"(x), "=d"(y));	\
    ((uint64)x|((uint64)y)<<32);			\
  })
#  else
#   define hrtime()					\
  ({							\
    register uint32 x, y;				\
    __asm__ __volatile__("rdtsc" : "=a"(x), "=d"(y));	\
    ((uint64)x|((uint64)y)<<32);			\
  })
#  endif

extern uint64 hrfreq(void);

#elif defined(__ppc__) || defined(__powerpc__)

# define HRTIME_METHOD "mftb"

/*
__inline__ uint64 hrtime(void) {
  unsigned long int hi, lo, tmp;
  do {
      __asm__ __volatile__ ("mftbu %0" : "=r"(hi));
      __asm__ __volatile__ ("mftb  %0" : "=r"(lo));
      __asm__ __volatile__ ("mftbu %0" : "=r"(tmp));
  } while (hi != tmp);
  return ( (uint64)lo )|( ((uint64)hi)<<32 );
}
#elif defined(__powerpc__)
# define HRTIME_METHOD "mftb_64"
*/

#define hrtime()					\
  ({							\
    register uint64 x;					\
    __asm__ __volatile__ ("mftb %0"  : "=r"(t));	\
    x;							\
  })

#define hrfreq() ((uint64)1<<20)

#elif defined(__sun__)

#include <sys/time.h>

# define HRTIME_METHOD "gethrtime"

#define hrtime() ((uint64)(gethrtime()))

#define hrfreq() ((uint64)1000000000ll)

#else

/* #error "No tick counter is available!" */

#include <sys/time.h>

# define HRTIME_METHOD "gettimeofday"

#define hrtime()						\
  ({								\
    struct timeval tp;						\
    gettimeofday(&tp, 0L);					\
    (uint64)tp.tv_sec*1000000 + ((uint64)tp.tv_usec);		\
  })

#define hrfreq() ((uint64)1000000ll)

#endif


/*  $RCSfile:  $   $Author: kazutomo $
 *  $Revision: 1.6 $  $Date: 2005/04/13 18:49:58 $
 */

#endif /* _CIAO_HRTIME_H */
