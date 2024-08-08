/*
 *  os_defs.h
 *
 *  Some OS definitions:
 *   - MAXPATHLEN
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_OS_DEFS_H
#define _CIAO_OS_DEFS_H

#include <limits.h>

/* Max path name */
#if !defined(MAXPATHLEN)
# if defined(PATH_MAX)
#  define MAXPATHLEN PATH_MAX
# else
#  define MAXPATHLEN 1024
# endif
#endif

#if defined(_WIN32) || defined(_WIN64)
#define PATHSEP "\\"
#else
#define PATHSEP "/"
#endif

#endif /* _CIAO_OS_DEFS_H */
