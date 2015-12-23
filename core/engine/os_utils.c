/*
 *  os_utils.c
 *
 *  Platform-independent interface to operating system calls
 *  (filesystem, environment, processes).
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *  Copyright (C) 2015 Jose F. Morales, Ciao Development Team
 */

#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/types.h>
#include <stdlib.h>
#include <dirent.h>
#include <ctype.h>
#include <errno.h>
#include <utime.h>

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#include <share.h> /* for our mkstemp fix */
#define MAXHOSTNAMELEN 256
#if !defined(WNOHANG)
#define WNOHANG 1
#endif
#else /* Not MinGW */
#include <grp.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/wait.h>
#include <pwd.h>
#endif

#include <ciao/datadefs.h>
/* (use Win32 only after datadefs.h) */
#if defined(Win32) && !defined(_WIN32) && !defined(_WIN64) /* Cygwin||MSYS2 */
#include <sys/cygwin.h>
#endif

#if !defined(S_ISDIR) /* Notably, Solaris */
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#define WIFEXITED(x) 1
//#define WIFSIGNALED(x) 0
#define WEXITSTATUS(x) ((x) & 0xFF)
//#define WTERMSIG(x) SIGTERM
#endif

#if defined(Solaris)
int gethostname(char *name, int namelen);
#endif

#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/term_support.h>
#include <ciao/os_signal.h>
#include <ciao/io_basic.h>
#include <ciao/os_utils.h>
#include <ciao/streams_basic.h>
#include <ciao/stacks.h>
#include <ciao/start.h>
#include <ciao/alloc.h>

#if !defined(MAXPATHLEN)
#define MAXPATHLEN 1024
#endif

#define COPY_FLAG_OVERWRITE 0x1
#define COPY_FLAG_TIMESTAMP 0x2
#define COPY_FLAG_SYMLINK   0x4
#define COPY_FLAG_APPEND    0x8

/* --------------------------------------------------------------------------- */

char cwd[MAXPATHLEN+1];/* Should be private --- each thread may cd freely! */

/* --------------------------------------------------------------------------- */

/* running in a Windows non-cygwin shell */
bool_t using_windows(void)
{
#if defined(Win32) /* Cygwin||MSYS2||MinGW */
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
  return TRUE;
#else
  return !getenv("CIAOSCRIPT");
#endif
#else
  return FALSE;
#endif
}

CBOOL__PROTO(prolog_using_windows)
{
  return using_windows();
}

/* --------------------------------------------------------------------------- */

#if defined(Win32) /* Cygwin||MSYS2||MinGW */
#define DriveLen 2
inline bool_t path_has_drive_selector(const char *path) {
  return (isalpha(path[0]) && path[1]==':' &&
	  (path[2]=='/' || path[2]=='\\' || path[2]==(char)0));
}
#endif

/* Local macros for expand_file_name */
#define TARGET_CONCAT(STR) { 			\
    strcpy(&target[d], (STR));			\
    d += strlen(&target[d]);			\
  }
#define TARGET_ADD(CH) { 			\
    target[d++] = (CH);				\
  }
#define NAME_END(C) ((C) == 0 || (C) == '/')
#define GET_NAME(STR, N) {			\
    N = 0;					\
    for (;;) {		                        \
      char c = *src;		                \
      if (NAME_END(c)) break;			\
      STR[N++] = c;				\
      src++;					\
    }						\
    STR[N] = 0;					\
  }

/* Expand and normalize a pathname */
/* TODO: merge with library(pathnames) */
/* abs==TRUE: obtain an absolute path name composing with CWD */
bool_t expand_file_name(const char *name, bool_t abs, char *target)
{
  const char *src;
  int d = 0;

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#warning "TODO(MinGW): Fix open_null_stream"
  /* TODO: hack to avoid expansion of 'nul' reserved name. Instead of
     this, pass abs=FALSE in most places */
  if (strcmp(name, "nul") == 0 ||
      strcmp(name, "NUL") == 0) abs = FALSE;
#endif
  
#if defined(Win32) /* Cygwin||MSYS2||MinGW */
  char src_buff[MAXPATHLEN+1];
  /* Replace '\\' by '/' (for Cygwin and MinGW) */
  {
    const char *src = name;
    char *dest = src_buff;
    for (;;) {
      char c = *src;
      if (c == 0) break;
      if (c == '\\') c = '/';
      *(dest++) = c;
      src++;
    }
    *dest = 0;
  }
  src = src_buff;
#else
  src = name;
#endif

  /* Collapse multiple consecutive slashes, cancel '..' and '.',
     remove trailing slash and expand $var/REL, ~user/REL (environment
     variable or home directory) */

  /* TODO: This does not expand inner occurrences of $var */
  /* TODO: Extend 'abs' so that it can disable ~ and $var expansion
     (that expansion should be explicit since those are not POSIX
     paths) */

  /* In order to be POSIX compliant root a double-slash ('//') is kept
     intact (this prefix is used in Unix-like systems like Cygwin or
     Apollo Domain/OS) */
  if (src[0] == '/' && src[1] == '/' && src[2] != '/') {
    /* Copy root double-slash (POSIX) */
    TARGET_ADD('/');
    TARGET_ADD('/');
    src += 2;
  }

  switch (*src) {
  case '/':        /* absolute path */
#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): check that drive letter is added for file name expansion of paths like \\foo"
    if (abs) {
      /* Pick drive letter from 'cwd' (two first characters) */
      TARGET_ADD(cwd[0]);
      TARGET_ADD(cwd[1]);
    }
#endif
    goto next;
  case '$':        /* environment var */
    goto envpath;
  case '~':        /* home directory */
    goto homepath;
  default:
#if defined(Win32)
    if (path_has_drive_selector(src)) goto drivepath;
#endif
    if (abs) {
      /* Add current working directory (ensuring trailing slash) */
      TARGET_CONCAT(cwd);
      if (target[d-1] != '/') TARGET_ADD('/');
    }
    goto comp;
  }

 envpath:
  src++; /* Skip $ */
  {
    char *var = &target[0]; /* (temporarily use target mem) */
    int vn; /* length of var */
    char *value;
    GET_NAME(var, vn); /* Copy component name */
    /* Expand variable value */
    if (vn == 0) {
      value = "$";
    } else {
      value = getenv(var);
      if (value == NULL) {
	USAGE_FAULT("file name: undefined variable");
      }
    }
    TARGET_CONCAT(value);
  }
  goto next;

 homepath:
  src++; /* Skip ~ */
  {
    char *var = &target[0]; /* (temporarily use target mem) */
    int vn; /* length of var */
    char *value;
    GET_NAME(var, vn); /* Copy component name */
    /* Expand home directory */
    if (vn == 0) {
      value = getenv("HOME");
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
      if (value == NULL) {
	/* Get home directory from APPDATA (compatible with emacs on Windows)
	   (typically C:\Users\username\AppData\Roaming on Windows Vista/7/2008)
	*/
	value = getenv("APPDATA");
      }
#endif      
      if (value == NULL) {
	USAGE_FAULT("file name: cannot obtain home directory for the current user");
      }
    } else {
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#warning "TODO(MinGW): expand arbitrary user home directory?"
      USAGE_FAULT("file name: cannot obtain home directory for other users");
#else
      struct passwd *pw = getpwnam(var);
      if (pw == NULL) {
	USAGE_FAULT("file name: no such user");
      }
      value = (char *)pw->pw_dir;
#endif
    }
    TARGET_CONCAT(value);
  }
  goto next;

#if defined(Win32)
 drivepath:
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
  /* X: or x: --> x: */
  TARGET_ADD(tolower(*src));
  TARGET_ADD(':');
#else /* Cygwin||MSYS2 */
  /* X: or x: --> /cygdrive/x */
  TARGET_CONCAT("/cygdrive/");
  TARGET_ADD(tolower(*src));
#endif
  src += DriveLen; /* length of "X:" part */
  goto next; /* *src is / here */
#endif

 comp: /* Process a path component */
  if (src[0] == '.') { /* special cases ".", ".." */
    if (NAME_END(src[1])) { /* "." component */
      goto skip_comp;
    } else if (src[1] == '.' && NAME_END(src[2])) { /* ".." component */
      if (d == 0) {
	/* target == "", cannot cancel */
      } else if (d == 1) {
	/* target == "/", skip ".." component */
	goto skip_comp;
      } else if (d == 3 && 
		 target[d-3] == '.' && target[d-2] == '.') {
	/* target == "../", cannot cancel */
      } else if (d > 3 && target[d-4] == '/' &&
		 target[d-3] == '.' && target[d-2] == '.') {
	/* target == "???/../", cannot cancel */
      } else {
	/* Cancel last element */
	d--; /* move before prev '/' */
	for (; d > 0 && target[d-1] != '/'; d--) {}
	goto skip_comp;
      }
    }
  }
  /* Not a special case */
  for (;;) { char c = *src; if (NAME_END(c)) break; TARGET_ADD(c); src++; }
  goto next;

 skip_comp: /* Skip component (for "." or "..") */
  for (;;) { char c = *src; if (NAME_END(c)) break; src++; }
  if (*src == 0) goto end;
  goto sep;
  
 next: /* Finish or add slash and process next component */
  if (*src == 0) goto end;
  TARGET_ADD('/');
  goto sep;

 sep: /* Skip all src slashes and process next component */
  /* assert(*src == '/'); */
  for (src++; *src == '/'; src++) {}
  goto comp;
  
 end: /* Finish path */
  if (d == 0) { /* Empty "" is expanded as "." */
    TARGET_CONCAT(".");
  } else {
    /* Remove trailing '/', only if target != "/" */
    if (d > 1 && target[d-1] == '/') d--;
  }
  TARGET_ADD(0);
  return TRUE;
}

#undef TARGET_CONCAT
#undef TARGET_ADD
#undef NAME_END
#undef GET_NAME

#if defined(FIX_PATHS) /* Renaming paths like /mounted/... */

struct ren_pair { char *from; char *to; };

static struct ren_pair rename_path_pairs[] = REN_PAIRS;

int fix_path(char *path)
{
  char *from, *p1, buf[MAXPATHLEN+1];
  struct ren_pair *rp;

  for (rp = rename_path_pairs; *(from = rp->from) ; rp++) {
    for (p1 = path ; *from && *p1 ; from++, p1++) {
      if (*from != *p1) {break;}; /* "path" does not start with "from" */
    }
    if (! *from) { /* "path" starts with "from" */
      strcpy(buf, p1);
      strcpy(path, rp->to);
      strcat(path, buf);
      return TRUE;
    }
  }

  return FALSE;
}
#endif

/* --------------------------------------------------------------------------- */
 
void compute_cwd(void)
{
  if (getcwd(cwd, MAXPATHLEN+1) == NULL) {
    /* TODO: throw an exception instead */
    perror("getcwd() in compute_cwd(): ");
    SERIOUS_FAULT("Aborting");
  }

#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): merge with expand_file_name"
  /* Normalize cwd (needed for expand_file_name, etc.) */
  /* X: or x: --> x: */
  if (path_has_drive_selector(cwd)) {
    cwd[0] = tolower(cwd[0]);
  }
  /* Replace '\\' by '/' */
  int i;
  for (i = 0; cwd[i] != '\0'; i++) {
    if (cwd[i] == '\\') cwd[i] = '/';
  }
#endif

#if defined(FIX_PATHS)
  fix_path(cwd);
#endif
}

CBOOL__PROTO(prolog_unix_cd)
{
  ERR__FUNCTOR("system:working_directory", 2);
  char pathBuf[MAXPATHLEN+1];
/*   struct stat statbuf; */
  Unify_constant(MakeString(cwd), X(0));
  DEREF(X(0), X(0));

  DEREF(X(1), X(1));
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }

  /* check type argument*/
  if (!IsAtom(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(1), 2);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(1)), TRUE, pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(1), 2);
  }
  /* If there is another problem ...*/
  if (chdir(pathBuf)) {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(1), 2);
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(1), 2);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }

  compute_cwd();
  return TRUE;
}

/* --------------------------------------------------------------------------- */

/* Return the arguments with which the current prolog was invoked */
CBOOL__PROTO(prolog_unix_argv)
{
  tagged_t list = atom_nil;
  char **p1 = prolog_argv;
  int i;

  for (i=prolog_argc; i>1;) {
    MakeLST(list, MakeString(p1[--i]), list);
  }
  return cunify(Arg, list, X(0));
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(check_errno, int index)
{
  /* Note: This ERR__FUNCTOR is not related to an exported predicate,
     since prolog_constant_codes is called from other places. I have
     some ideas of refining error reporting without hindering
     performance (please, ask me before changing this code). -- JFMC
   */
  ERR__FUNCTOR("system:$throw_errno", 1);
  switch(errno) {
  case EISDIR: 
  case ENAMETOOLONG:
  case ENOTDIR: 
  case EFAULT:
    /* TODO: This is not a domain error error, leave it such as until
             we find a better solution */
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(index), index + 1);
    break;
  case EPERM:
  case EACCES:
  case EROFS:
    BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(index), index + 1);
    break;
  case ENOENT:
    BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(index), index + 1);
    break;
  case EMFILE:
  case ENFILE:
  case ENOMEM:
  case ELOOP:
  case ENOSPC:
    BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(index), index + 1);
    break;
  case EEXIST:
    BUILTIN_ERROR(SYSTEM_ERROR, X(index), index + 1);
    break;
  default:
    BUILTIN_ERROR(SYSTEM_ERROR, X(index), index + 1);
    break;
  }
}

/* --------------------------------------------------------------------------- */

#if defined(_WIN32) || defined(_WIN64)
/* Fixed mkstemp from mingw sources. It should not use _O_TEMPORARY.
   There is a bug report but it has not been fixed yet.
   -- JFMC, Aug 22, 2015 */
int __cdecl c_mkstemp(char *template_name) {
  int i, j, fd, len, index;

  /* These are the (62) characters used in temporary filenames. */
  static const char letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

  /* The last six characters of template must be "XXXXXX" */
  if (template_name == NULL || (len = strlen (template_name)) < 6
      || memcmp (template_name + (len - 6), "XXXXXX", 6)) {
    errno = EINVAL;
    return -1;
  }

  /* User may supply more than six trailing Xs */
  for (index = len - 6; index > 0 && template_name[index - 1] == 'X'; index--);

  /* Like OpenBSD, mkstemp() will try at least 2 ** 31 combinations
     before giving up. */
  for (i = 0; i >= 0; i++) {
    for(j = index; j < len; j++) {
      template_name[j] = letters[rand () % 62];
    }
    fd = _sopen(template_name,
		_O_RDWR | _O_CREAT | _O_EXCL | /*_O_TEMPORARY |*/ _O_BINARY,
                _SH_DENYRW, _S_IREAD | _S_IWRITE);
    if (fd != -1) return fd;
    if (fd == -1 && errno != EEXIST) return -1;
  }

  return -1;
}
#else
inline static int c_mkstemp(char *template_name) {
  return mkstemp(template_name);
}
#endif

CBOOL__PROTO(prolog_unix_mktemp)
{
  ERR__FUNCTOR("system:mktemp", 2);
  char template[STATICMAXATOM];
  int fildes;

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);

  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  strcpy(template, GetString(X(0)));

  /* if c_mkstemp fails, give a system error */
  if ((fildes = c_mkstemp(template)) <  0) {
    return check_errno(Arg, 0);
  } else {
    close(fildes);   // Do not leave it open, since the stream is not seen
                     // at Prolog level
    return cunify(Arg, MakeString(template), X(1));
  }
}

CBOOL__PROTO(prolog_unix_access)
{
  ERR__FUNCTOR("system:file_exists", 2);
  char pathBuf[MAXPATHLEN+1];
  int mode;

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  DEREF(X(1), X(1));

  if (!TagIsSmall(X(1)) || (mode = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1), 2, TY_BYTE);

  if (!expand_file_name(GetString(X(0)), TRUE, pathBuf))
    return FALSE;

  if (access(pathBuf, mode))
    {
/*       perror("% access in file_exists/2"); /\* --this must be quiet. *\/ */
      /*  MINOR_FAULT("access() failed");  */
      /* --MCL: no need to raise any exception */
      return FALSE;
    }
  return TRUE;
}

/* directory_files(+Path, FileList) */

CBOOL__PROTO(prolog_directory_files)
{
  ERR__FUNCTOR("system:directory_files", 2);
  char pathBuf[MAXPATHLEN+1];
  DIR *dir;
  int gap;
  struct dirent *direntry;

  /* Using X(2) to build the result - DCG */

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), TRUE, pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  
  /* Check for system errors */
  if (! (dir = opendir(pathBuf))) {
    if (current_ferror_flag==atom_on) {
      /* First, identifying the error type: */
      switch(errno) {
      case EACCES:
	BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(0), 1);
	break;
      case ENOENT:
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
	break;
      case ENOTDIR:
	BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
	break;
      case EMFILE:
      case ENFILE:
      case ENOMEM:
	BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(0), 1);
	break;
      default:
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  } else {
    X(2) = atom_nil;
    gap = HeapDifference(w->global_top, Heap_End)-CONTPAD;
    while ((direntry = readdir(dir))) {
      if ((gap -= 2) < 0) {
	explicit_heap_overflow(Arg, CONTPAD+32, 3);
	gap += 32;
      }
      MakeLST(X(2), MakeString(direntry->d_name), X(2));
    }
    closedir(dir);
  }
  return cunify(Arg, X(2), X(1));
}

/* file_properties(+File, Type, Linkto, ModTime, Protection, Size)

   ModTime: the time (in seconds since 1, Jan, 1970), since file File
   (absolute path) was last modified.
*/

CBOOL__PROTO(prolog_file_properties)
{
  ERR__FUNCTOR("system:file_properties", 6);
  struct stat statbuf;
  char pathBuf[MAXPATHLEN+1];
  char symlinkName[STATICMAXATOM+1];

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(0)), TRUE, pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(2), X(2));
  if (X(2) != atom_nil) { /* Link wanted */
    symlinkName[0] = (char) 0;
#if defined(_WIN32) || defined(_WIN64)
    /* No symlinks in MinGW */
#else
    int len;
    if ((len=readlink(pathBuf, symlinkName, STATICMAXATOM)) > 0) {
      symlinkName[len] = (char) 0;
    }
#endif
    Unify_constant(MakeString(symlinkName), X(2));
  }

  DEREF(X(1), X(1));
  DEREF(X(3), X(3));
  DEREF(X(4), X(4));
  DEREF(X(5), X(5));
  if (   (X(1)!=atom_nil)
         || (X(3)!=atom_nil)
         || (X(4)!=atom_nil)
         || (X(5)!=atom_nil) ) {

    if (stat(pathBuf, &statbuf)) {
      if (current_ferror_flag==atom_on) {
	switch (errno) {
	  case ENOENT: /* File does not exists */ 
	    BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
	    break ;
	  case EACCES: /* We dont have permissions in the directory */
	    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	    break ;
	  default: /* Who knows */
	    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);		    
	    break ;
	  }
      } else {
	/* Silently fails */
	return FALSE;
      }
    }

    if (X(1)!=atom_nil) {
      Unify_constant(( S_ISREG(statbuf.st_mode) ? atom_regular
                       : S_ISDIR(statbuf.st_mode) ? atom_directory
#if !(defined(_WIN32) || defined(_WIN64))
                       : S_ISLNK(statbuf.st_mode) ? atom_symlink
#endif
                       : S_ISFIFO(statbuf.st_mode) ? atom_fifo
#if !(defined(_WIN32) || defined(_WIN64))
                       : S_ISSOCK(statbuf.st_mode) ? atom_socket
#else
#warning "S_ISSOCK should be implemented in a different way for MinGW (winsock2)"
#endif
                       : atom_unknown), X(1));
    }

    if (X(3)!=atom_nil) {
      /* Cannot be Unify_constant because it is a large integer */
      if (!cunify(Arg, MakeInteger(Arg, statbuf.st_mtime), X(3))) {
	return FALSE;
      }
    }
    if (X(4)!=atom_nil) {
      Unify_constant(MakeSmall(statbuf.st_mode&0xfff), X(4));
    }
    if (X(5)!=atom_nil) {
      Unify_constant(MakeSmall(statbuf.st_size), X(5));
    }
  }

  return TRUE;
}

/* prolog_touch(+Path) */

CBOOL__PROTO(prolog_touch)
{
  ERR__FUNCTOR("system:touch", 1);
  char file[MAXPATHLEN+1];
  int status;
  int fd = -1;
  int open_errno = 0;

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument */
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  if (!expand_file_name(GetString(X(0)), TRUE, file))
    return FALSE;

  /* Try to open the file, create it if necessary. */
  fd = open(file, O_WRONLY | O_CREAT
#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): fix touch: struct stat sb; if (stat(file,&sb)) { create } else utime"
#else
	    | O_NONBLOCK | O_NOCTTY
#endif
	    ,
	    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (fd == -1) open_errno = errno;

  if (fd != -1 && close(fd) < 0) {
    BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK), X(0), 1);
  }

  /* If we have write access to the file, but do not own it, we will
     not be able to set the modification time arbitrarily. If we pass
     NULL, we just update it to the current time (which is allowed) */
  status = utime(file, NULL);

  if (status) {
    if (open_errno) {
      /* TODO: open_errno is not used */
      BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
    } else {
      /* TODO: errno is not used */
      BUILTIN_ERROR(PERMISSION_ERROR(MODIFY, SOURCE_SINK), X(0), 1);
    }
  }

  return TRUE;
}

CBOOL__PROTO(prolog_unix_chmod)
{
  ERR__FUNCTOR("system:chmod", 2);
  char pathBuf[MAXPATHLEN+1];
/*   struct stat statbuf; */
  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), TRUE, pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(1), X(1));
  /* check instatiation error to the other argument*/
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* and check type argument again */
  if (!TagIsSmall(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
  }

  /* make call to chmod, if there is any problem, raise a system error */
  if (chmod(pathBuf, GetSmall(X(1)))) {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      return FALSE;
    }   
  }
 
  return TRUE;
}

CBOOL__PROTO(prolog_unix_umask)
{
  ERR__FUNCTOR("system:umask", 2);
  int i;

  DEREF(X(1), X(1));
  /* check argument instantiation error */
  if (IsVar(X(1))) {
    if (X(1)==X(0)) {
      i = umask(0);
      (void)umask(i);
      return cunify(Arg, MakeSmall(i), X(0));
    } else
      BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  } else {
    /* check type argument*/
    if (!TagIsSmall(X(1)))
      BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
    return cunify(Arg, MakeSmall(umask(GetSmall(X(1)))), X(0));
  }
}




CBOOL__PROTO(prolog_unix_delete)
{
  ERR__FUNCTOR("system:delete_file", 1);
  char pathBuf[MAXPATHLEN+1];

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check argument domain error */
  if (!expand_file_name(GetString(X(0)), TRUE, pathBuf)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  /* Try to unlink, if anything go wrong, raise an error */
  if (unlink(pathBuf)) {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }
    
  return TRUE;
}


CBOOL__PROTO(prolog_unix_rename)
{
  ERR__FUNCTOR("system:rename_file", 2);
  char
    orig_name[MAXPATHLEN+1],
    new_name[MAXPATHLEN+1];

  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  DEREF(X(1), X(1));
  /* check instantiation error to the other argument */
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* check type the other argument*/
  if (!TagIsATM(X(1))) {
    ERROR_IN_ARG(X(1), 2, STRICT_ATOM);
  }
  /* check domain of the two arguments */
  if (!expand_file_name(GetString(X(0)), TRUE, orig_name)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  if (!expand_file_name(GetString(X(1)), TRUE, new_name)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(1), 2);
  }
  /* if anything fails, raise and exception */
  if (rename(orig_name, new_name)) {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(1), 2); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(1), 2);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }

  return TRUE;
}


CBOOL__PROTO(prolog_unix_mkdir)
{
  ERR__FUNCTOR("system:make_directory", 2);
  char dirname[MAXPATHLEN+1];

  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* check domain argument */
  if (!expand_file_name(GetString(X(0)), TRUE, dirname)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  DEREF(X(1), X(1));
  /* check instantiation error */
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* check type argument*/
  if (!TagIsSmall(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
  }
#if !(defined(_WIN32) || defined(_WIN64))
  int mode = GetSmall(X(1));
#endif
  
  /* call to mkdir, if there is a problem, raise a system error */
#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): inspired by others, mkdir mode is ignored (we could use chmod() afterwards but this may be ignored if noacl is used)"
  if (mkdir(dirname)) /* mode is ignored in MinGW */
#else
  if (mkdir(dirname, mode))
#endif
  {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      case ENOENT: /* Path does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      /* Silently fails */
      return FALSE;
    }
  }
       
  return TRUE;
}


CBOOL__PROTO(prolog_unix_rmdir)
{
  ERR__FUNCTOR("system:delete_directory", 1);
  char dirname[MAXPATHLEN+1];
  /*   struct stat statbuf; */
  DEREF(X(0), X(0));
  /* check instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  /* Check domain error */
  if (!expand_file_name(GetString(X(0)), TRUE, dirname)) {
    BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1);
  }
  /* check that file exists */

  /* try to make rmdir, else, system_error */
  if (rmdir(dirname)) {
    if (current_ferror_flag==atom_on) {
      switch (errno) {
      case ENOENT: /* File does not exists */ 
	BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); 
	break ;
      case EACCES: /* We dont have permissions in the directory */
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
	break ;
      default: /* Who knows */
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
	break ;
      }
    } else {
      return FALSE;
    }
  }
  return TRUE;
}

/* --------------------------------------------------------------------------- */

/*
 *  current_host(?HostName).
 */
CBOOL__PROTO(prolog_current_host)
{
  ERR__FUNCTOR("system:current_host", 1);
  char hostname[MAXHOSTNAMELEN*4];

  if (gethostname(hostname, sizeof(hostname)) < 0)
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  if (!strchr(hostname, '.')) {
    struct hostent *host_entry;
    char **aliases;

    /* If the name is not qualified, then pass the name through the name
       server to try get it fully qualified */
    /* if null, its a system error */
    if ((host_entry = gethostbyname(hostname)) == NULL)
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    strcpy(hostname, host_entry->h_name);

    /* If h_name is not qualified, try one of the aliases */

    if ((aliases=host_entry->h_aliases)) {
      while (!strchr(hostname, '.') && *aliases)
	strcpy(hostname, *aliases++);
      if (!strchr(hostname, '.'))
	strcpy(hostname, host_entry->h_name);
    }

#if HAS_NIS
    /* If still unqualified, then get the domain name explicitly.
       This code is NIS specific, and causes problems on some machines.
       Apollos don't have getdomainname, for example. */
    if (!strchr(hostname, '.')) {
      char domain[MAXHOSTNAMELEN*3];

      if (getdomainname(domain, sizeof(domain)) < 0)
	BUILTIN_ERROR(SYSTEM_ERROR, Arg, 1);
      strcat(hostname, ".");
      strcat(hostname, domain);
    }
#endif
    /*free(host_entry);*/
  }

  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(hostname), X(0));
}

/* --------------------------------------------------------------------------- */

/* internal_getenvstr(+Name, -Value) */

/* CBOOL__PROTO(prolog_c_getenvstr) */
/* { */
/*   char *s; */
/*   int i; */
/*   tagged_t cdr; */

/*   DEREF(X(0), X(0)); */
/*   DEREF(X(1), X(1)); */
/*   /\* check instantiation error *\/ */
/*   /\* */
/*   if (IsVar(X(0))) */
/*     BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1); */
/*   *\/ */
/*   /\* check type argument*\/ */
/*   /\* */
/*   if (!TagIsATM(X(0))) */
/*     ERROR_IN_ARG(X(0), 1, STRICT_ATOM); */
/*   *\/ */
/*   if ((s = getenv(GetString(X(0)))) == NULL) return FALSE; */

/*   s += (i = strlen(s)); */

/*   if (HeapDifference(w->global_top, Heap_End)<CONTPAD+(i<<1)) */
/*     explicit_heap_overflow(Arg, CONTPAD+(i<<1), 2); */

/*   cdr = atom_nil; */
/*   while (i>0) { */
/*     i--; */
/*     MakeLST(cdr, MakeSmall(*(--s)), cdr); */
/*   } */
/*   return cunify(Arg, cdr, X(1)); */
/* } */


/* setenvstr(+Name, +Value) */

#if defined(Solaris)
/* emulate setenv in terms of putenv (from rpm 2.0.9) */
int setenv(const char *name, const char *value, int overwrite)
{
  int len;
  if (!overwrite && getenv(name)) return 0;
  len = strlen(name) + strlen(value) + 2;
  if (len < 255) {
    char buf[256];
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    return putenv(buf);
  } else {
    char *buf = checkalloc_ARRAY(char, len);
    int ret;
    strcpy(buf, name);
    strcat(buf, "=");
    strcat(buf, value);
    ret = putenv(buf);
    checkdealloc_ARRAY(char, len, buf);
    return ret;
  }
}
#elif defined(_WIN32) || defined(_WIN64) /* MinGW */
#warning "TODO(MinGW): windows environment is stored as unicode, please review setenv/getenv implementation"
int setenv(const char *name, const char *value, int overwrite) {
  if (!overwrite && getenv(name)) {
    errno = EEXIST;
    return -1;
  }
  if (SetEnvironmentVariable(name, value)) {
    errno = ENOMEM;
    return -1;
  }
  return 0;
}
int unsetenv(const char *name) {
  if (!getenv(name)) {
    return 0;
  }
  if (SetEnvironmentVariable(name, NULL)) {
    errno = ENOMEM;
    return -1;
  }
  return 0;
}
#endif

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_c_winpath) /* EMM */
{
#if defined(Win32) && !defined(_WIN32) && !defined(_WIN64)
  /* MSYS2/Cygwin */
  char *posixpath;
  char winpath[MAX_PATH + 1];
  DEREF(X(0),X(0));
  posixpath = GetString(X(0));
  cygwin_conv_to_full_win32_path(posixpath, winpath);
  return cunify(Arg, MakeString(winpath), X(1));
#else
  return cunify(Arg, X(0), X(1));
#endif
}

CBOOL__PROTO(prolog_c_winfile) /* EMM */
{
#if defined(Win32) && !defined(_WIN32) && !defined(_WIN64)
  /* MSYS2/Cygwin */
  char *posixpath;
  char winpath[MAX_PATH + 1];
  DEREF(X(0),X(0));
  posixpath = GetString(X(0));
  cygwin_conv_to_win32_path(posixpath, winpath);
  return cunify(Arg, MakeString(winpath), X(1));
#else
  return cunify(Arg, X(0), X(1));
#endif
}

CBOOL__PROTO(prolog_c_posixpath) /* EMM */
{
#if defined(Win32) && !defined(_WIN32) && !defined(_WIN64)
  /* MSYS2/Cygwin */
  char *winpath;
  char posixpath[MAX_PATH + 1];
  DEREF(X(0),X(0));
  winpath = GetString(X(0));
  cygwin_conv_to_full_posix_path(winpath, posixpath);
  return cunify(Arg, MakeString(posixpath), X(1));
#else
  return cunify(Arg, X(0), X(1));
#endif
}

CBOOL__PROTO(prolog_c_posixfile) /* EMM */
{
#if defined(Win32) && !defined(_WIN32) && !defined(_WIN64)
  /* MSYS2/Cygwin */
  char *winpath;
  char posixpath[MAX_PATH + 1];
  DEREF(X(0), X(0));
  winpath = GetString(X(0));
  cygwin_conv_to_posix_path(winpath, posixpath);
  return cunify(Arg, MakeString(posixpath), X(1));
#else
  return cunify(Arg, X(0), X(1));
#endif
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_c_errno)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeSmall(errno), X(0));
}

CBOOL__PROTO(prolog_c_strerror)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(strerror(errno)), X(0));
}

/* --------------------------------------------------------------------------- */

#define BUF_MAX 65536

CBOOL__PROTO(prolog_c_copy_file)
{
  ERR__FUNCTOR("system:copy_file", 2);
  char *source, *destination;
  int fd_source, fd_destination, flags;
  int copy_flag;
  ssize_t s;
  struct stat stat_buf;
  char buffer[BUF_MAX];
  errno = 0;
  DEREF(X(0), X(0));
  source = GetString(X(0));
  DEREF(X(1), X(1));
  destination = GetString(X(1));
  DEREF(X(2), X(2));
  copy_flag = GetInteger(X(2));
  if (copy_flag & COPY_FLAG_SYMLINK) {
#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): silent copy or error?"
    SERIOUS_FAULT("TODO(MinGW): symlink not available in MinGW");
#else
    if (copy_flag & COPY_FLAG_OVERWRITE) {
      if(unlink(destination)==-1) {
	if (errno != ENOENT)
	  return check_errno(Arg, 1);
      }
#if defined(__CYGWIN32__) || defined(__CYGWIN__)
      if(access(destination, 0)) {
	if(unlink(destination)==-1) {
	  if (errno != ENOENT)
	    return check_errno(Arg, 1);
	}
      }
#endif
    }
    if (symlink(source, destination)==-1) {
      return check_errno(Arg, 1);
    }
#endif
  }
  else {
    fd_source = open(source, O_RDONLY);
    if(fd_source==-1) {
      /* First, identifying the error type: */
      return check_errno(Arg, 0);
    }

    if(copy_flag & COPY_FLAG_OVERWRITE) {
      flags = O_WRONLY|O_CREAT|O_TRUNC;
    }
    else if(copy_flag & COPY_FLAG_APPEND) {
      flags = O_WRONLY|O_CREAT|O_APPEND;
    }
    else {
      flags = O_WRONLY|O_CREAT|O_EXCL;
    }
    fstat(fd_source, &stat_buf);
    fd_destination = open(destination, flags, stat_buf.st_mode);
    if (fd_destination==-1) {
      /* Now we must close source */
      close(fd_source);
      /* Identifying the error type: */
      return check_errno(Arg, 1);
    }
    while((s=read(fd_source, buffer, BUF_MAX))!=0){
      if(s==-1){
	close(fd_source);
	close(fd_destination);
	BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
      }
      else
	{
	  if(write(fd_destination, buffer, s)==-1){
	    close(fd_source);
	    close(fd_destination);
	    BUILTIN_ERROR(SYSTEM_ERROR, X(1), 2);
	  }
	}
    }
    close(fd_source);
    close(fd_destination);
    if(copy_flag & COPY_FLAG_TIMESTAMP) {
      struct utimbuf buf;
      buf.actime  = stat_buf.st_atime;
      buf.modtime = stat_buf.st_mtime;
      utime(destination, &buf);
    }
  }
  return TRUE;
}

/* --------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_c_get_env)
{
  char *name, *value;
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  name = GetString(X(0));
  value = getenv(name);
  if(value==NULL)
    return FALSE;
  else
    return cunify(Arg, MakeString(value), X(1));
}

CBOOL__PROTO(prolog_c_set_env)
{
  ERR__FUNCTOR("system:set_env", 2);
  char *name, *value;
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  name = GetString(X(0));
  value = GetString(X(1));
  if (setenv(name, value, 1) != 0) {
    BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED), X(0), 1);
  } else {
    return TRUE;
  }
}

CBOOL__PROTO(prolog_c_del_env)
{
  char *name;
  DEREF(X(0), X(0));
  name = GetString(X(0));
  unsetenv(name);
  return TRUE;
}

extern char **environ;

CBOOL__PROTO(prolog_c_current_env)
{
  int n, index;
  char *nameval, *value;
  DEREF(X(0), X(0));
  index = GetInteger(X(0));
  nameval = environ[index];
  if (nameval == NULL) return FALSE; /* end of environment */
  
  DEREF(X(1), X(1));
  DEREF(X(2), X(2));
  value = strchr(environ[index], '=');
  n = (int)(value-nameval);
  value++;
  tagged_t value_term = MakeString(value);

  char *name = checkalloc_ARRAY(char, n+1);
  memcpy(name, nameval, (int)(n+1)*sizeof(char));
  name[n] = '\0';
  tagged_t name_term = MakeString(name);
  checkdealloc_ARRAY(char, n+1, name);

  return (cunify(Arg, name_term, X(1)) &&
	  cunify(Arg, value_term, X(2)));
}

/* --------------------------------------------------------------------------- */

/*
  pause(+Seconds): make this process sleep for Seconds seconds
*/

CBOOL__PROTO(prolog_pause)
{
  ERR__FUNCTOR("system:pause", 1);
  tagged_t x0;
  int time;

  DEREF(x0, X(0));
  /* check instantiation_error */
  if (IsVar(x0))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsSmall(x0))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(0), 1);
  time = GetSmall(x0);

  sleep(time);

  return TRUE;
}

/* --------------------------------------------------------------------------- */
/* Users and groups */

/* NOTE: Windows (MinGW) do not have UID or GID, but a more complex
   SID (security identifier) which is unique across machines or
   domains. To make sure that Prolog code is aware of this, predicates
   operating on UID and GID are invalid in non-POSIX platforms (which
   is what other languages also do).

   Alternatives:

    - return 0 for both UID and GID (to be compatible with some MSVCRC
      functions like 'stat')

    - expose USID and GSID directly (as atoms) (done in Go lang)

    - map those to fake UID and GID (done in MSYS2/Cygwin, see
      http://cygwin.com/cygwin-ug-net/ntsec.html for details)
*/

/*
  get_pid(?PID): PID is unified with  the process identificator number
  of this process
*/
CBOOL__PROTO(prolog_getpid)
{
  tagged_t x0;

  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getpid()));
}

/* Get UID (unavailable in non-POSIX systems) */
CBOOL__PROTO(prolog_getuid)
{
#if defined(_WIN32) || defined(_WIN64)
  SERIOUS_FAULT("TODO(MinGW): getuid is not available in non-POSIX systems");
#else
  tagged_t x0;
  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getuid()));
#endif
}

/* Get GID (unavailable in non-POSIX systems) */
CBOOL__PROTO(prolog_getgid)
{
#if defined(_WIN32) || defined(_WIN64)
  SERIOUS_FAULT("TODO(MinGW): getgid is not available in non-POSIX systems");
#else
  tagged_t x0;
  DEREF(x0, X(0));
  return cunify(Arg, x0, MakeSmall(getgid()));  
#endif
}

/* Get group name (unavailable in non-POSIX systems) */
CBOOL__PROTO(prolog_getgrnam)
{
#if defined(_WIN32) || defined(_WIN64)
  SERIOUS_FAULT("TODO(MinGW): getgrnam is not available in non-POSIX systems");
#else
  tagged_t x0;
  struct group * g;

  DEREF(x0, X(0));
  g = getgrgid(getgid());
  if (g == NULL) {
    return FALSE; 
  } else { 
    return cunify(Arg, x0, MakeString(g->gr_name)); 
  } 
#endif
}

/* Get user name */
CBOOL__PROTO(prolog_getpwnam)
{
  char *name;
  tagged_t x0;
  DEREF(x0, X(0));
  
#if defined(_WIN32) || defined(_WIN64)
  char user_name[100];
  DWORD len = sizeof(user_name);
  if (!GetUserName(user_name, &len))
    return FALSE;
  name = user_name;
#else
  struct passwd * p;
  p = getpwuid(getuid());
  name = p->pw_name;
#endif
  
  return cunify(Arg, x0, MakeString(name));
}

/* --------------------------------------------------------------------------- */

/* internals:$find_file(+LibDir, +Path, +Opt, +Suffix, ?Found, -AbsPath, -AbsBase, -AbsDir)
 * (See documentation in internals.pl)
 */

CBOOL__PROTO(prolog_find_file)
{
  char *libDir, *path, *opt, *suffix;
  char pathBuf[MAXPATHLEN+8];
  /* MAXATOM may change dynamically to make room for longer atoms */
  int relBufSize =2*MAXATOM+2;
  char *relBuf = checkalloc_ARRAY(char, relBufSize);
  char *bp;
  char *cp;
  struct stat file_status;
  time_t t_opt, t_pri;

  DEREF(X(0), X(0));
  libDir = GetString(X(0));
  DEREF(X(1), X(1));
  path = GetString(X(1));
  DEREF(X(2), X(2));
  opt = GetString(X(2));
  DEREF(X(3), X(3));
  suffix = GetString(X(3));

  if (path[0] == '/' || path[0] == '$' || path[0] == '~'
#if defined(Win32)
      || path[0] == '\\' || path_has_drive_selector(path)
#endif
      ) {
    /* absolute path, copy path to relBuf */
    strcpy(relBuf, path);
  } else if (strcmp(libDir, ".") == 0) {
    /* copy path into relBuf */
    strcpy(relBuf, path);
  } else {
    /* concatenate libDir and path into relBuf */
    strcpy(relBuf, libDir);
    if (relBuf[strlen(relBuf)-1]!='/')
      strcat(relBuf, "/");
    strcat(relBuf, path);
  }

  if (!expand_file_name(relBuf, TRUE, pathBuf)){
    checkdealloc_ARRAY(char, relBufSize, relBuf);
    return FALSE;
  }

#if defined(FIX_PATHS)
  fix_path(pathBuf);
#endif

  cp = pathBuf + strlen(pathBuf);

  t_opt = t_pri = 0;

 searchPath:

  if (*opt) {
    strcpy(cp, opt);
    bp = cp + strlen(cp);
    strcpy(bp, suffix);
    if(!access(pathBuf, F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_opt = file_status.st_mtime;    /* found path+opt+suffix */
    }
  }

  bp = cp;

  if (*suffix) {
    strcpy(bp, suffix);
    if(!access(pathBuf, F_OK)) {
      stat(pathBuf, &file_status);
      if (!S_ISDIR(file_status.st_mode))
        t_pri = file_status.st_mtime;    /* found path+suffix */
    }
  }

  if (t_pri > t_opt) { /* path+suffix exists, path+opt+suffix older|absent */
    Unify_constant(atom_true, X(4));
    goto giveVals;
  } else if (t_opt > 0) { /* newer path+opt+suffix exists */
    /* recreate opt+suffix */
    strcpy(cp, opt);
    bp = cp + strlen(cp);
    strcpy(bp, suffix);
    Unify_constant(atom_true, X(4));
    goto giveVals;
  }

  *bp = 0; /* reset to path (no opt, no suffix) */ 

  if(!access(pathBuf, F_OK)){
    stat(pathBuf, &file_status);
    if (S_ISDIR(file_status.st_mode)) {    /* directory */
      while (*bp!='/') --bp;               /* duplicate dir name */
      *cp++ = *bp++ ;
      while (*bp!='/')
        *cp++ = *bp++ ;
      *cp = 0;
      goto searchPath;                     /* search inside */
    } else {
      Unify_constant(atom_true, X(4));      /* found path */
      if (*suffix && strcmp(bp -= strlen(suffix), suffix))
        /* does not end in suffix */
        bp = cp;
      goto giveVals;
    }
  }

  /* not found */
  Unify_constant(atom_fail, X(4));

 giveVals:

  /* absolute path+suffix */
  Unify_constant(MakeString(pathBuf), X(5));

  /* absolute path (without suffix) */
  *bp = 0;
  Unify_constant(MakeString(pathBuf), X(6));

  /* dirname */
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#warning "TODO(MinGW): Fix open_null_stream"
  /* TODO: pathBuf may be 'nul' or 'NUL' (see expand_file_name) */
  while (bp > pathBuf && *bp!='/')
    --bp;
#else
  while (*bp!='/')
    --bp;
#endif
  *bp = 0;

  Unify_constant(MakeString(pathBuf), X(7));

  checkdealloc_ARRAY(char, relBufSize, relBuf);
  return TRUE;
}

/* --------------------------------------------------------------------------- */

static bool_t c_path_is_absolute(const char *path) {
#if defined(Win32)
  if (path_has_drive_selector(path)) return TRUE;
#endif
  return (path[0] == '/');
}

CBOOL__PROTO(prolog_path_is_absolute)
{
  ERR__FUNCTOR("internals:$path_is_absolute", 1);
  char *path;

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* check type argument*/
  if (!TagIsATM(X(0))) {
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);
  }
  path = GetString(X(0));

  return c_path_is_absolute(path);
}

CBOOL__PROTO(prolog_expand_file_name)
{
  char *path;
  /* MAXATOM may change dinamically to make room for longer atoms */
  int relBufSize =2*MAXATOM+2;
  char *relBuf = checkalloc_ARRAY(char, relBufSize);

  DEREF(X(0), X(0));
  path = GetString(X(0));

  DEREF(X(1), X(1));
  bool_t abs = (X(1) == atom_true);
  
  if (!expand_file_name(path, abs, relBuf)) {
    checkdealloc_ARRAY(char, relBufSize, relBuf);
    return FALSE;
  }

#if defined(FIX_PATHS)
  fix_path(relBuf);
#endif

  Unify_constant(MakeString(relBuf), X(2));
  checkdealloc_ARRAY(char, relBufSize, relBuf);
  return TRUE;
}

/* --------------------------------------------------------------------------- */
/* PATH and location of executables */ 

/* Path list separator character */
/* See system:pathlistsep/1 */
#if defined(_WIN32) || defined(_WIN64)
#define PATHLIST_SEP ';'
#else /* POSIX */
#define PATHLIST_SEP ':'
#endif

/* (C version of system_extra:extract_paths/2) -- note that this one
   does not add '.' by default */
char **c_extract_paths(char *envpath) {
  char *p, **paths;

  if (envpath == NULL || envpath[0] == '\0') {
    /* No PATH */
    return NULL;
  }

  /* Do a copy of PATH */
  envpath = strdup(envpath);
  if (envpath == NULL) SERIOUS_FAULT("allocation error");

  /* Split internally by replacing PATHLIST_SEP by '\0'. Count number
     of items */
  p = envpath;
  int n = 0;
  while (p) {
    char *item = p;
    p = strchr(p, PATHLIST_SEP);
    if (p) *p++ = '\0';
    if (item[0] == '\0') continue; /* Empty item */
    n++;
  }
  if (n == 0) return NULL; /* No items */

  paths = malloc((n+1)*sizeof(char *));
  if (paths == NULL) SERIOUS_FAULT("allocation error");
  p = envpath;
  int i = 0;
  do {
    if (p[0] != '\0') {
      char *item = strdup(p);
      if (item == NULL) SERIOUS_FAULT("allocation error");
      paths[i++] = item;
    }
    p += strlen(p) + 1; /* Go to next item */
  } while (i < n);
  paths[i] = NULL;
  
  free(envpath);

  return paths;
}

/* Deallocate the result of c_extract_paths() */
void c_free_paths(char **paths) {
  if (paths == NULL) return;
  int i;
  for (i = 0; paths[i] != NULL; i++) free(paths[i]);
  free(paths);
}

char **c_extract_paths(char *envpath);
void c_free_paths(char **path);

/* Exists dir+cmd[+.exe] and is not a directory */
/* Return: a string (client must deallocate) */
char *c_lookup_exec(const char *dir, const char *cmd) {
#if defined(_WIN32) || defined(_WIN64)
  char path[MAX_PATH];
#else
  char path[MAXPATHLEN+1];
#endif
  strcpy(path, dir);
#if defined(_WIN32) || defined(_WIN64)
  strcat(path, "\\");
#else
  strcat(path, "/");
#endif
  strcat(path, cmd);
  
#if defined(_WIN32) || defined(_WIN64)
  /* Adds .exe extension if needed */
  int len = strlen(cmd);
  if (len >= 4 && !strcasecmp(cmd+len-4, ".exe")) {
    /* ends in .exe */
  } else {
    strcat(path, ".exe");
  }
#endif

#if defined(_WIN32) || defined(_WIN64)
  /* X_OK is not valid in some Windows/MinGW versions */
  if (access(path, F_OK) != 0) return NULL;
#else
  if (access(path, X_OK) != 0) return NULL;
#endif
  struct stat file_status;
  stat(path, &file_status);
  if (S_ISDIR(file_status.st_mode)) return NULL; /* directory */
  
  char *res = strdup(path);
  if (res == NULL) SERIOUS_FAULT("allocation error");
  return res;
}

/* 'path' cannot be split anymore */
/* (Equivalent to path_split(X, '', X)) */
bool_t c_path_is_simple(const char *path) {
  if (strchr(path, '/') != NULL) return FALSE;
#if defined(_WIN32) || defined(_WIN64)
  if (strchr(path, '\\') != NULL) return FALSE;
#endif
  return TRUE;
}

/* (C version of system:find_executable/2) */
/* TODO: check for c_path_is_simple() is not in the Prolog version */
char *c_find_exec(const char *cmd) {
  if (!c_path_is_simple(cmd)) {
    char *path = strdup(cmd);
    if (path == NULL) SERIOUS_FAULT("allocation error");
    return path;
  } else {
    char *envpath = getenv("PATH");
    char **paths = c_extract_paths(envpath);
    if (paths == NULL) return NULL;
    int i;
    char *path = NULL;
    for (i = 0; paths[i] != NULL; i++) {
      path = c_lookup_exec(paths[i], cmd);
      if (path != NULL) break;
    }
    c_free_paths(paths);
    return path;
  }
}

/* 'path' is in the path list 'envpath' */
static bool_t in_paths(const char *path, const char *envpath) {
  const char *aux;
  size_t len = strlen(path);
  for (;;) {
    aux = (const char*)strchr(envpath, PATHLIST_SEP);
    if (aux == NULL) break; /* no separator found */
    size_t n = aux - envpath; /* length of this item */
    if (n == len && strncmp(envpath, path, n) == 0) {
      /* Found exact match (same size and characters) */
      return TRUE;
    }
    envpath = aux + 1; /* skip item and separator (+1) */
  }
  return (strcmp(envpath, path) == 0);
}

/* Add 'path' to 'envpath' (a path list) if not already there */
/* Return: NULL if already in 'envpath'
           or a string (client must deallocate) */
char *c_paths_insert_new(const char *envpath, const char *path) {
  if (in_paths(path, envpath)) return NULL;

  /* Is PATHLIST_SEP needed? */
  bool_t needs_sep = (envpath[0] != '\0' &&
		      envpath[strlen(envpath) - 1] != PATHLIST_SEP);

  /* Length of new_envpath */
  int len = strlen(envpath) + (needs_sep ? 1 : 0);
  len += strlen(path);

  /* Concat 'envpath' (+ PATHLIST_SEP) + 'path' */
  char *new_envpath = (char *)malloc((len + 1) * sizeof(char));
  strcpy(new_envpath, envpath);
  static char sep[] = {(char)PATHLIST_SEP, '\0'};
  if (needs_sep) strcat(new_envpath, sep);
  strcat(new_envpath, path);

  return new_envpath;
}

/* --------------------------------------------------------------------------- */

extern char *eng_architecture;

/*
 *  get_arch(?ArchName).
 */
CBOOL__PROTO(prolog_getarch)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(eng_architecture), X(0));
}


extern char *eng_os;

/*
 *  get_os(?OsName).
 */
CBOOL__PROTO(prolog_getos)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(eng_os), X(0));
}

extern char *eng_debug_level;

/*
 *  system_info:eng_debug_level(?DebugLevel).
 */
CBOOL__PROTO(prolog_eng_debug_level)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(eng_debug_level), X(0));
}

extern int eng_is_sharedlib;

CBOOL__PROTO(prolog_eng_is_sharedlib)
{
  return eng_is_sharedlib;
}

extern char *ciao_version;
extern char *ciao_patch;
extern char *ciao_commit_branch;
extern char *ciao_commit_id;
extern char *ciao_commit_date;
extern char *ciao_commit_desc;

/*
 *  $ciao_version(?Version, ?Patch, ?CommitBranch, ?CommitId, ?CommitDate, ?CommitDesc) 
 *    for current_prolog_flag(version, ?V).
 */
CBOOL__PROTO(prolog_version)
{
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));
  DEREF(X(2), X(2));
  DEREF(X(3), X(3));
  DEREF(X(4), X(4));
  DEREF(X(5), X(5));
  return (cunify(Arg, MakeString(ciao_version),  X(0)) &&
	  cunify(Arg, MakeString(ciao_patch),    X(1)) &&
	  cunify(Arg, MakeString(ciao_commit_branch), X(2)) &&
	  cunify(Arg, MakeString(ciao_commit_id), X(3)) &&
	  cunify(Arg, MakeString(ciao_commit_date), X(4)) &&
	  cunify(Arg, MakeString(ciao_commit_desc), X(5)));
}

extern char *ciao_suffix;
extern char *exec_suffix;
extern char *so_suffix;

CBOOL__PROTO(prolog_get_ciao_ext)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(ciao_suffix), X(0));
}

CBOOL__PROTO(prolog_get_exec_ext)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(exec_suffix), X(0));
}

CBOOL__PROTO(prolog_get_so_ext)
{
  DEREF(X(0), X(0));
  return cunify(Arg, MakeString(so_suffix), X(0));
}

extern char *foreign_opts_cc;
extern char *foreign_opts_ld;
extern char *foreign_opts_ccshared;
extern char *foreign_opts_ldshared;

#define DEF_PROLOG_GET_STR(NAME) \
CBOOL__PROTO(prolog_get_##NAME) { \
  DEREF(X(0), X(0)); \
  return cunify(Arg, MakeString(NAME), X(0)); \
}

DEF_PROLOG_GET_STR(foreign_opts_cc);
DEF_PROLOG_GET_STR(foreign_opts_ld);
DEF_PROLOG_GET_STR(foreign_opts_ccshared);
DEF_PROLOG_GET_STR(foreign_opts_ldshared);

/* --------------------------------------------------------------------------- */
/* External processes */

#if defined(_WIN32) || defined(_WIN64)
/* (wait on process created with c_create_process, similar to waitpid()) */
pid_t c_waitpid(pid_t pid, int *status) {
  HANDLE h;

  h = (HANDLE)OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, FALSE, pid);

  if (WaitForSingleObject(h, INFINITE) != WAIT_OBJECT_0) {
    CloseHandle(h);
    errno = ECHILD;
    return -1;
  }

  GetExitCodeProcess(h, (LPDWORD)status);
  CloseHandle(h);
  
  return pid;
}
#else 
inline static pid_t c_waitpid(pid_t pid, int *status) {
  return waitpid(pid, status, 0);
}
#endif

CBOOL__PROTO(prolog_wait)
{
  ERR__FUNCTOR("system:wait", 2);
  int waited_pid, status;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(0), 1);
  }

  waited_pid = c_waitpid(GetSmall(X(0)), &status);
  
  /* TODO: throw exceptions to capture more informative errors? */
  /* Process did not terminated normally */
  if (!WIFEXITED(status)) return FALSE;
  /* Some error */
  if (waited_pid == -1) return FALSE;

  int retcode = WEXITSTATUS(status);
  
  return cunify(Arg, X(1), MakeSmall(retcode));
}

#if defined(_WIN32) || defined(_WIN64)
int kill(pid_t pid, int sig) {
  HANDLE h = (HANDLE)pid;
  /* NOTE: Windows exit status does not distingish between ExitProcess
     and TerminateProcess */
  if (!TerminateProcess (h, sig)) {
    errno = ECHILD;
    return -1;
  }
  return 0;
}
#endif

CBOOL__PROTO(prolog_kill)
{
  ERR__FUNCTOR("system:kill", 2);

  DEREF(X(0), X(0));
  /* check argument instantiation error */
  if (IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  }
  /* and check type argument again */
  if (!TagIsSmall(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(0), 1);
  }

  DEREF(X(1), X(1));
  /* check instatiation error to the other argument*/
  if (IsVar(X(1))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(1), 2);
  }
  /* and check type argument again */
  if (!TagIsSmall(X(1))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(1), 2);
  }

  /* sends the signal specified by X(1) to X(0) process (pid) */
  /* if there is any problem, raise a system error */
  if (kill(GetSmall(X(0)), GetSmall(X(1)))) {
    if (current_ferror_flag==atom_on) {
      /* TODO: more detailed handling of errors? (EINVAL, EPERM, ESRCH) */
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    } else {
      return FALSE;
    }   
  }
 
  return TRUE;
}

/*
 * internals:'$exec'(+Cmd, +Args, ?InS, ?OutS, ?ErrS,
 * 		     +Env, +Cwd, +Flags, -PID):
 *   See library(process) for details.
 */

#define EXEC_FLAG_ENGFORK  0x1 /* Fork the engine (ignore Cmd) */
#define EXEC_FLAG_PATHEXEC 0x2 /* Search executable in PATH */
#define EXEC_FLAG_SETSID   0x4 /* Call setsid() in child */

#define Read  0
#define Write 1

#define STDIN  0
#define STDOUT 1
#define STDERR 2

#if defined(_WIN32) || defined(_WIN64)
int err_win_to_posix(DWORD winerr)
{
	int error = ENOSYS;
	switch(winerr) {
	case ERROR_ACCESS_DENIED: error = EACCES; break;
	case ERROR_ACCOUNT_DISABLED: error = EACCES; break;
	case ERROR_ACCOUNT_RESTRICTION: error = EACCES; break;
	case ERROR_ALREADY_ASSIGNED: error = EBUSY; break;
	case ERROR_ALREADY_EXISTS: error = EEXIST; break;
	case ERROR_ARITHMETIC_OVERFLOW: error = ERANGE; break;
	case ERROR_BAD_COMMAND: error = EIO; break;
	case ERROR_BAD_DEVICE: error = ENODEV; break;
	case ERROR_BAD_DRIVER_LEVEL: error = ENXIO; break;
	case ERROR_BAD_EXE_FORMAT: error = ENOEXEC; break;
	case ERROR_BAD_FORMAT: error = ENOEXEC; break;
	case ERROR_BAD_LENGTH: error = EINVAL; break;
	case ERROR_BAD_PATHNAME: error = ENOENT; break;
	case ERROR_BAD_PIPE: error = EPIPE; break;
	case ERROR_BAD_UNIT: error = ENODEV; break;
	case ERROR_BAD_USERNAME: error = EINVAL; break;
	case ERROR_BROKEN_PIPE: error = EPIPE; break;
	case ERROR_BUFFER_OVERFLOW: error = ENAMETOOLONG; break;
	case ERROR_BUSY: error = EBUSY; break;
	case ERROR_BUSY_DRIVE: error = EBUSY; break;
	case ERROR_CALL_NOT_IMPLEMENTED: error = ENOSYS; break;
	case ERROR_CANNOT_MAKE: error = EACCES; break;
	case ERROR_CANTOPEN: error = EIO; break;
	case ERROR_CANTREAD: error = EIO; break;
	case ERROR_CANTWRITE: error = EIO; break;
	case ERROR_CRC: error = EIO; break;
	case ERROR_CURRENT_DIRECTORY: error = EACCES; break;
	case ERROR_DEVICE_IN_USE: error = EBUSY; break;
	case ERROR_DEV_NOT_EXIST: error = ENODEV; break;
	case ERROR_DIRECTORY: error = EINVAL; break;
	case ERROR_DIR_NOT_EMPTY: error = ENOTEMPTY; break;
	case ERROR_DISK_CHANGE: error = EIO; break;
	case ERROR_DISK_FULL: error = ENOSPC; break;
	case ERROR_DRIVE_LOCKED: error = EBUSY; break;
	case ERROR_ENVVAR_NOT_FOUND: error = EINVAL; break;
	case ERROR_EXE_MARKED_INVALID: error = ENOEXEC; break;
	case ERROR_FILENAME_EXCED_RANGE: error = ENAMETOOLONG; break;
	case ERROR_FILE_EXISTS: error = EEXIST; break;
	case ERROR_FILE_INVALID: error = ENODEV; break;
	case ERROR_FILE_NOT_FOUND: error = ENOENT; break;
	case ERROR_GEN_FAILURE: error = EIO; break;
	case ERROR_HANDLE_DISK_FULL: error = ENOSPC; break;
	case ERROR_INSUFFICIENT_BUFFER: error = ENOMEM; break;
	case ERROR_INVALID_ACCESS: error = EACCES; break;
	case ERROR_INVALID_ADDRESS: error = EFAULT; break;
	case ERROR_INVALID_BLOCK: error = EFAULT; break;
	case ERROR_INVALID_DATA: error = EINVAL; break;
	case ERROR_INVALID_DRIVE: error = ENODEV; break;
	case ERROR_INVALID_EXE_SIGNATURE: error = ENOEXEC; break;
	case ERROR_INVALID_FLAGS: error = EINVAL; break;
	case ERROR_INVALID_FUNCTION: error = ENOSYS; break;
	case ERROR_INVALID_HANDLE: error = EBADF; break;
	case ERROR_INVALID_LOGON_HOURS: error = EACCES; break;
	case ERROR_INVALID_NAME: error = EINVAL; break;
	case ERROR_INVALID_OWNER: error = EINVAL; break;
	case ERROR_INVALID_PARAMETER: error = EINVAL; break;
	case ERROR_INVALID_PASSWORD: error = EPERM; break;
	case ERROR_INVALID_PRIMARY_GROUP: error = EINVAL; break;
	case ERROR_INVALID_SIGNAL_NUMBER: error = EINVAL; break;
	case ERROR_INVALID_TARGET_HANDLE: error = EIO; break;
	case ERROR_INVALID_WORKSTATION: error = EACCES; break;
	case ERROR_IO_DEVICE: error = EIO; break;
	case ERROR_IO_INCOMPLETE: error = EINTR; break;
	case ERROR_LOCKED: error = EBUSY; break;
	case ERROR_LOCK_VIOLATION: error = EACCES; break;
	case ERROR_LOGON_FAILURE: error = EACCES; break;
	case ERROR_MAPPED_ALIGNMENT: error = EINVAL; break;
	case ERROR_META_EXPANSION_TOO_LONG: error = E2BIG; break;
	case ERROR_MORE_DATA: error = EPIPE; break;
	case ERROR_NEGATIVE_SEEK: error = ESPIPE; break;
	case ERROR_NOACCESS: error = EFAULT; break;
	case ERROR_NONE_MAPPED: error = EINVAL; break;
	case ERROR_NOT_ENOUGH_MEMORY: error = ENOMEM; break;
	case ERROR_NOT_READY: error = EAGAIN; break;
	case ERROR_NOT_SAME_DEVICE: error = EXDEV; break;
	case ERROR_NO_DATA: error = EPIPE; break;
	case ERROR_NO_MORE_SEARCH_HANDLES: error = EIO; break;
	case ERROR_NO_PROC_SLOTS: error = EAGAIN; break;
	case ERROR_NO_SUCH_PRIVILEGE: error = EACCES; break;
	case ERROR_OPEN_FAILED: error = EIO; break;
	case ERROR_OPEN_FILES: error = EBUSY; break;
	case ERROR_OPERATION_ABORTED: error = EINTR; break;
	case ERROR_OUTOFMEMORY: error = ENOMEM; break;
	case ERROR_PASSWORD_EXPIRED: error = EACCES; break;
	case ERROR_PATH_BUSY: error = EBUSY; break;
	case ERROR_PATH_NOT_FOUND: error = ENOENT; break;
	case ERROR_PIPE_BUSY: error = EBUSY; break;
	case ERROR_PIPE_CONNECTED: error = EPIPE; break;
	case ERROR_PIPE_LISTENING: error = EPIPE; break;
	case ERROR_PIPE_NOT_CONNECTED: error = EPIPE; break;
	case ERROR_PRIVILEGE_NOT_HELD: error = EACCES; break;
	case ERROR_READ_FAULT: error = EIO; break;
	case ERROR_SEEK: error = EIO; break;
	case ERROR_SEEK_ON_DEVICE: error = ESPIPE; break;
	case ERROR_SHARING_BUFFER_EXCEEDED: error = ENFILE; break;
	case ERROR_SHARING_VIOLATION: error = EACCES; break;
	case ERROR_STACK_OVERFLOW: error = ENOMEM; break;
	case ERROR_SWAPERROR: error = ENOENT; break;
	case ERROR_TOO_MANY_MODULES: error = EMFILE; break;
	case ERROR_TOO_MANY_OPEN_FILES: error = EMFILE; break;
	case ERROR_UNRECOGNIZED_MEDIA: error = ENXIO; break;
	case ERROR_UNRECOGNIZED_VOLUME: error = ENODEV; break;
	case ERROR_WAIT_NO_CHILDREN: error = ECHILD; break;
	case ERROR_WRITE_FAULT: error = EIO; break;
	case ERROR_WRITE_PROTECT: error = EROFS; break;
	}
	return error;
}
#endif

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
int pipe(int filedes[2])
{
  HANDLE h[2];

  /* this creates non-inheritable handles */
  if (!CreatePipe(&h[0], &h[1], NULL, 8192)) {
    errno = err_win_to_posix(GetLastError());
    return -1;
  }
  filedes[0] = _open_osfhandle((intptr_t)h[0], O_NOINHERIT);
  if (filedes[0] < 0) {
    CloseHandle(h[0]);
    CloseHandle(h[1]);
    return -1;
  }
  filedes[1] = _open_osfhandle((intptr_t)h[1], O_NOINHERIT);
  if (filedes[1] < 0) {
    close(filedes[0]);
    CloseHandle(h[1]);
    return -1;
  }
  return 0;
}
#endif

typedef char *string_pair_t[2];

typedef struct process_ process_t;
struct process_ {
  pid_t pid;
  char **argv; /* NULL ended array for arguments (argv[0] is program name) */
  char *cwd;
  string_pair_t *env;
  
  unsigned eng_fork:1;
  unsigned do_setsid:1;

  /* read/write pairs of file descriptor for stdin, stdout, and stderr */
  int pair_in[2];
  int pair_out[2];
  int pair_err[2];
};

#if defined(_WIN32) || defined(_WIN64)
/* Quote 'arg' into 'dst' and compute length or quoted 'arg'.
   If 'dst' is NULL, no actual copy is made. */
/* Returns: the length of quoted 'arg' (even if 'dst' is NULL) */
static int win32_quote_arg(char *dst, const char *arg) {
  /* Count chars to quote */
  int len = 0, n = 0;
  bool_t force_quotes = FALSE;
  const char *p = arg;
  if (p[0] == '\0') force_quotes = TRUE;
  while (*p != '\0') {
    if (isspace(*p) || *p == '*' || *p == '?' || *p == '{' || *p == '\'') {
      force_quotes = TRUE;
    } else if (*p == '"') {
      n++;
    } else if (*p == '\\') {
      int count = 0;
      while (*p == '\\') {
	count++;
	p++;
	len++;
      }
      if (*p == '"') n += count*2 + 1;
      continue;
    }
    len++;
    p++;
  }
  if (!force_quotes && n == 0) {
    if (dst != NULL) strcpy(dst, arg);
    return len;
  }

  if (dst != NULL) { /* Quote arg */
    char *d = dst;
    *d++ = '"';
    while (*arg) {
      if (*arg == '"')
	*d++ = '\\';
      else if (*arg == '\\') {
	int count = 0;
	while (*arg == '\\') {
	  count++;
	  *d++ = *arg++;
	}
	if (*arg == '"') {
	  while (count-- > 0) *d++ = '\\';
	  *d++ = '\\';
	}
      }
      *d++ = *arg++;
    }
    *d++ = '"';
    *d++ = 0;
  }

  /* Length of quoted arg */
  return len+n+2;
}

/* Quote a NULL-ended array of arguments (for Win32 CreateProcess) */
/* Return: a new string with quoted arguments (client must deallocate) */
 
/* (this implementation is based on msysgit code and
    "Parsing C++ Command-Line Arguments"
    http://msdn2.microsoft.com/en-us/library/17w5ykft(vs.71).aspx)
 */
static char *win32_quote_args(const char **argv) {
  int i;

  /* Just compute length (no copy) */
  int len = 0;
  for (i = 0; argv[i] != NULL; i++) {
    len += win32_quote_arg((char *)NULL, argv[i]);
    if (argv[i+1] != NULL) len++; /* for ' ' */
  }

  /* Allocate and quote */
  char *qargs = (char *)malloc((len + 1) * sizeof(char));
  if (qargs == NULL) SERIOUS_FAULT("allocation error");
  char *p = qargs;
  for (i = 0; argv[i] != NULL; i++) {
    p += win32_quote_arg(p, argv[i]);
    if (argv[i+1] != NULL) *p++ = ' ';
  }
  *p = '\0';
  return qargs;
}
#endif

#if defined(_WIN32) || defined(_WIN64)
/* Return an environment block updated with 'deltaenv' (client must
   deallocate) */
/* TODO: bad complexity (sort env and do binary search?) */
static char *updated_envblk(string_pair_t *deltaenv) {
  int i;
  int len;

  /* Return NULL if deltaenv is empty */
  if (deltaenv == NULL || deltaenv[0][0] == NULL) {
    return NULL;
  }

  /* Compute upper approximation of envblk size (assumes that all
     deltaenv are new) */
  len = 0;
  for (i = 0; environ[i] != NULL; i++) {
    len += strlen(environ[i]) + 1;
  }
  for (i = 0; deltaenv[i][0] != NULL; i++) {
    if (deltaenv[i][1] == NULL) continue;
    len += (strlen(deltaenv[i][0]) +
	    1 + /* '=' */
	    strlen(deltaenv[i][1]) +
	    1); /* '\0' */
  }
  len++; /* Final '\0' */

  /* Allocate the environment block. All entries will be ended by
     '\0'. An additional '\0' marks the end. */
  char *envblk = (char *)malloc(len * sizeof(char));

  /* Copy all environ entries not appearing in deltaenv */
  char *p = envblk;
  for (i = 0; environ[i] != NULL; i++) {
    int j;
    bool_t match = FALSE;
    for (j = 0; deltaenv[j][0] != NULL; j++) {
      int jlen = strlen(deltaenv[j][0]);
      if (strncmp(environ[i], deltaenv[j][0], jlen) == 0 &&
	  environ[i][jlen] == '=') match = TRUE;
    }
    if (match) continue;
    int ilen = strlen(environ[i]); /* copy */
    strncpy(p, environ[i], ilen); p += ilen;
    *p++ = '\0';
  }

  /* Copy all not null entries from deltaenv */
  for (i = 0; deltaenv[i][0] != NULL; i++) {
    if (deltaenv[i][1] == NULL) continue; /* deleted item */
    int nlen = strlen(deltaenv[i][0]); /* name */
    int vlen = strlen(deltaenv[i][1]); /* value */
    strncpy(p, deltaenv[i][0], nlen); p += nlen;
    *p++ = '=';
    strncpy(p, deltaenv[i][1], vlen); p += vlen;
    *p++ = '\0';
  }
  *p++ = '\0'; /* Final '\0' */

  return envblk;
}
#else
/* Update the environment with 'deltaenv' */
static void update_env(string_pair_t *deltaenv) {
  int i = 0;
  for (;;) {
    char *name = deltaenv[i][0];
    char *value = deltaenv[i][1];
    if (name == NULL) break;
    if (value != NULL) {
      if (setenv(name, value, 1) != 0) return;
    } else {
      unsetenv(name);
    }
    i++;
  }
}
#endif

#if defined(_WIN32) || defined(_WIN64)
/* Is the process associated to a console? */
static bool_t has_console(void) {
  HANDLE cons = CreateFile("CONOUT$", GENERIC_WRITE,
			   FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
			   FILE_ATTRIBUTE_NORMAL, NULL);
  if (cons == INVALID_HANDLE_VALUE) {
    return FALSE;
  } else {
    CloseHandle(cons);
    return TRUE;
  }
}

/* spawn based on CreateProcess */
static void spawn_process(process_t *pr) {
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  unsigned flags = 0;
  BOOL ret;
  const char **argv = (const char **)pr->argv;
  string_pair_t *deltaenv = pr->env;
  const char *dir = pr->cwd;
  
  /* Duplicate fds if needed */
  int fhin = pr->pair_in[Read];
  int fhout = pr->pair_out[Write];
  int fherr = pr->pair_err[Write];
  if (fhin != -1 && fhin != 0) fhin = dup(fhin);
  if (fhout != -1 && fhout != 1) fhout = dup(fhout);
  if (fherr != -1 && fherr != 2) fherr = dup(fherr);

  // flags |= CREATE_UNICODE_ENVIRONMENT;

  /* Do not create a Window if the parent process is not associated to
     a console */
  if (!has_console()) {
    /* TODO: Use DETACH_PROCESS or CREATE_NO_WINDOW? Both
       DETACH_PROCESS and CREATE_NO_WINDOW has the same
       effect. However CREATE_NO_WINDOW still allows the child to
       access the parent's console (AttachConsole(ATTACH_PARENT_PROCESS)).
       So, use DETACHED_PROCESS here. */
    flags |= CREATE_NO_WINDOW;
    // flags |= DETACHED_PROCESS;
  }

  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

#define FDSTD(FD,STD) \
  ( (FD) == -1 ? GetStdHandle((STD)) : \
    (HANDLE)_get_osfhandle((FD)) )
  
  si.hStdInput = FDSTD(fhin, STD_INPUT_HANDLE);
  si.hStdOutput = FDSTD(fhout, STD_OUTPUT_HANDLE);
  si.hStdError = FDSTD(fherr, STD_ERROR_HANDLE);

#undef FDSTD

  /* Application name and quoted command line string */
  const char *cmd = argv[0]; /* Note: repeated in qargs */
  char *qargs = win32_quote_args(argv);

  /* Environment block */
  char *envblk = updated_envblk(deltaenv);

  memset(&pi, 0, sizeof(pi));
//  fprintf(stderr, "CreateProcess([%s], [%s])\n", cmd, qargs);
  ret = CreateProcess(cmd, qargs, NULL, NULL, TRUE, flags,
		      envblk, dir, &si, &pi);
  if (envblk != NULL) free(envblk);
  free(qargs);

  if (!ret) {
    errno = ENOENT;
    pr->pid = -1;
    return;
  }
  CloseHandle(pi.hThread);

  pr->pid = (pid_t)pi.dwProcessId;
  
  /* close duplicated fds */
  if (fhin != -1 && fhin != 0) close(fhin);
  if (fhout != -1 && fhout != 1) close(fhout);
  if (fherr != -1 && fherr != 2) close(fherr);
}
#else
/* spawn based on fork+execv (returns on parent or forked child) */
static void spawn_process(process_t *pr) {
  pid_t pid;
  pid = fork();
  pr->pid = pid;
  if (pid == -1) return;
  if (pid == 0) { /* Child process */
    bool_t execstat;

    /* Close pipe ends from parent */
    if (pr->pair_in[Write] != -1) close(pr->pair_in[Write]);
    if (pr->pair_out[Read] != -1) close(pr->pair_out[Read]);
    if (pr->pair_err[Read] != -1) close(pr->pair_err[Read]);

    /* Duplicate fds into 0,1,2 (if needed), then close them */
    if (pr->pair_in[Read] != -1 && pr->pair_in[Read] != 0)
      dup2(pr->pair_in[Read], STDIN);
    if (pr->pair_out[Write] != -1 && pr->pair_out[Write] != 1)
      dup2(pr->pair_out[Write], STDOUT);
    if (pr->pair_err[Write] != -1 && pr->pair_err[Write] != 2)
      dup2(pr->pair_err[Write], STDERR);

    if (pr->pair_in[Read] > 2)
      close(pr->pair_in[Read]);
    if (pr->pair_out[Write] > 2 && pr->pair_out[Write] != pr->pair_in[Read])
      close(pr->pair_out[Write]);
    if (pr->pair_err[Write] > 2 && pr->pair_err[Write] != pr->pair_out[Write] && pr->pair_err[Write] != pr->pair_in[Read])
      close(pr->pair_err[Write]);
    
    /* Update environment (set or unset values) */
    update_env(pr->env);

    /* Change directory (if needed) */
    if (pr->cwd) {
      /* TODO: is this right? */
      if (chdir(pr->cwd)) { /* (ignore errors) */ }
    }

    if (pr->do_setsid) setsid();

    if (pr->eng_fork) return; /* forked child */

    /* Replace the current process image with a new program */
    /* If there is any error while exec'ing, we have to give an
     * error and then abort completely. Unfortunately, if we have
     * redirected the standard error output, the error message will
     * be lost! */
    execstat = execv(pr->argv[0], pr->argv);
    /* (if execv returns there is an error and execstat < 0) */
    if (execstat < 0) {
      /* _EXIT is needed to avoid flushing and closing standard I/O
       * channels from the parent */
      _EXIT(-1); /* Should never return! */
    }
  }
}
#endif

/* Create an external process. Returns TRUE on parent or FALSE on
   child (for forked processes). pr->pid is -1 on fork error, 0 on
   child (for forked child) or >0 for parent. */
void c_create_process(process_t *pr) {
#if defined(_WIN32) || defined(_WIN64)
  if (pr->do_setsid) {
#warning "TODO(MinGW): implement detached"
    pr->pid = -1; /* Error */
    SERIOUS_FAULT("TODO(MinGW): setsid not available");
    return;
  }

  if (pr->eng_fork) {
    pr->pid = -1; /* Error */
    SERIOUS_FAULT("TODO(MinGW): eng_fork not available");
    return;
  }
#endif
  
  /* Empty buffers before launching child */
  fflush(NULL);

  spawn_process(pr);
  if (pr->pid == -1) return; /* error */
#if !(defined(_WIN32) || defined(_WIN64))
  if (pr->pid == 0) return; /* forked child (eng_fork must be TRUE) */
#endif

  /* Close pipe ends from child (if needed) */
  if (pr->pair_in[Read] != -1 && pr->pair_in[Write] != -1) {
    close(pr->pair_in[Read]);
  }
  if (pr->pair_out[Read] != -1 && pr->pair_out[Write] != -1) {
    close(pr->pair_out[Write]);
  }
  if (pr->pair_err[Read] != -1 && pr->pair_err[Write] != -1) {
    close(pr->pair_err[Write]);
  }
}

inline static void default_fd_pair(int pair_fd[2]) {
  pair_fd[Read] = -1; pair_fd[Write] = -1;
}

/* Obtain a pair of file descriptors (read/write) for the specified
 * connection std (a stream for user provided, a variable for pipe, or
 * [] for default).
 */
CVOID__PROTO(get_fd_pair, 
	     tagged_t std,
	     int pair_fd[2],
	     int mode)
{
  stream_node_t *s;

  default_fd_pair(pair_fd);

  DEREF(std, std);
  s = stream_to_ptr(std, (mode == Read ? 'r' : 'w'));
  if (s) {
    /* Set only the corresponding file descriptor */
    pair_fd[mode] = fileno(s->streamfile);
  } else if (std == atom_nil) {
    /* Use default fd */
  } else {
    /* Create a pipe */
    if (pipe(pair_fd) < 0) {
      /* TODO: throw an exception instead */
      perror("pipe() in get_fd_pair(): ");
      SERIOUS_FAULT("Aborting");
    }
  }
}

/* When std specifies a pipe, unify it with a Prolog stream connected
 * to the parent's end of the pipe */
CBOOL__PROTO(unify_fd_pair, 
	     tagged_t streamname,
	     tagged_t std,
	     int pair_fd[2],
	     int mode)
{
  stream_node_t *str = NULL;
  if (pair_fd[mode] != -1) {
    char *mode_str = (mode == Read ? "r" : "w");
    str = new_stream(streamname, mode_str, fdopen(pair_fd[mode], mode_str));
    /* (It should never fail) */
    return cunify(Arg, ptr_to_stream(Arg, str),  std);
  } else {
    return TRUE;
  }
}

/* Parse internal representation for env(Env) (see process.pl) */
CBOOL__PROTO(get_deltaenv, 
	     tagged_t list, string_pair_t **deltaenv, int *deltaenv_n)
{
  tagged_t head;
  tagged_t name;
  tagged_t val;

  /* Get length of list and allocate an array for deltaenv */
  DEREF(list, list);
  string_pair_t *denv;
  int len = CFUN__EVAL(c_list_length, list) + 1; /* +1 for NULL end */

  denv = checkalloc_ARRAY(string_pair_t, len);
  
  DEREF(list, list);
  int i = 0;
  while(!IsVar(list) && TagIsLST(list)) {
    DEREF(head, *TagToCar(list));
    if (TagIsSTR(head) && (TagToHeadfunctor(head)==functor_minus)) {
      DerefArg(name,head,1);
      if (!TagIsATM(name)) goto wrong;
      DerefArg(val,head,2);
      if (!TagIsATM(val)) goto wrong;
      denv[i][0] = GetString(name);
      denv[i][1] = GetString(val);
    } else {
      if (!TagIsATM(head)) goto wrong;
      denv[i][0] = GetString(head);
      denv[i][1] = NULL;
    }
    i++;
    list = *TagToCdr(list);
    DEREF(list, list);
  }
  denv[i][0] = NULL;
  denv[i][1] = NULL;
  
  *deltaenv = denv;
  *deltaenv_n = len;
  return TRUE;
 wrong:
  checkdealloc_ARRAY(string_pair_t, len, denv);
  return FALSE;
}

CBOOL__PROTO(prolog_exec)
{
  ERR__FUNCTOR("internals:$exec", 9);
  tagged_t head, list;
  int flags;
  bool_t path_exec;
  char *command;

  process_t pr;

  int args_n;
  char **args;

  /* Get command */
  DEREF(X(0), X(0));
  if (!IsAtom(X(0))) {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), X(0), 1);
  }
  command = GetString(X(0));

  /* Compute number of arguments and check types */
  args_n = 0;
  DEREF(list, X(1));
  while(!IsVar(list) && TagIsLST(list)) {
    args_n++;
    DEREF(head, *TagToCar(list));
    if (!TagIsATM(head)) { /* We only allow atoms */
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM), head, 2);
    }
    list = *TagToCdr(list);
    DEREF(list, list);
  }
  /* Make sure we had a real list */
  if (!(!IsVar(list) && TagIsATM(list) && (list == atom_nil))) {
    BUILTIN_ERROR(TYPE_ERROR(LIST), X(1), 2);
  }
  
  /* Get (optional) child execution directory */
  DEREF(X(6), X(6));
  if (IsAtom(X(6))) {
    pr.cwd = GetString(X(6));
  } else {
    pr.cwd = NULL;
  }

  /* Get flags */
  DEREF(X(7), X(7));
  if (!TagIsSmall(X(7))) {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER), X(7), 8);
  }
  flags = GetSmall(X(7));
  
  pr.eng_fork = ((flags & EXEC_FLAG_ENGFORK) != 0);
  path_exec = ((flags & EXEC_FLAG_PATHEXEC) != 0);
  pr.do_setsid = ((flags & EXEC_FLAG_SETSID) != 0);

  /* Lookup command in PATH (if needed) and check existence */
  if (path_exec) {
    command = c_find_exec(command);
    if (command == NULL) {
      BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1);
    }
  }
  if (!pr.eng_fork && access(command, X_OK) != 0) { /* TODO: right error? */
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, SOURCE_SINK), X(0), 1);
  }

  /* Allocate and fill array of command+arguments */
  args_n += 2; /* extra space for command and NULL */
  args = checkalloc_ARRAY(char *, args_n);

  int args_i = 0;
  args[args_i++] = command;

  DEREF(list, X(1));
  while(!IsVar(list) && TagIsLST(list)) {
    DEREF(head, *TagToCar(list));
    args[args_i++] = GetString(head);
    list = *TagToCdr(list);
    DEREF(list, list);
  }
  args[args_i] = NULL;
  
  pr.argv = args;

  /* Parse environment (set or unset values) */
  string_pair_t *deltaenv;
  int deltaenv_n;
  if (!CBOOL__SUCCEED(get_deltaenv, X(5), &deltaenv, &deltaenv_n)) {
    if (path_exec) free(command);
    checkdealloc_ARRAY(char *, args_n, args);
    SERIOUS_FAULT("internals:$exec/9 bad environment list");
  }
  pr.env = deltaenv;
  
  /* Get fd pairs for stdin, stdout, stderr (creating pipes if
   * needed) */
  DEREF(X(2), X(2));
  get_fd_pair(Arg, X(2), pr.pair_in, Read);
  DEREF(X(3), X(3));
  get_fd_pair(Arg, X(3), pr.pair_out, Write);
  DEREF(X(4), X(4));
  if (X(4) == atom_stdout) {
    /* redirect stderr to stdout */
    pr.pair_err[Read] = -1;
    pr.pair_err[Write] = pr.pair_out[Write];
  } else {
    get_fd_pair(Arg, X(4), pr.pair_err, Write);
  }

  /* Create the process */
  c_create_process(&pr);
  
  if (path_exec) free(command);
  checkdealloc_ARRAY(char *, args_n, args);
  checkdealloc_ARRAY(string_pair_t, deltaenv_n, deltaenv);

  if (pr.pid == 0) { /* Forked child process (not portable!) */
    if (!pr.eng_fork) {
      _EXIT(1); /* TODO: this code should not be reachable */
    }

    /* Update child streams if fd 0,1,2 have been redirected */
    if (pr.pair_in[Read] != -1 && pr.pair_in[Read] != 0)
      update_stream(stream_user_input, stdin);
    if (pr.pair_out[Write] != -1 && pr.pair_out[Write] != 1)
      update_stream(stream_user_output, stdout);
    if (pr.pair_err[Write] != -1 && pr.pair_err[Write] != 2)
      update_stream(stream_user_error, stderr);

    /* Update cwd (if changed) */
    if (pr.cwd) {
      compute_cwd();
    }
    /* Continue child execution (fail exit, must finish with halt/1) */
    return FALSE;
  } else { /* Parent process */
    if (pr.pid == -1) {
      SERIOUS_FAULT("internals:$exec/9 could not fork new process");
    }

    // TODO: None of those unifications should fail (this is an
    //   internal predicate, failure is treated from the Prolog
    //   wrapper code).

    /* Unify the parent end of the pipe (if needed) */
    bool_t unif_stdin, unif_stdout, unif_stderr;
    unif_stdin = unify_fd_pair(Arg, X(0), X(2), pr.pair_in, Write);
    unif_stdout = unify_fd_pair(Arg, X(0), X(3), pr.pair_out, Read);
    unif_stderr = unify_fd_pair(Arg, X(0), X(4), pr.pair_err, Read);

    return (unif_stdin && unif_stdout && unif_stderr &&
	    cunify(Arg, MakeSmall(pr.pid), X(8)));
  }
}

#if defined(_WIN32) || defined(_WIN64)
#define SHELL_ENV_NAME "COMSPEC"
#else
#define SHELL_ENV_NAME "SHELL"
#endif

/* Execute a command using the OS-specific shell (see @pred{shell/2}
   for details) */

/* TODO: Here we should give better errors!  However there is no
   leeway to return them within the ISO-defined exceptions.  Besides,
   when an exception involves a newly created process which is not
   able to accomplish its task, the (internally created) exception
   should force the process to finish as well.
*/

static int spawn_shell(char *shellcmd) {
  /* Lookup shellname in PATH (if needed) and check existence */
  char *shellname = getenv(SHELL_ENV_NAME);
  if (shellname == NULL) {  // This means an error if no SHELL
    SERIOUS_FAULT("No " SHELL_ENV_NAME " environment variable defined");
  }
  shellname = c_find_exec(shellname);
  if (shellname == NULL || access(shellname, X_OK) != 0) {
    SERIOUS_FAULT("Could not find executable file pointed by " SHELL_ENV_NAME " environment variable");
  }

  int args_n;
  char **args;
  if (shellcmd == NULL) {
    /* Shell without command */
    args_n = 1+1;
    args = checkalloc_ARRAY(char *, args_n);
    args[0] = shellname;
    args[1] = NULL;
  } else {
    /* Shell with a command */
#if defined(_WIN32) || defined(_WIN64) /* %COMSPEC% args */
    args_n = 4+1;
#else /* $SHELL args */
    args_n = 3+1;
#endif
    args = checkalloc_ARRAY(char *, args_n);
    args[0] = shellname;
#if defined(_WIN32) || defined(_WIN64) /* %COMSPEC% args */
    args[1] = "/s"; args[2] = "/c"; args[3] = shellcmd; args[4] = NULL;
#else /* $SHELL args */
    args[1] = "-c"; args[2] = shellcmd; args[3] = NULL;
#endif
  }

  /* Fill process struct */
  process_t pr;
  pr.cwd = NULL;
  pr.eng_fork = 0;
  pr.argv = args;
  pr.env = (string_pair_t[]){{NULL,NULL}};
  default_fd_pair(pr.pair_in);
  default_fd_pair(pr.pair_out);
  default_fd_pair(pr.pair_err);

  /* Create the process */
  c_create_process(&pr);
  
  free(shellname);
  checkdealloc_ARRAY(char *, args_n, args);

  if (pr.pid == -1) {
    SERIOUS_FAULT("Could not start process for new shell");
  }
  
  int status;
  if (c_waitpid(pr.pid, &status) != pr.pid) return -1;
  if (!WIFEXITED(status)) return -1;
  return WEXITSTATUS(status);
}

CBOOL__PROTO(prolog_unix_shell0)
{
  // ERR__FUNCTOR("system:shell", 0);
  int retcode = spawn_shell(NULL);
  /* TODO: retcode == -1 on error, we should throw exception */
  return (retcode == 0);
}

CBOOL__PROTO(prolog_unix_shell2)
{
  ERR__FUNCTOR("system:shell", 2);

  DEREF(X(0), X(0));

  /* check argument instantiation error */
  if (IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(0), 1);
  /* check type argument*/
  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0), 1, STRICT_ATOM);

  int retcode = spawn_shell(GetString(X(0)));
  /* TODO: retcode == -1 on error, we should throw exception */
  return cunify(Arg, MakeSmall(retcode), X(1));
}
