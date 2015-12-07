/*
 *  os_utils.h
 *
 *  Platform-independent interface to operating system calls
 *  (filesystem, environment, processes).
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *  Copyright (C) 2015 Jose F. Morales, Ciao Development Team
 */

#ifndef _CIAO_OS_UTILS_H
#define _CIAO_OS_UTILS_H

#include <stdlib.h> /* getenv */

/* The ...STKSIZE constants may be overridden by env. variables.
   This macro first looks for one, and if not found, uses the default. */
#define GETENV(VALUE,WORK,STRING,VAR) \
  if ((WORK = getenv(STRING))) \
    VALUE = atoi(WORK); \
  else \
    VALUE = VAR;

bool_t expand_file_name(const char *name, bool_t abs, char *target);
void compute_cwd(void);
char *c_find_exec(const char *cmd);

char *c_paths_insert_new(const char *envpath, const char *path);

bool_t using_windows(void);

CBOOL__PROTO(prolog_using_windows);
CBOOL__PROTO(prolog_exec);
CBOOL__PROTO(prolog_wait);
CBOOL__PROTO(prolog_kill);
CBOOL__PROTO(prolog_unix_cd);
CBOOL__PROTO(prolog_unix_shell0);
CBOOL__PROTO(prolog_unix_shell2);
CBOOL__PROTO(prolog_unix_argv);
CBOOL__PROTO(prolog_unix_exit);
CBOOL__PROTO(prolog_unix_mktemp);
CBOOL__PROTO(prolog_unix_access);
CBOOL__PROTO(prolog_directory_files);
CBOOL__PROTO(prolog_file_properties);
CBOOL__PROTO(prolog_touch);
CBOOL__PROTO(prolog_unix_chmod);
CBOOL__PROTO(prolog_unix_umask);
CBOOL__PROTO(prolog_unix_delete);
CBOOL__PROTO(prolog_unix_rename);
CBOOL__PROTO(prolog_unix_mkdir);
CBOOL__PROTO(prolog_unix_rmdir);
CBOOL__PROTO(prolog_current_host);
CBOOL__PROTO(prolog_c_environs);
CBOOL__PROTO(prolog_c_get_env);
CBOOL__PROTO(prolog_c_set_env);
CBOOL__PROTO(prolog_c_del_env);
CBOOL__PROTO(prolog_c_current_env);
CBOOL__PROTO(prolog_c_errno);
CBOOL__PROTO(prolog_c_strerror);
CBOOL__PROTO(prolog_c_copy_file);
CBOOL__PROTO(prolog_c_winpath);
CBOOL__PROTO(prolog_c_posixpath);
CBOOL__PROTO(prolog_c_winfile);
CBOOL__PROTO(prolog_c_posixfile);
CBOOL__PROTO(prolog_pause);
CBOOL__PROTO(prolog_getpid);
CBOOL__PROTO(prolog_getuid);
CBOOL__PROTO(prolog_getgid);
CBOOL__PROTO(prolog_getpwnam);
CBOOL__PROTO(prolog_getgrnam);
CBOOL__PROTO(prolog_find_file);
CBOOL__PROTO(prolog_path_is_absolute);
CBOOL__PROTO(prolog_expand_file_name);
CBOOL__PROTO(prolog_getarch);
CBOOL__PROTO(prolog_getos);
CBOOL__PROTO(prolog_eng_debug_level);
CBOOL__PROTO(prolog_eng_is_sharedlib);
CBOOL__PROTO(prolog_get_ciao_ext);
CBOOL__PROTO(prolog_get_exec_ext);
CBOOL__PROTO(prolog_get_so_ext);
CBOOL__PROTO(prolog_version);
CBOOL__PROTO(prolog_get_foreign_opts_cc);
CBOOL__PROTO(prolog_get_foreign_opts_ld);
CBOOL__PROTO(prolog_get_foreign_opts_ccshared);
CBOOL__PROTO(prolog_get_foreign_opts_ldshared);

#endif /* _CIAO_OS_UTILS_H */
