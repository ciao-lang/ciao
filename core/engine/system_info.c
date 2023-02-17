/*
 *  system_info.c
 *
 *  Runtime system information
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_registry.h>
#endif

#define DEF_PROLOG_GET_STR(NAME, VAR) \
CBOOL__PROTO(prolog_##NAME) { \
  CBOOL__LASTUNIFY(GET_ATOM(VAR), X(0)); \
}

#if defined(OPTIM_COMP)
extern char *library_directory;
DEF_PROLOG_GET_STR(ciao_lib_dir, library_directory);
#endif

extern char *eng_architecture;
/* get_arch(?ArchName) */
DEF_PROLOG_GET_STR(getarch, eng_architecture);

extern char *eng_os;
/* get_os(?OsName) */
DEF_PROLOG_GET_STR(getos, eng_os);

#if !defined(OPTIM_COMP)
extern char *eng_debug_level;
/* eng_debug_level(?DebugLevel) */
DEF_PROLOG_GET_STR(eng_debug_level, eng_debug_level);
#endif

#if !defined(OPTIM_COMP)
extern int eng_is_sharedlib;
CBOOL__PROTO(prolog_eng_is_sharedlib) {
  return eng_is_sharedlib;
}
#endif

#if !defined(OPTIM_COMP)
extern char *ciao_suffix;
extern char *exec_suffix;
extern char *so_suffix;
DEF_PROLOG_GET_STR(get_ciao_ext, ciao_suffix);
DEF_PROLOG_GET_STR(get_exec_ext, exec_suffix);
DEF_PROLOG_GET_STR(get_so_ext, so_suffix);
#endif

extern char *foreign_opts_cc;
extern char *foreign_opts_ld;
extern char *foreign_opts_ccshared;
extern char *foreign_opts_ldshared;
#if defined(OPTIM_COMP)
DEF_PROLOG_GET_STR(get_so_cc, foreign_opts_cc);
DEF_PROLOG_GET_STR(get_so_ld, foreign_opts_ld);
DEF_PROLOG_GET_STR(get_so_cc_opts, foreign_opts_ccshared);
DEF_PROLOG_GET_STR(get_so_ld_opts, foreign_opts_ldshared);
#else
DEF_PROLOG_GET_STR(get_foreign_opts_cc, foreign_opts_cc);
DEF_PROLOG_GET_STR(get_foreign_opts_ld, foreign_opts_ld);
DEF_PROLOG_GET_STR(get_foreign_opts_ccshared, foreign_opts_ccshared);
DEF_PROLOG_GET_STR(get_foreign_opts_ldshared, foreign_opts_ldshared);
#endif

#if defined(OPTIM_COMP)
// extern char *emulator__so_libs; // TODO: deprecate
// DEF_PROLOG_GET_STR(get_so_libs, emulator__so_libs);
DEF_PROLOG_GET_STR(get_so_libs, "");
DEF_PROLOG_GET_STR(get_exec_ext, "");
#endif
