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

extern char *eng_architecture;

/* get_arch(?ArchName) */
CBOOL__PROTO(prolog_getarch) {
  CBOOL__LASTUNIFY(GET_ATOM(eng_architecture), X(0));
}

extern char *eng_os;

/* get_os(?OsName) */
CBOOL__PROTO(prolog_getos) {
  CBOOL__LASTUNIFY(GET_ATOM(eng_os), X(0));
}

extern char *eng_debug_level;

/* eng_debug_level(?DebugLevel) */
CBOOL__PROTO(prolog_eng_debug_level) {
  CBOOL__LASTUNIFY(GET_ATOM(eng_debug_level), X(0));
}

extern int eng_is_sharedlib;

CBOOL__PROTO(prolog_eng_is_sharedlib) {
  return eng_is_sharedlib;
}

extern char *ciao_suffix;
extern char *exec_suffix;
extern char *so_suffix;

CBOOL__PROTO(prolog_get_ciao_ext) {
  CBOOL__LASTUNIFY(GET_ATOM(ciao_suffix), X(0));
}

CBOOL__PROTO(prolog_get_exec_ext) {
  CBOOL__LASTUNIFY(GET_ATOM(exec_suffix), X(0));
}

CBOOL__PROTO(prolog_get_so_ext) {
  CBOOL__LASTUNIFY(GET_ATOM(so_suffix), X(0));
}

extern char *foreign_opts_cc;
extern char *foreign_opts_ld;
extern char *foreign_opts_ccshared;
extern char *foreign_opts_ldshared;

#define DEF_PROLOG_GET_STR(NAME) \
CBOOL__PROTO(prolog_get_##NAME) { \
  CBOOL__LASTUNIFY(GET_ATOM(NAME), X(0)); \
}

DEF_PROLOG_GET_STR(foreign_opts_cc);
DEF_PROLOG_GET_STR(foreign_opts_ld);
DEF_PROLOG_GET_STR(foreign_opts_ccshared);
DEF_PROLOG_GET_STR(foreign_opts_ldshared);

