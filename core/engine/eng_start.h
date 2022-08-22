/*
 *  eng_start.h
 *
 *  Load and execute a bytecode executable.
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_ENG_START_H
#define _CIAO_ENG_START_H

#if !defined(OPTIM_COMP)
extern char **prolog_argv;
extern int prolog_argc;
CVOID__PROTO(load_ql_files, FILE *qfile);
worker_t *create_and_init_wam(void);
void create_source_path(char *pathname);
#endif

void engine_set_opts(const char **optv, int optc, const char **boot_path);
void engine_init(const char *boot_path, const char *emulator);
#if defined(OPTIM_COMP)
CVOID__PROTO(engine_finish);
#endif

int engine_start(int argc, char **argv);
void engine_exit(int exit_code);

#endif /* _CIAO_ENG_START_H */

