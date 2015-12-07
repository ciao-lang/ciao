/*
 *  start.h
 *
 *  Load and execute a bytecode executable.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_START_H
#define _CIAO_START_H

extern char **prolog_argv;
extern int prolog_argc;

CVOID__PROTO(load_ql_files, FILE *qfile);
worker_t *create_and_init_wam(void);
void create_source_path(char *pathname);
int start(int argc, char **argv);
void at_exit(int exit_code);

void engine_set_opts(const char **optv, int optc, const char **boot_path);
void engine_init(const char *boot_path, const char *emulator);

#endif /* _CIAO_START_H */
