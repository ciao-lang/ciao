/*
 *  eng_main.c
 *
 *  Main file for an executable. It just calls the entry point, which
 *  can as well be called by any other executable to load and start a
 *  Ciao engine.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <ciao/eng.h>
#if !defined(OPTIM_COMP)
#include <ciao/eng_start.h>
#endif

#if !defined(OPTIM_COMP)
#include <stdlib.h> /* atoi() */

/* String that holds the size of the engine executable stub. The last
   characters are patched by fix_size.c */
char eng_stub_length_holder[] = "This emulator executable has a size of        ";

void eng_stub_set_length(int length);
#endif

int main(int argc, char *argv[]) {
#if !defined(OPTIM_COMP)
  int len = atoi(&eng_stub_length_holder[38]);
  eng_stub_set_length(len);
#endif
  engine_exit(engine_start(argc, argv));
  return 0;
}
