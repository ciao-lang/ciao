/*
 *  main.c
 *
 *  Main file for an executable. It just calls the entry point, which
 *  can as well be called by any other executable to load and start a
 *  Ciao engine.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <stdlib.h> /* atoi() */

/* String that holds the size of the engine executable stub. The last
   characters are patched by fix_size.c */
char eng_stub_length_holder[] = "This emulator executable has a size of        ";

void eng_stub_set_length(int length);
void at_exit(int exit_code);
int start(int argc, char **argv);

int main(int argc, char *argv[])
{
  int len = atoi(&eng_stub_length_holder[38]);
  eng_stub_set_length(len);
  at_exit(start(argc, argv));
  return 0;
}
