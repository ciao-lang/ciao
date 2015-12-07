#include <stdio.h>
#include <time.h>

time_t init_time;

void init(void) {
  printf("This is the initialization function.\n");
  init_time = time(NULL);
}

void print_time(void) {
  printf("This module was loaded at %s\n", ctime(&init_time));
}






