#include <stdlib.h>
#include <stdio.h>

char *get_static_str(void) {
  return "this is a string Prolog should not free";
}

char *get_str(int n) {
  char *s;
  int size;
  int i;
  int c;
  if (n < 0) n = -n;
  size = (n%4) + 5;
  s = (char *)malloc(size+1);
  for (i = 0, c = ((i + n) % ('z' - 'a' + 1)) + 'a'; i < size; i++,c++) {
    if (c > 'z') c = 'a'; 
    s[i] = c;
  }
  s[i] = 0;
  return s;
}

void put_str(char *s) {
  if (s) {
    printf("From C: \"%s\"\n", s);
  } else {
    printf("From C: null\n");
  }
}







