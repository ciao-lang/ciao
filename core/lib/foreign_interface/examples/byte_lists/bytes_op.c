#include <stdlib.h>
#include <stdio.h>

void obtain_list(int n, size_t *l, unsigned char **s) {
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (unsigned char *)malloc(*l);
  for (i = 0; i < *l; i++) {
    (*s)[i] = i;
  }
}

void show_list(size_t l, unsigned char *s) {
  if (s) {
    size_t n;
    printf("From C:");
    for (n = 0; n < l; n++) {
      printf(" %d", s[n]);
    }
    printf(".\n");
  } else {
    printf("From C: []\n");
  }
}







