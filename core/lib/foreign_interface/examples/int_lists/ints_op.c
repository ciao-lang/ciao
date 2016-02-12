#include <stdlib.h>
#include <stdio.h>

void obtain_list(size_t n, size_t *l, int **s) {
  int i;
  int c;
  *l = n;
  *s = (int *)malloc((*l) * sizeof(int));
  for (i = 0; i < *l; i++) {
    (*s)[i] = i;
  }
}

void show_list(size_t l, int *s) {
  if (s) {
    int n;
    printf("From C:");
    for (n = 0; n < l; n++) {
      printf(" %d", s[n]);
    }
    printf(".\n");
  } else {
    printf("From C: []\n");
  }
}







