#include <string.h>
#include <ciao_prolog.h>

int codes_to_number_c(char *s) {
  char *endptr;
  int n;
  n = strtol(s, &endptr, 10);
  if (endptr == NULL || *endptr != '\0') {
    ciao_raise_exception(ciao_structure("codes_to_number_exception", 
                                        1,
                                        ciao_atom(s)));
  }
  return n;
}


