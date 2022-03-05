#include <stdio.h>
#include <string.h>
#include <ciao_prolog.h>
#include "newtype_c.h"

ciao_term ciao_newtype_to_term(ciao_ctx ctx, newtype_t p) {
  return ciao_structure_s(ctx, "$newtype", 2,
                          ciao_mk_c_int_s(ctx, p.a),
                          ciao_mk_c_int_s(ctx, p.b));
}

newtype_t ciao_term_to_newtype(ciao_ctx ctx, ciao_term term) {
  newtype_t p;
  p.a = ciao_get_c_int_s(ctx, ciao_structure_arg_s(ctx, term, 1));
  p.b = ciao_get_c_int_s(ctx, ciao_structure_arg_s(ctx, term, 2));
  return p;
}

ciao_bool ciao_is_newtype(ciao_ctx ctx, ciao_term term) {
  return (ciao_is_structure_s(ctx, term) &&
          strcmp(ciao_structure_name_s(ctx, term), "$newtype") == 0 &&
          ciao_structure_arity_s(ctx, term) == 2);
}

newtype_t newtype_mk(int a, int b) {
  newtype_t p;
  p.a = a;
  p.b = b;
  return p;
}

void newtype_show(newtype_t p) {
  printf("[from C] newtype(%d,%d)\n", p.a, p.b);
}
