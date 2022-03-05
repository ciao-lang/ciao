#if !defined(__NEWTYPE_C__)
#define __NEWTYPE_C__ 1

#include <ciao/ciao_gluecode.h>

typedef struct _newtype {
  int a;
  int b;
} newtype_t;

ciao_term ciao_newtype_to_term(ciao_ctx ctx, newtype_t p);

newtype_t ciao_term_to_newtype(ciao_ctx ctx, ciao_term term);

ciao_bool ciao_is_newtype(ciao_ctx ctx, ciao_term term);

#endif /* __NEWTYPE_C__ */
