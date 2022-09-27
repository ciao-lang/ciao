/*
 *  ciao_prolog.c
 *
 *  Ciao/C API for extending or embedding Ciao in C programs (as part
 *  of the foreign function interface).
 *
 *  Copyright (C) 2002-2020 The Ciao Development Team
 */

#include <ciao_prolog.h>

#if defined(BSD) || defined(_WIN32) || defined(_WIN64)
/* alloca defined in malloc */
#else
#include <alloca.h>
#endif
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>

#if defined(__svr4__) || defined(DARWIN) || defined(BSD)
#include <unistd.h>                                            /* sbrk () */
#include <stdlib.h>                                           /* malloc() */
#else                                                            /* SunOS */
#include <sys/types.h>
#include <malloc.h>
#endif
#include <sys/param.h>

#if !defined(X_OK)
# define X_OK 1
#endif

#if !defined(OPTIM_COMP)
#include <ciao/eng.h>
#include <ciao/internals.h>
#include <ciao/eng_start.h>
#include <ciao/eng_profile.h>
#include <ciao/atomic_basic.h>
#include <ciao/eng_bignum.h>
#include <ciao/eng_gc.h>
#include <ciao/basiccontrol.h>
#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif
#define deffunctor(NAME, ARITY) SetArity(GET_ATOM((NAME)),(ARITY))
#define SetChoice(Chpt) ({ \
  w->previous_choice = (Chpt); /* needed? */ \
  w->choice = (Chpt); \
  SetShadowregs(w->choice); \
  PROFILE__HOOK_CUT; \
})
#define SetDeep() ({ w->next_alt = NULL; })
// TODO: missing test_choice_overflow
#define CODE_CHOICE_NEW(B, ALT) ({ \
  ComputeA(w->local_top,w->choice); /* get_frame_top */ \
  G->next_alt = (ALT); \
  (B) = GEN_ChoiceNext00(w->choice, GEN_ChoiceSize0(G)); \
  w->choice = (B); \
  NewShadowregs(G->heap_top); \
  (B)->trail_top = G->trail_top; \
  (B)->heap_top = G->heap_top; \
  (B)->local_top = G->local_top; \
})
#define CODE_NECK_TRY(B) ({ \
  (B)->frame = G->frame; \
  (B)->next_insn = G->next_insn; \
  (B)->next_alt = G->next_alt; \
  intmach_t arity = ChoiceArity(B); \
  for (int i = 0; i < arity; i++) { \
    (B)->x[i] = w->x[i]; \
  } \
})
#define CODE_ALLOC(Frame) ComputeE_((Frame))
// TODO: missing set event on frame overflow
#define CODE_CFRAME(Frame, NextInsn) ({ \
  (Frame)->next_insn = G->next_insn; \
  (Frame)->frame = G->frame; \
  G->frame = (Frame); \
  G->next_insn = (NextInsn); \
  G->local_top = (frame_t *)StackCharOffset((Frame),FrameSize(G->next_insn)); \
})
#define DEALLOCATE(Frame) ({ \
  G->next_insn = (Frame)->next_insn; \
  G->frame = (Frame)->frame; \
})
#define InvalidateLocalTop() G->local_top = NULL
#endif

void ciao_exit(int result);

/* ------------------------------------------------------------------------- */

ciao_ctx ciao_implicit_ctx;

/* ------------------------------------------------------------------------- */

extern char source_path[];

#if defined(OPTIM_COMP)
extern char *library_directory;
#else
extern char *ciaoroot_directory;
extern char *c_headers_directory;
#endif

/* Memory management routines --- now only interfaces to library, but they
   might evolve in access to a custom memory management library */

void *ciao_malloc(size_t size) {
  return malloc(size);
}

void ciao_free(void *pointer) {
  free(pointer);
}

/* Low level term operations */

ciao_term ciao_ref(ciao_ctx ctx, tagged_t x);
tagged_t ciao_unref(ciao_ctx ctx, ciao_term term);

#if defined(OPTIM_COMP)
struct _ciao_query {
  ciao_ctx ctx;
  ciao_choice base_choice;
};
#endif

/* ------------------------------------------------------------------------- */

void ciao_ensure_heap(ciao_ctx ctx, size_t cells) {
  WITH_WORKER(ctx->worker_registers, {
    TEST_HEAP_OVERFLOW(G->heap_top, cells*sizeof(tagged_t)+CONTPAD, 0);
  });
}

/* ------------------------------------------------------------------------- */
/* Initialization */

/* Parse options */
int ciao_opts(const char *program_name, int programc, const char **programv, int optc, const char **optv, const char **p_boot_path) {
  prolog_argc = programc;
  prolog_argv = (char **)programv;
  engine_set_opts(optv, optc, p_boot_path);
  return 0;
}

/* Initialization */

extern bcp_t call_code;
extern bcp_t default_code;
extern bcp_t null_code;
extern try_node_t nullgoal_alt;
extern try_node_t defaultgoal_alt;
extern try_node_t startgoal_alt;

/* (must be called after ciao_opts) */
void ciao_init(const char *boot_path) {
  engine_init(boot_path, NULL);
}

#if defined(OPTIM_COMP)
void ciao_finish(ciao_ctx ctx) {
  WITH_WORKER(ctx->worker_registers, {
    CVOID__CALL(engine_finish);
  });
}
#endif

/* Reinitialization */

void ciao_reinit(void) {
  glb_init_each_time();
}

/* ------------------------------------------------------------------------- */
/* Ciao context creation */

ciao_ctx ciao_ctx_new(void) {
#if defined(OPTIM_COMP)
  // TODO: see 'core' version
  return gimme_a_new_gd();
#else
  static int first = 1;
  if (first) {
    return init_first_gd_entry();
  } else {
    return gimme_a_new_gd();
  }
#endif
}

void ciao_ctx_free(ciao_ctx ctx) {
#if defined(OPTIM_COMP)
  // TODO: see 'core' version
  release_goal_desc(ctx);
#else
  if ((ctx->state != PENDING_SOLS) &&
      (ctx->state != FAILED))
    return; /* Trying to release a worker either working or without assigned work */

  make_goal_desc_free(ctx);
#endif
}

/* ------------------------------------------------------------------------- */
/* Code loading operations */

#if !defined(OPTIM_COMP)
bool_t expand_file_name(const char *name, bool_t abs, char *target);

FILE *ciao_open_qfile(const char *boot_path) {
  expand_file_name((char *)boot_path,TRUE,(char *)source_path);
#if defined(Win32)
  {
    int i = strlen(source_path)-4;
    if (i > 0 && strcmp(source_path+i,".bat") == 0){
      source_path[i+1] = 'c';
      source_path[i+2] = 'p';
      source_path[i+3] = 'x';
    } else if (i > 0 && strcmp(source_path+i,".cpx") != 0) {
      strcat(source_path,".cpx");
    }

    if (access(source_path,R_OK)) {
      source_path[strlen(source_path)-4] = '\0'; /* Take out .cpx */
    }
  }
#endif
  FILE *qfile = fopen(source_path,"rb");
  if (qfile == NULL) {
    fprintf(stderr, "%s: boot file not found\n", source_path);
    engine_exit(1);
    return NULL;
  } else { /* We have a bootfile we can read from */
    return qfile;
  }
}

void ciao_load_qfile_s(ciao_ctx ctx, const char *boot_path) {
  FILE *qfile;
  qfile = ciao_open_qfile(boot_path);
  load_ql_files(ctx->worker_registers, qfile);
  fclose(qfile);
}
#endif

#if defined(OPTIM_COMP)
CBOOL__PROTO(load_boot, const char *boot_path); /* eng_start.c */

void ciao_load_qfile_s(ciao_ctx ctx, const char *boot_path) {
  WITH_WORKER(ctx->worker_registers, {
    if (!CBOOL__SUCCEED(load_boot, boot_path)) {
      PANIC_FAULT("cannot load boot file");
    }
  });
}
#endif

void ciao_load_qfile(const char *boot_path) {
  ciao_load_qfile_s(ciao_implicit_ctx, boot_path);
}

/* Load a qfile embedded in an executable */

#if !defined(OPTIM_COMP)
FILE *ciao_open_embedded_qfile(const char *program_name);
void ciao_open_exec_skip_stub(const char *file, FILE **stream);

void ciao_load_embedded_qfile_s(ciao_ctx ctx, const char *program_name) {
  FILE *qfile;
  qfile = ciao_open_embedded_qfile(program_name);
  load_ql_files(ctx->worker_registers, qfile);
  fclose(qfile);
}

void ciao_load_embedded_qfile(const char *program_name) {
  ciao_load_embedded_qfile_s(ciao_implicit_ctx, program_name);
}

FILE *ciao_open_embedded_qfile(const char *program_name) {
  FILE *qfile = NULL;
  ciao_open_exec_skip_stub(program_name,&qfile);
  if (qfile == NULL) {
    fprintf(stderr,"%s: file not found\n", program_name);
    engine_exit(1);
  }
  expand_file_name((char *)program_name,TRUE,(char *)source_path);
  return qfile;
}

void ciao_open_exec_skip_stub(const char *file, FILE **stream) {
  /* TODO: see open_exec_skip_stub() in eng_start.c */
  fprintf(stderr,"{ERROR: ciao_open_exec_skip_stub() is not implemented yet}\n");
}
#endif

/* ------------------------------------------------------------------------- */
/* Term creation */

ciao_term ciao_var_s(ciao_ctx ctx) {
  tagged_t *pt;
  tagged_t to;
  ciao_ensure_heap(ctx, 1);
  WITH_WORKER(ctx->worker_registers, {
    pt = G->heap_top;
    to = Tagp(HVA, pt);
    HeapPush(pt, to);
    G->heap_top = pt;
  });
  return ciao_ref(ctx, to);
}

ciao_term ciao_var(void) {
  return ciao_var_s(ciao_implicit_ctx);
}

ciao_term ciao_structure_a_s(ciao_ctx ctx, const char *name, int arity, ciao_term *args) {
  tagged_t x;
  WITH_WORKER(ctx->worker_registers, {
    if (arity == 0) {
      x = GET_ATOM((char *)name);
    } else if (strcmp(name, ".") == 0 && arity == 2) {
      tagged_t list;
      ciao_ensure_heap(ctx, 3);
      MakeLST(list, ciao_unref(ctx, args[0]), ciao_unref(ctx, args[1]));
      x = list;
    } else {
      int i;
      tagged_t *pt;
      tagged_t *s;
      tagged_t functor;
      ciao_ensure_heap(ctx, 2 + arity);
      functor = deffunctor((char *)name, arity);
      pt = G->heap_top;
      s = pt;
      HeapPush(pt, functor);
      for (i = 0; i < arity; i++) {
        HeapPush(pt, ciao_unref(ctx, args[i]));
      }
      G->heap_top = pt;
      x = Tagp(STR, s);
    }
  });
  return ciao_ref(ctx, x);
}

ciao_term ciao_structure_a(const char *name, int arity, ciao_term *args) {
  return ciao_structure_a_s(ciao_implicit_ctx, name, arity, args);
}

#if defined(OPTIM_COMP)
#define LongToTagged(X) IntmachToTagged((X))
#define TaggedToLong(X) TaggedToIntmach((X))
#define BlobFunctorFixLong BlobFunctorFixIntmach
#endif

#define Def_ciao_mk_X(CType, DeclType, Cells, Make, CastType) \
ciao_term ciao_mk_##CType##_s(ciao_ctx ctx, DeclType x) {       \
  tagged_t t;                                                   \
  ciao_ensure_heap(ctx, Cells);                                 \
  WITH_WORKER(ctx->worker_registers, {                          \
    t = Make((CastType)x);                                      \
  });                                                           \
  return ciao_ref(ctx, t);                                      \
}                                                               \
ciao_term ciao_mk_##CType(DeclType x) {                 \
  return ciao_mk_##CType##_s(ciao_implicit_ctx, x); \
}

/* TODO: make sure that estimation for Cells is right */
Def_ciao_mk_X(c_short, short, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_int, int, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_long, long, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_ushort, unsigned short, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_uint, unsigned int, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_ulong, unsigned long, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_float, float, 4, BoxFloat, double)
Def_ciao_mk_X(c_double, double, 4, BoxFloat, double)
Def_ciao_mk_X(c_uintptr, uintptr_t, 4, IntmachToTagged, uintptr_t)
Def_ciao_mk_X(c_size, size_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_int8, int8_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_int16, int16_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_int32, int32_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_int64, int64_t, 4, IntmachToTagged, intmach_t) // TODO: WRONG in 32 bits
Def_ciao_mk_X(c_uint8, uint8_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_uint16, uint16_t, 4, IntmachToTagged, intmach_t)
Def_ciao_mk_X(c_uint32, uint32_t, 4, IntmachToTagged, intmach_t) // TODO: WRONG in 32 bits (sign bit)
Def_ciao_mk_X(c_uint64, uint64_t, 4, IntmachToTagged, intmach_t) // TODO: WRONG in 32 bits, WRONG in 64 bits (sign bit)

#define Def_ciao_get_X(CType, DeclType, Get) \
DeclType ciao_get_##CType##_s(ciao_ctx ctx, ciao_term term) {   \
  tagged_t t;                                                   \
  t = ciao_unref(ctx, term);                                    \
  DEREF(t, t);                                                  \
  return (DeclType)Get(t);                                      \
}                                                               \
DeclType ciao_get_##CType(ciao_term term) {                     \
  return ciao_get_##CType##_s(ciao_implicit_ctx, term); \
}

Def_ciao_get_X(c_short, short, TaggedToIntmach)
Def_ciao_get_X(c_int, int, TaggedToIntmach)
Def_ciao_get_X(c_long, long, TaggedToIntmach)
Def_ciao_get_X(c_ushort, unsigned short, TaggedToIntmach)
Def_ciao_get_X(c_uint, unsigned int, TaggedToIntmach)
Def_ciao_get_X(c_ulong, unsigned long, TaggedToIntmach)
Def_ciao_get_X(c_float, float, TaggedToFloat)
Def_ciao_get_X(c_double, double, TaggedToFloat)
Def_ciao_get_X(c_uintptr, uintptr_t, TaggedToIntmach)
Def_ciao_get_X(c_size, size_t, TaggedToIntmach)
Def_ciao_get_X(c_int8, int8_t, TaggedToIntmach)
Def_ciao_get_X(c_int16, int16_t, TaggedToIntmach)
Def_ciao_get_X(c_int32, int32_t, TaggedToIntmach)
Def_ciao_get_X(c_int64, int64_t, TaggedToIntmach) // TODO: WRONG in 32 bits
Def_ciao_get_X(c_uint8, uint8_t, TaggedToIntmach)
Def_ciao_get_X(c_uint16, uint16_t, TaggedToIntmach)
Def_ciao_get_X(c_uint32, uint32_t, TaggedToIntmach) // TODO: WRONG in 32 bits (sign bit)
Def_ciao_get_X(c_uint64, uint64_t, TaggedToIntmach) // TODO: WRONG in 32 bits, WRONG in 64 bits (sign bit)

/* TODO: Assumes LP64 data model (sizeof(long) == sizeof(int *) == sizeof(tagged_t)) */
ciao_bool ciao_fits_in_c_long_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
#if defined(OPTIM_COMP)
  return TaggedIsSmall(t) || (TaggedIsSTR(t) && TaggedToHeadfunctor(t) == BlobFunctorFixLong);
#else
  /* Pre: bignums is in canonical form (if more than one word is
     needed, it does not fit into an integer) */
  return TaggedIsSmall(t) || (IsInteger(t) && (bn_length(TaggedToBignum(t)) == 1));
#endif
}

ciao_bool ciao_fits_in_c_long(ciao_term term) {
  return ciao_fits_in_c_long_s(ciao_implicit_ctx, term);
}

#if tagged_size == 64
ciao_bool ciao_fits_in_c_int_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  intmach_t x;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  if (!TaggedIsSmall(t)) return FALSE;
#if defined(OPTIM_COMP)
  x = TaggedToIntmach(t);
#else
  x = GetSmall(t);
#endif
  return (x >= INT_MIN && x <= INT_MAX);
}
#else
ciao_bool ciao_fits_in_c_int_s(ciao_ctx ctx, ciao_term term) {
  return ciao_fits_in_c_long_s(ctx, term);
}
#endif
ciao_bool ciao_fits_in_c_int(ciao_term term) {
  return ciao_fits_in_c_int_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_variable_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return IsVar(t);
}

ciao_bool ciao_is_variable(ciao_term term) {
  return ciao_is_variable_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_integer_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return IsInteger(t);
}

ciao_bool ciao_is_integer(ciao_term term) {
  return ciao_is_integer_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_number_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return IsNumber(t);
}

ciao_bool ciao_is_number(ciao_term term) {
  return ciao_is_number_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_float_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return IsFloat(t);
}

ciao_bool ciao_is_float(ciao_term term) {
  return ciao_is_float_s(ciao_implicit_ctx, term);
}

char *ciao_get_number_chars_s(ciao_ctx ctx, ciao_term term) {
  tagged_t number;
  char *number_result;
  WITH_WORKER(ctx->worker_registers, {
    number = ciao_unref(ctx, term);
    /* number_to_string() handles all kinds of numbers; it leaves the
       result in Atom_Buffer */
    CVOID__CALL(number_to_string, number, GetSmall(current_radix));
    number_result = ciao_malloc(strlen(Atom_Buffer) + 1);
    strcpy(number_result, Atom_Buffer);
  });
  return number_result;
}

char *ciao_get_number_chars(ciao_term term) {
  return ciao_get_number_chars_s(ciao_implicit_ctx, term);
}

/* PRECONDITION: number_result should really represent a number */
/* TODO: raise a proper exception */
ciao_term ciao_put_number_chars_s(ciao_ctx ctx, char *number_string) {
  tagged_t result;
  WITH_WORKER(ctx->worker_registers, {
    (void)CBOOL__SUCCEED(string_to_number,
                         number_string,
                         GetSmall(current_radix),
                         &result,
                         0);
  });
  return ciao_ref(ctx, result);
}

ciao_term ciao_put_number_chars(char *number_string) {
  return ciao_put_number_chars_s(ciao_implicit_ctx, number_string);
}


ciao_bool ciao_is_atom_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return TaggedIsATM(t);
}

ciao_bool ciao_is_atom(ciao_term term) {
  return ciao_is_atom_s(ciao_implicit_ctx, term);
}

const char *ciao_atom_name_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  if (!TaggedIsATM(t)) {
    return (const char *)NULL;
  } else { 
    atom_t *atomptr;
    atomptr = TaggedToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_atom_name(ciao_term term) {
  return ciao_atom_name_s(ciao_implicit_ctx, term);
}

char *ciao_atom_name_dup_s(ciao_ctx ctx, ciao_term term) {
  const char *s2;
  char *s;
  s2 = ciao_atom_name_s(ctx, term);
  s = (char *)ciao_malloc(sizeof(char *) * (strlen(s2) + 1));
  strcpy(s, s2);
  return s;
}

char *ciao_atom_name_dup(ciao_term term) {
  return ciao_atom_name_dup_s(ciao_implicit_ctx, term);
}

const char *ciao_structure_name_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) {
    return (const char *)NULL;
  } else {
    tagged_t f = TaggedToHeadfunctor(t);
    return GetString(f);
  }
}

const char *ciao_structure_name(ciao_term term) {
  return ciao_structure_name_s(ciao_implicit_ctx, term);
}

int ciao_structure_arity_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) {
    return 0;
  } else { 
    tagged_t f;
    f = TaggedToHeadfunctor(t);
    return Arity(f);
  }
}

int ciao_structure_arity(ciao_term term) {
  return ciao_structure_arity_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_list_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return TaggedIsLST(t);
}

ciao_bool ciao_is_list(ciao_term term) {
  return ciao_is_list_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_empty_list_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return TaggedIsATM(t) && t == GET_ATOM("[]"); // TODO: use predefined atom
}

ciao_bool ciao_is_empty_list(ciao_term term) {
  return ciao_is_empty_list_s(ciao_implicit_ctx, term);
}

ciao_bool ciao_is_structure_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  return TaggedIsSTR(t);
}

ciao_bool ciao_is_structure(ciao_term term) {
  return ciao_is_structure_s(ciao_implicit_ctx, term);
}

ciao_term ciao_structure_arg_s(ciao_ctx ctx, ciao_term term, int i) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  if (!TaggedIsSTR(t)) return CIAO_ERROR;
  a = *TaggedToArg(t, i);
  return ciao_ref(ctx, a);
}

ciao_term ciao_structure_arg(ciao_term term, int i) {
  return ciao_structure_arg_s(ciao_implicit_ctx, term, i);
}

ciao_term ciao_list_head_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  a = *TaggedToCar(t);
  return ciao_ref(ctx, a);
}

ciao_term ciao_list_head(ciao_term term) {
  return ciao_list_head_s(ciao_implicit_ctx, term);
}

ciao_term ciao_list_tail_s(ciao_ctx ctx, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(ctx, term);
  DEREF(t, t);
  a = *TaggedToCdr(t);
  return ciao_ref(ctx, a);
}

ciao_term ciao_list_tail(ciao_term term) {
  return ciao_list_tail_s(ciao_implicit_ctx, term);
}

/* Helper functions */

ciao_term ciao_atom_s(ciao_ctx ctx, const char *name) {
  return ciao_structure_s(ctx, name, 0);
}

ciao_term ciao_atom(const char *name) {
  return ciao_atom_s(ciao_implicit_ctx, name);
}

ciao_term ciao_empty_list_s(ciao_ctx ctx) {
  return ciao_atom_s(ctx, "[]"); // TODO: use predefined atom
}

ciao_term ciao_empty_list(void) {
  return ciao_empty_list_s(ciao_implicit_ctx);
}

ciao_term ciao_list_s(ciao_ctx ctx, ciao_term head, ciao_term tail) {
  return ciao_structure_s(ctx, ".", 2, head, tail); // TODO: use predefined functor
}

ciao_term ciao_list(ciao_term head, ciao_term tail) {
  return ciao_list_s(ciao_implicit_ctx, head, tail);
}

ciao_term ciao_dlist_a_s(ciao_ctx ctx, int len, ciao_term *args, ciao_term tail) {
  /* PRECONDITION: len >= 1 */ 
  int i;
  ciao_term list;
  
  list = tail;
  for (i = len - 1; i >= 0; i--) {
    list = ciao_list_s(ctx, args[i], list);
  }

  return list;
}

ciao_term ciao_dlist_a(int len, ciao_term *args, ciao_term tail) {
  return ciao_dlist_a_s(ciao_implicit_ctx, len, args, tail);
}

ciao_term ciao_listn_a_s(ciao_ctx ctx, int len, ciao_term *args) {
  return ciao_dlist_a_s(ctx, len, args, ciao_empty_list_s(ctx));
}

ciao_term ciao_listn_a(int len, ciao_term *args) {
  return ciao_listn_a_s(ciao_implicit_ctx, len, args);
}

#define GETARGS(LENGTH) \
  ciao_term *args; \
  int i; \
  va_list p; \
  args = alloca(sizeof(ciao_term) * LENGTH); \
  va_start(p, LENGTH); /* last argument before '...' */ \
  for (i = 0; i < LENGTH; i++) { \
    args[i] = va_arg(p, ciao_term); \
  } \
  va_end(p);

ciao_term ciao_structure_s(ciao_ctx ctx, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a_s(ctx, name, arity, args);
}

ciao_term ciao_structure(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a(name, arity, args);
}

ciao_term ciao_listn_s(ciao_ctx ctx, size_t length, ...) {
  GETARGS(length)
  return ciao_listn_a_s(ctx, length, args);
}

ciao_term ciao_listn(size_t length, ...) {
  GETARGS(length)
  return ciao_listn_a(length, args);
}

ciao_term ciao_dlist_s(ciao_ctx ctx, size_t length, ...) {
  GETARGS(length)
  return ciao_dlist_a_s(ctx, length - 1, args, args[length - 1]);
}

ciao_term ciao_dlist(size_t length, ...) {
  GETARGS(length)
  return ciao_dlist_a(length - 1, args, args[length - 1]);
}


ciao_term ciao_copy_term_s(ciao_ctx src_ctx, ciao_term src_term, ciao_ctx dst_ctx) {
  tagged_t x;
  WITH_WORKER(dst_ctx->worker_registers, {
    x = CFUN__EVAL(cross_copy_term, ciao_unref(dst_ctx, src_term));
  });
  return ciao_ref(dst_ctx, x);
}

ciao_term ciao_copy_term(ciao_term src_term) {
  return ciao_copy_term_s(ciao_implicit_ctx, src_term, ciao_implicit_ctx);
}

ciao_bool ciao_unify_s(ciao_ctx ctx, ciao_term x, ciao_term y) {
  ciao_bool ok;
  WITH_WORKER(ctx->worker_registers, {
    ok = CBOOL__SUCCEED(cunify, ciao_unref(ctx, x), ciao_unref(ctx, y));
  });
  return ok;
}

ciao_bool ciao_unify(ciao_term x, ciao_term y) {
  return ciao_unify_s(ciao_implicit_ctx, x, y);
}

ciao_bool ciao_equal_s(ciao_ctx ctx, ciao_term x, ciao_term y) {
  tagged_t a, b;
  a = ciao_unref(ctx, x);
  b = ciao_unref(ctx, y);
  DEREF(a, a);
  DEREF(b, b);
  return a == b; // TODO: not exactly ==/2 builtin!
}

ciao_bool ciao_equal(ciao_term x, ciao_term y) {
  return ciao_equal_s(ciao_implicit_ctx, x, y);
}

void ciao_exit(int result) {
  engine_exit(result);
}

int ciao_firstgoal(ciao_ctx ctx, ciao_term goal) {
  int i;
  WITH_WORKER(ctx->worker_registers, {
    i = CFUN__EVAL(call_firstgoal, ciao_unref(ctx, goal), ctx);
  });
  return i;
}

int ciao_boot(ciao_ctx ctx) {
  return ciao_firstgoal(ctx, ciao_structure_s(ctx, "internals:boot", 0));
}

/* ------------------------------------------------------------------------- */

ciao_bool ciao_is_char_code_list(ciao_ctx ctx, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(ctx, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!TaggedIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

ciao_bool ciao_is_int_list(ciao_ctx ctx, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(ctx, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsInteger(car)) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

ciao_bool ciao_is_num_list(ciao_ctx ctx, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(ctx, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TaggedIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsNumber(car)) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

CFUN__PROTO(c_list_length, int, tagged_t list);

int ciao_list_length(ciao_ctx ctx, ciao_term term) {
  int len;
  WITH_WORKER(ctx->worker_registers, {
    tagged_t cdr = ciao_unref(ctx, term);
    len = CFUN__EVAL(c_list_length, cdr);
  });
  return len;
}

#define TEMPLATE(Name, X, XC) \
void Name(ciao_ctx ctx, ciao_term list, size_t length, X *array) { \
  size_t i; \
  tagged_t car, cdr; \
  cdr = ciao_unref(ctx, list); \
  DEREF(cdr, cdr); \
  for (i = 0; i < length; i++) { \
    DerefCar(car,cdr); \
    array[i] = XC(car); \
    DerefCdr(cdr,cdr); \
  } \
}
TEMPLATE(ciao_get_c_uint8_array_l, unsigned char, GetSmall)
TEMPLATE(ciao_get_c_int_array_l, int, TaggedToIntmach)
TEMPLATE(ciao_get_c_double_array_l, double, TaggedToFloat)
#undef TEMPLATE

#define TEMPLATE(Name, X, NameL) \
X *Name(ciao_ctx ctx, ciao_term list) { \
  X *array; \
  size_t length; \
  length = ciao_list_length(ctx, list); \
  if (length == 0) return NULL; /* sure? */ \
  array = (X *)ciao_malloc(sizeof(X) * length); \
  NameL(ctx, list, length, array); \
  return array; \
}
TEMPLATE(ciao_get_c_uint8_array, unsigned char, ciao_get_c_uint8_array_l)
TEMPLATE(ciao_get_c_int_array, int, ciao_get_c_int_array_l)
TEMPLATE(ciao_get_c_double_array, double, ciao_get_c_double_array_l)
#undef TEMPLATE

#define TEMPLATE(Name, X, XC, XS) \
ciao_term Name(ciao_ctx ctx, X *s, size_t length) { \
  size_t i; \
  tagged_t cdr; \
  WITH_WORKER(ctx->worker_registers, { \
    ciao_ensure_heap(ctx, length * XS); \
    cdr = atom_nil; \
    s += length; \
    for (i = 0; i < length; i++) { \
      s--; \
      MakeLST(cdr, XC, cdr); \
    } \
  }); \
  return ciao_ref(ctx, cdr); \
}
TEMPLATE(ciao_mk_c_uint8_list, const unsigned char, MakeSmall(*s), 2)
TEMPLATE(ciao_mk_c_int_list, int, IntmachToTagged((intmach_t)(*s)), 4)
TEMPLATE(ciao_mk_c_double_list, double, BoxFloat(*s), 8)
#undef TEMPLATE

/* ------------------------------------------------------------------------- */

char *ciao_list_to_str(ciao_ctx ctx, ciao_term list) {
  char *string;
  size_t length;
  length = ciao_list_length(ctx, list);
  string = (char *)ciao_malloc(sizeof(char) * (length + 1));
  ciao_get_c_uint8_array_l(ctx, list, length, (unsigned char *)string);
  string[length] = 0;
  return string;
}

ciao_term ciao_str_to_list(ciao_ctx ctx, const char *string) {
  size_t length;
  length = strlen(string);
  return ciao_mk_c_uint8_list(ctx, (unsigned char *)string, length);
}

/* ------------------------------------------------------------------------- */

ciao_term ciao_pointer_to_address(ciao_ctx ctx, void *pointer) {
  return ciao_structure_s(ctx, "$address", 1, ciao_mk_c_uintptr_s(ctx, (uintptr_t)pointer));
}

void *ciao_address_to_pointer(ciao_ctx ctx, ciao_term term) {
  return (void *)ciao_get_c_uintptr_s(ctx, ciao_structure_arg_s(ctx, term, 1));
}

ciao_bool ciao_is_address(ciao_ctx ctx, ciao_term term) {
  return (ciao_is_structure_s(ctx, term) && strcmp(ciao_structure_name_s(ctx, term), "$address") == 0 && ciao_structure_arity_s(ctx, term) == 1);
}

/* ------------------------------------------------------------------------- */

ciao_choice ciao_get_choice(ciao_ctx ctx) {
  ciao_choice c;
  WITH_WORKER(ctx->worker_registers, {
    c = ChoiceToTagged(w->choice);
  });
  return c;
}

ciao_bool ciao_more_solutions(ciao_ctx ctx, ciao_choice choice) {
  return ciao_get_choice(ctx) > choice;
}

void ciao_cut(ciao_ctx ctx, ciao_choice choice) {
  if (!ciao_more_solutions(ctx, choice)) return;
  WITH_WORKER(ctx->worker_registers, {
    choice_t *c = ChoiceFromTagged(choice);
    SetChoice(c);
  });
}

void ciao_fail(ciao_ctx ctx) {
  WITH_WORKER(ctx->worker_registers, {
    CVOID__CALL(wam, ctx);
  });
}

/* ------------------------------------------------------------------------- */

ciao_bool ciao_query_next(ciao_query *query) {
  if (!ciao_query_ok(query)) return FALSE;
  query->ctx->action = BACKTRACKING | KEEP_STACKS;
  ciao_fail(query->ctx);
  return ciao_query_ok(query);
}

ciao_bool ciao_query_ok(ciao_query *query) {
  ciao_bool ok;
  WITH_WORKER(query->ctx->worker_registers, {
    try_node_t *alt = G->next_alt;
    if (alt == &nullgoal_alt ||
        (alt == NULL && w->choice->next_alt == &nullgoal_alt)) {
      ok = FALSE;
    } else {
      ok = TRUE;
    }
  });
  return ok;
}

void ciao_query_end(ciao_query *query) {
  choice_t *b;
  ciao_ctx ctx = query->ctx;
  if (ciao_query_ok(query)) {
    ciao_cut(ctx, query->base_choice);
  }
  WITH_WORKER(ctx->worker_registers, {
    b = w->choice;
    b = ChoiceCont(b);
    SetChoice(b);
    SetDeep();
  });
  ciao_free(query);
}

ciao_query *ciao_query_begin_term_s(ciao_ctx ctx, ciao_term goal) {
  choice_t *b;
  ciao_query *query;

  goal = ciao_structure_s(ctx, "hiord_rt:call", 1, goal);

  WITH_WORKER(ctx->worker_registers, {
    DEREF(X(0), ciao_unref(ctx, goal)); /* Will be the arg. of a call/1 */

    /* push null choice */
    G->next_insn = default_code;
    CODE_CHOICE_NEW(b, &defaultgoal_alt);
    CODE_NECK_TRY(b);
    SetDeep();

    query = (ciao_query *)ciao_malloc(sizeof(ciao_query));
    query->ctx = ctx;
    query->base_choice = ciao_get_choice(ctx);

    /* push choice for starting goal */
    G->next_insn = call_code;
    CODE_CHOICE_NEW(b, &startgoal_alt);
    CODE_NECK_TRY(b);
    SetDeep();

    ctx->action = BACKTRACKING | KEEP_STACKS;
    CVOID__CALL(wam, ctx);
  });
  return query;
}

ciao_query *ciao_query_begin_term(ciao_term goal) {
  return ciao_query_begin_term_s(ciao_implicit_ctx, goal);
}

ciao_query *ciao_query_begin_s(ciao_ctx ctx, const char *name, int arity, ...) {
  GETARGS(arity);
  return ciao_query_begin_term_s(ctx, ciao_structure_a_s(ctx, name, arity, args));
}

ciao_query *ciao_query_begin(const char *name, int arity, ...) {
  GETARGS(arity);
  return ciao_query_begin_term(ciao_structure_a(name, arity, args));
}

ciao_bool ciao_commit_call_term_s(ciao_ctx ctx, ciao_term goal) {
  ciao_bool ok;
  ciao_query *query;

  query = ciao_query_begin_term_s(ctx, goal);
  ok = ciao_query_ok(query);
  ciao_query_end(query);

  return ok;
}

ciao_bool ciao_commit_call_term(ciao_term goal) {
  return ciao_commit_call_term_s(ciao_implicit_ctx, goal);
}

ciao_bool ciao_commit_call_s(ciao_ctx ctx, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term_s(ctx, ciao_structure_a_s(ctx, name, arity, args));
}

ciao_bool ciao_commit_call(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term(ciao_structure_a(name, arity, args));
}

/* ------------------------------------------------------------------------- */

#if !defined(OPTIM_COMP)
/* True if this query has been suspended with internals:'$yield'/0 [EXPERIMENTAL] */
bool_t ciao_query_suspended(ciao_query *query) {
  goal_descriptor_t *ctx = query->ctx;
  bool_t b;
  WITH_WORKER(ctx->worker_registers, {
    b = IsSuspendedGoal(w);
  });
  return b;
}

/* Resume the execution of query suspended with internals:'$yield'/0 [EXPERIMENTAL] */
void ciao_query_resume(ciao_query *query) {
  goal_descriptor_t *ctx = query->ctx;
  WITH_WORKER(ctx->worker_registers, {
    Stop_This_Goal(w) = FALSE;
    SetSuspendedGoal(w, FALSE);
    UnsetEvent(); // TODO: SetEvent() usage
    CVOID__CALL(wam, ctx); // (continue with '$yield' alternative, resume execution)
  });
}
#endif

/* ------------------------------------------------------------------------- */

jmp_buf ciao_gluecode_jmpbuf;

void ciao_raise_exception_s(ciao_ctx ctx, ciao_term exception) {
  WITH_WORKER(ctx->worker_registers, {
    X(0) = ciao_unref(ctx, exception);
  });
  longjmp(ciao_gluecode_jmpbuf, 1);
}

void ciao_raise_exception(ciao_term exception) {
  ciao_raise_exception_s(ciao_implicit_ctx, exception);
}

/* ------------------------------------------------------------------------- */

#define REF_TABLE_PAD 4
#define REF_TABLE_CHUNK_SIZE 32
#define REF_TABLE_CHUNKS 1

tagged_t create_ref_table(ciao_ctx ctx, int chunks) {
  int i, j;
  tagged_t *pt, *pt0;
  tagged_t functor;
  tagged_t x;

  ciao_ensure_heap(ctx, REF_TABLE_CHUNK_SIZE * chunks + 1);
  WITH_WORKER(ctx->worker_registers, {
    functor = deffunctor("$reftable", REF_TABLE_CHUNK_SIZE - 1); // TODO: GET_ATOM is slow
    pt = G->heap_top;
    pt0 = pt;
    for (j = 0; j < chunks - 1; j++) {
      HeapPush(pt, functor);
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
        HeapPush(pt, Tagp(HVA, pt));
      }
      HeapPush(pt, Tagp(STR, pt + 1));
    }
    if (chunks > 0) {
      HeapPush(pt, functor);
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
        HeapPush(pt, Tagp(HVA, pt));
      }
    }
    G->heap_top = pt;
    x = Tagp(STR, pt0);
  });
  return x;
}

CFUN__PROTO(ciao_ref_w, ciao_term, ciao_ctx ctx, tagged_t x) {
  ciao_term term;
  frame_t *frame;
  int next, chunks;

  frame = G->frame;

  next = GetSmall(frame->x[0]);
  {
    tagged_t ta;
    ta = *TaggedToArg(frame->x[2], next);
    if (ta!=x) {
      if (!CBOOL__SUCCEED(cunify,ta,x)) { goto fail; }
    }
    goto ok;
  }
 fail:
  /* fatal error */
  SERIOUS_FAULT("Error registering term");
 ok:
  term = next;
  next++;
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == (REF_TABLE_CHUNK_SIZE - 1)) 
    next++; /* skip functor */
  if ((next & (REF_TABLE_CHUNK_SIZE - 1)) == 0) 
    next++; /* skip str tag */
  frame->x[0] = MakeSmall(next);

  chunks = GetSmall(frame->x[1]);
  if (chunks * REF_TABLE_CHUNK_SIZE - next < REF_TABLE_PAD) {
    /* chunk overflow! */
    tagged_t *x, *y;
    tagged_t new_table;
    int i, j, new_chunks, k;

    new_chunks = chunks * 2;
    /* old table is in frame->x[2] so don't care about gc here */  
    new_table = create_ref_table(ctx, new_chunks); 

    x = TaggedToArg(frame->x[2], 0);
    y = TaggedToArg(new_table, 0);
    k = 0;
    for (j = 0; j < chunks - 1; j++) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
        y[k] = x[k];
        if (k == term) goto end;
        k++;
      }
      k++;
    }
    if (chunks > 0) {
      k++;
      for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
        y[k] = x[k];
        if (k == term) goto end;
        k++;
      }
    }
  end:
    frame->x[2] = new_table;
    frame->x[1] = MakeSmall(new_chunks);
  }
  CFUN__PROCEED(term);
}

ciao_term ciao_ref(ciao_ctx ctx, tagged_t x) {
  ciao_term term;
  WITH_WORKER(ctx->worker_registers, {
    term = CFUN__EVAL(ciao_ref_w, ctx, x);
  });
  return term;
}

tagged_t ciao_unref(ciao_ctx ctx, ciao_term term) {
  tagged_t x;
  WITH_WORKER(ctx->worker_registers, {
    frame_t *frame = G->frame;
    x = *TaggedToArg(frame->x[2], term);
  });
  return x;
}

// TODO: deprecate?
ciao_term ciao_refer(tagged_t x) {
  return ciao_ref(ciao_implicit_ctx,x);
}
tagged_t ciao_unrefer(ciao_term term) {
  return ciao_unref(ciao_implicit_ctx,term);
}

void ciao_frame_begin_s(ciao_ctx ctx) {
  WITH_WORKER(ctx->worker_registers, {
    frame_t *frame;
    int arity;
    arity = 3;
    CODE_ALLOC(frame);
    CODE_CFRAME(frame, CONTCODE(arity));
    frame->x[0] = MakeSmall(1); /* next free ref */
    frame->x[1] = MakeSmall(REF_TABLE_CHUNKS); /* chunks */
    frame->x[2] = create_ref_table(ctx, REF_TABLE_CHUNKS);
  });
}

void ciao_frame_begin(void) {
  ciao_frame_begin_s(ciao_implicit_ctx);
}

void ciao_frame_end_s(ciao_ctx ctx) {
  WITH_WORKER(ctx->worker_registers, {
    frame_t *a;
    a = G->frame; 
    InvalidateLocalTop(); /* SetLocalTop(a); <= only if there are no pendign choices */
    DEALLOCATE(a);
  });
}

void ciao_frame_end(void) {
  ciao_frame_end_s(ciao_implicit_ctx);
}

//static ciao_ctx ciao_aux_ctx;

void ciao_frame_re_begin(ciao_ctx ctx) {
//  ciao_aux_ctx = ciao_implicit_ctx; //re-entry
  ciao_implicit_ctx = ctx;
  ciao_frame_begin();
}
void ciao_frame_re_end(void) {
  ciao_frame_end();
//  ciao_implicit_ctx = ciao_aux_ctx;
}
