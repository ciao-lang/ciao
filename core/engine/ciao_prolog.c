/*
 *  ciao_prolog.c
 *
 *  Ciao/C API for extending or embedding Ciao in C programs (as part
 *  of the foreign function interface).
 *
 *  Copyright (C) 2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Developer Team
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

#include <ciao/datadefs.h>
#include <ciao/task_areas.h>
#include <ciao/wam.h>
#include <ciao/tasks.h>
#include <ciao/start.h>
#include <ciao/startgoal.h>
#include <ciao/profile_hooks.h>
#include <ciao/term_support.h>
#include <ciao/bignum.h>
#include <ciao/stacks.h>
#include <ciao/wam_macros.h>

#include <ciao/instrdefs.h> /* for ciao_initcode() */

#include <ciao/os_utils.h>

#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif

#include <string.h>

void ciao_at_exit(int result);

/*---------------------------------------------------------------------------*/

ciao_state ciao_implicit_state;

/*---------------------------------------------------------------------------*/

extern char source_path[];
extern bool_t interactive_flag_bool;

extern char *library_directory;
extern char *c_headers_directory;

/* Memory management routines --- now only interfaces to library, but they
   might evolve in access to a custom memory management library */

void *ciao_malloc(int size) {
  return malloc(size);
}

void ciao_free(void *pointer){
  free(pointer);
}


/* Low level term operations */

ciao_term ciao_ref(ciao_state state, tagged_t x);
tagged_t ciao_unref(ciao_state state, ciao_term term);

/* ------------------------------------------------------------------------- */

void ciao_ensure_heap(ciao_state state, int cells) {
  worker_t *w = state->worker_registers;
  ENSURE_HEAP(cells, 0);
}

/* ------------------------------------------------------------------------- */

ciao_bool ciao_is_char_code_list(ciao_state state, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!TagIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) break;
    DerefCdr(cdr,cdr);
  }
  return cdr == atom_nil;
}

int ciao_is_int_list(ciao_state state, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsInteger(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? 1 : 0;
}

int ciao_is_double_list(ciao_state state, ciao_term term) {
  tagged_t cdr, car;

  cdr = ciao_unref(state, term);
  DEREF(cdr, cdr);

  while (cdr != atom_nil) {
    if (IsVar(cdr)) break;
    if (!TagIsLST(cdr)) break;
    DerefCar(car,cdr);
    if (IsVar(car)) break;
    if (!IsNumber(car)) break;
    DerefCdr(cdr,cdr);
  }
  return (cdr==atom_nil) ? 1 : 0;
}

int ciao_list_length(ciao_state state, ciao_term term) {
  worker_t *w = state->worker_registers;
  tagged_t cdr = ciao_unref(state, term);
  CFUN__LASTCALL(c_list_length, cdr);
}

#define TEMPLATE(Name, X, XC) \
void Name(ciao_state state, ciao_term list, int length, X *array) { \
  int i; \
  tagged_t car, cdr; \
  cdr = ciao_unref(state, list); \
  DEREF(cdr, cdr); \
  for (i = 0; i < length; i++) { \
    DerefCar(car,cdr); \
    array[i] = XC(car); \
    DerefCdr(cdr,cdr); \
  } \
}
TEMPLATE(ciao_list_to_byte_array_l, unsigned char, GetSmall)
TEMPLATE(ciao_list_to_int_array_l, int, GetInteger)
TEMPLATE(ciao_list_to_double_array_l, double, GetFloat)
#undef TEMPLATE

#define TEMPLATE(Name, X, NameL) \
X *Name(ciao_state state, ciao_term list) { \
  X *array; \
  int length; \
  length = ciao_list_length(state, list); \
  if (length == 0) return NULL; /* sure? */ \
  array = (X *)ciao_malloc(sizeof(X) * length); \
  NameL(state, list, length, array); \
  return array; \
}
TEMPLATE(ciao_list_to_byte_array, unsigned char, ciao_list_to_byte_array_l)
TEMPLATE(ciao_list_to_int_array, int, ciao_list_to_int_array_l)
TEMPLATE(ciao_list_to_double_array, double, ciao_list_to_double_array_l)
#undef TEMPLATE

char *ciao_list_to_str(ciao_state state, ciao_term list) {
  char *string;
  int length;
  length = ciao_list_length(state, list);
  string = (char *)ciao_malloc(sizeof(char) * (length + 1));
  ciao_list_to_byte_array_l(state, list, length, (unsigned char *)string);
  string[length] = 0;
  return string;
}

#define TEMPLATE(Name, X, XC, XS) \
ciao_term Name(ciao_state state, X *s, int length) { \
  worker_t *w = state->worker_registers; \
  int i; \
  tagged_t cdr; \
  ciao_ensure_heap(state, length * XS); \
  cdr = atom_nil; \
  s += length; \
  for (i = 0; i < length; i++) { \
    s--; \
    MakeLST(cdr, XC, cdr); \
  } \
  return ciao_ref(state, cdr); \
}
TEMPLATE(ciao_byte_listn, const unsigned char, MakeSmall(*s), 2)
TEMPLATE(ciao_int_listn, int, MakeInteger(w, (intmach_t)(*s)), 4)
TEMPLATE(ciao_double_listn, double, MakeFloat(w, *s), 8)
#undef TEMPLATE

ciao_term ciao_str_to_list(ciao_state state, const char *string) {
  int length;
  length = strlen(string);
  return ciao_byte_listn(state, (unsigned char *)string, length);
}

/* ------------------------------------------------------------------------- */

ciao_term ciao_intptr_s(ciao_state state, intptr_t i) {
  worker_t *w = state->worker_registers;
  ciao_ensure_heap(state, 4);
  return ciao_ref(state, MakeInteger(w, (intptr_t)i));
}

ciao_term ciao_intptr(intptr_t i) {
  return ciao_integer_s(ciao_implicit_state, i);
}

intptr_t ciao_to_intptr_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_intptr(state, term) */
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return GetInteger(t);
}

intptr_t ciao_to_intptr(ciao_term term) {
  return ciao_to_intptr_s(ciao_implicit_state, term);
}

ciao_term ciao_pointer_to_address(ciao_state state, void *pointer) {
  return ciao_structure_s(state, "$address", 1, ciao_intptr_s(state, (intptr_t)pointer));
}

void *ciao_address_to_pointer(ciao_state state, ciao_term term) {
  return (void *)ciao_to_intptr_s(state, ciao_structure_arg_s(state, term, 1));
}

ciao_bool ciao_is_address(ciao_state state, ciao_term term) {
  return (ciao_is_structure_s(state, term) && strcmp(ciao_structure_name_s(state, term), "$address") == 0 && ciao_structure_arity_s(state, term) == 1);
}

/* ------------------------------------------------------------------------- */
/* Initialization */

extern bool_t quiet_flag_bool;

/* Parse options */
int ciao_opts(const char *program_name, int programc, const char **programv, int optc, const char **optv, const char **p_boot_path) {
  prolog_argc = programc;
  prolog_argv = (char **)programv;
  engine_set_opts(optv, optc, p_boot_path);
  return 0;
}

/* Initialization */

bcp_t call_code;
bcp_t default_code;
bcp_t null_code;
try_node_t nullgoal_alt;
try_node_t defaultgoal_alt;
try_node_t startgoal_alt;

void ciao_initcode(void)
{
  int frame_size = 3;

  {
    bcp_t P;
    call_code = (bcp_t)checkalloc_ARRAY(char, 8*sizeof(tagged_t)); /* TODO: size overapprox. */
    P = call_code;
    EMIT_Q(0);
    EMIT_e((EToY0+frame_size)*sizeof(tagged_t));
    call_code = P;
    EMIT_o(CALLQ);
    EMIT_Q(0);
    EMIT_E(address_call);
    EMIT_e((EToY0+frame_size)*sizeof(tagged_t));                    /* initial FrameSize */
    EMIT_o(EXIT_TOPLEVEL);

    startgoal_alt.node_offset = ArityToOffset(1);
    startgoal_alt.number = 0;
    startgoal_alt.emul_p = call_code;
    startgoal_alt.emul_p2 = call_code;
    startgoal_alt.next = NULL;
  }

  {
    bcp_t P;
    null_code = (bcp_t)checkalloc_ARRAY(char, 8*sizeof(tagged_t)); /* TODO: size overapprox. */
    P = null_code;
    EMIT_o(EXIT_TOPLEVEL);

    nullgoal_alt.node_offset = ArityToOffset(0);
    nullgoal_alt.number = 0;
    nullgoal_alt.emul_p = null_code;
    nullgoal_alt.emul_p2 = null_code;
    nullgoal_alt.next = &nullgoal_alt; /* loop forever */
  }

  {
    bcp_t P;
    default_code = (bcp_t)checkalloc_ARRAY(char, 8*sizeof(tagged_t)); /* TODO: size overapprox. */
    P = default_code;
    EMIT_Q(0);
    EMIT_e((EToY0+frame_size)*sizeof(tagged_t));
    default_code = P;
    EMIT_o(EXIT_TOPLEVEL);

    defaultgoal_alt.node_offset = ArityToOffset(0);
    defaultgoal_alt.number = 0;
    defaultgoal_alt.emul_p = default_code;
    defaultgoal_alt.emul_p2 = default_code;
    defaultgoal_alt.next = &nullgoal_alt; 
  }
}

/* (must be called after ciao_opts) */
void ciao_init(const char *boot_path) 
{
  engine_init(boot_path, NULL);
}

/* Reinitialization */

void ciao_reinit(void) 
{
  glb_init_each_time();
}

/* ------------------------------------------------------------------------- */
/* WAM creation */

//static ciao_state ciao_aux_state;

void init_state_from_WAM(ciao_state state)
{
//  ciao_aux_state = ciao_implicit_state; //re-entry
  ciao_implicit_state = state;
  ciao_frame_begin();
}

ciao_state ciao_state_new(void) {
  static int first = 1;
  if (first) {
    return init_first_gd_entry();
  } else {
    return gimme_a_new_gd();
  }
}

void ciao_state_free(ciao_state state) {
  if ((state->state != PENDING_SOLS) &&
      (state->state != FAILED))
    return; /* Trying to release a worker either working or without assigned work */

  make_goal_desc_free(state);
}

/* ------------------------------------------------------------------------- */
/* WAM operations */

void ciao_load_ql_files(ciao_state state, FILE *qfile) {
  worker_t *w = state->worker_registers;
  load_ql_files(Arg, qfile);
}

/* Load a qfile */

FILE *ciao_open_qfile(const char *boot_path);

void ciao_load_qfile_s(ciao_state state, const char *boot_path) {
  FILE *qfile;
  qfile = ciao_open_qfile(boot_path);
  ciao_load_ql_files(state, qfile);
  fclose(qfile);
}

void ciao_load_qfile(const char *boot_path) {
  ciao_load_qfile_s(ciao_implicit_state, boot_path);
}

FILE *ciao_open_qfile(const char *boot_path) {
  FILE *qfile = NULL;
#if defined(Win32)
  int i;
#endif

  expand_file_name((char *)boot_path,TRUE,(char *)source_path);
#if defined(Win32)
  i = strlen(source_path)-4;
  if (i > 0 && strcmp(source_path+i,".bat") == 0){
    source_path[i+1] = 'c';
    source_path[i+2] = 'p';
    source_path[i+3] = 'x';
  } else if (i > 0 && strcmp(source_path+i,".cpx") != 0)
    strcat(source_path,".cpx");

  if (access(source_path,R_OK))
    source_path[strlen(source_path)-4] = '\0'; /* Take out .cpx */
#endif
  if (qfile == NULL) qfile = fopen(source_path,"r");
  if (qfile == NULL) {
    fprintf(stderr, "%s: boot file not found\n", source_path);
    at_exit(1);
    return NULL;
  } else { /* We have a bootfile we can read from */
    return qfile;
  }
}

/* Load a qfile embedded in an executable */

FILE *ciao_open_embedded_qfile(const char *program_name);
void ciao_open_exec_skip_stub(const char *file, FILE **stream);

void ciao_load_embedded_qfile_s(ciao_state state, const char *program_name) {
  FILE *qfile;
  qfile = ciao_open_embedded_qfile(program_name);
  ciao_load_ql_files(state, qfile);
  fclose(qfile);
}

void ciao_load_embedded_qfile(const char *program_name) {
  ciao_load_embedded_qfile_s(ciao_implicit_state, program_name);
}

FILE *ciao_open_embedded_qfile(const char *program_name) {
  FILE *qfile = NULL;
  ciao_open_exec_skip_stub(program_name,&qfile);
  if (qfile == NULL) {
    fprintf(stderr,"%s: file not found\n", program_name);
    at_exit(1);
  }
  return qfile;
}

void ciao_open_exec_skip_stub(const char *file, FILE **stream) {
  /* TODO: see open_exec_skip_stub() in start.c */
  fprintf(stderr,"{ERROR: ciao_open_exec_skip_stub() is not implemented yet}\n");
}

/* ------------------------------------------------------------------------- */
/* Term creation */

ciao_term ciao_var_s(ciao_state state) {
  tagged_t *pt;
  tagged_t to;
  worker_t *w = state->worker_registers;
  ciao_ensure_heap(state, 1);
  pt = w->global_top;
  HeapPush(pt, to = TagHVA(pt));
  w->global_top = pt;  
  return ciao_ref(state, to);
}

ciao_term ciao_var(void) {
  return ciao_var_s(ciao_implicit_state);
}

ciao_term ciao_structure_a_s(ciao_state state, const char *name, int arity, ciao_term *args) {
  worker_t *w = state->worker_registers;
  if (arity == 0) {
    return ciao_ref(state, MakeString((char *)name));
  } else if (strcmp(name, ".") == 0 && arity == 2) {
    tagged_t list;
    ciao_ensure_heap(state, 3);
    MakeLST(list, ciao_unref(state, args[0]), ciao_unref(state, args[1]));
    return ciao_ref(state, list);
  } else {
    int i;
    tagged_t *pt;
    tagged_t functor;
    ciao_ensure_heap(state, 2 + arity);
    functor = SetArity(MakeString((char *)name), arity);
    pt = w->global_top;
    HeapPush(pt, functor);
    for (i = 0; i < arity; i++) {
      HeapPush(pt, ciao_unref(state, args[i]));
    }
    w->global_top = pt;  
    return ciao_ref(state, Tag(STR, HeapOffset(pt, -(arity+1))));
  }
}

ciao_term ciao_structure_a(const char *name, int arity, ciao_term *args) {
  return ciao_structure_a_s(ciao_implicit_state, name, arity, args);
}

/* TODO: Add versions for all C integer types */

ciao_term ciao_integer_s(ciao_state state, int i) {
  worker_t *w = state->worker_registers;
  ciao_ensure_heap(state, 4);
  return ciao_ref(state, MakeInteger(w, (intmach_t)i));
}

ciao_term ciao_integer(int i) {
  return ciao_integer_s(ciao_implicit_state, i);
}

ciao_term ciao_float_s(ciao_state state, double f) {
  worker_t *w = state->worker_registers;
  ciao_ensure_heap(state, 4);
  return ciao_ref(state, MakeFloat(w, f));
}

ciao_term ciao_float(double f) {
  return ciao_float_s(ciao_implicit_state, f);
}

ciao_bool ciao_is_integer_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsInteger(t);
}

ciao_bool ciao_is_integer(ciao_term term) {
  return ciao_is_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_fits_in_int_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  /* This relies on lazy evaluation. Bignums are arrays of integers, and are
     always (explicitly) canonized after every evaluation.  If more than one
     word is needed, then it does not fit into an integer. */
  /*  if (IsFloat(t)) printf("IsFloat\n"); else printf("Not IsFloat\n"); */

  return TagIsSmall(t) || (IsInteger(t) && (bn_length((bignum_t *)TagToSTR(t)) == 1));
}

ciao_bool ciao_fits_in_int(ciao_term term) {
  return ciao_fits_in_int_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_variable_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsVar(t);
}

ciao_bool ciao_is_variable(ciao_term term) {
  return ciao_is_variable_s(ciao_implicit_state, term);
}

int ciao_to_integer_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_integer(state, term) */
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return GetInteger(t);
}

int ciao_to_integer(ciao_term term) {
  return ciao_to_integer_s(ciao_implicit_state, term);
}

ciao_bool ciao_to_integer_check_s(ciao_state state, ciao_term term, int *res) {
  if (ciao_fits_in_int_s(state, term)) {
    *res = ciao_to_integer_s(state, term);
    return TRUE;
  } else return FALSE;
}

ciao_bool ciao_to_integer_check(ciao_term term, int *res) {
  return ciao_to_integer_check_s(ciao_implicit_state, term, res);
}

ciao_bool ciao_is_number_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsNumber(t);
}

ciao_bool ciao_is_number(ciao_term term) {
  return ciao_is_number_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_float_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsFloat(t);
}

ciao_bool ciao_is_float(ciao_term term) {
  return ciao_is_float_s(ciao_implicit_state, term);
}

double ciao_to_float_s(ciao_state state, ciao_term term) {
  /* PRECONDITION: ciao_is_number(state, term) */
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return GetFloat(t);
}

double ciao_to_float(ciao_term term) {
  return ciao_to_float_s(ciao_implicit_state, term);
}

char *ciao_get_number_chars_s(ciao_state state, ciao_term term) {
  tagged_t number;
  char *number_result;
  worker_t *w = state->worker_registers;

  number = ciao_unref(state, term);
  /* number_to_string() handles al kinds of numbers; it leaves the result in
     Atom_Buffer */
  number_to_string(Arg, number, GetSmall(current_radix));
  number_result = ciao_malloc(strlen(Atom_Buffer) + 1);
  strcpy(number_result, Atom_Buffer);
  return number_result;
}

char *ciao_get_number_chars(ciao_term term) {
  return ciao_get_number_chars_s(ciao_implicit_state, term);
}

/* PRECONDITION: number_result should really represent a number */
/* TO DO: raise a proper exception */
ciao_term ciao_put_number_chars_s(ciao_state state, char *number_string) {
  tagged_t result;
  (void)string_to_number( state->worker_registers, 
                          number_string,
			  GetSmall(current_radix),
                          &result,
			  0);
  return ciao_ref(state, result);
}

ciao_term ciao_put_number_chars(char *number_string) {
  return ciao_put_number_chars_s(ciao_implicit_state, number_string);
}


ciao_bool ciao_is_atom_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsAtom(t);
}

ciao_bool ciao_is_atom(ciao_term term) {
  return ciao_is_atom_s(ciao_implicit_state, term);
}

const char *ciao_atom_name_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!IsAtom(t)) {
    return (const char *)NULL;
  } else { 
    atom_t *atomptr;
    atomptr = TagToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_atom_name(ciao_term term) {
  return ciao_atom_name_s(ciao_implicit_state, term);
}

char *ciao_atom_name_dup_s(ciao_state state, ciao_term term) {
  const char *s2;
  char *s;
  s2 = ciao_atom_name_s(state, term);
  s = (char *)ciao_malloc(sizeof(char *) * (strlen(s2) + 1));
  strcpy(s, s2);
  return s;
}

char *ciao_atom_name_dup(ciao_term term) {
  return ciao_atom_name_dup_s(ciao_implicit_state, term);
}

const char *ciao_structure_name_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) {
    return (const char *)NULL;
  } else {
    tagged_t f;
    atom_t *atomptr;
    f = TagToHeadfunctor(t);
    t = SetArity(f,0);
    atomptr = TagToAtom(t);
    return atomptr->name;
  }
}

const char *ciao_structure_name(ciao_term term) {
  return ciao_structure_name_s(ciao_implicit_state, term);
}

int ciao_structure_arity_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) {
    return 0;
  } else { 
    tagged_t f;
    f = TagToHeadfunctor(t);
    return Arity(f);
  }
}

int ciao_structure_arity(ciao_term term) {
  return ciao_structure_arity_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_list_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TagIsLST(t);
}

ciao_bool ciao_is_list(ciao_term term) {
  return ciao_is_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_empty_list_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return IsAtom(t) && t == MakeString("[]");
}

ciao_bool ciao_is_empty_list(ciao_term term) {
  return ciao_is_empty_list_s(ciao_implicit_state, term);
}

ciao_bool ciao_is_structure_s(ciao_state state, ciao_term term) {
  tagged_t t;
  t = ciao_unref(state, term);
  DEREF(t, t);
  return TagIsSTR(t);
}

ciao_bool ciao_is_structure(ciao_term term) {
  return ciao_is_structure_s(ciao_implicit_state, term);
}

ciao_term ciao_structure_arg_s(ciao_state state, ciao_term term, int i) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  if (!TagIsSTR(t)) return CIAO_ERROR;
  RefArg(a, t, i);
  return ciao_ref(state, a);
}

ciao_term ciao_structure_arg(ciao_term term, int i) {
  return ciao_structure_arg_s(ciao_implicit_state, term, i);
}

ciao_term ciao_list_head_s(ciao_state state, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  RefCar(a, t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_head(ciao_term term) {
  return ciao_list_head_s(ciao_implicit_state, term);
}

ciao_term ciao_list_tail_s(ciao_state state, ciao_term term) {
  tagged_t t;
  tagged_t a;
  t = ciao_unref(state, term);
  DEREF(t, t);
  RefCdr(a, t);
  return ciao_ref(state, a);
}

ciao_term ciao_list_tail(ciao_term term) {
  return ciao_list_tail_s(ciao_implicit_state, term);
}

/* Helper functions */

ciao_term ciao_atom_s(ciao_state state, const char *name) {
  return ciao_structure_s(state, name, 0);
}

ciao_term ciao_atom(const char *name) {
  return ciao_atom_s(ciao_implicit_state, name);
}

ciao_term ciao_empty_list_s(ciao_state state) {
  return ciao_atom_s(state, "[]");
}

ciao_term ciao_empty_list(void) {
  return ciao_empty_list_s(ciao_implicit_state);
}

ciao_term ciao_list_s(ciao_state state, ciao_term head, ciao_term tail) {
  return ciao_structure_s(state, ".", 2, head, tail);
}

ciao_term ciao_list(ciao_term head, ciao_term tail) {
  return ciao_list_s(ciao_implicit_state, head, tail);
}

ciao_term ciao_dlist_a_s(ciao_state state, int len, ciao_term *args, ciao_term tail) {
  /* PRECONDITION: len >= 1 */ 
  int i;
  ciao_term list;
  
  list = tail;
  for (i = len - 1; i >= 0; i--) {
    list = ciao_list_s(state, args[i], list);
  }

  return list;
}

ciao_term ciao_dlist_a(int len, ciao_term *args, ciao_term tail) {
  return ciao_dlist_a_s(ciao_implicit_state, len, args, tail);
}

ciao_term ciao_listn_a_s(ciao_state state, int len, ciao_term *args) {
  return ciao_dlist_a_s(state, len, args, ciao_empty_list());
}

ciao_term ciao_listn_a(int len, ciao_term *args) {
  return ciao_listn_a_s(ciao_implicit_state, len, args);
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

ciao_term ciao_structure_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a_s(state, name, arity, args);
}

ciao_term ciao_structure(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_structure_a(name, arity, args);
}

ciao_term ciao_listn_s(ciao_state state, int length, ...) {
  GETARGS(length)
  return ciao_listn_a_s(state, length, args);
}

ciao_term ciao_listn(int length, ...) {
  GETARGS(length)
  return ciao_listn_a(length, args);
}

ciao_term ciao_dlist_s(ciao_state state, int length, ...) {
  GETARGS(length)
  return ciao_dlist_a_s(state, length - 1, args, args[length - 1]);
}

ciao_term ciao_dlist(int length, ...) {
  GETARGS(length)
  return ciao_dlist_a(length - 1, args, args[length - 1]);
}


ciao_term ciao_copy_term_s(ciao_state src_state, ciao_term src_term, ciao_state dst_state) {
  worker_t *w;
  w = dst_state->worker_registers;
  return ciao_ref(dst_state, cross_copy_term(w, ciao_unref(dst_state, src_term)));
}

ciao_term ciao_copy_term(ciao_term src_term) {
  return ciao_copy_term_s(ciao_implicit_state, src_term, ciao_implicit_state);
}

ciao_bool ciao_unify_s(ciao_state state, ciao_term x, ciao_term y) {
  worker_t *w = state->worker_registers;
  return cunify(w, ciao_unref(state, x), ciao_unref(state, y));
}

ciao_bool ciao_unify(ciao_term x, ciao_term y) {
  return ciao_unify_s(ciao_implicit_state, x, y);
}

ciao_bool ciao_equal_s(ciao_state state, ciao_term x, ciao_term y) {
  tagged_t a, b;
  a = ciao_unref(state, x);
  b = ciao_unref(state, y);
  DEREF(a, a);
  DEREF(b, b);
  return a == b;
}

ciao_bool ciao_equal(ciao_term x, ciao_term y) {
  return ciao_equal_s(ciao_implicit_state, x, y);
}

void ciao_at_exit(int result) {
  at_exit(result);
}

int ciao_firstgoal(ciao_state state, ciao_term goal) {
  goal_descriptor_t *goal_desc = state;
  tagged_t goal_term = ciao_unref(state, goal);
  return firstgoal(goal_desc, goal_term);
}

int ciao_boot(ciao_state state) {
  return ciao_firstgoal(state, ciao_structure_s(state, "internals:boot", 0));
}

/* ------------------------------------------------------------------------- */

ciao_choice ciao_get_choice(ciao_state state) {
  worker_t *w = state->worker_registers;
  return ChoiceToInt(w->node);
}

ciao_bool ciao_more_solutions(ciao_state state, ciao_choice choice) {
  return ciao_get_choice(state) > choice;
}

void ciao_cut(ciao_state state, ciao_choice choice) {
  worker_t *w = state->worker_registers;
  if (!ciao_more_solutions(state, choice)) return;
  w->node = ChoiceFromInt(choice);
  SetShadowregs(w->node);
  PROFILE__HOOK_CIAOCUT;
}

void ciao_fail(ciao_state state) {
  worker_t *w = state->worker_registers;
  wam(w, state);
}

/* ------------------------------------------------------------------------- */

ciao_bool ciao_query_next(ciao_query *query) {
  if (!ciao_query_ok(query)) return FALSE;
  query->state->action = BACKTRACKING | KEEP_STACKS;
  ciao_fail(query->state);
  return ciao_query_ok(query);
}

ciao_bool ciao_query_ok(ciao_query *query) {
  try_node_t *next_alt;

  next_alt = query->state->worker_registers->next_alt;
  
  if (next_alt == &nullgoal_alt ||
      (next_alt == NULL && query->state->worker_registers->node->next_alt == &nullgoal_alt)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

void ciao_query_end(ciao_query *query) {
  node_t *b;
  ciao_state state = query->state;
  worker_t *w = state->worker_registers;

  if (ciao_query_ok(query)) {
    ciao_cut(state, query->base_choice);
  }

  b = w->node;
  w->node = b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  SetShadowregs(b);
  w->next_alt = NULL;

  ciao_free(query);
}

ciao_query *ciao_query_begin_term_s(ciao_state state, ciao_term goal) {
  worker_t *w = state->worker_registers;
  tagged_t *b0;
  node_t *b;
  ciao_query *query;

  goal = ciao_structure_s(state, "hiord_rt:call", 1, goal);

  DEREF(X(0), ciao_unref(state, goal));

  /* push null choice */

  w->next_insn = default_code;
  b0 = (tagged_t *)w->node;
  b = ChoiceCharOffset(b0, ArityToOffset(0));
  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = &defaultgoal_alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
    
  w->next_alt = NULL; 

  query = (ciao_query *)ciao_malloc(sizeof(ciao_query));
  query->state = state;
  query->base_choice = ciao_get_choice(state);
  
  /* push choice for starting goal */
  
  w->next_insn = call_code;
  b0 = (tagged_t *)w->node;
  b = ChoiceCharOffset(b0, ArityToOffset(1));
  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = &startgoal_alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  b->term[0] = X(0);    /* Will be the arg. of a call/1 */
    
  w->next_alt = NULL; 

  state->action = BACKTRACKING | KEEP_STACKS;
  wam(w, state);
  return query;
}

ciao_query *ciao_query_begin_term(ciao_term goal) {
  return ciao_query_begin_term_s(ciao_implicit_state, goal);
}

ciao_query *ciao_query_begin_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_query_begin_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_query *ciao_query_begin(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_query_begin_term(ciao_structure_a(name, arity, args));
}

ciao_bool ciao_commit_call_term_s(ciao_state state, ciao_term goal) {
  ciao_bool ok;
  ciao_query *query;

  query = ciao_query_begin_term_s(state, goal);
  ok = ciao_query_ok(query);
  ciao_query_end(query);

  return ok;
}

ciao_bool ciao_commit_call_term(ciao_term goal) {
  return ciao_commit_call_term_s(ciao_implicit_state, goal);
}

ciao_bool ciao_commit_call_s(ciao_state state, const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term_s(state, ciao_structure_a_s(state, name, arity, args));
}

ciao_bool ciao_commit_call(const char *name, int arity, ...) {
  GETARGS(arity)
  return ciao_commit_call_term(ciao_structure_a(name, arity, args));
}

/* ------------------------------------------------------------------------- */

jmp_buf ciao_gluecode_jmpbuf;

void ciao_raise_exception_s(ciao_state state, ciao_term exception) {
  worker_t *w = state->worker_registers;

  X(0) = ciao_unref(state, exception);
  longjmp(ciao_gluecode_jmpbuf, 1);
}

void ciao_raise_exception(ciao_term exception) {
  ciao_raise_exception_s(ciao_implicit_state, exception);
}

/* ------------------------------------------------------------------------- */

#define GARBAGE_PROTECTION

#ifdef GARBAGE_PROTECTION 

#define REF_TABLE_PAD 4
#define REF_TABLE_CHUNK_SIZE 32
#define REF_TABLE_CHUNKS 1

tagged_t create_ref_table(ciao_state state, int chunks) {
  worker_t *w = state->worker_registers;
  int i, j;
  tagged_t *pt, *pt0;
  tagged_t functor;

  ciao_ensure_heap(state, REF_TABLE_CHUNK_SIZE * chunks + 1);
  functor = SetArity(MakeString("$reftable"), (REF_TABLE_CHUNK_SIZE - 1));
  pt = w->global_top;
  pt0 = pt;
  for (j = 0; j < chunks - 1; j++) {
    HeapPush(pt, functor);
    for (i = 0; i < REF_TABLE_CHUNK_SIZE - 2; i++) {
      HeapPush(pt, TagHVA(pt));
    }
    HeapPush(pt, Tag(STR, pt + 1));
  }
  if (chunks > 0) {
    HeapPush(pt, functor);
    for (i = 0; i < REF_TABLE_CHUNK_SIZE - 1; i++) {
      HeapPush(pt, TagHVA(pt));
    }
  }
  w->global_top = pt;  
  return Tag(STR, pt0);
}

ciao_term ciao_ref(ciao_state state, tagged_t x) {
  worker_t *w = state->worker_registers;
  tagged_t *pt1;
  ciao_term term;
  int next, chunks;

  SetE(w->frame);

  next = GetSmall(Y(0));
  {
    tagged_t ta;
    ta = *TagToArg(Y(2), next);

    if (ta!=x) {
      if (!cunify(Arg,ta,x)) goto fail;
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
  Y(0) = MakeSmall(next);

  chunks = GetSmall(Y(1));
  if (chunks * REF_TABLE_CHUNK_SIZE - next < REF_TABLE_PAD) { /* chunk overflow! */
    tagged_t *x, *y;
    tagged_t new_table;
    int i, j, new_chunks, k;

    new_chunks = chunks * 2;
    /* old table is in Y(2) so don't care about gc here */  
    new_table = create_ref_table(state, new_chunks); 

    x = TagToArg(Y(2), 0);
    y = TagToArg(new_table, 0);
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
    Y(2) = new_table;
    Y(1) = MakeSmall(new_chunks);
  }

  return term;
}

ciao_term ciao_refer(tagged_t x) {
  return ciao_ref(ciao_implicit_state,x);
}

tagged_t ciao_unref(ciao_state state, ciao_term term) {
  worker_t *w = state->worker_registers;
  tagged_t *pt1;
  tagged_t x;

  SetE(w->frame);

  x = *TagToArg(Y(2), term);
  return x;
}

tagged_t ciao_unrefer(ciao_term term) {
  return ciao_unref(ciao_implicit_state,term);
}

void ciao_frame_begin_s(ciao_state state) {
  tagged_t *pt1;
  worker_t *w = state->worker_registers;
  int arity;

  arity = 3;

  ComputeE;		
  E->next_insn = w->next_insn;
  E->frame = w->frame;
  w->frame = E;
  w->next_insn = CONTCODE(arity);
  w->local_top = (frame_t *)Offset(E,EToY0+arity);
  Y(0) = MakeSmall(1); /* next free ref */
  Y(1) = MakeSmall(REF_TABLE_CHUNKS); /* chunks */
  Y(2) = create_ref_table(state, REF_TABLE_CHUNKS);
}

void ciao_frame_begin(void) {
  ciao_frame_begin_s(ciao_implicit_state);
}

void ciao_frame_end_s(ciao_state state) {
  worker_t *w = state->worker_registers;
  tagged_t *pt1;

  SetE(w->frame); 

  w->local_top = NULL;
  w->frame = E->frame;
  w->next_insn = E->next_insn;
}

void ciao_frame_end(void) {
  ciao_frame_end_s(ciao_implicit_state);
}

void re_ciao_frame_end(void)
{
  ciao_frame_end();
//  ciao_implicit_state = ciao_aux_state;
}

#else

ciao_term ciao_ref(ciao_state state, tagged_t x) {
  return (ciao_term)x;
}

tagged_t ciao_unref(ciao_state state, ciao_term term) {
  return (tagged_t)term;
}

void ciao_frame_begin_s(ciao_state state) {
}
  
void ciao_frame_end_s(ciao_state state) {
}

void ciao_frame_begin(void) {
}
  
void ciao_frame_end(void) {
}

#endif


