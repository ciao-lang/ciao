/*
 *  ciao_prolog.h
 *
 *  Ciao/C API for extending or embedding Ciao in C programs (as part
 *  of the foreign function interface).
 *
 *  Copyright (C) 2002 UPM-CLIP
 */

#ifndef _CIAO_PROLOG_H
#define _CIAO_PROLOG_H

#ifdef __cplusplus
extern "C" {
#endif

#define CIAO_ERROR 0
#define ciao_true 1
#define ciao_false 0

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>

typedef goal_descriptor_t *ciao_state;

extern ciao_state ciao_implicit_state;

typedef unsigned int ciao_choice;
typedef unsigned int ciao_term;
typedef unsigned int ciao_bool;

typedef struct _ciao_query_ _ciao_query_t;
struct _ciao_query_ {
  ciao_state state;
  ciao_choice base_choice;
};

/* Initialization */

void init_state_from_WAM(ciao_state state);

int ciao_opts(const char *program_name, 
              int programc, 
              const char **programv, 
              int optc,
              const char **optv, 
              const char **boot_path);
void ciao_init(const char *boot_path);
void ciao_reinit(void);

/* Engine boot */

int ciao_boot(ciao_state state);

/* WAM creation */

ciao_state ciao_state_new(void);
void ciao_state_free(ciao_state state);

/* PO load operations */

void ciao_load_embedded_qfile_s(ciao_state  state, 
                                const char *program_name); 
void ciao_load_embedded_qfile(const char   *program_name); 
void ciao_load_qfile_s(ciao_state  state, 
                       const char *boot_path);
void ciao_load_qfile(const char *boot_path);

/* Memory management */

void *ciao_malloc(int size);
void ciao_free(void *pointer);

/* Basic term creation */

ciao_term ciao_var_s(ciao_state state);
ciao_term ciao_var(void);

ciao_term ciao_structure_a_s(ciao_state  state, 
                             const char *name, 
                             int         arity, 
                             ciao_term  *args);
ciao_term ciao_structure_a(const char *name, 
                           int         arity, 
                           ciao_term  *args);

ciao_term ciao_integer_s(ciao_state state, 
                         int        i);
ciao_term ciao_integer(int i);

ciao_term ciao_float_s(ciao_state state,
                       double     i);
ciao_term ciao_float(double i);

/* Helper functions for term creation */

ciao_term ciao_list_s(ciao_state state, 
                      ciao_term  head, 
                      ciao_term  tail);
ciao_term ciao_list(ciao_term head, 
                    ciao_term tail);

ciao_term ciao_atom_s(ciao_state  state, 
                      const char *name);
ciao_term ciao_atom(const char *name);

ciao_term ciao_empty_list_s(ciao_state state);
ciao_term ciao_empty_list(void);

ciao_term ciao_dlist_a_s(ciao_state state, 
                         int        len, 
                         ciao_term *args, 
                         ciao_term  base);
ciao_term ciao_dlist_a(int len, 
                       ciao_term *args, 
                       ciao_term  base);

ciao_term ciao_listn_a_s(ciao_state state, 
                         int        len, 
                         ciao_term *args);
ciao_term ciao_listn_a(int        len,
                       ciao_term *args);

ciao_term ciao_structure_s(ciao_state  state, 
                           const char *name,
                           int         arity, 
                           ...);
ciao_term ciao_structure(const char *name, 
                         int         arity, 
                         ...);

ciao_term ciao_listn_s(ciao_state state, 
                       int        length,
                       ...); 
ciao_term ciao_listn(int length, 
                     ...); 

ciao_term ciao_dlist_s(ciao_state state, 
                       int        length,
                       ...); 
ciao_term ciao_dlist(int length,
                     ...); 


/* Term navigation */

ciao_bool ciao_is_variable_s(ciao_state state, 
                             ciao_term  term);
ciao_bool ciao_is_variable(ciao_term term);

ciao_bool ciao_is_integer_s(ciao_state state, 
                            ciao_term  term);
ciao_bool ciao_is_integer(ciao_term term);

ciao_bool ciao_to_integer_check_s(ciao_state state, 
                                  ciao_term  term, 
                                  int       *res);
ciao_bool ciao_to_integer_check(ciao_term  term, 
                                int       *res);

ciao_bool ciao_fits_in_int_s(ciao_state state, 
                             ciao_term  term);
ciao_bool ciao_fits_in_int(ciao_term term);

/* PRECONDITION: ciao_is_integer_s(state, term) */
int ciao_to_integer_s(ciao_state state, 
                      ciao_term  term); 
/* PRECONDITION: ciao_is_integer(term) */
int ciao_to_integer(ciao_term term);

ciao_bool ciao_is_number_s(ciao_state state, 
                           ciao_term  term);
ciao_bool ciao_is_number(ciao_term term);

ciao_bool ciao_is_float_s(ciao_state state, 
                           ciao_term  term);
ciao_bool ciao_is_float(ciao_term term);

 /* PRECONDITION: ciao_is_number_s(state, term) */
double ciao_to_float_s(ciao_state state, 
                       ciao_term  term);
/* PRECONDITION: ciao_is_number(term) */
double ciao_to_float(ciao_term term); 

/* PRECONDITION: ciao_is_number(term) */
char *ciao_get_number_chars_s(ciao_state state,
                              ciao_term  term);
char *ciao_get_number_chars(ciao_term term);

/* PRECONDITION: number_string must represent a syntactically valid number */
ciao_term ciao_put_number_chars_s(ciao_state state,
                                  char      *number_string);
ciao_term ciao_put_number_chars(char *number_string);

ciao_bool ciao_is_atom_s(ciao_state state, 
                         ciao_term  atom);
ciao_bool ciao_is_atom(ciao_term atom);

const char *ciao_atom_name_s(ciao_state state,
                             ciao_term  atom);
const char *ciao_atom_name(ciao_term atom);

char *ciao_atom_name_dup_s(ciao_state state,
                           ciao_term  atom);
char *ciao_atom_name_dup(ciao_term atom);

ciao_bool ciao_is_list_s(ciao_state state, 
                         ciao_term  term);
ciao_bool ciao_is_list(ciao_term term);

ciao_term ciao_list_head_s(ciao_state state, 
                           ciao_term  term);
ciao_term ciao_list_head(ciao_term term);

ciao_term ciao_list_tail_s(ciao_state state,
                           ciao_term  term);
ciao_term ciao_list_tail(ciao_term term);

ciao_bool ciao_is_empty_list_s(ciao_state state, 
                               ciao_term  term);
ciao_bool ciao_is_empty_list(ciao_term term);

ciao_bool ciao_is_structure_s(ciao_state state, 
                              ciao_term  term);
ciao_bool ciao_is_structure(ciao_term term);

const char *ciao_structure_name_s(ciao_state state, 
                                  ciao_term  term);
const char *ciao_structure_name(ciao_term term);

int ciao_structure_arity_s(ciao_state state, 
                           ciao_term  term);
int ciao_structure_arity(ciao_term term);

ciao_term ciao_structure_arg_s(ciao_state state,
                               ciao_term  term, 
                               int i);
ciao_term ciao_structure_arg(ciao_term term, 
                             int       i);


/* Frames for term creation */

void ciao_frame_begin_s(ciao_state state);
void ciao_frame_begin(void);
void re_ciao_frame_end(void);
void ciao_frame_end_s(ciao_state state);
void ciao_frame_end(void);

/* Queries and calls */

typedef _ciao_query_t ciao_query;

ciao_query *ciao_query_begin_s(ciao_state  state, 
                               const char *name, 
                               int         arity,
                               ...);
ciao_query *ciao_query_begin(const char *name, 
                             int         arity, 
                             ...);

ciao_query *ciao_query_begin_term_s(ciao_state state, 
                                    ciao_term  goal);
ciao_query *ciao_query_begin_term(ciao_term goal);

ciao_bool ciao_query_ok(ciao_query   *query);
ciao_bool ciao_query_next(ciao_query *query);
void ciao_query_end(ciao_query       *query);

ciao_bool ciao_commit_call_s(ciao_state  state,
                             const char *name, 
                             int         arity, 
                             ...);
ciao_bool ciao_commit_call(const char *name, 
                           int         arity, ...);

ciao_bool ciao_commit_call_term_s(ciao_state state, 
                                  ciao_term  goal);
ciao_bool ciao_commit_call_term(ciao_term    goal);

/* Helper functions */

ciao_term ciao_copy_term_s(ciao_state src_desc, 
                           ciao_term  src_term, 
                           ciao_state dst_desc);
ciao_term ciao_copy_term(ciao_term    src_term);

/* =/2 */
ciao_bool ciao_unify_s(ciao_state state, 
                       ciao_term  x, 
                       ciao_term  y); 
ciao_bool ciao_unify(ciao_term    x, 
                     ciao_term    y); 
/* ==/2 */
ciao_bool ciao_equal_s(ciao_state state,
                       ciao_term  x,
                       ciao_term  y); 
ciao_bool ciao_equal(ciao_term x, 
                     ciao_term y); 

/* Exceptions */

void ciao_raise_exception_s(ciao_state state, 
                            ciao_term  exception);
void ciao_raise_exception(ciao_term    exception);

/* Miscellaneous */

ciao_bool ciao_is_char_code_list(ciao_state state, 
                                 ciao_term  term);
int ciao_is_int_list(ciao_state state, 
                     ciao_term  term);
int ciao_is_double_list(ciao_state state, 
			ciao_term term); 
int ciao_list_length(ciao_state state, 
                     ciao_term  term);
unsigned char *ciao_list_to_byte_array(ciao_state state, 
				       ciao_term  list);
int *ciao_list_to_int_array(ciao_state state, 
                            ciao_term  list);
double *ciao_list_to_double_array(ciao_state state, 
                            ciao_term  list);
char *ciao_list_to_str(ciao_state state, 
		       ciao_term   list);
ciao_term ciao_byte_listn(ciao_state state, 
                          const unsigned char *s, 
                          int        length);
ciao_term ciao_int_listn(ciao_state   state,
                         int         *s, 
                         int          length);
ciao_term ciao_double_listn(ciao_state   state,
                         double      *s,
                         int          length);
ciao_term ciao_str_to_list(ciao_state  state, 
                           const char *string);
ciao_term ciao_pointer_to_address(ciao_state state, 
                                  void      *pointer);
void *ciao_address_to_pointer(ciao_state state, 
                              ciao_term  term);
ciao_bool ciao_is_address(ciao_state state, 
                          ciao_term  term);

  /* tagged_t <-> ciao_term */

ciao_term ciao_refer(tagged_t x);
tagged_t ciao_unrefer(ciao_term term);

void ciao_ensure_heap(ciao_state state, int cells);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _CIAO_PROLOG_H */
