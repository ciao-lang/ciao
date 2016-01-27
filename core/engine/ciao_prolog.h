/*
 *  ciao_prolog.h
 *
 *  Ciao/C API for extending or embedding Ciao in C programs (as part
 *  of the foreign function interface).
 *
 *  Copyright (C) 2016 Jose F. Morales
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

typedef goal_descriptor_t *ciao_ctx;

extern ciao_ctx ciao_implicit_ctx;

typedef unsigned int ciao_choice;
typedef unsigned int ciao_term;
typedef unsigned int ciao_bool;

typedef struct _ciao_query_ _ciao_query_t;
struct _ciao_query_ {
  ciao_ctx ctx;
  ciao_choice base_choice;
};

/* Initialization */

int ciao_opts(const char *program_name, 
              int programc, 
              const char **programv, 
              int optc,
              const char **optv, 
              const char **boot_path);
void ciao_init(const char *boot_path);
void ciao_reinit(void);

/* Creation of a ciao_ctx context */

ciao_ctx ciao_ctx_new(void);
void ciao_ctx_free(ciao_ctx ctx);

/* Engine boot */

int ciao_boot(ciao_ctx ctx);

/* PO load operations */

void ciao_load_embedded_qfile_s(ciao_ctx ctx, const char *program_name);
void ciao_load_embedded_qfile(const char *program_name);
void ciao_load_qfile_s(ciao_ctx ctx, const char *boot_path);
void ciao_load_qfile(const char *boot_path);

/* Memory management */

void *ciao_malloc(size_t size);
void ciao_free(void *pointer);

/* Basic term creation */

ciao_term ciao_var_s(ciao_ctx ctx);
ciao_term ciao_var(void);

ciao_term ciao_structure_a_s(ciao_ctx ctx, const char *name, int arity, ciao_term *args);
ciao_term ciao_structure_a(const char *name, int arity, ciao_term *args);

#define Decl_ciao_mk_X(CType, DeclType)				\
  ciao_term ciao_mk_##CType(DeclType x);			\
  ciao_term ciao_mk_##CType##_s(ciao_ctx ctx, DeclType x);

Decl_ciao_mk_X(c_short, short)
Decl_ciao_mk_X(c_int, int)
Decl_ciao_mk_X(c_long, long)
Decl_ciao_mk_X(c_ushort, unsigned short)
Decl_ciao_mk_X(c_uint, unsigned int)
Decl_ciao_mk_X(c_ulong, unsigned long)
Decl_ciao_mk_X(c_float, float)
Decl_ciao_mk_X(c_double, double)
Decl_ciao_mk_X(c_uintptr, uintptr_t)
Decl_ciao_mk_X(c_size, size_t)
Decl_ciao_mk_X(c_int8, int8_t)
Decl_ciao_mk_X(c_int16, int16_t)
Decl_ciao_mk_X(c_int32, int32_t)
Decl_ciao_mk_X(c_int64, int64_t)
Decl_ciao_mk_X(c_uint8, uint8_t)
Decl_ciao_mk_X(c_uint16, uint16_t)
Decl_ciao_mk_X(c_uint32, uint32_t)
Decl_ciao_mk_X(c_uint64, uint64_t)

#undef Decl_ciao_mk_X

/* Pre:
   - For integer values: ciao_is_integer_s(ctx, term) and ciao_fits_in_X_s(ctx, term)
   - For floating point: ciao_is_number_s(ctx, term)
*/

#define Decl_ciao_get_X(CType, DeclType)		       \
  DeclType ciao_get_##CType##_s(ciao_ctx ctx, ciao_term term); \
  DeclType ciao_get_##CType(ciao_term term);

Decl_ciao_get_X(c_short, short)
Decl_ciao_get_X(c_int, int)
Decl_ciao_get_X(c_long, long)
Decl_ciao_get_X(c_ushort, unsigned short)
Decl_ciao_get_X(c_uint, unsigned int)
Decl_ciao_get_X(c_ulong, unsigned long)
Decl_ciao_get_X(c_float, float)
Decl_ciao_get_X(c_double, double)
Decl_ciao_get_X(c_uintptr, uintptr_t)
Decl_ciao_get_X(c_size, size_t)
Decl_ciao_get_X(c_int8, int8_t)
Decl_ciao_get_X(c_int16, int16_t)
Decl_ciao_get_X(c_int32, int32_t)
Decl_ciao_get_X(c_int64, int64_t) // WRONG in 32 bits
Decl_ciao_get_X(c_uint8, uint8_t)
Decl_ciao_get_X(c_uint16, uint16_t)
Decl_ciao_get_X(c_uint32, uint32_t) // WRONG in 32 bits (sign bit)
Decl_ciao_get_X(c_uint64, uint64_t) // WRONG in 32 bits, WRONG in 64 bits (sign bit)

#undef Decl_ciao_get_X

ciao_bool ciao_fits_in_c_int_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_fits_in_c_int(ciao_term term);
ciao_bool ciao_fits_in_c_long_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_fits_in_c_long(ciao_term term);

unsigned char *ciao_get_c_uint8_array(ciao_ctx ctx, ciao_term list);
int *ciao_get_c_int_array(ciao_ctx ctx, ciao_term list);
double *ciao_get_c_double_array(ciao_ctx ctx, ciao_term list);

ciao_term ciao_mk_c_uint8_list(ciao_ctx ctx, const unsigned char *s, size_t length);
ciao_term ciao_mk_c_int_list(ciao_ctx ctx, int *s, size_t length);
ciao_term ciao_mk_c_double_list(ciao_ctx ctx, double *s, size_t length);

/* Helper functions for term creation */

ciao_term ciao_list_s(ciao_ctx ctx, ciao_term head, ciao_term tail);
ciao_term ciao_list(ciao_term head, ciao_term tail);

ciao_term ciao_atom_s(ciao_ctx ctx, const char *name);
ciao_term ciao_atom(const char *name);

ciao_term ciao_empty_list_s(ciao_ctx ctx);
ciao_term ciao_empty_list(void);

ciao_term ciao_dlist_a_s(ciao_ctx ctx, int len, ciao_term *args, ciao_term base);
ciao_term ciao_dlist_a(int len, ciao_term *args, ciao_term  base);

ciao_term ciao_listn_a_s(ciao_ctx ctx, int len, ciao_term *args);
ciao_term ciao_listn_a(int len, ciao_term *args);

ciao_term ciao_structure_s(ciao_ctx ctx, const char *name, int arity, ...);
ciao_term ciao_structure(const char *name, int arity, ...);

ciao_term ciao_listn_s(ciao_ctx ctx, size_t length, ...);
ciao_term ciao_listn(size_t length, ...);

ciao_term ciao_dlist_s(ciao_ctx ctx, size_t length, ...);
ciao_term ciao_dlist(size_t length, ...);

/* Term checks */

ciao_bool ciao_is_variable_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_variable(ciao_term term);

ciao_bool ciao_is_integer_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_integer(ciao_term term);

ciao_bool ciao_is_number_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_number(ciao_term term);

ciao_bool ciao_is_float_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_float(ciao_term term);

/* Pre: ciao_is_number(term) */
char *ciao_get_number_chars_s(ciao_ctx ctx, ciao_term term);
char *ciao_get_number_chars(ciao_term term);

/* Pre: number_string must represent a syntactically valid number */
ciao_term ciao_put_number_chars_s(ciao_ctx ctx, char *number_string);
ciao_term ciao_put_number_chars(char *number_string);

ciao_bool ciao_is_atom_s(ciao_ctx ctx, ciao_term atom);
ciao_bool ciao_is_atom(ciao_term atom);

const char *ciao_atom_name_s(ciao_ctx ctx, ciao_term atom);
const char *ciao_atom_name(ciao_term atom);

char *ciao_atom_name_dup_s(ciao_ctx ctx, ciao_term atom);
char *ciao_atom_name_dup(ciao_term atom);

ciao_bool ciao_is_list_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_list(ciao_term term);

ciao_term ciao_list_head_s(ciao_ctx ctx, ciao_term term);
ciao_term ciao_list_head(ciao_term term);

ciao_term ciao_list_tail_s(ciao_ctx ctx, ciao_term term);
ciao_term ciao_list_tail(ciao_term term);

ciao_bool ciao_is_empty_list_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_empty_list(ciao_term term);

ciao_bool ciao_is_structure_s(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_structure(ciao_term term);

const char *ciao_structure_name_s(ciao_ctx ctx, ciao_term term);
const char *ciao_structure_name(ciao_term term);

int ciao_structure_arity_s(ciao_ctx ctx, ciao_term term);
int ciao_structure_arity(ciao_term term);

ciao_term ciao_structure_arg_s(ciao_ctx ctx, ciao_term term, int i);
ciao_term ciao_structure_arg(ciao_term term, int i);

/* Frames for term creation */

void ciao_frame_begin_s(ciao_ctx ctx);
void ciao_frame_begin(void);
void ciao_frame_end_s(ciao_ctx ctx);
void ciao_frame_end(void);

/* Special operations for re-entrant calls (see tabling) */
  
void ciao_frame_re_begin(ciao_ctx ctx);
void ciao_frame_re_end(void);

/* Queries and calls */

typedef _ciao_query_t ciao_query;

ciao_query *ciao_query_begin_s(ciao_ctx ctx, const char *name, int arity, ...);
ciao_query *ciao_query_begin(const char *name, int arity, ...);

ciao_query *ciao_query_begin_term_s(ciao_ctx ctx, ciao_term goal);
ciao_query *ciao_query_begin_term(ciao_term goal);

ciao_bool ciao_query_ok(ciao_query *query);
ciao_bool ciao_query_next(ciao_query *query);
void ciao_query_end(ciao_query *query);

ciao_bool ciao_commit_call_s(ciao_ctx ctx, const char *name, int arity, ...);
ciao_bool ciao_commit_call(const char *name, int arity, ...);

ciao_bool ciao_commit_call_term_s(ciao_ctx ctx, ciao_term  goal);
ciao_bool ciao_commit_call_term(ciao_term goal);

/* Helper functions */

ciao_term ciao_copy_term_s(ciao_ctx src_desc, ciao_term src_term, ciao_ctx dst_desc);
ciao_term ciao_copy_term(ciao_term src_term);

/* =/2 */
ciao_bool ciao_unify_s(ciao_ctx ctx, ciao_term x, ciao_term y);
ciao_bool ciao_unify(ciao_term x, ciao_term y);
/* ==/2 */
ciao_bool ciao_equal_s(ciao_ctx ctx, ciao_term x, ciao_term y);
ciao_bool ciao_equal(ciao_term x, ciao_term y);

/* Exceptions */

void ciao_raise_exception_s(ciao_ctx ctx, ciao_term  exception);
void ciao_raise_exception(ciao_term exception);

/* Other data checks and conversions */

ciao_bool ciao_is_char_code_list(ciao_ctx ctx, ciao_term term);
int ciao_is_int_list(ciao_ctx ctx, ciao_term term);
int ciao_is_num_list(ciao_ctx ctx, ciao_term term);
int ciao_list_length(ciao_ctx ctx, ciao_term term);

char *ciao_list_to_str(ciao_ctx ctx, ciao_term list);
ciao_term ciao_str_to_list(ciao_ctx ctx, const char *string);
ciao_term ciao_pointer_to_address(ciao_ctx ctx, void *pointer);
void *ciao_address_to_pointer(ciao_ctx ctx, ciao_term term);
ciao_bool ciao_is_address(ciao_ctx ctx, ciao_term term);

/* tagged_t <-> ciao_term */

ciao_term ciao_refer(tagged_t x);
tagged_t ciao_unrefer(ciao_term term);

void ciao_ensure_heap(ciao_ctx ctx, size_t cells);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _CIAO_PROLOG_H */
