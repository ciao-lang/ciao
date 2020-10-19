/*
 *  stream_basic.h
 *
 *  Stream handling primitives (see engine(stream_basic)).
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_STREAM_BASIC_H
#define _CIAO_STREAM_BASIC_H

#include <stdio.h>

extern bool_t interactive_flag_bool;

#if defined(OPTIM_COMP)
#define TaggedToStream(X) TermToPointer(stream_node_t, X)
#endif

/* Streams pointing to "user" -- should be shared */
extern stream_node_t *stream_user_input; /* Shared */
extern stream_node_t *stream_user_output; /* Shared */
extern stream_node_t *stream_user_error; /* Shared */

/* root of the stream pointers -- shared */
extern LOCK stream_list_l;
extern stream_node_t *root_stream_ptr; /* Shared & locked */

/* (initialization) */
void init_streams(void);

/* operations on streams */

stream_node_t *new_stream(tagged_t name, char *mode, FILE *file);
stream_node_t *stream_to_ptr(tagged_t t, int mode);
stream_node_t *stream_to_ptr_check(tagged_t t, int mode, int *errcode);

stream_node_t *insert_new_stream(stream_node_t *new_stream);
void update_stream(stream_node_t *s, FILE *file);

CFUN__PROTO(ptr_to_stream_noalias, tagged_t, stream_node_t *n);
CFUN__PROTO(ptr_to_stream, tagged_t, stream_node_t *n);

#endif /* _CIAO_STREAM_BASIC_H */
