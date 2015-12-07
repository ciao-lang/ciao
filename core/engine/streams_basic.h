/*
 *  streams_basic.h
 *
 *  Stream handling primitives (see engine(streams_basic)).
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#ifndef _CIAO_STREAMS_BASIC_H
#define _CIAO_STREAMS_BASIC_H

/* Streams pointing to "user" -- should be shared */

extern stream_node_t *stream_user_input;                   /* Shared */
extern stream_node_t *stream_user_output;                  /* Shared */
extern stream_node_t *stream_user_error;                   /* Shared */


/* root of the stream pointers -- shared */

extern stream_node_t *root_stream_ptr;            /* Shared & locked */

/* initialization */

void init_streams(void);

/* operations on streams */

stream_node_t *insert_new_stream(stream_node_t *new_stream);
void update_stream(stream_node_t *s, FILE *file);

CFUN__PROTO(ptr_to_stream_noalias, tagged_t, stream_node_t *n);
CFUN__PROTO(ptr_to_stream, tagged_t, stream_node_t *n);

/* stream predicates */

CBOOL__PROTO(prolog_bootversion);
CBOOL__PROTO(prolog_force_interactive);
CBOOL__PROTO(prolog_sourcepath);
CBOOL__PROTO(prolog_open);
CBOOL__PROTO(prolog_close);
CBOOL__PROTO(prolog_unix_popen);
CBOOL__PROTO(prolog_pipe);
void ENG_perror(char *s);
CBOOL__PROTO(prolog_current_input);
CBOOL__PROTO(prolog_set_input);
CBOOL__PROTO(prolog_current_output);
CBOOL__PROTO(prolog_set_output);
CBOOL__PROTO(prolog_get_stream);
CBOOL__PROTO(prolog_replace_stream);
CBOOL__PROTO(prolog_stream_code);
CBOOL__PROTO(character_count);
CBOOL__PROTO(line_position);
CBOOL__PROTO(line_count);

CBOOL__PROTO(current_stream);
CBOOL__PROTO(nd_current_stream);

#endif /* _CIAO_STREAMS_BASIC_H */
