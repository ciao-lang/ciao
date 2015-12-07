/*
 *  io_basic.h
 *
 *  Input/output predicates (see engine(io_basic)).
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_INOUT_H
#define _CIAO_INOUT_H

CBOOL__PROTO(flush_output);
CBOOL__PROTO(flush_output1);
CBOOL__PROTO(code_class);
CBOOL__PROTO(getct);
CBOOL__PROTO(getct1);
CBOOL__PROTO(get);
CBOOL__PROTO(get2);
CBOOL__PROTO(get1);
CBOOL__PROTO(get12);
CBOOL__PROTO(peek);
CBOOL__PROTO(peek2);
CBOOL__PROTO(nl);
CBOOL__PROTO(nl1);
CBOOL__PROTO(put);
CBOOL__PROTO(put2);
CBOOL__PROTO(tab);
CBOOL__PROTO(tab2);
CBOOL__PROTO(skip);
CBOOL__PROTO(skip2);
CBOOL__PROTO(skip_line);
CBOOL__PROTO(skip_line1);
CBOOL__PROTO(get_byte1);
CBOOL__PROTO(get_byte2);
CBOOL__PROTO(put_byte1);
CBOOL__PROTO(put_byte2);
void print_string(stream_node_t *stream, char *p);
CVOID__PROTO(print_variable, stream_node_t *stream, tagged_t term);
CVOID__PROTO(print_number, stream_node_t *stream, tagged_t term);
CVOID__PROTO(print_atom, stream_node_t *stream, tagged_t term);
CBOOL__PROTO(prolog_display);
CBOOL__PROTO(prolog_display2);
CBOOL__PROTO(prolog_displayq);
CBOOL__PROTO(prolog_displayq2);
CBOOL__PROTO(prolog_clearerr);
CBOOL__PROTO(prolog_fast_read_in_c);
CBOOL__PROTO(prolog_fast_write_in_c);

CBOOL__PROTO(compressLZ); /* OPA */
CBOOL__PROTO(copyLZ); /* OPA */

CVOID__PROTO(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

#define ENG_PRINTF(S, FMT, ...) {	  \
    char m_buf[2048];			  \
    sprintf(m_buf, FMT , ## __VA_ARGS__); \
    print_string(S, m_buf);		  \
  }

#define ENG_TTYPRINTF(FMT, ...) ENG_PRINTF(Error_Stream_Ptr, FMT , ## __VA_ARGS__)

#endif /* _CIAO_INOUT_H */
