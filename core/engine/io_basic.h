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

#define RUNE_EOF            -1
#define RUNE_PAST_EOF       -2
#define RUNE_VOID           -100

#if defined(USE_MULTIBYTES)

#define RUNE_SURROGATE_MIN  0x00d800
#define RUNE_SURROGATE_MAX  0x00dfff
#define RUNE_ERROR          0x00fffd
#define RUNE_MAX            0x10ffff

#define C_MB_LEN_MAX        4

static inline bool_t
isValidRune(c_rune_t rune){
  uint32_t r = (uint32_t) rune; 
  return (r < RUNE_SURROGATE_MIN || RUNE_SURROGATE_MAX < r || r <= RUNE_MAX);
}

#else

#define RUNE_MAX            0xff

static inline bool_t
isValidRune(c_rune_t rune){
  uint32_t r = (uint32_t) rune; 
  return (r <= RUNE_MAX);
}

#endif // USE_MULTIBYTES

#if defined(USE_MULTIBYTES)

/* c_mbtorune(c_rune_t *pr, const char *s) converts a multibyte UTF8
 * character s into a rune, stores the result in the object pointer by
 * pr, and returns the number of bytes consumed. If the input is not a
 * in proper UTF format, s is set to C_RuneError and the function
 * return 1;
 */
int c_mbtorune(c_rune_t *pr, const char *s);

/* c_runetomb(char * s, c_rune_t rune) converts a rune, r, into a UTF8
 * ultibyte character, stores the result in s. The object pointed by s
 * must be large enough to accommodate the multibyte character, which
 * may be up to C_MbLenMax bytes. If the rune is not a valid rune, the
 * rune RUNE_ERROR is write instead. the function returns the number
 * writen in s.
 */
int c_runetomb(char * s, c_rune_t r);

/*
 * c_mblen(const char *s) determines the length, in bytes, of a
 * multibyte character s, which may be up to C_MbLenMax.  The function
 * looks at the first byte of s only. If the first byte of s is not in
 * a proper UTF8 format, the function return -1.
 */
inline int c_mblen(const char *s);


/* c_mbstrlen(const char *s) computes the number of bytes used by a
 * multibyte character string s. The function returns the number of
 * bytes that precede the terminating NULL multibytes chararacter. If
 * the string is not in proper UTF8 format, the behaviour is
 * unspecified.
 */
int c_mbstrlen(const char * s);

#endif // USE_MULTIBYTES

#endif /* _CIAO_INOUT_H */
