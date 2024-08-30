/*
 *  io_basic.h
 *
 *  Input/output predicates (see engine(io_basic)).
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_IO_BASIC_H
#define _CIAO_IO_BASIC_H

#include <ciao/rune.h>

/* --------------------------------------------------------------------------- */

/* Deref (Term) and display */
#define DerefDisplayTerm(Term,Stream,Quoted) ({ \
  tagged_t t_ = (Term); \
  DEREF(t_, t_); \
  CVOID__CALL(display_term, t_, (Stream), (Quoted)); \
})
/* Note: term is expected to be dereferenced */
CVOID__PROTO(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

/* --------------------------------------------------------------------------- */

#define StreamPrintf(S, FMT, ...) ({ \
  char m_buf[2048]; \
  snprintf(m_buf, sizeof(m_buf), FMT, ## __VA_ARGS__); \
  CVOID__CALL(print_string, S, m_buf); \
})

CVOID__PROTO(print_string, stream_node_t *stream, char *p);

/* --------------------------------------------------------------------------- */

#define RUNE_EOF -1
#define RUNE_PAST_EOF -2
#define RUNE_VOID -100

#define RUNE_ERROR 0xFFFD /* Unicode Replacement character */

/* --------------------------------------------------------------------------- */

#if defined(USE_MULTIBYTES)

#define RUNE_SURROGATE_MIN  0x00d800
#define RUNE_SURROGATE_MAX  0x00dfff
#define RUNE_MAX            0x10ffff

#define C_MB_LEN_MAX        4

static inline bool_t isValidRune(c_rune_t rune) {
  uint32_t r = (uint32_t)rune; /* force unsigned */
  return (r < RUNE_SURROGATE_MIN || RUNE_SURROGATE_MAX < r || r <= RUNE_MAX);
}

#else /* !defined(USE_MULTIBYTES) */

#define RUNE_MAX 0xff

static inline bool_t isValidRune(c_rune_t rune) {
  uint32_t r = (uint32_t)rune; /* force unsigned */
  return (r <= RUNE_MAX);
}

#endif

#if defined(USE_MULTIBYTES)
int c_mbtorune(c_rune_t *pr, const char *s);
int c_runetomb(char * s, c_rune_t r);
int c_mblen(const char *s);
int c_mbstrlen(const char * s);
#endif

void print_syserror(char *s); /* TODO: move somewhere else? */ 

#endif /* _CIAO_IO_BASIC_H */
