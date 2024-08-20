/*
 *  io_basic.c
 *
 *  Input/output predicates (see engine(io_basic)).
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#include <ciao/eng.h>
#include <ciao/io_basic.h> /* CheckGetRune, rune.h */

#if !defined(OPTIM_COMP)
#include <ciao/eng_registry.h> /* GET_ATOM */
#include <ciao/eng_interrupt.h> /* int_address */
#include <ciao/atomic_basic.h> /* number_to_string, string_to_number */
#include <ciao/stream_basic.h> /* stream_to_ptr_check, stream aliases */
#include <ciao/eng_bignum.h> /* StringToInt */
#include <ciao/eng_gc.h> /* explicit_heap_overflow */
#endif

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#else
#include <sys/select.h> /* select() */ 
#endif

/* TODO: improve stream abstraction and separate generic code */

#if defined(OPTIM_COMP)
/* The creation of new streams should be atomic. */
LOCK stream_list_l;

stream_node_t *root_stream_ptr;               /* Shared and _locked_ */
#endif

/* --------------------------------------------------------------------------- */

/* Own version of getc() that normalizes EOF (<0) to -1 */
#if (EOF == -1)
static inline int c_getc(FILE *f) {
  return getc(f);
}
#else
static inline int c_getc(FILE *f) {
  int i = getc(f);
  return (i < 0 ? -1 : i);
}
#endif

#define BYTE_EOF       (-1)
#define BYTE_PAST_EOF  (-2)

static CFUN__PROTO(readrune, c_rune_t, stream_node_t *s, int op_type, definition_t *pred_address);
static CVOID__PROTO(writerune, c_rune_t r, stream_node_t *s);
static CVOID__PROTO(writerunen, c_rune_t r, int i, stream_node_t *s);
static CFUN__PROTO(readbyte, int, stream_node_t *s, int op_type, definition_t *pred_address);
static CVOID__PROTO(writebyte, int ch, stream_node_t *s);

#if defined(USE_MULTIBYTES)
c_rune_t getmb(FILE * f);
c_rune_t putmb(c_rune_t c, FILE * f);
int readmb(int fildes, c_rune_t * c);
int writemb(int fildes, c_rune_t c);

#define BIT1 7
#define BITX 6
#define BIT2 5
#define BIT3 4
#define BIT4 3

#define BYTE1 ((1 << BIT1) ^ 0xff)             // 0111 1111
#define BYTEX ((1 << BITX) ^ 0xff)             // 1011 1111
#define BYTE2 ((1 << BIT2) ^ 0xff)             // 1101 1111
#define BYTE3 ((1 << BIT3) ^ 0xff)             // 1110 1111
#define BYTE4 ((1 << BIT4) ^ 0xff)             // 1111 0111

#define TAG1  (((1 << (BIT1 + 1)) - 1) ^ 0xff) // 0000 0000
#define TAGX  (((1 << (BITX + 1)) - 1) ^ 0xff) // 1000 0000
#define TAG2  (((1 << (BIT2 + 1)) - 1) ^ 0xff) // 1100 0000
#define TAG3  (((1 << (BIT3 + 1)) - 1) ^ 0xff) // 1110 0000
#define TAG4  (((1 << (BIT4 + 1)) - 1) ^ 0xff) // 1111 0000

#define MASKX ((1 << BITX) - 1)                // 0011 1111

#define RUNE1 ((1 << (BIT1 + 0*BITX)) - 1)     // 0000 0000 0000 0000 0111 1111
#define RUNE2 ((1 << (BIT2 + 1*BITX)) - 1)     // 0000 0000 0000 0111 1111 1111
#define RUNE3 ((1 << (BIT3 + 2*BITX)) - 1)     // 0000 0000 1111 1111 1111 1111
#define RUNE4 ((1 << (BIT4 + 3*BITX)) - 1)     // 0001 1111 1111 1111 1111 1111 

#endif // USE_MULTIBYTES

#define CheckGetRune(X,C,ArgNo) ({                                      \
  Sw_NUM_Large_Other((X), {                                             \
    C = GetSmall((X));                                                  \
  }, { /* bigint */      \
    BUILTIN_ERROR(ERR_representation_error(character_code), (X), (ArgNo));  \
  }, {                                                              \
    ERROR_IN_ARG((X), (ArgNo), ERR_type_error(integer)); \
  });                                                                   \
  if (!isValidRune((C))) {                                              \
    BUILTIN_ERROR(ERR_representation_error(character_code), (X), (ArgNo));  \
  }                                                                     \
})

#define CheckGetByte(X,C,ArgNo) ({             \
  if (!TaggedIsSmall((X))) {                   \
    ERROR_IN_ARG((X), (ArgNo), ERR_type_error(byte)); \
  }                                            \
  C = GetSmall((X));                           \
  if ((C) < 0 || (C) > 255) {                  \
    ERROR_IN_ARG((X), (ArgNo), ERR_type_error(byte)); \
  } \
})

/* TODO: throw better exception */
#define IO_ERROR(Message) ({ \
  perror((Message)); \
  UNLOCATED_EXCEPTION(ERR_resource_error(r_undefined)); \
})

/* UTF8 Support */
#if defined(USE_MULTIBYTES)

/* c_mbtorune(c_rune_t *pr, const char *s) converts a multibyte UTF8
 * character s into a rune, stores the result in the object pointer by
 * pr, and returns the number of bytes consumed. If the input is not a
 * in proper UTF format, s is set to C_RuneError and the function
 * return 1;
 */
int c_mbtorune(c_rune_t *pr, const char *s) {
  uint32_t s0, sx;
  c_rune_t c;
    
  s0 = (unsigned char) s[0];
  
  c = s0;
  
  if (s0 <= BYTE1) {    // 0 <= s[0] <= BYTE1, i.e. 1 byte sequence 
    *pr = c;
    return 1;
  }
  if (s0 <= BYTEX) {    // BYTE1 < s[0] <= BYTEX, i.e. improper first byte
    goto bad;
  }

  // At least 2 bytes sequence

  sx = ((unsigned char) s[1]);
  if (sx <= BYTE1 || BYTEX < sx) {  // !(BYTE1 < s[1] <= BYTEX), i.e. improper second byte
    goto bad;
  }
  c = (c << BITX) | (sx & MASKX);
  
  if (s0 <= BYTE2) {    // BYTEX < s[0] <= BYTE2, i.e. 2 bytes sequence 
    c = c & RUNE2;
    if (c <= RUNE1) {   // overlong sequence, i.e. c should be encoded using 1 byte
      goto bad;
    }
    *pr = c;
    return 2;
  }
  
  // At least 3 bytes sequence
  
  sx = ((unsigned char) s[2]);
  if (sx <= BYTE1 || BYTEX < sx) {  // !(BYTE1 < s[2] <= BYTEX), i.e. improper third byte
    goto bad;
  }
  c = (c << BITX) | (sx & MASKX);
    
  if (s0 <= BYTE3) {    // BYTE2 < s[0] <= BYTE3, i.e. 3 bytes sequence
    c = c & RUNE3;
    if (c <= RUNE2) {   // overlong sequence, c should be encoded using 2 bytes or less
      goto bad;
    }
    if (RUNE_SURROGATE_MIN <= c && c <= RUNE_SURROGATE_MAX) { // c is an invalid rune
      goto bad;
    }
    *pr = c;
    return 3;
  }
  
  // 4 bytes sequence

  sx = ((unsigned char) s[3]);
  if (sx <= BYTE1 || BYTEX < sx) {  // !(BYTE1 < s[3] <= BYTEX), i.e. improper fourth byte
    goto bad;
  }

  c = (c << BITX) | (sx & MASKX);

  if (s0 <= BYTE4) {    // BYTE3 < s[0] <= BYTE4, i.e. 4 bytes sequence
    c = c & RUNE4;
    if (c <= RUNE3) {   // overlong sequence, c should be encoded using 3 bytes or less
      goto bad;
    }
    if (RUNE_MAX < c) { // c is an invalide rune
      goto  bad;
    }
    *pr = c;
    return 4;
  }

  // BYTE4 < s[0], i.e. improper first byte
      
 bad:
  *pr = RUNE_ERROR;
  return 1;
}

/* c_runetomb(char * s, c_rune_t rune) converts a rune, r, into a UTF8
 * ultibyte character, stores the result in s. The object pointed by s
 * must be large enough to accommodate the multibyte character, which
 * may be up to C_MbLenMax bytes. If the rune is not a valid rune, the
 * rune RUNE_ERROR is write instead. the function returns the number
 * writen in s.
 */
int c_runetomb(char * s, c_rune_t rune) {
  uint32_t c = (uint32_t) rune;

  if (c <= RUNE1) { 
    // rune encodes to 1 byte
    *s =  TAG1 | c;
    return 1;
  }
  
  if (c <= RUNE2) {
    // rune encodes to 2 bytes
    *(s++) = TAG2 |  (c >> 1*BITX);
    *s     = TAGX | ((c >> 0*BITX) & MASKX);
    return 2;
  } 
  
  // Do this test here because RUNE_ERROR uses 3 bytes
  if (!isValidRune(c)) {
    c = (uint32_t) RUNE_ERROR;
  }
  
  if (c <= RUNE3) {
    // rune encodes to 3 bytes
    *(s++) = TAG3 |  (c >> 2*BITX);
    *(s++) = TAGX | ((c >> 1*BITX) & MASKX);
    *s     = TAGX | ((c >> 0*BITX) & MASKX);
    return 3;
  }
  
  // rune encodes to 4 bytes
  *(s++) = TAG4 |  (c >> 3*BITX);
  *(s++) = TAGX | ((c >> 2*BITX) & MASKX);
  *(s++) = TAGX | ((c >> 1*BITX) & MASKX);
  *s     = TAGX | ((c >> 0*BITX) & MASKX);
  return 4;
}

/*
 * c_mblen(const char *s) determines the length, in bytes, of a
 * multibyte character s, which may be up to C_MbLenMax.  The function
 * looks at the first byte of s only. If the first byte of s is not in
 * a proper UTF8 format, the function return -1.
 */
int c_mblen(const char *s) {
  uint32_t c = (unsigned char) s[0];

  if (c <= BYTE1) return 1;
  if (c <= BYTEX) return -1;
  if (c <= BYTE2) return 2;
  if (c <= BYTE3) return 3;
  if (c <= BYTE4) return 4;
  return -1;
}

/* c_mbstrlen(const char *s) computes the number of bytes used by a
 * multibyte character string s. The function returns the number of
 * bytes that precede the terminating NULL multibytes chararacter. If
 * the string is not in proper UTF8 format, the behaviour is
 * unspecified.
 */
int c_mbstrlen(const char * s) {
  int i = 0;

  while(*s) {
    s += c_mblen(s);
    i++;
  }

  return i;
}

c_rune_t getmb(FILE * f) {
  char buff[C_MB_LEN_MAX];
  c_rune_t c;
  int i, n;
  
  c = c_getc(f);
  if (c < 0) {   // IO error or EOF
    return RUNE_EOF;
  }

  buff[0] = c;
  n = c_mblen(buff);
  if (n < 0) {   // Improper first byte
    return RUNE_ERROR;
  }
  
  for (i = 1; i < n; i++) {
    c = c_getc(f);
    if (c <= BYTE1 || BYTEX < c) { // IO error or EOF or improper following byte
      return RUNE_ERROR;
    }
    buff[i] = (char) c;
  }

  c_mbtorune(&c, buff);
  return c;
}

c_rune_t putmb(c_rune_t c, FILE * f) {
  char buff[C_MB_LEN_MAX];
  int n;
  
  n = c_runetomb(buff, c);
  
  for (int i=0; i<n; i++) {
    if (putc(buff[i], f) < 0) {
      return RUNE_EOF;
    }
  }
  
  return c;
}

int readmb(int fildes, c_rune_t *c) {
  char buff[C_MB_LEN_MAX];
  int d, i, m, n;
  
  m = read(fildes, buff, 1);
  if (m <= 0) { // IO error or EOF
    return m;
  }

  n = c_mblen(buff);
  if (n < 0) {
    *c = RUNE_ERROR;
    return 1;
  }
  
  for (i = 1; i < n; i++) {
    m = read(fildes, buff+i, 1);
    d = (unsigned char) buff[i];
    if (m <= 0 || d <= BYTE1 || BYTEX < d) {
      // IO error or EOF or improper following byte
      *c = RUNE_ERROR;
      return i;
    }
  }
  
  c_mbtorune(c, (char*) buff);
  return i;  
}

int writemb(int fildes, c_rune_t c) {
  char buff[C_MB_LEN_MAX];
  int n;

  n = c_runetomb(buff, c);
  return write(fildes, buff, n);  
}

#endif // defined(USE_MULTIBYTES)

CBOOL__PROTO(code_class) {
  ERR__FUNCTOR("io_basic:code_class", 2);
  c_rune_t i;

  DEREF(X(0), X(0));
  CheckGetRune(X(0),i,1);
  // CBOOL__TEST(TaggedIsSmall(X(0)));
  // i = GetSmall(X(0));

  CBOOL__LASTUNIFY(X(1),MakeSmall(get_rune_class(i)));
}

static inline void inc_counts(int ch, stream_node_t * stream) {
  stream->rune_count++;
  if (ch == 0xd) {
    stream->last_nl_pos = stream->rune_count;
    stream->nl_count++;
  } else if (ch == 0xa) {
    stream->last_nl_pos = stream->rune_count;
    if (stream->previous_rune != 0xd) {
      stream->nl_count++;
    }
  }
  stream->previous_rune = ch;
}

static CVOID__PROTO(writerune, int ch, stream_node_t *s) {
  FILE *f = s->streamfile;
  if (s->isatty) {
    s = root_stream_ptr;
    /* ignore errors on tty */
    putc(ch, f);
  } else if (s->streammode != 's') { /* not a socket */
    if (putc(ch, f) < 0) {
      IO_ERROR("putc() in writerune()");
    }
  } else { /* a socket */
    char p;
    p = (char)ch;
    if (write(TaggedToIntmach(s->label), &p, (size_t)1) < 0) {
      IO_ERROR("write() in writerune()");
    }
  }
  inc_counts(ch,s);
}

static CVOID__PROTO(writerunen, int ch, int i, stream_node_t *s) {
  while (--i >= 0) {
    CVOID__CALL(writerune, ch, s);
  }
}

static CVOID__PROTO(writebyte, int ch, stream_node_t *s) {
  FILE *f = s->streamfile;
  if (s->isatty) {
    s = root_stream_ptr;
    /* ignore errors on tty */
    putc(ch, f);
  } else if (s->streammode != 's') { /* not a socket */
    if (putc(ch, f) < 0) {
      IO_ERROR("putc() in writebyte()");
    }
  } else { /* a socket */
    char p;
    p = (char)ch;
    if (write(TaggedToIntmach(s->label), &p, (size_t)1) < 0) {
      IO_ERROR("write() in writebyte()");
    }
  }
}

#define DELRET -5
#define PEEK   -4
#define GET    -3
#define GET1   -2
#define SKIPLN -1

static inline int read_exit_cond(int op_type, c_rune_t r) {
  return (op_type<GET1 ||
          r==RUNE_EOF ||
          (op_type==GET1 && get_rune_class(r)>0) ||
          (op_type==SKIPLN && (r==0xa || r==0xd)) ||
          op_type==r);
}

static inline int read_give_back_cond(int op_type, c_rune_t r) {
  return (op_type==PEEK ||
          (op_type==SKIPLN && r==RUNE_EOF) ||
          (op_type==DELRET && r!=0xa));
}

/* Returns RUNE_PAST_EOF when attempting to read past end of file)

   op_type: DELRET, PEEK, GET, GET1, SKIPLN, or >= 0 for SKIP
 */
static CFUN__PROTO(readrune, c_rune_t, stream_node_t *s, int op_type, definition_t *pred_address) {
  FILE *f = s->streamfile;
  c_rune_t r;

  if (s->isatty) {
    int_address = pred_address;
    while (TRUE) {
      if (root_stream_ptr->rune_count==root_stream_ptr->last_nl_pos) {
        CVOID__CALL(print_string, stream_user_output, GetString(current_prompt));
        /* fflush(stdout); into print_string() MCL */
      }

      if (s->pending_rune == RUNE_VOID) { /* There is no char returned by peek */
        /* ignore errors in tty */
        r = c_getc(f);
      } else {
        r = s->pending_rune;
        s->pending_rune = RUNE_VOID;
      }

      if (read_give_back_cond(op_type,r)) {
        s->pending_rune = r;
      } else {
        inc_counts(r,root_stream_ptr);
      }

      if (r==RUNE_EOF) clearerr(f);

      if (read_exit_cond(op_type,r)) {
        int_address = NULL; 
        return r;
      }
    }
  } else if (s->streammode != 's') { /* not a socket */
    if (s->pending_rune == RUNE_VOID && feof(f)) {
      return RUNE_PAST_EOF; /* attempt to read past end of stream */
    }
    
    while (TRUE) {
      if (s->pending_rune != RUNE_VOID) { /* There is a char returned by peek */
        r = s->pending_rune;
        s->pending_rune = RUNE_VOID;
      } else {
        r = c_getc(f);
        if (r < 0 && ferror(f)) {
          IO_ERROR("getc() in readrune()");
        }
      }

      if (read_give_back_cond(op_type,r)) {
        s->pending_rune = r;
      } else {
        inc_counts(r,s);
      }
      
      if (read_exit_cond(op_type,r)) return r;
    }
  } else { /* a socket */
    int fildes = TaggedToIntmach(s->label);
    
    if (s->socket_eof) return RUNE_PAST_EOF; /* attempt to read past end of stream */
    
    while (TRUE) {
      unsigned char ch;
      if (s->pending_rune == RUNE_VOID) { /* There is a char returned by peek */
        switch(read(fildes, (void *)&ch, 1)) {
        case 0:
          r = RUNE_EOF;
          break;
        case 1: 
          r = (int)ch;
          break;
        default:
          IO_ERROR("read() in readrune()");
        }
      } else {
        r = s->pending_rune;
        s->pending_rune = RUNE_VOID;
      }
      
      if (read_give_back_cond(op_type,r)) {
        s->pending_rune = r;
      } else {
        inc_counts(r,s);
        if (r==RUNE_EOF) s->socket_eof = TRUE;
      }

      if (read_exit_cond(op_type,r)) return r;
    }
  }
}

static CFUN__PROTO(readbyte, int, stream_node_t *s, int op_type,
                   definition_t *pred_address) {
  FILE *f = s->streamfile;
  int i;

  if (s->isatty) {
    int_address = pred_address;
    if (root_stream_ptr->rune_count==root_stream_ptr->last_nl_pos) {
      CVOID__CALL(print_string, stream_user_output,GetString(current_prompt));
      /* fflush(stdout); into print_string() MCL */
    }

    if (s->pending_rune == RUNE_VOID) { /* There is no char returned by peek */
      /* ignore errors in tty */
      i = c_getc(f);
    } else {
      i = s->pending_rune;
      s->pending_rune = RUNE_VOID;
    }
      
    if (op_type == PEEK) { /* read_give_back_cond */
      s->pending_rune = i;
    }

    if (i==BYTE_EOF) clearerr(f);
    
    int_address = NULL; 
    return i;
  } else if (s->streammode != 's') { /* not a socket */
    if (s->pending_rune == RUNE_VOID && feof(f)) {
      return BYTE_PAST_EOF; /* attempt to read past end of stream */
    }
    if (s->pending_rune != RUNE_VOID) { /* There is a char returned by peek */
      i = s->pending_rune;
      s->pending_rune = RUNE_VOID;
    } else {
      i = c_getc(f);
      if (i < 0 && ferror(f)) {
        IO_ERROR("getc() in readbyte()");
      }
    }
    if (op_type == PEEK) { /* read_give_back_cond */
      s->pending_rune = i;
    }
    return i;
  } else { /* a socket */
    unsigned char ch;
    int fildes = TaggedToIntmach(s->label);
    
    if (s->socket_eof) return BYTE_PAST_EOF; /* attempt to read past end of stream */
    
    if (s->pending_rune == RUNE_VOID) { /* There is a char returned by peek */
      switch(read(fildes, (void *)&ch, 1)) {
      case 0:
        i = BYTE_EOF;
        break;
      case 1: 
        i = (int)ch;
        break;
      default:
        IO_ERROR("read() in readbyte()");
      }
    } else {
      i = s->pending_rune;
      s->pending_rune = RUNE_VOID;
    }

    if (op_type == PEEK) { /* read_give_back_cond */
      s->pending_rune = i;
    } else {
      if (i==BYTE_EOF) s->socket_eof = TRUE;
    }

    return i;
  }
}

/* ------------------------------------------------------------------------- */

void print_syserror(char *s) {
  /* StreamPrintf(stream_user_error, "%s: %s\n", s, sys_errlist[errno]); */
  /* StreamPrintf(stream_user_error, "ERROR: %s: %s\n", s, strerror(errno)); */
  fprintf(stderr, "ERROR: %s: %s\n", s, strerror(errno));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(get) {
  ERR__FUNCTOR("io_basic:get_code", 1);
  c_rune_t r;

  r = CFUN__EVAL(readrune,Input_Stream_Ptr,GET,address_get);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(r));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get2) {
  ERR__FUNCTOR("io_basic:get_code", 2);
  c_rune_t r;
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  r = CFUN__EVAL(readrune,s,GET,address_get2);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(r));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get1) {
  ERR__FUNCTOR("io_basic:get1_code", 1);
  c_rune_t r;
  
  r = CFUN__EVAL(readrune,Input_Stream_Ptr,GET1,address_get1); /* skip whitespace */
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(r));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get12) {
  ERR__FUNCTOR("io_basic:get1_code", 2);
  c_rune_t r;
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  r = CFUN__EVAL(readrune,s,GET1,address_get12); /* skip whitespace */
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(r));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(peek) {
  ERR__FUNCTOR("io_basic:peek_code", 1);
  c_rune_t r;

  r = CFUN__EVAL(readrune,Input_Stream_Ptr,PEEK,address_peek);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(r));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(peek2) {
  ERR__FUNCTOR("io_basic:peek_code", 2);
  c_rune_t r;
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  r = CFUN__EVAL(readrune,s,PEEK,address_peek2);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(r));
}

/* ------------------------------------------------------------------------- */

/* Read a UTF8 rune (return value) and assign a rune class to *typ */
static CFUN__PROTO(readrune_mb, c_rune_t,
                   stream_node_t *s, int op_type,
                   definition_t *pred_address, int *typ) {
  c_rune_t r;
  int typ_;
  
 again:
  r = CFUN__EVAL(readrune,s,op_type,pred_address);
  if (r<0) { *typ = -1; return r; }
  if (r <= 0x7F) { /* 1 byte */
  } else if (r <= 0xBF) {
    r = RUNE_ERROR;
  } else { /* 2 or more bytes */
    unsigned char b[4];
    int len;
    /* get length and read pending bytes */
    if (r <= 0xDF) { len=2; }
    else if (r <= 0xEF) { len=3; }
    else if (r <= 0xF7) { len=4; }
    else { len=0; }
    b[0] = (unsigned char)r;
    for (int i=1; i<len; i++) {
      r = CFUN__EVAL(readrune,s,GET,pred_address);
      if (r<0 || (r&0xC0)!=0x80) { len=0; break; } /* force error */
      b[i] = (unsigned char)r;
    }
    /* compose rune */
    switch(len) {
    case 2: /* 2 bytes */
      r = ((b[0]&0x1F)<<6)|(b[1]&0x3F);
      if (r < 0x80) r = RUNE_ERROR;
      break;
    case 3: /* 3 bytes */
      r = ((b[0]&0xF)<<12)|((b[1]&0x3F)<<6)|(b[2]&0x3F);
      if (r < 0x800) r = RUNE_ERROR;
      break;
    case 4: /* 4 bytes */
      r = ((b[0]&0x7)<<18)|((b[1]&0x3F)<<12)|((b[2]&0x3F)<<6)|(b[3]&0x3F);
      if (r < 0x10000) r = RUNE_ERROR;
      break;
    default:
      r = RUNE_ERROR; 
      break;
    }
  }
  typ_ = get_rune_class(r);
  if (op_type == GET1 && typ_ == RUNETY_LAYOUT) goto again;
  *typ = typ_;
  return r;
}

CBOOL__PROTO(getct) {
  ERR__FUNCTOR("io_basic:getct", 2);
  c_rune_t r;
  int typ;
  r = CFUN__EVAL(readrune_mb,Input_Stream_Ptr,GET,address_getct,&typ);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }
  CBOOL__UNIFY(X(0),MakeSmall(r));
  CBOOL__LASTUNIFY(X(1),MakeSmall(typ));
}

CBOOL__PROTO(getct1) {
  ERR__FUNCTOR("io_basic:getct1", 2);
  c_rune_t r;
  int typ;
  r = CFUN__EVAL(readrune_mb,Input_Stream_Ptr,GET1,address_getct1,&typ);
  if (r == RUNE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }
  CBOOL__UNIFY(X(0),MakeSmall(r));
  CBOOL__LASTUNIFY(X(1),MakeSmall(typ));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(nl) {
  CVOID__CALL(writerune, '\n',Output_Stream_Ptr);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(nl1) {
  ERR__FUNCTOR("io_basic:nl", 1);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  CVOID__CALL(writerune, '\n',s);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put) {
  ERR__FUNCTOR("io_basic:put_code", 1);
  c_rune_t r;

  DEREF(X(0), X(0));
  CheckGetRune(X(0),r,1);
  CVOID__CALL(writerune, r, Output_Stream_Ptr);

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put2) {
  ERR__FUNCTOR("io_basic:put_code", 2);
  c_rune_t r;
  int errcode;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1), X(1));
  CheckGetRune(X(1),r,2);
  CVOID__CALL(writerune, r, s);

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/* output stream always write or append */

CBOOL__PROTO(tab) {
  ERR__FUNCTOR("io_basic:tab", 1);
  DEREF(X(0),X(0));
  if (!IsInteger(X(0))) {
    ERROR_IN_ARG(X(0),1,ERR_type_error(integer));
  }

  CVOID__CALL(writerunen, ' ', TaggedToIntmach(X(0)), Output_Stream_Ptr);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(tab2) {
  ERR__FUNCTOR("io_basic:tab", 2);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  if (!IsInteger(X(1))) {
    ERROR_IN_ARG(X(1),2,ERR_type_error(integer));
  }

  CVOID__CALL(writerunen, ' ', TaggedToIntmach(X(1)), s);
  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip) {
  ERR__FUNCTOR("io_basic:skip_code", 1);
  c_rune_t r, r1;

  DEREF(X(0),X(0));
  CheckGetRune(X(0),r,1);

  for (r1=r+1; r1!=r;) {
    r1 = CFUN__EVAL(readrune,Input_Stream_Ptr,r,address_skip);
    if (r1 == RUNE_PAST_EOF) {
      BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
    }
  }

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip2) {
  ERR__FUNCTOR("io_basic:skip_code", 2);
  c_rune_t r, r1;
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  CheckGetRune(X(1),r,2);

  for (r1=r+1; r1!=r;) {
    r1 = CFUN__EVAL(readrune,s,r,address_skip2);
    if (r1 == RUNE_PAST_EOF) {
      BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
    }
  }

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip_line) {
  // ERR__FUNCTOR("io_basic:skip_line", 0);
  int r;

  for (r=0; r!=0xa && r!=0xd && r>=0;) {
    r = CFUN__EVAL(readrune,Input_Stream_Ptr,SKIPLN,address_skip_line);
  }

  if (r == 0xd) { /* Delete a possible 0xa (win end-of-line) */
    (void)CFUN__EVAL(readrune,Input_Stream_Ptr,DELRET,address_skip_line);
  }

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip_line1) {
  ERR__FUNCTOR("io_basic:skip_line", 1);
  int errcode;
  c_rune_t r;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  for (r=0; r!=0xa && r!=0xd && r>=0;) {
    r = CFUN__EVAL(readrune,s,SKIPLN,address_skip_line1);
  }

  if (r == 0xd) { /* Delete a possible 0xa (win end-of-line) */
    (void)CFUN__EVAL(readrune,s,DELRET,address_skip_line1);
  }

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(get_byte1) {
  ERR__FUNCTOR("io_basic:get_byte", 1);
  int i;

  i = CFUN__EVAL(readbyte,Input_Stream_Ptr,GET,address_get_byte1);
  if (i == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(i));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(get_byte2) {
  ERR__FUNCTOR("io_basic:get_byte", 2);
  int i, errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  i = CFUN__EVAL(readbyte,s,GET,address_get_byte2);
  if (i == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(i));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(peek_byte1) {
  ERR__FUNCTOR("io_basic:peek_byte", 1);
  int i;

  i = CFUN__EVAL(readbyte,Input_Stream_Ptr,PEEK,address_peek_byte1);
  if (i == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  CBOOL__LASTUNIFY(X(0),MakeSmall(i));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(peek_byte2) {
  ERR__FUNCTOR("io_basic:peek_byte", 2);
  int i, errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  i = CFUN__EVAL(readbyte,s,PEEK,address_peek_byte2);
  if (i == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),X(0),1);
  }

  CBOOL__LASTUNIFY(X(1),MakeSmall(i));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(put_byte1) {
  ERR__FUNCTOR("io_basic:put_byte", 1);
  int i;

  DEREF(X(0),X(0));
  CheckGetByte(X(0),i,1);;
  CVOID__CALL(writebyte,i,Output_Stream_Ptr);

  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(put_byte2) {
  ERR__FUNCTOR("io_basic:put_byte", 2);
  int i;
  int errcode;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  CheckGetByte(X(1),i,2);;
  CVOID__CALL(writerune, i, s);

  CBOOL__PROCEED;
}

/*----------------------------------------------------------------*/

// TODO: set dopeek=FALSE for end_of_stream(_) property

/* Return RUNE_EOF if we are 'at'-end-of-stream, RUNE_PAST_EOF if we
   are 'past'-end-of-stream or 0 otherwise. If 'dopeek' then a byte
   will be peeked (if possible) to determine if the stream has data. */
static CFUN__PROTO(stream_end_of_stream, int, stream_node_t *s, bool_t dopeek) {
  FILE *f = s->streamfile;
  int i;

  if (s->streammode != 's') { /* not a socket */
    if (s->pending_rune == RUNE_VOID && feof(f)) {
      return RUNE_PAST_EOF; /* attempt to read past end of stream */
    }
    if (s->pending_rune != RUNE_VOID) { /* There is a char returned by peek */
      return (s->pending_rune == RUNE_EOF ? RUNE_EOF : 0);
    } else {
      /* TODO: "pending_rune" should be a small byte buffer */
      if (dopeek) { /* peek a byte */
        i = c_getc(f);
        if (i < 0) { /* EOF */
          if (s->isatty) { /* ignore errors in tty */ /* TODO: sure? */
            clearerr(f);
          } else {
            if (ferror(f)) IO_ERROR("getc() in stream_end_of_stream()");
          }
          s->pending_rune = i;
          return RUNE_EOF;
        } else { /* not EOF */
          s->pending_rune = i; /* TODO: pending byte */
        }
      }
      return 0;
    }
  } else { /* a socket */
    unsigned char ch;
    int fildes = TaggedToIntmach(s->label);

    if (s->socket_eof) return RUNE_PAST_EOF; /* attempt to read past end of stream */

    if (s->pending_rune == RUNE_VOID) { /* There is a char returned by peek */
      if (dopeek) { /* peek a byte */
        switch(read(fildes, (void *)&ch, 1)) {
        case 0:
          i = BYTE_EOF;
          break;
        case 1: 
          i = (int)ch;
          break;
        default:
          IO_ERROR("read() in readbyte()");
        }
        if (i < 0) { /* EOF */
          s->pending_rune = i;
          return RUNE_EOF;
        } else { /* not EOF */
          s->pending_rune = i; /* TODO: pending byte */
        }
      }
      return 0;
    } else {
      return (s->pending_rune == RUNE_EOF ? RUNE_EOF : 0);
    }
  }
}

CBOOL__PROTO(at_end_of_stream0) {
  //ERR__FUNCTOR("io_basic:at_end_of_stream0", 0);
  return CFUN__EVAL(stream_end_of_stream, Input_Stream_Ptr, TRUE) < 0;
}

CBOOL__PROTO(at_end_of_stream1) {
  ERR__FUNCTOR("io_basic:at_end_of_stream1", 1);
  int errcode;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  return CFUN__EVAL(stream_end_of_stream, s, TRUE) < 0;
}

/*----------------------------------------------------------------*/
/* NOTE: Moved from stream_basic.c (DCG) */
/*----------------------------------------------------------------*/

/* TODO: should fflush() be moved where it is needed? slow? */

/* This is essentially an open-coded fputs().  
   fputs() starts paying off at string lengths above 50 or so.
 */
CVOID__PROTO(print_string, stream_node_t *stream, char *p) {
  FILE *fileptr = stream->streamfile;
  c_rune_t r;

  if (stream->isatty) {
    stream = root_stream_ptr;
    for (r = *p++; r; r = *p++) {
      /* ignore errors on tty */
      putc(r,fileptr);
      inc_counts(r,stream);
    }
  } else if (stream->streammode != 's') { /* not a socket */
    for (r = *p++; r; r = *p++) {
      if (putc(r,fileptr) < 0) {
        IO_ERROR("putc() in in print_string()");
      }
      inc_counts(r,stream);
    }
  } else { /* a socket */
    size_t size = 0;
    char *q = p;

    for (r = *q++; r; r = *q++) {
      inc_counts(r,stream);
      size++;
    }
    if (write(TaggedToIntmach(stream->label), p, size) < 0) {
      IO_ERROR("write() in print_string()");
    }
  }
  fflush(fileptr);
}

/* From HVA/SVA to NUM (for printing) */
CFUN__PROTO(var_address, tagged_t, tagged_t term) {
  intval_t n;
#if defined(OPTIM_COMP) /* TODO:[oc-merge] divide by sizeof(tagged_t)? */
  if (TaggedIsSVA(term)) {
    n = (TaggedToPointer(term) - Stack_Start) * sizeof(tagged_t) + HeapCharSize();
  } else {
    n = (TaggedToPointer(term) - Heap_Start) * sizeof(tagged_t);
  }
#else
  if (TaggedIsSVA(term)) {
    n = TaggedToPointer(Tagp(HVA,Heap_End+(TagpPtr(SVA,term)-Stack_Start)))-Heap_Start;
  } else {
    n = TaggedToPointer(term)-Heap_Start;
  }
#endif
  return IntmachToTagged(n);
}

CVOID__PROTO(print_variable, stream_node_t *stream, tagged_t term) {
  CVOID__CALL(number_to_string, CFUN__EVAL(var_address, term), 10);
  CVOID__CALL(print_string, stream, "_");
  CVOID__CALL(print_string, stream, Atom_Buffer);
}

CVOID__PROTO(print_number, stream_node_t *stream, tagged_t term) {
  CVOID__CALL(number_to_string, term, 10);
  CVOID__CALL(print_string, stream, Atom_Buffer);
}

#define FULL_ESCAPE_QUOTED_ATOMS 1

/* Max size of printed atom (+3 due to 0'', ..., 0'', 0'\0) */
#define PRINT_ATOM_BUFF_SIZE PRINT_RUNE_BUFF_SIZE*MAXATOM+3
/* Max size of a single printed rune */
#if defined(FULL_ESCAPE_QUOTED_ATOMS)
#define PRINT_RUNE_BUFF_SIZE 5 /* (e.g., 0'1  => 0'\\ 0'0 0'0 0'1 0'\\ */
#else
#define PRINT_RUNE_BUFF_SIZE 2 /* (e.g., 0'\n => 0'\\ 0'n */
#endif      

#define PRINT_CONTROL_RUNE(X) { *bp++ = '\\'; *bp++ = (X); }

CVOID__PROTO(print_atom, stream_node_t *stream, tagged_t term) {
  atom_t *atomptr = TaggedToAtom(term);

  if (!atomptr->has_special) {
    CVOID__CALL(print_string, stream, atomptr->name);
  } else {
    // TODO: do not use checkalloc_ARRAY, use the Atom_Buffer instead?! just check the amot length (JFMC)
    char *buf = checkalloc_ARRAY(char, PRINT_ATOM_BUFF_SIZE);
    unsigned char *ch = (unsigned char *)atomptr->name;
    char *bp = buf;
    c_rune_t r;
      
    *bp++ = '\'';
#if defined(FULL_ESCAPE_QUOTED_ATOMS)
    while ((r = *ch++)) {
      /* See tokenize.pl for table of symbolic control chars */
      if (r <= 0x7F && get_rune_class(r) == 0) { /* TODO: only for ASCII, is it OK? */
        switch (r) {
        case 7: PRINT_CONTROL_RUNE('a'); break;
        case 8: PRINT_CONTROL_RUNE('b'); break;
        case 9: PRINT_CONTROL_RUNE('t'); break;
        case 10: PRINT_CONTROL_RUNE('n'); break;
        case 11: PRINT_CONTROL_RUNE('v'); break;
        case 12: PRINT_CONTROL_RUNE('f'); break;
        case 13: PRINT_CONTROL_RUNE('r'); break;
          /* case 27: PRINT_CONTROL_RUNE('e'); break; */
        case 32: *bp++ = ' '; break;
          /* case 127: PRINT_CONTROL_RUNE('d'); break; */
        default:
          *bp++ = '\\';
          *bp++ = '0' + ((r >> 6) & 7);
          *bp++ = '0' + ((r >> 3) & 7);
          *bp++ = '0' + (r & 7);
          *bp++ = '\\';
        }
      } else {
        if (r=='\'' || r=='\\') { *bp++ = r; }
        *bp++ = r;
      }
    }
#else
    if (atomptr->has_squote) {
      while ((r = *ch++)) {
        if (r=='\'' || r=='\\') { *bp++ = r; }
        *bp++ = r;
      }
    } else {
      while ((r = *ch++)) {
        if (r=='\\') { *bp++ = r; }
        *bp++ = r;
      }
    }
#endif
    *bp++ = '\'';
    *bp++ = 0;
    CVOID__CALL(print_string, stream, buf);
    // TODO: do not use checkalloc_ARRAY, use the Atom_Buffer instead?! just check the atom length (JFMC)
    checkdealloc_ARRAY(char, PRINT_ATOM_BUFF_SIZE, buf);
  }
}

/* --------------------------------------------------------------------------- */

CVOID__PROTO(display_term, tagged_t term, stream_node_t *stream, bool_t quoted) {
  tagged_t aux;
  int arity,i;

  SwOnAnyTagB(term, t_head_functor, { /* HVA */
    goto variable;
  }, { /* SVA */
    goto variable;
  }, { /* CVA */
  variable:
    CVOID__CALL(print_variable,stream,term);
  }, { /* NUM */
  number:
    CVOID__CALL(print_number,stream,term);
  }, { /* ATM */
    if (quoted) {
      CVOID__CALL(print_atom,stream,term);
    } else {
      CVOID__CALL(print_string,stream,TaggedToAtom(term)->name);
    }
  }, { /* LST */
    CVOID__CALL(writerune,'[',stream);
    DerefCar(aux,term);
    CVOID__CALL(display_term,aux, stream, quoted);
    DerefCdr(term,term);
    while(TaggedIsLST(term)) {
      CVOID__CALL(writerune,',',stream);
      DerefCar(aux,term);
      CVOID__CALL(display_term,aux, stream, quoted);
      DerefCdr(term,term);
    }
    if (term!=atom_nil) {
      CVOID__CALL(writerune,'|',stream);
      CVOID__CALL(display_term,term, stream, quoted);
    }
    CVOID__CALL(writerune,']',stream);
  }, { /* STR(blob) */
    /* todo[ts]: change if more blob types are added */
    goto number;
  }, { /* STR(struct) */
    CVOID__CALL(display_term, t_head_functor, stream, quoted);
    CVOID__CALL(writerune,'(',stream);
    arity = Arity(t_head_functor);
    for (i=1; i<=arity; i++) {
      if (i>1) CVOID__CALL(writerune,',',stream);
      DerefArg(aux,term,i);
      CVOID__CALL(display_term,aux, stream, quoted);
    }
    CVOID__CALL(writerune,')',stream);
  });
}

CBOOL__PROTO(prolog_display) {
  DerefDisplayTerm(X(0),Output_Stream_Ptr,FALSE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_display2) {
  ERR__FUNCTOR("io_basic:display", 2);
  int errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DerefDisplayTerm(X(1),stream,FALSE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_displayq) {
  DerefDisplayTerm(X(0),Output_Stream_Ptr,TRUE);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_displayq2) {
  ERR__FUNCTOR("io_basic:displayq", 2);
  int errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DerefDisplayTerm(X(1),stream,TRUE);
  CBOOL__PROCEED;
}

/* ------------------------------------------------------------------------- */
/* fastrw */

#if !defined(OPTIM_COMP)
/* TODO: this code needs serious changes (JF)          */
/*  - see arithmetic.c for code that keep roots for GC */
/*  - do not limit number of variables */
/*  - recode to do better tail recursion */
/*  - single calls to readbyte() can be very slow (specially for sockets) */

#define FASTRW_VERSION  'C'
#define FASTRW_MAX_VARS 1024

#define SPACE_FACTOR 64  /* kludge to ensure more heap space before reading */

CBOOL__PROTO(prolog_fast_read_in_c_aux, 
             tagged_t *out,
             tagged_t *vars,
             int *lastvar);

/* OPA */
CBOOL__PROTO(prolog_fast_read_in_c) {
  ERR__FUNCTOR("fastrw:fast_read", 1);
  int i,lastvar = 0;
  tagged_t term, vars[FASTRW_MAX_VARS];

  /* MCL, JC: Changed getc() to readbyte() because of wrong assumptions when
     using sockets (i.e., streamfile = NULL.  */

  /* NULL as predaddress (really did not bother to find out what to put)  */

  i = CFUN__EVAL(readbyte, Input_Stream_Ptr, GET, NULL);
  if (i == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }
  CBOOL__TEST(i == FASTRW_VERSION);

  TEST_HEAP_OVERFLOW(G->heap_top, SPACE_FACTOR*kCells*sizeof(tagged_t)+CONTPAD, 1);

  CBOOL__CALL(prolog_fast_read_in_c_aux,&term,vars,&lastvar);

  CBOOL__LASTUNIFY(X(0),term);
}

#define CHECK_HEAP_SPACE                                        \
  if (HeapCharDifference(w->heap_top,Heap_End) < CONTPAD) { \
    fprintf(stderr, "Out of heap space in fast_read()\n");      \
  }

CBOOL__PROTO(prolog_fast_read_in_c_aux, 
             tagged_t *out,
             tagged_t *vars,
             int *lastvar) {
  ERR__FUNCTOR("fastrw:fast_read", 1);
  int i,k,j;
  unsigned char *s = (unsigned char *) Atom_Buffer;
  int base;
  
  k = CFUN__EVAL(readbyte, Input_Stream_Ptr, GET, NULL);
  if (k == BYTE_PAST_EOF) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }

  switch(k) {
  case ']':
    *out = atom_nil;
    CBOOL__PROCEED;
  case '[':
    {
      tagged_t *h = w->heap_top;
      w->heap_top += 2;
      CBOOL__CALL(prolog_fast_read_in_c_aux,h,vars,lastvar);
      CBOOL__CALL(prolog_fast_read_in_c_aux,h+1,vars,lastvar);
      *out = Tagp(LST,h);
    }
    CHECK_HEAP_SPACE;
    CBOOL__PROCEED;
  case '_':
  case 'I':
  case 'F':
  case 'A':
  case '"':
  case 'S':
    j = 1;
    for (i=0; j; i++) {
      ENSURE_ATOM_BUFFER(i, { s = (unsigned char *)Atom_Buffer+i; });
      j = CFUN__EVAL(readbyte, Input_Stream_Ptr, GET, NULL);
      if (j == BYTE_PAST_EOF) {
        BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
      }
      *s++ = j;
    }
    switch (k) {
    case '_':
      {
        tagged_t *h = w->heap_top;
        if ((i = atoi(Atom_Buffer)) == *lastvar)
          *h = vars[(*lastvar)++] = Tagp(HVA,w->heap_top++);
        *out = vars[i];
      }
      CHECK_HEAP_SPACE;
      CBOOL__PROCEED;
    case 'I':
      base = GetSmall(current_radix);
      StringToInt(Atom_Buffer, base, *out, 1);
      CHECK_HEAP_SPACE;
      CBOOL__PROCEED;
    case 'F':
      CVOID__CALL(string_to_number, Atom_Buffer, 10, out, 2);
      CHECK_HEAP_SPACE;
      CBOOL__PROCEED;
    case 'A':
      *out = GET_ATOM(Atom_Buffer);
      CBOOL__PROCEED;
    case '"':
      {
        tagged_t *h = w->heap_top;
        i--;
        /* ENSURE_HEAP_LST(i, 1); */
        while (i--) {
          MakeLST(*out,MakeSmall(((unsigned char *)Atom_Buffer)[i]),*out);
        }
        CBOOL__CALL(prolog_fast_read_in_c_aux,h+1,vars,lastvar);
      }
      CHECK_HEAP_SPACE;
      CBOOL__PROCEED;
    case 'S':
      i = CFUN__EVAL(readbyte, Input_Stream_Ptr, GET, NULL);
      if (i == BYTE_PAST_EOF) {
        BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
      }
      {
        tagged_t *h = w->heap_top;
        /* TEST_HEAP_OVERFLOW(h, (i+1)*sizeof(tagged_t)+CONTPAD, 1); */
        *h = SetArity(GET_ATOM(Atom_Buffer),i);
        *out = Tagp(STR,h++);
        w->heap_top += i+1;
        while(i--) {
          CBOOL__CALL(prolog_fast_read_in_c_aux,h++,vars,lastvar);
        }
      }
      CHECK_HEAP_SPACE;
      CBOOL__PROCEED;
    }
  default:
    CBOOL__FAIL;
  }
}

static inline CVOID__PROTO(fast_write_string, stream_node_t *stream, const char * s) {
  for (;*s; s++) {
    CVOID__CALL(writebyte, *s, stream);
  }
}

CVOID__PROTO(fast_write_number,
             stream_node_t *stream,
             tagged_t term) {
  CVOID__CALL(number_to_string,term, 10);
  CVOID__CALL(fast_write_string, stream, Atom_Buffer);
}

CVOID__PROTO(prolog_fast_write_in_c_aux,
             tagged_t in,
             tagged_t *vars, 
             int *lastvar);

/* OPA */
CBOOL__PROTO(prolog_fast_write_in_c) {
  tagged_t vars[FASTRW_MAX_VARS];
  int lastvar = 0;

  DEREF(X(0),X(0));
  CVOID__CALL(writebyte, FASTRW_VERSION, Output_Stream_Ptr);
  CVOID__CALL(prolog_fast_write_in_c_aux,X(0),vars,&lastvar);
  CBOOL__PROCEED;
}

CVOID__PROTO(prolog_fast_write_in_c_aux,
             tagged_t in,
             tagged_t *vars,
             int *lastvar) {
  int i, j;
  intmach_t b;
  tagged_t term;

  switch (TagOf(in)) {
  case LST:
    DerefCar(term,in);
    DerefCdr(in,in);
    if (TaggedIsSmall(term) && (b = GetSmall(term)))
      if ((b > 0) && (b < 256)) {
        for (CVOID__CALL(writebyte,'"',Output_Stream_Ptr);(b > 0) && (b < 256);) {
          CVOID__CALL(writebyte,b,Output_Stream_Ptr);
          if (TagOf(in) == LST) {
            DerefCar(term,in);
            DerefCdr(in,in);
            if (!TaggedIsSmall(term)) {
              break;
            } else {
              b = GetSmall(term);
            }
          } else {
            CVOID__CALL(writebyte,0,Output_Stream_Ptr);
            CVOID__CALL(prolog_fast_write_in_c_aux,in,vars,lastvar);
            return;
          }       
        }
        CVOID__CALL(writebyte,0,Output_Stream_Ptr);
      }
    CVOID__CALL(writebyte,'[',Output_Stream_Ptr);
    CVOID__CALL(prolog_fast_write_in_c_aux,term,vars,lastvar);
    CVOID__CALL(prolog_fast_write_in_c_aux,in,vars,lastvar);
    return;
  case UBV:
  case SVA:
  case HVA:
  case CVA:
    CVOID__CALL(writebyte,'_',Output_Stream_Ptr);
    DEREF(in,in);
    for (i = 0;i < *lastvar; i++) {
      if (vars[i] == in) break;
    }
    if (i == *lastvar) {
      vars[(*lastvar)++] = in;
    }
    sprintf((char *) Atom_Buffer,"%i",i);
    CVOID__CALL(fast_write_string,Output_Stream_Ptr,Atom_Buffer);
    CVOID__CALL(writebyte,0,Output_Stream_Ptr);
    return;
  case STR:
    if (!STRIsLarge(in)) {
      CVOID__CALL(writebyte,'S',Output_Stream_Ptr);
      CVOID__CALL(fast_write_string,Output_Stream_Ptr,TaggedToAtom(TaggedToHeadfunctor(in))->name);
      CVOID__CALL(writebyte,0,Output_Stream_Ptr);
      CVOID__CALL(writebyte,j = Arity(TaggedToHeadfunctor(in)),Output_Stream_Ptr);
      for (i = 1; i <= j; CVOID__CALL(prolog_fast_write_in_c_aux,term,vars,lastvar)) {
        DerefArg(term,in,i++);
      }
      return;
    }
  case NUM:
    if (IsFloat(in)) {
      CVOID__CALL(writebyte,'F',Output_Stream_Ptr);
    } else {
      CVOID__CALL(writebyte,'I',Output_Stream_Ptr);
    }
    CVOID__CALL(fast_write_number,Output_Stream_Ptr,in);
    CVOID__CALL(writebyte,0,Output_Stream_Ptr);
    return;
  case ATM:
    if (in != atom_nil) {
      CVOID__CALL(writebyte,'A',Output_Stream_Ptr);
      CVOID__CALL(fast_write_string,Output_Stream_Ptr,TaggedToAtom(in)->name);
      CVOID__CALL(writebyte,0,Output_Stream_Ptr);
    } else {
      CVOID__CALL(writebyte,']',Output_Stream_Ptr);
    }
    return;
  }
}
#endif

/* ------------------------------------------------------------------------- */

// TODO:[oc-merge] merge both versions
#if defined(OPTIM_COMP)
CBOOL__PROTO(prolog_copy_stdout) {
  ERR__FUNCTOR("io_basic:$copy_stdout", 1);
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  if ((i = c_getc(s->streamfile)) < -1) {
    BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
  }
  
  while (i != EOF) {
    CVOID__CALL(writebyte, i, Output_Stream_Ptr);
    if ((i = c_getc(s->streamfile)) < -1) {
      BUILTIN_ERROR(ERR_permission_error(access, past_end_of_stream),atom_nil,0);
    }
  }
  CBOOL__PROCEED;
}
#else
#define COPYBUFSIZE 4096

/* Copy a stream to Output_Stream_Ptr (low level access to FILE)  */
CBOOL__PROTO(raw_copy_stdout) {
  ERR__FUNCTOR("io_basic:$raw_copy_stdout", 1);
  char buffer[COPYBUFSIZE];
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }
  if (Output_Stream_Ptr->streammode == 's') { /* a socket */
    IO_ERROR("socket not supported in '$raw_copy_stdout'/1");
  }

  FILE *f = s->streamfile;
  FILE *out_f = Output_Stream_Ptr->streamfile; /* (assume this is not a socket) */

  for(;;) {
    int r = fread(buffer, 1, COPYBUFSIZE, f);
    if (r < 1) {
      if (ferror(f) != 0 && feof(f) == 0) {
        IO_ERROR("fread() in '$raw_copy_stdout'/1");
      }
      break;
    }
    if (fwrite(buffer, r, 1, out_f) != 1) {
      IO_ERROR("fwrite() in '$raw_copy_stdout'/1");
    }
  }
  CBOOL__PROCEED;
}
#endif

/* --------------------------------------------------------------------------- */

// TODO:[oc-merge] merge stream_wait.pl into io_basic.pl

// NOTE: be careful! data may be lost if we do buffered reads before
CBOOL__PROTO(prolog_set_unbuf) {
  ERR__FUNCTOR("io_basic:$set_unbuf", 1);
  int errcode;
  stream_node_t *s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }
  setbuf(s->streamfile, NULL);
  CBOOL__PROCEED;
}

CBOOL__PROTO(prolog_input_wait) {
  ERR__FUNCTOR("io_basic:$input_wait", 3);
#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#warning "TODO(MinGW): we need select() in io_basic:$input_wait"
  CBOOL__PROCEED; 
#else
  int errcode;
  stream_node_t *s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  int fd = fileno(s->streamfile);

  if (s->pending_rune != RUNE_VOID) { /* RUNE_EOF or valid rune */
    CBOOL__PROCEED;
  }

  fd_set set;
  struct timeval timeout;
  int rv;
        
  DEREF(X(1), X(1));
  timeout.tv_sec  = TaggedToIntmach(X(1));
  DEREF(X(2), X(2));
  timeout.tv_usec = TaggedToIntmach(X(2));
    
  FD_ZERO(&set); 
  FD_SET(fd, &set); 
    
  rv = select(fd + 1, &set, NULL, NULL, &timeout);
    
  if (rv < 0) {
    BUILTIN_ERROR(ERR_system_error, TaggedZero, 0);
  }

  CBOOL__LASTTEST(rv != 0);
#endif
}

/* --------------------------------------------------------------------------- */

// TODO:[oc-merge] backport
#if defined(OPTIM_COMP)
#define DISPLAY_STRING__BUFFER_SIZE 1024
#define DISPLAY_STRING__FLUSH { \
  if (fwrite(buffer, sizeof(unsigned char), buffer_n, out)) {} else {} \
  buffer_n = 0; \
}
#define DISPLAY_STRING__PUT(I) { \
  buffer[buffer_n++] = (I); \
  if (buffer_n >= DISPLAY_STRING__BUFFER_SIZE) DISPLAY_STRING__FLUSH; \
}
/* Fast string write */
CBOOL__PROTO(prolog_display_string) {
  tagged_t term;
  tagged_t t0;
  FILE *out;
  intmach_t i;
  intmach_t buffer_n;
  unsigned char buffer[DISPLAY_STRING__BUFFER_SIZE];

  out = Output_Stream_Ptr->streamfile;
  
  buffer_n = 0;

  DEREF(term, X(0));
  while(TaggedIsLST(term)) {
    DerefCar(t0, term);
    DerefCdr(term, term);
    if (!TaggedIsSmall(t0)) goto error;
    i = GetSmall(t0);
    if (i < 0 || i > 255) goto error;
    DISPLAY_STRING__PUT(i);
  }
  if (term != atom_nil) goto error;
  DISPLAY_STRING__FLUSH;
  CBOOL__PROCEED;
 error:
  DISPLAY_STRING__FLUSH;
  CBOOL__FAIL;
}
#endif

/* --------------------------------------------------------------------------- */
/* Support for the format/2, format/3 predicates. */

/*
 * prolog_format_print_float(formatChar,arg,precision):
 *   formatChar;        Selects type of format.
 *   arg;               Value to be printed.
 *   precision;         Precision of printed item.
 *
 * Description: Print a FLOAT value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * total number of digits for some FORMATS.
 */

#define MAX_OUTPUT_DIGITS 1023
#define BUFF_SIZE         2048

/* See format.pl documentation, using the underlying C printf
   implementation is the expected behavior */
CBOOL__PROTO(prolog_format_print_float) {
  int precision;
  char fmtstr[16];
  char b[BUFF_SIZE];
  char format_ch;
  flt64_t f;

  DEREF(X(0),X(0));
  format_ch = TaggedToIntmach(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = TaggedToIntmach(X(2));
  
  f = TaggedToFloat(X(1));

  /* Limit precision */
  if (precision > 1023) {
    precision = 1023;
  } else if (precision<0) {
    precision = 6; /* default printf precision */
  }

  /* Assume format_ch in {'e','E','f','g','G'} */
  snprintf(fmtstr, 16, "%%.%d%c", precision, format_ch);
  snprintf(b, BUFF_SIZE, fmtstr, f);
  CVOID__CALL(print_string, Output_Stream_Ptr, b);

  CBOOL__PROCEED;
}

/*
 * prolog_format_print_integer(formatChar,arg,precision):
 *   formatChar: Selects type of format.
 *   arg:        Value to be printed.
 *   precision:  Precision or radix of printed item.
 *
 * Description: Print an INTEGER value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * radix for some formats.
 */

CFUN__PROTO(fu1_integer, tagged_t, tagged_t X0);

extern liveinfo_t prolog_format_print_integer__liveinfo;

CBOOL__PROTO(prolog_format_print_integer) {
  char formatChar;
  int precision;
  int base;
  
  DEREF(X(0),X(0));
  formatChar = GetSmall(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = TaggedToIntmach(X(2));

  if (formatChar=='r')
    base = ((precision<2 || precision>36) ? 8 : precision);
  else if (formatChar=='R')
    base = ((precision<2 || precision>36) ? -8 : -precision);
  else
    base = 10;

  if (IsFloat(X(1))) { /* TODO: fail? format.pl ensures that this never happens */
    w->liveinfo = prolog_format_print_integer__liveinfo;
    X(1) = CFUN__EVAL(fu1_integer, X(1));
  }
  CVOID__CALL(number_to_string, X(1), base);
  
  if ((formatChar=='d' || formatChar=='D') && precision > 0)
    {
      int usilen = strlen(Atom_Buffer) - (Atom_Buffer[0]=='-');
      int n = precision-usilen+1;
      
      if (n>0)
        {
          int i;
          int dig1 = (Atom_Buffer[0] == '-');
          int slen = strlen(Atom_Buffer);
          
#if defined(OPTIM_COMP)
          while (slen+n+1 > Atom_Buffer_Length) {
            Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
            Atom_Buffer_Length <<= 1;
          }
#else
          while (slen+n+1 > (i=Atom_Buffer_Length)) {
            Atom_Buffer_Length <<= 1;
            Atom_Buffer = checkrealloc_ARRAY(char,
                                             i,
                                             Atom_Buffer_Length,
                                             Atom_Buffer);
          }
          UpdateHeapMargins();
#endif
          
          for (i=slen; i>=dig1; i--)
            Atom_Buffer[i+n] = Atom_Buffer[i];
          for (i=dig1+n-1; i>=dig1; i--)
            Atom_Buffer[i] = '0';
        }

      {
        int i;
        int slen = strlen(Atom_Buffer);
        int ppos = slen-precision;
        
#if defined(OPTIM_COMP)
        if (slen+2 > Atom_Buffer_Length) {
          Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
          Atom_Buffer_Length <<= 1;
        }
#else
        if (slen+2 > (i=Atom_Buffer_Length)) {
          Atom_Buffer_Length <<= 1;
          Atom_Buffer = checkrealloc_ARRAY(char,
                                           i,
                                           Atom_Buffer_Length,
                                           Atom_Buffer);
          UpdateHeapMargins();
        }
#endif
        
        for (i=slen; i>=ppos; i--)
          Atom_Buffer[i+1] = Atom_Buffer[i];
        Atom_Buffer[ppos] = '.';
      }
    }
  if (formatChar=='D')
    {
      int i, count;
      int slen = strlen(Atom_Buffer);
      int dig1 = (Atom_Buffer[0]=='-');
      int ppos = slen;
      
      for (i=dig1, count=0; i<ppos; i++)
        {
          if (Atom_Buffer[i]=='.') ppos=i;
          else count++;
        }
      count = (count-1)/3;
      
      if (count>0)
        {
#if defined(OPTIM_COMP)
          if (slen+count+1 > Atom_Buffer_Length) {
            Atom_Buffer = CHECKREALLOC0_ARRAY(char, Atom_Buffer, Atom_Buffer_Length, 2 * Atom_Buffer_Length);
            Atom_Buffer_Length <<= 1;
          }
#else
          if (slen+count+1 > (i=Atom_Buffer_Length)) {
            Atom_Buffer_Length <<= 1;
            Atom_Buffer = checkrealloc_ARRAY(char,
                                             i,
                                             Atom_Buffer_Length,
                                             Atom_Buffer);
            UpdateHeapMargins();
          }
#endif
          
          for (i=slen; i>=ppos; i--) {
            Atom_Buffer[i+count] = Atom_Buffer[i];
          }
          for (i=ppos-1; count>0; count--) {
            Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
            Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
            Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
            Atom_Buffer[i+count] = ',';
          }
        }
    }
  
  CVOID__CALL(print_string, Output_Stream_Ptr, Atom_Buffer);
  CBOOL__PROCEED;
}

/* --------------------------------------------------------------------------- */

#if defined(OPTIM_COMP)
void init_streams(void) {
#if defined(USE_THREADS)
  Init_lock(stream_list_l);
#endif

  root_stream_ptr = checkalloc_TYPE(stream_node_t);
  root_stream_ptr->label=ERRORTAG;
  root_stream_ptr->streamname=ERRORTAG;
  root_stream_ptr->forward=root_stream_ptr;
  root_stream_ptr->backward=root_stream_ptr;
  root_stream_ptr->last_nl_pos = 0;               /* used for tty streams */
  root_stream_ptr->nl_count = 0;
  root_stream_ptr->rune_count = 0;

  stream_user_input = new_stream(ERRORTAG, "r", stdin);
  stream_user_output = new_stream(ERRORTAG, "a", stdout);
  stream_user_error = new_stream(ERRORTAG, "a", stderr);
}
#endif
