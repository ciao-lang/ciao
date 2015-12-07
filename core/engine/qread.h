/*
 *  qread.h
 *
 *  Reader of quickload objects.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_QREAD_H
#define _CIAO_QREAD_H

int buffered_input(FILE *stream);
CBOOL__PROTO(pop_qlinfo);
CBOOL__PROTO(prolog_qread);
CBOOL__PROTO(push_qlinfo);
CBOOL__PROTO(qread1, FILE *qfile, tagged_t *rungoal);
int is_a_script(FILE *file);
void expand_qload(void);
void reloc_counter(intmach_t Label);
void reloc_emul_entry(int Li, intmach_t Label);
void reloc_pointer(int Li, intmach_t Label);
void skip_to_ctrl_l(FILE *file);

/* We want to use buffered reading of .po files --- it is much faster! */

#define BUFFERED_PO

/* We want to have support for loading compressed bytecode */

#define ALLOW_COMPRESSED_CODE


#if defined(ALLOW_COMPRESSED_CODE)
# define GETC(f) readLZ(f)
int readLZ(FILE *stream);
void is_compressed(FILE *file);
#endif

#if defined(BUFFERED_PO)
# if defined(ALLOW_COMPRESSED_CODE)
#  define GETC_LZ(f) buffered_input(f)
# else
#  define GETC(f) buffered_input(f)
# endif   
# if defined(DEBUG)
#  define UNGETC(chr, f) \
   if (!qlbuffidx)  fprintf(stderr, "Error UNGETting: buffer underfull!\n"); \
   else qlbuffidx--
# else
#  define UNGETC(chr, f) qlbuffidx-- 
# endif
#else
# if defined(ALLOW_COMPRESSED_CODE)
#  define GETC_LZ(f) getc(f)
# else
#  define GETC(f) getc(f)
# endif
# define UNGETC(chr, f) ungetc(chr, f)
#endif

#endif /* _CIAO_QREAD_H */
