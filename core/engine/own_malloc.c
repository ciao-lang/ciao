/*
 *  own_malloc.c
 *
 *  New memory manager.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if !defined(Win32)
# include <strings.h>
#endif

#if defined(DEBUG)
#include <stdio.h>
#endif

#include <ciao/configure.h>
#include <ciao/termdefs.h>
#include <ciao/eng_dbg.h>

/* TODO: some of those definitions are shared in configure.c */
#if defined(USE_OWN_MALLOC)
#if defined(USE_MMAP) && OWNMALLOC_MmapAllowed
#include <ciao/own_mmap.h>
#endif
#endif
#include <ciao/own_malloc.h>

#if defined(USE_OWN_MALLOC)

#define ALIGN sizeof(tagged_t)       /* blocks suitably aligned for any use */
#define TW_TO_CHARS(Tw) (Tw)*ALIGN
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)

// #include <ciao/own_malloc_linear.h>
#include <ciao/own_malloc_bin.h>

#endif /* USE_OWN_MALLOC */
