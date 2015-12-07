/*
 *  own_malloc.h
 *
 *  New memory manager
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Author:
 *    Manuel Carro
 */

#ifndef _CIAO_OWN_MALLOC_H
#define _CIAO_OWN_MALLOC_H

#if defined(USE_OWN_MALLOC)
tagged_t *own_malloc(intmach_t size);
tagged_t *own_realloc(tagged_t *ptr, intmach_t size_in_chars);
void own_free(tagged_t *ptr);
void init_mm(void);
void init_own_malloc(void);
#endif

#endif /* _CIAO_OWN_MALLOC_H */
