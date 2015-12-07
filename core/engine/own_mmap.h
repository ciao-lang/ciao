/*
 *  own_mmap.h
 *
 *  Abstraction layer for mmap.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_OWN_MMAP_H
#define _CIAO_OWN_MMAP_H

#if defined(USE_MMAP)

int own_fixed_mmap(void * addr, size_t len);
int own_fixed_munmap(void * addr, size_t len);
 
#endif

#endif /* _CIAO_OWN_MMAP_H */
