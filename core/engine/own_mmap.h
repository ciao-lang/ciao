/*
 *  own_mmap.h
 *
 *  Abstraction layer for mmap.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
*/

#ifndef _CIAO_OWN_MMAP_H
#define _CIAO_OWN_MMAP_H

#if defined(USE_MMAP)

int own_fixed_mmap(void * addr, size_t len);
int own_fixed_munmap(void * addr, size_t len);
 
#endif

#endif /* _CIAO_OWN_MMAP_H */
