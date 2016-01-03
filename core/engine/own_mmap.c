/*
 *  own_mmap.c
 *
 *  Abstraction layer for mmap.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#if defined(USE_MMAP)

#if !defined(DARWIN)

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
#include <ciao/win32_mman.h>
#else
#include <sys/mman.h>
#endif

/* Some BSD systems (like NetBSD 6) do not define this synonym  */
#if !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

# if defined(ANONYMOUS_MMAP)
#  if defined(Solaris)
#   define MMAP_FLAGS (MAP_ANON|MAP_PRIVATE|MAP_FIXED)
#  else
#   define MMAP_FLAGS (MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED)
#  endif
# else
#  define MMAP_FLAGS  (MAP_PRIVATE|MAP_FIXED)
# endif

#  define MMAP_FILE   "/dev/zero"

/* return 0 in case of success */ 
int 
own_fixed_mmap(void * addr, size_t len){


  /* The file descriptor has to be -1 for Solaris mmap 
     if the MAP_ANON flag is specified. 
     It does not seem to be relevant for other OSs. */
  int fd_mmap_file=-1;


#if !defined(ANONYMOUS_MMAP)
  char *mmap_file = MMAP_FILE;

  if ((fd_mmap_file = open(MMAP_FILE, O_RDWR|O_CREAT)) < 0) {
    fprintf(stderr, "\n\n**** Error opening map file in vm_alloc.c ****\n\n");
    exit(1);
  }
#endif

  return  (mmap(addr, len, PROT_READ|PROT_WRITE, MMAP_FLAGS, fd_mmap_file, 0) == MAP_FAILED) ;
}

/* return 0 in case of success */ 
int
own_fixed_munmap(void *addr, size_t len){

#if defined(ANONYMOUS_MMAP)
  return munmap(addr, len);
#else
  return ( munmap(addr, len) || close(fd_mmap_file));
#endif
    
}

#else /* !defined(DARWIN) */
    // With MAP_FIXED, Apple's mmap overwrites already mapped regions.
    // This behaviour seems to be conformant with IEEE Std 1003.1-2001.
    // We use the underlying Mach Microkernel calls directly,
    // which are much nicer.
 
    // RH: Explanation from GHC source code:
    //     http://darcs.haskell.org/ghc-6.12/ghc/rts/posix/OSMem.c

#include <mach/mach_init.h>
#include <mach/vm_map.h>

/* return 0 in case of success */ 
int 
own_fixed_mmap(void *addr, size_t len){
  
  vm_address_t result = (vm_address_t)addr;

  if( vm_allocate(mach_task_self(), &result, len, 0) )
    return 1;    

  return ( vm_protect(mach_task_self(), (vm_address_t)result, len, 0, VM_PROT_READ|VM_PROT_WRITE) );
}

/* return 0 in case of success */ 
int 
own_fixed_munmap(void *addr, size_t len){
  
    return !vm_deallocate(mach_task_self(), (vm_address_t)addr, len);

}

#endif /* !defined(DARWIN) */

#else /* !defined(USE_MMAP) */

int own_mmap__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif /* defined(USE_MMAP) */

