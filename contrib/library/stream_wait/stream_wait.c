#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/support.h>
#include <ciao/io_basic.h> /* RUNE defs */
#include <ciao_prolog.h>

#include <stdio.h>
#include <sys/select.h>

CBOOL__PROTO(input_set_unbuf__c) 
{
  setbuf(Input_Stream_Ptr->streamfile, NULL);

  return TRUE;
}

CBOOL__PROTO(input_wait__c) 
{
  ERR__FUNCTOR("input_wait", 3);
  int fd = fileno(Input_Stream_Ptr->streamfile);
  // printf(">%d<", ftell(Input_Stream_Ptr->streamfile));
  setbuf(Input_Stream_Ptr->streamfile, NULL);

  if (Input_Stream_Ptr->pending_rune != RUNE_VOID) { /* RUNE_EOF or valid rune */
    return TRUE;
  } else {
    fd_set set;
    struct timeval timeout;
    int rv;
        
    DEREF(X(0), X(0));
    timeout.tv_sec  = GetInteger(X(0));
    
    DEREF(X(1), X(1));
    timeout.tv_usec = GetInteger(X(1));
    
    FD_ZERO(&set); 
    FD_SET(fd, &set); 
    
    rv = select(fd + 1, &set, NULL, NULL, &timeout);
    
    if (rv < 0) {
      BUILTIN_ERROR(SYSTEM_ERROR, TaggedZero, 0);
    }
    
    /*    if (rv == 0) {
      printf("%i - %i\n", ftell(Input_Stream_Ptr->streamfile),  lseek( fd, 0, SEEK_CUR ));
      //    printf("%i\n", getchar());
    }
    */

    return (rv != 0);
  }
}
