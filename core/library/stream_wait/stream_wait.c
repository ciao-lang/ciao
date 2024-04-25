#include <ciao/eng.h>
#include <ciao/io_basic.h> /* RUNE defs */
#include <ciao_prolog.h>

#include <stdio.h>
#include <sys/select.h>

// TODO: be careful! data may be lost if we do buffered reads before
CBOOL__PROTO(input_set_unbuf__c) {
  setbuf(Input_Stream_Ptr->streamfile, NULL);
  return TRUE;
}

CBOOL__PROTO(input_wait__c) {
  ERR__FUNCTOR("input_wait", 3);
  int fd = fileno(Input_Stream_Ptr->streamfile);

  if (Input_Stream_Ptr->pending_rune != RUNE_VOID) { /* RUNE_EOF or valid rune */
    return TRUE;
  } else {
    fd_set set;
    struct timeval timeout;
    int rv;
        
    DEREF(X(0), X(0));
    timeout.tv_sec  = TaggedToIntmach(X(0));
    DEREF(X(1), X(1));
    timeout.tv_usec = TaggedToIntmach(X(1));
    
    FD_ZERO(&set); 
    FD_SET(fd, &set); 
    
    rv = select(fd + 1, &set, NULL, NULL, &timeout);
    
    if (rv < 0) {
      BUILTIN_ERROR(SYSTEM_ERROR, TaggedZero, 0);
    }
    
    return (rv != 0);
  }
}
