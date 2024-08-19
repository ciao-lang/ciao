/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include <ciao/eng.h>
#include <ciao/io_basic.h> /* RUNE_VOID */
#include <ciao/stream_basic.h>
#include <ciao/eng_gc.h>
#include <ciao/eng_registry.h>

#if defined(LINUX)||defined(EMSCRIPTEN)||defined(DARWIN)||defined(BSD)
#include <string.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>


#include <stdio.h>
void perror(const char *s);

#include <errno.h>
/*int errno;*/


/* Local atoms */

tagged_t atom_stream;
tagged_t atom_dgram;
tagged_t atom_raw;
tagged_t atom_seqpacket;
tagged_t atom_rdm;

tagged_t atom_read;
tagged_t atom_write;
tagged_t atom_read_write;


/*
tagged_t atom_buff;
tagged_t atom_unbuff;
*/

/* TODO: reuse code from stream_basic.h (e.g., update_stream, new_stream)? */

void update_socket_stream(stream_node_t *s, int socket) {
  s->label = MakeSmall(socket);
  s->streamfile = NULL;
  s->isatty = FALSE;
  /* s->socket_is_unbuffered = 0; */
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->rune_count = 0; /* less than perfect */
}

stream_node_t *new_socket_stream(tagged_t streamname, int socket) {
  stream_node_t *s;

  s = checkalloc_TYPE(stream_node_t);
  s->streamname = streamname;
  s->streammode = 's';
  s->pending_rune = RUNE_VOID;
  s->socket_eof = FALSE;
  update_socket_stream(s,socket);

  return insert_new_stream(s);
}

/* connect_to_socket(+Hostname, +Port, +Type, -Stream) */

#define MAX_SOCK_NUMBER 65535

CBOOL__PROTO(prolog_connect_to_socket_type) {
  ERR__FUNCTOR("sockets:connect_to_socket_type", 4);
  int sock;
  int port_number;
  tagged_t host_deref;
  struct hostent *host;
  struct sockaddr_in server_inet_addr;
  int    socket_type;
  tagged_t socket_atm;
  char   socket_name[512];

  DEREF(host_deref, X(0));
  if (!TaggedIsATM(host_deref))
    BUILTIN_ERROR(ERR_type_error(atom), X(0), 1);
    // "connect_to_socket_type/[3,4]: 1st argument must be an atom");

  DEREF(X(1), X(1));
  if (!TaggedIsSmall(X(1)))
    BUILTIN_ERROR(ERR_type_error(integer), X(1), 2);
    //    USAGE_FAULT("connect_to_socket_type/[3,4]: 2nd argument must be a port number");

 if ((port_number = TaggedToIntmach(X(1))) > MAX_SOCK_NUMBER)
// USAGE_FAULT("connect_to_socket/[3,4]: port number greater than 65535");
   BUILTIN_ERROR(SYSTEM_ERROR,X(1),2);

  DEREF(socket_atm, X(2));
  if (!TaggedIsATM(socket_atm))
    BUILTIN_ERROR(ERR_type_error(atom), X(2), 3);
// USAGE_FAULT("connect_to_socket_type/[3,4]: 3rd argument must be an atom");

  if (socket_atm == atom_stream)
    socket_type = SOCK_STREAM;
  else if (socket_atm == atom_dgram)
    socket_type = SOCK_DGRAM;
  else if (socket_atm == atom_raw)
    socket_type = SOCK_RAW;
  else if (socket_atm == atom_seqpacket)
    socket_type = SOCK_SEQPACKET;
  else if (socket_atm == atom_rdm)
    socket_type = SOCK_RDM;
  else BUILTIN_ERROR(DOMAIN_ERROR(FLAG_VALUE),X(2),3);
//USAGE_FAULT("connect_to_socket_type/[3,4]: unrecognized connection type");


  if ((host = gethostbyname(GetString(host_deref))) == NULL)
    //    MAJOR_FAULT("connect_to_socket/[3,4]: gethostbyname() failed");
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);

  if ((sock = socket(AF_INET, socket_type, 0)) < 0)
    //    MAJOR_FAULT("connect_to_socket/[3,4]: socket creation failed");
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);


 /* Specify that we may want to reuse the address  */

  /*
  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, NULL, 0))
    MAJOR_FAULT("connect_to_socket/[3,4]: error setting option");
  */

  server_inet_addr.sin_family = AF_INET;
  memcpy((char *)&(server_inet_addr.sin_addr), 
         (char *)host->h_addr, 
         host->h_length);

  server_inet_addr.sin_port = htons(port_number);
    
  if (connect(sock, 
              (struct sockaddr *)&server_inet_addr, 
              sizeof(struct sockaddr)) < 0){
    //    perror("connect() in prolog_connect_to_socket");
    //    MAJOR_FAULT("connect_to_socket_type/[3,4]: cannot connect()");
    BUILTIN_ERROR(SYSTEM_ERROR,X(0),1);
  }
  
  sprintf(socket_name, "<%s:%d>", GetString(host_deref), port_number);
  
  CBOOL__LASTUNIFY(ptr_to_stream(Arg,new_socket_stream(GET_ATOM(socket_name),sock)), X(3));
}


/* bind_socket(?Port, +Lenght, -Sock) */

CBOOL__PROTO(prolog_bind_socket)
{
  ERR__FUNCTOR("sockets:bind_socket", 3);
  int sock, port;
  struct sockaddr_in sa;
  int reuse_address = 1;

  DEREF(X(2), X(2));
  if (!IsVar(X(2)))
    //"bind_socket: 3rd argument must be a variable");
    BUILTIN_ERROR(INSTANTIATION_ERROR, X(2), 3);

  DEREF(X(1), X(1));
  if (!TaggedIsSmall(X(1)))
    //    USAGE_FAULT("bind_socket: 2nd argument must be a (small) number");
    BUILTIN_ERROR(ERR_type_error(integer), X(1), 2);

  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    //    MAJOR_FAULT("bind_socket/3: socket creation failed");
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  DEREF(X(0), X(0));
  if (IsVar(X(0)))
    sa.sin_port = 0;                      /* Undocumented in the manuals! */
  else {
    if (!TaggedIsSmall(X(0))) return FALSE;
    sa.sin_port = htons(GetSmall(X(0)));
  }
  sa.sin_family = AF_INET;
  sa.sin_addr.s_addr = INADDR_ANY;

 /* Specify that we may want to reuse the address  */

  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, 
                 (void *)&reuse_address, sizeof(int)))
    // MAJOR_FAULT("connect_to_socket/[3,4]: error setting option");
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  if (bind(sock, (struct sockaddr *)&sa, sizeof(sa)) < 0)
    // MAJOR_FAULT("bind_socket: cannot bind");
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);


  if (IsVar(X(0))){
    unsigned int length = sizeof(sa);

    if (getsockname(sock, (struct sockaddr *)&sa, &length) < 0)
      // MAJOR_FAULT("bind_socket: cannot get socket name");
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

    port = ntohs(sa.sin_port);
    CBOOL__UNIFY(MakeSmall(port), X(0));
  }

  if (listen(sock, GetSmall(X(1)))) { // Error
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
  }

  CBOOL__LASTUNIFY(MakeSmall(sock), X(2));
}

/* socket_accept(+Sock, -Stream) */
CBOOL__PROTO(prolog_socket_accept) {
  ERR__FUNCTOR("sockets:socket_accept", 2);
  int socket, new_s;
  struct sockaddr isa;
  unsigned int isalen;
  char new_s_name[16];
  
  DEREF(X(0), X(0));
  if (!TaggedIsSmall(X(0)))
    BUILTIN_ERROR(ERR_type_error(integer), X(0), 1);
  socket = GetSmall(X(0));

  isalen = sizeof(struct sockaddr);
  if ((new_s = accept(socket, &isa, &isalen)) == -1)
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  sprintf(new_s_name, "<socket %d>", new_s);

  return
    cunify(Arg, 
           ptr_to_stream(Arg, new_socket_stream(GET_ATOM(new_s_name),new_s)),
           X(1));
}

/* Aux function */
static CFUN__PROTO(stream_list, tagged_t,
                   int max_fd, fd_set *ready_set) {
  int this_fd;
  tagged_t list = atom_nil;
  stream_node_t *temp_stream;
  
  for (this_fd = max_fd; this_fd >= 0; this_fd--)       
    if (FD_ISSET(this_fd, ready_set)){  /* stream for every fd in the set */
      temp_stream = root_stream_ptr->forward;
      while (
             (GetSmall(temp_stream->label) != this_fd) && 
             (temp_stream != root_stream_ptr)
             )
        temp_stream = temp_stream->forward;
      if (temp_stream != root_stream_ptr)
        MakeLST(list, ptr_to_stream(Arg,temp_stream), list);
    }
  return list;
}


/* select_socket(+Socket, -NewStream, +TO_ms, +Streams, -ReadStreams) */
CBOOL__PROTO(prolog_select_socket) {
  ERR__FUNCTOR("sockets:select_socket", 5);
  int 
    listen_sock = 0,  /* Avoid compiler complaints */
    max_fd = 0;
  struct timeval timeout, *timeoutptr;
  fd_set ready;
  bool_t unify_result = TRUE, watch_connections;
  tagged_t car, cdr;
  stream_node_t *stream, *socket_stream;
  char new_s_name[16];
  int newsock, fd_to_include;
  
  DEREF(X(0), X(0));           /* Do we have to wait for new connections? */
  watch_connections = IsInteger(X(0));
  
/* Construct the set of descriptors to watch for. Include socket, if
   specified.  */

  FD_ZERO(&ready);
  if (watch_connections) {
    max_fd = listen_sock = TaggedToIntmach(X(0));
    FD_SET(max_fd, &ready);
  }

  DEREF(X(2), X(2));                                   /* Get the timeout */
  if (X(2) == atom_off)
    timeoutptr = NULL;
  else if (IsInteger(X(2))){
    intmach_t miliseconds_i = TaggedToIntmach(X(2));
    timeout.tv_sec = miliseconds_i / 1000;
    timeout.tv_usec = (miliseconds_i - timeout.tv_sec * 1000) * 1000;
    timeoutptr = &timeout;
  } else if (IsFloat(X(2))){
    flt64_t miliseconds_f = TaggedToFloat(X(2));
    timeout.tv_sec = (int)(miliseconds_f / 1000);
    timeout.tv_usec = (int)(miliseconds_f - timeout.tv_sec * 1000) * 1000;
    timeoutptr = &timeout;
  } else
    USAGE_FAULT("select_socket/5: 3rd argument must be either \"off\" or a number");
  
  if ((timeoutptr != NULL) && ((timeout.tv_sec < 0) || (timeout.tv_usec < 0)))
    USAGE_FAULT("select_socket/5: timeout must be non-negative");

  DEREF(X(3), X(3));                      /* Get list of streams to watch */
  if (!TaggedIsLST(X(3)) && !(atom_nil == X(3)))
    BUILTIN_ERROR(ERR_type_error(list), X(3), 4);
  
  DEREF(cdr,X(3));
  while (cdr!=atom_nil) {
    DerefCar(car,cdr);
    DerefCdr(cdr,cdr);
        
    if (!(stream = stream_to_ptr(car, 'r')))          /* Check any stream */
      USAGE_FAULT("select_socket/5: illegal stream (or stream mode) in list");

    fd_to_include = GetSmall(stream->label);
    if (fd_to_include < 0 || fd_to_include > FD_SETSIZE)
      USAGE_FAULT("select_socket/5: illegal stream in list or wrong file descriptor in stream");
        
    FD_SET(fd_to_include, &ready);
    if (fd_to_include > max_fd)  max_fd = fd_to_include;
  }

  if (select(max_fd+1, &ready, (fd_set *)NULL, (fd_set *)NULL, timeoutptr) < 0)
    BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    //MAJOR_FAULT("select_socket/5: select() call failed");

  if (watch_connections && FD_ISSET(listen_sock, &ready)) {

    if ((newsock = accept(listen_sock, NULL, 0)) < 0)
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);
    //      MAJOR_FAULT("select_socket/5: accept() call failed");
    
    sprintf(new_s_name, "<socket %d>", newsock);
    socket_stream = new_socket_stream(GET_ATOM(new_s_name), newsock);
    unify_result = cunify(Arg, ptr_to_stream(Arg, socket_stream), X(1));
    FD_CLR(listen_sock,&ready);
  }
  
  return unify_result && cunify(Arg,X(4), stream_list(Arg, max_fd, &ready));
}

/* TODO: use another buffer? share with part of prolog_constant_codes */

/* Copy list X(ci) to the atom buffer, return its length */
static CFUN__PROTO(bytelist_to_atmbuf, intmach_t, intmach_t ci,
                   char *err__name, intmach_t err__arity) {
  tagged_t cdr, car;
  int msglen;
  unsigned char *buffpt;
  DEREF(X(ci), X(ci));
  cdr = X(ci);
  buffpt = (unsigned char *)Atom_Buffer;
  for (msglen=0; cdr!=atom_nil; msglen++) {
    if (IsVar(cdr)) {
      BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,ci+1);
    } else if (!TaggedIsLST(cdr)) {
      BUILTIN_ERROR(ERR_type_error(list),X(ci),ci+1);
    } else if (msglen == Atom_Buffer_Length) {                   /* realloc */
      Atom_Buffer = checkrealloc_ARRAY(char,
                                       msglen,
                                       Atom_Buffer_Length<<=1,
                                       Atom_Buffer);
      buffpt = (unsigned char *)Atom_Buffer+msglen;
    }
    DerefCar(car,cdr);
    if (IsVar(car)) {
      BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,ci+1);
    }

    if (!TaggedIsSmall(car) || (car<TaggedZero) || (car>=MakeSmall(256))) {
      BUILTIN_ERROR(ERR_type_error(byte),X(ci),ci+1); // TODO:[JF] list element, is it right?
    }
    
    *buffpt++ = GetSmall(car);
    DerefCdr(cdr,cdr);
  }
  return msglen;
}

/* Send all bytes from buffer, returns len on success and -1 on error */
ssize_t sendall(int sockfd, const unsigned char *buf, size_t len) {
  ssize_t bytes_sent = 0;
  ssize_t n = 0;
  while (len > 0) {
    n = send(sockfd, buf+bytes_sent, len, 0);
    if (n == -1) return -1;
    bytes_sent += n;
    len -= n;
  }
  return bytes_sent;
}

/* socket_send(+Stream, +Bytes, ?Sent) */
CBOOL__PROTO(prolog_socket_send) {
  ERR__FUNCTOR("sockets:socket_send", 3);
  stream_node_t *s;
  int msglen;
  int bytes_sent;

  int errcode;
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);
  if (s->streammode != 's')
    USAGE_FAULT("socket_send/3: first argument must be a socket stream");

  msglen = CFUN__EVAL(bytelist_to_atmbuf, 1, err__name, err__arity);
  unsigned char *buffpt = (unsigned char *)Atom_Buffer;

  bytes_sent = send(GetSmall(s->label), buffpt, msglen, 0);
  if (bytes_sent < 0)
    MAJOR_FAULT("socket_send/3: send() call failed");
    //BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  CBOOL__LASTUNIFY(MakeSmall(bytes_sent), X(2));
}

/* socket_sendall(+Stream, +Bytes) */
CBOOL__PROTO(prolog_socket_sendall) {
  ERR__FUNCTOR("sockets:socket_sendall", 2);
  stream_node_t *s;
  int msglen;
  int bytes_sent;

  int errcode;
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);
  if (s->streammode != 's')
    USAGE_FAULT("socket_sendall/2: first argument must be a socket stream");

  msglen = CFUN__EVAL(bytelist_to_atmbuf, 1, err__name, err__arity);
  unsigned char *buffpt = (unsigned char *)Atom_Buffer;

  bytes_sent = sendall(GetSmall(s->label), buffpt, msglen);
  if (bytes_sent < 0)
    MAJOR_FAULT("socket_sendall/2: send() call failed");
    //BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1);

  return TRUE;
}

#define BUFFSIZE 4096

/* socket_send_stream(+Stream, +FromStream) */
CBOOL__PROTO(prolog_socket_send_stream) {
  ERR__FUNCTOR("sockets:socket_send_stream", 2);
  stream_node_t *s;
  stream_node_t *from_stream;
  unsigned char buffer[BUFFSIZE];
  int msglen;
  int bytes_sent;

  int errcode;
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);
  if (s->streammode != 's')
    USAGE_FAULT("socket_send_stream/2: first argument must be a socket stream");

  from_stream = stream_to_ptr_check(X(1), 'r', &errcode);
  if (!from_stream) BUILTIN_ERROR(errcode, X(1), 2);
  if (from_stream->streammode == 's')
    USAGE_FAULT("socket_send_stream/2: second argument cannot be a socket stream");
  FILE *from_file = from_stream->streamfile;

  while(1) {
    msglen = fread(buffer, sizeof(unsigned char), BUFFSIZE, from_file);
    if (msglen == 0) break; /* EOF and empty */
    bytes_sent = sendall(GetSmall(s->label), buffer, msglen);
    if (bytes_sent < 0)
      MAJOR_FAULT("socket_send_stream/2: send() call failed");
    if (msglen < BUFFSIZE) break; /* EOF */
  } 
  return TRUE;
}

/* socket_recv(+Stream, ?Bytes, ?Length).  Needs socket in connected state. */

CBOOL__PROTO(prolog_socket_receive) {
  ERR__FUNCTOR("sockets:socket_recv", 3);
  stream_node_t *s;
  /* We could use Atom_Buffer, but its size changes during execution; we'd
     better keep everything under control. */
  unsigned char *buffpt;
  unsigned char buffer[BUFFSIZE];
  int bytes_read, total_bytes;
  tagged_t cdr;

  int errcode;
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);

  if (s->streammode != 's')
    USAGE_FAULT("socket_recv/3: first argument must be a socket stream");
  
  /* recv is about to disappear (c.f. Linux' manpage), so we'll use
     recvfrom instead */

  /* bytes_read = recv(GetSmall(s->label), buffer, BUFFSIZE, 0); */

  bytes_read = recvfrom(GetSmall(s->label), buffer, BUFFSIZE, 0, NULL, NULL);
  total_bytes = bytes_read;

  if (bytes_read < 0)
    MAJOR_FAULT("socket_recv/3: recv() call failed")

  /*
    else if (bytes_read > BUFFSIZE)        
    MAJOR_FAULT("socket_recv/3: internal buffer overrun")
  */

  if (HeapCharAvailable(w->heap_top) < CONTPAD+(bytes_read<<1)*sizeof(tagged_t)) {
    explicit_heap_overflow(Arg, (CONTPAD+(bytes_read<<1)*sizeof(tagged_t))*2, 2);
  }
    
  buffpt = &buffer[bytes_read-1];
  cdr = atom_nil;
  while (bytes_read>0)  {
    bytes_read--;
    MakeLST(cdr,MakeSmall(*(buffpt--)), cdr);/* No need to cast *(buffpt--) */
  }

  CBOOL__UNIFY(cdr,X(1));
  CBOOL__LASTUNIFY(MakeSmall(total_bytes), X(2));
}


/* socket_shutdown(+Stream, +Type) */

/* Patch constants apparently not defined in all Linux implementations (at
   least at the moment) */

#if !defined(SHUT_RD)
#define SHUT_RD    0
#define SHUT_WR    1
#define SHUT_RDWR  2
#endif

CBOOL__PROTO(prolog_socket_shutdown) {
  ERR__FUNCTOR("sockets:socket_shutdown", 2);
  stream_node_t *s;
  int access_required;
  tagged_t shutdown_type;
  int how;
    
  DEREF(X(0), X(0));
  DEREF(shutdown_type, X(1));

  if (shutdown_type == atom_read) {
    access_required = 'r';
    how = SHUT_RD;
  } else if (shutdown_type == atom_write){
    access_required = 'w';
    how = SHUT_WR;
  } else if (shutdown_type == atom_read_write) {
    access_required = 'w';
    how = SHUT_RDWR;
  } else {
    USAGE_FAULT("socket_shutdown/2: error in second argument");
  }

  int errcode;
  s = stream_to_ptr_check(X(0), access_required, &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);

  if (s->streammode != 's')
    USAGE_FAULT("socket_shutdown/2: first argument must be a socket stream");
    
  if ((errcode = shutdown(GetSmall(s->label), how)))
    MAJOR_FAULT("socket_shutdown/2: error in call to shutdown()");

  return TRUE;
}


/* socket_buffering(+Stream, +Direction, -OldBuf, +NewBuffer) */
/*
CBOOL__PROTO(prolog_socket_buffering) {
  ERR__FUNCTOR("sockets:socket_buffering", 4);
    stream_node_t *s;
    int access_required;
    int errcode;
    tagged_t direction;
    tagged_t oldbuf;
    tagged_t newbuf;
    int flag;
    
    DEREF(direction, X(1));

    if (direction == atom_read) {
      access_required = 'r';
    } else if (direction == atom_write){
      access_required = 'w';
    } else USAGE_FAULT("socket_buffering/4: error in second argument");

    DEREF(X(0), X(0));
    s = stream_to_ptr_check(X(0), access_required, &errcode);
    if (!s) BUILTIN_ERROR(errcode, X(0), 1);

    if (s->streammode != 's')
    USAGE_FAULT("socket_buffering/4: first argument must be a socket stream");

    oldbuf = (s->socket_is_unbuffered ? atom_unbuff : atom_buff);

    DEREF(newbuf, X(3));
    if (newbuf == atom_unbuff)
      s->socket_is_unbuffered = 1;
    else if (newbuf == atom_buff)
      s->socket_is_unbuffered = 0;
    else USAGE_FAULT("socket_buffering/4: error in fourth argument");
    
    flag = s->socket_is_unbuffered;

    if (setsockopt(GetSmall(s->label),
                   IPPROTO_TCP,
                   TCP_NODELAY, 
                   (void *)&flag,
                   sizeof(int)))
      MAJOR_FAULT("socket_buffering/4: error setting option");

    CBOOL__LASTUNIFY(X(2), oldbuf);
}
*/

// // assume s is a connected socket
// 
// socklen_t len;
// struct sockaddr_storage addr;
// char ipstr[INET6_ADDRSTRLEN];
// int port;
// 
// len = sizeof addr;
// getpeername(s, (struct sockaddr*)&addr, &len);
// 
// // deal with both IPv4 and IPv6:
// if (addr.ss_family == AF_INET) {
//     struct sockaddr_in *s = (struct sockaddr_in *)&addr;
//     port = ntohs(s->sin_port);
//     inet_ntop(AF_INET, &s->sin_addr, ipstr, sizeof ipstr);
// } else { // AF_INET6
//     struct sockaddr_in6 *s = (struct sockaddr_in6 *)&addr;
//     port = ntohs(s->sin6_port);
//     inet_ntop(AF_INET6, &s->sin6_addr, ipstr, sizeof ipstr);
// }
// 
// printf("Peer IP address: %s\n", ipstr);


/* hostname_address(+Hostname, ?Address) */

#define MAX_BYTES_IN_HOST_ADDRESS 8             /* It is 4 at the present */

CBOOL__PROTO(prolog_hostname_address) {
  ERR__FUNCTOR("sockets:hostname_address", 2);
  tagged_t hostname;
  /* 3 chars per byte plus dots plus trailing zero */
  char address[4*MAX_BYTES_IN_HOST_ADDRESS];  
  int address_index = 0;
  int bytes_index = 0;
  struct hostent *host;

  DEREF(hostname, X(0));
  if (!TaggedIsATM(hostname))
    USAGE_FAULT("hostname_address/2: 1st argument must be an atom");

  if ((host = gethostbyname(GetString(hostname))) == NULL)
    MAJOR_FAULT("hostname_address/2: gethostbyname() failed");

  /* TODO: use inet_ntop() instead? */
  while(bytes_index < host->h_length) {
    sprintf(&address[address_index], 
           "%u.", 
           (unsigned char)(host->h_addr_list[0][bytes_index]));
    while(address[address_index])
      address_index++;
    bytes_index++;
  }
  address[--address_index] = 0;

  CBOOL__LASTUNIFY(X(1), GET_ATOM(address));
}


/* socket_getpeername(+Stream, ?Address) */

CBOOL__PROTO(prolog_socket_getpeername) {
  ERR__FUNCTOR("sockets:socket_getpeername", 2);
  stream_node_t *s;
  int errcode;

  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) BUILTIN_ERROR(errcode, X(0), 1);

  if (s->streammode != 's')
    USAGE_FAULT("socket_getpeername/2: first argument must be a socket stream");
  
  socklen_t len;
  struct sockaddr_storage addr;
  char ipstr[INET6_ADDRSTRLEN];
  /* int port; */

  len = sizeof(addr);
  getpeername(GetSmall(s->label), (struct sockaddr*)&addr, &len);

  /* TODO: support IPv6 in sockets! */
  if (addr.ss_family == AF_INET) {
    struct sockaddr_in *s = (struct sockaddr_in *)&addr;
    /* port = ntohs(s->sin_port); */
    inet_ntop(AF_INET, &s->sin_addr, ipstr, sizeof(ipstr));
  } else { /* AF_INET6 */
    struct sockaddr_in6 *s = (struct sockaddr_in6 *)&addr;
    /* port = ntohs(s->sin6_port); */
    inet_ntop(AF_INET6, &s->sin6_addr, ipstr, sizeof(ipstr));
  }

  CBOOL__LASTUNIFY(X(1), GET_ATOM(ipstr));
}

CBOOL__PROTO(sockets_c_init) {
  atom_stream = GET_ATOM("stream");
  atom_dgram = GET_ATOM("dgram");
  atom_raw = GET_ATOM("raw");
  atom_seqpacket = GET_ATOM("seqpacket");
  atom_rdm = GET_ATOM("rdm");

  atom_read = GET_ATOM("read");
  atom_write = GET_ATOM("write");
  atom_read_write = GET_ATOM("read_write");
  /*
  atom_buff = GET_ATOM("fullbuf");
  atom_unbuff = GET_ATOM("unbuf");
  */
  return TRUE;
}

CBOOL__PROTO(init_from_ciaopp) { /* TODO: this should not be needed */
  CBOOL__LASTCALL(sockets_c_init);
}

