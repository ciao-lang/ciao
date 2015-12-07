#ifndef _CIAO_ZMQ_LL_H
#define _CIAO_ZMQ_LL_H

#include <zmq.h>
#include <ciao_prolog.h>

extern void
ciao_zmq_init_(void);

extern void
ciao_zmq_term_(void);

extern void
ciao_zmq_socket(char *socket_atom, char *type_atom);

extern void
ciao_zmq_close(char *socket_atom);

extern void
ciao_zmq_bind(char *socket_atom, char *endpoint);

extern void
ciao_zmq_connect(char *socket_atom, char *endpoint);

extern void
ciao_zmq_subscribe(char *socket_atom, int size, ciao_term byte_prefix);

extern void
ciao_zmq_unsubscribe(char *socket_atom, int size, ciao_term byte_prefix);

extern void
ciao_zmq_send(char *socket_atom, int indicative_size, 
	      ciao_term byte_list, ciao_term options);

extern ciao_term
ciao_zmq_recv(char *socket_atom, int *size, char **byte_list, ciao_term options);

extern ciao_term
ciao_zmq_multipart_pending(char *socket_atom);

extern ciao_term
ciao_zmq_poll(ciao_term socket_list, int timeout);

extern void
ciao_zmq_device(char *type_atom, char *frontent, char *backend);

extern ciao_term
ciao_zmq_error_check(void);

extern ciao_term 
ciao_zmq_errors(void);

#define CIAO_ZMQ_MAX_ATOM_LEN 128

typedef struct _ciao_zmq_socket_assoc {
  struct _ciao_zmq_socket_assoc *next;
  void *zmq_socket;
  char socket_atom_chars[CIAO_ZMQ_MAX_ATOM_LEN];
  int padding;
} ciao_zmq_socket_assoc;

typedef struct _ciao_zmq_error_record {
  struct _ciao_zmq_error_record *next;
  int error_code;
  char *error_functor;
  char socket_atom_chars[CIAO_ZMQ_MAX_ATOM_LEN];
  int padding;
} ciao_zmq_error_record;

typedef struct _ciao_zmq_state {
  void *zmq_context;
  ciao_zmq_socket_assoc *socket_list;
  ciao_zmq_error_record *error_list;
} ciao_zmq_state;

#define CIAO_ZMQ_MAX_MESSAGE_SIZE 1048576

#endif /* _CIAO_ZMQ_LL_H */
