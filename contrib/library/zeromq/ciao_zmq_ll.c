// #################################################################
// ################# Ciao-ZMQ Low-Level Interface ##################
// #################################################################

// Author: Dragan Ivanovic <idragan@clip.dia.fi.upm.es>
// Date:   10 May 2011

#include "ciao_zmq_ll.h"
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

// -- Key used for accessing thread-specific state -----------------
static pthread_key_t zmq_ll_state_key;
static pthread_once_t zmq_ll_once= PTHREAD_ONCE_INIT;

// -- Make sure the state key is created ---------------------------
static void
make_state_key(void) {
  pthread_key_create(&zmq_ll_state_key, NULL);
}

// -- Get the state, initializing it first if necessary ------------
static ciao_zmq_state *
get_state(void) {
  // .. Execute once per process ...................................
  pthread_once(&zmq_ll_once, make_state_key);

  // .. Get the state pointer ......................................
  ciao_zmq_state *state =
    (ciao_zmq_state *)pthread_getspecific(zmq_ll_state_key);

  // .. If necessary, allocate it and initialize ...................
  if(state==NULL) {
    state= (ciao_zmq_state *)malloc(sizeof(ciao_zmq_state));
    state->zmq_context= zmq_init(1);
    state->socket_list= NULL;
    state->error_list= NULL;
    pthread_setspecific(zmq_ll_state_key, state);
  }

  return state;
}

// -- Allocate and collect the data --------------------------------

static size_t
collect_bytes(int indicative_size,
	      ciao_term byte_list,
	      char **buff)
{
  size_t len, size, i;
  ciao_term lp, h;  
  *buff= NULL;

  // .. If the indicative size < 0, it is the list length ..........

  if(indicative_size < 0) {
    len= 0;
    lp= byte_list;
    for(len=0, lp=byte_list;
	ciao_is_list(lp);
	lp= ciao_list_tail(lp), len++) ;
    size= len;
  } else {
    size= indicative_size;
  }
  
  // .. Allocate if size > 0 .......................................

  if(size > 0) {

    *buff= (char *)malloc(size);

    // .. First, fill the bytes ....................................

    for(lp= byte_list, i=0;
	(i < size) && ciao_is_list(lp);
	lp= ciao_list_tail(lp), i++) {
      h= ciao_list_head(lp);
      (*buff)[i]= (ciao_is_integer(h)? (char)ciao_to_integer(h): 0);
    }

    // .. Fill the rest with zeros .................................

    while(i<size) (*buff)[i++]= 0;
  }

  return size;
}

// -- Initialize the ZMQ context for the current thread ------------
void
ciao_zmq_init_(void) {
  get_state();
}

// -- terminatednevn the ZMQ context for the current thread -------------
void
ciao_zmq_term_(void) {
  // .. Execute once per process ...................................
  pthread_once(&zmq_ll_once, make_state_key);

  // .. Access the state pointer ...................................
  ciao_zmq_state *state =
    (ciao_zmq_state *)pthread_getspecific(zmq_ll_state_key);

  // .. Finish if there is currently no state (nothing to temrinate) ..
  if(state!=NULL) {
    // .. Void the state for the current thread ....................
    pthread_setspecific(zmq_ll_state_key, NULL);

    // .. Deallocate all error records .............................
    while(state->error_list != NULL) {
      ciao_zmq_error_record *rec= state->error_list;
      state->error_list= state->error_list->next;
      free(rec);
    }

    // .. Close all sockets and deallocate association tables ......
    while(state->socket_list !=NULL) {
      ciao_zmq_socket_assoc *assoc= state->socket_list;
      state->socket_list= state->socket_list->next;
      zmq_close(assoc->zmq_socket);
      free(assoc);
    }

    // .. Finally, terminate the context ...........................
    zmq_term(state->zmq_context);

    // .. Deallocate the state record ..............................
    free(state);
  }
}

// -- Find socket association record by (atom) name ----------------
static ciao_zmq_socket_assoc *
find_socket(char *socket_atom) {
  // .. Get the state ..............................................
  ciao_zmq_state *state= get_state();

  // .. Get the first socket in the list ...........................
  ciao_zmq_socket_assoc *current= state->socket_list;
  
  while(current!=NULL) {
    // .. Try to match the socket name .............................
    if(!strncmp(current->socket_atom_chars, 
		socket_atom, 
		CIAO_ZMQ_MAX_ATOM_LEN))
      break;
    // .. Else move to the next socket in the list .................
    current= current->next;
  }
  return current;
}

// -- Option handling ----------------------------------------------

typedef struct _ciao_zmq_atom_option {
  char *name; // must be NULL for the last entry in an option table
  int value;
} ciao_zmq_atom_option;

static ciao_zmq_atom_option *
find_option(ciao_zmq_atom_option *option_table, char *atom) {
  while(option_table->name!=NULL) {
    if(!strcmp(option_table->name, atom))
      return option_table;
    option_table++;
  }
  return NULL;
}

// -- Registering an error -----------------------------------------

static void
report_error(int error_code, char *error_functor, char *socket_atom) {
  if(error_code!=0) {
    ciao_zmq_state *state= get_state();
    ciao_zmq_error_record *error= malloc(sizeof(ciao_zmq_error_record));
    error->next= state->error_list;
    error->error_code= error_code;
    error->error_functor= error_functor;
    strncpy(error->socket_atom_chars, socket_atom, CIAO_ZMQ_MAX_ATOM_LEN);
    state->error_list= error;
  }
}

// == Creating a socket ============================================

static ciao_zmq_atom_option socket_options[] = {
  {"req",    ZMQ_REQ},
  {"rep",    ZMQ_REP},
  {"dealer", ZMQ_DEALER},
  {"router", ZMQ_ROUTER},
  {"pub",    ZMQ_PUB},
  {"sub",    ZMQ_SUB},
  {"push",   ZMQ_PUSH},
  {"pull",   ZMQ_PULL},
  {"pair",   ZMQ_PAIR},
  {NULL, 0}
};

void 
ciao_zmq_socket(char *socket_atom, char *type_atom) {

  // .. Check if the socket exists .................................
  if(find_socket(socket_atom)!=NULL) {
    report_error(EINVAL, "socket_already_exists", socket_atom);
    return;
  }

  // .. Fetch the socket type ......................................
  ciao_zmq_atom_option *type_option= find_option(socket_options, 
						   type_atom);
  if(type_option==NULL) {
    report_error(EINVAL, "invalid_socket_type", socket_atom);
    return;
  }

  // .. Create the new socket ......................................

  ciao_zmq_state *state= get_state();
  void *newsocket= zmq_socket(state->zmq_context, type_option->value);
  if(newsocket==NULL) {
    report_error(errno, "error_creating_socket", socket_atom);
    return;
  }

  // .. Add the new socket record ..................................
  ciao_zmq_socket_assoc *assoc= malloc(sizeof(ciao_zmq_socket_assoc));
  assoc->next= state->socket_list;
  assoc->zmq_socket= newsocket;
  strncpy(assoc->socket_atom_chars, socket_atom, CIAO_ZMQ_MAX_ATOM_LEN);
  state->socket_list= assoc;
}

// == Closing a socket =============================================

void
ciao_zmq_close(char *socket_atom) {

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc==NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return;
  }

  // .. Perform closing ............................................
  if(zmq_close(assoc->zmq_socket)) {
    report_error(errno, "error_closing_socket", socket_atom);
    return;
  }

  // .. Remove socket record .......................................

  ciao_zmq_state *state= get_state();
  ciao_zmq_socket_assoc **ptr;

  ptr= &(state->socket_list);
  while((*ptr)!=assoc) {
    ptr= &((*ptr)->next);
  }
  *ptr= assoc->next;
  free(assoc);
}

// == Binding a socket =============================================

void
ciao_zmq_bind(char *socket_atom, char *endpoint) {

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc==NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return;
  } 

  // .. Perform binding ............................................
  if(zmq_bind(assoc->zmq_socket, endpoint)) {
    report_error(errno, "error_binding_socket", socket_atom);
  }
}

// == Connect a socket =============================================

void
ciao_zmq_connect(char *socket_atom, char *endpoint) {

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc==NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return;
  } 

  // .. Make connection ............................................
  if(zmq_connect(assoc->zmq_socket, endpoint)) {
    report_error(errno, "error_connecting_socket", socket_atom);
  }
}

// == Send a message (part) over a socket ==========================

static ciao_zmq_atom_option send_options[] = {
  {"noblock", ZMQ_NOBLOCK},
  {"sndmore", ZMQ_SNDMORE},
  {NULL, 0}
};

void
ciao_zmq_send(char *socket_atom, 
	      int indicative_size, ciao_term byte_list, 
	      ciao_term option_list) {

  char *buff;
  size_t size= collect_bytes(indicative_size, byte_list, &buff);

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc == NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return;
  } 

  // .. Parse options ..............................................
  int flags= 0;
  if(!ciao_is_variable(option_list)) {
    while(ciao_is_list(option_list)) {
      ciao_term option_term= ciao_list_head(option_list);
      option_list= ciao_list_tail(option_list);
      if(ciao_is_atom(option_term)) {
	char *atom= (char *)ciao_atom_name(option_term);
	ciao_zmq_atom_option *atom_option= find_option(send_options, atom);
	if(atom_option!=NULL) {
	  flags|= atom_option->value;
	} else {
	  report_error(EINVAL, "unknown_send_option", socket_atom);
	  if(buff != NULL) free(buff);
	  return;
	}
      } else {
	report_error(EINVAL, "invalid_send_option", socket_atom);
	if(buff != NULL) free(buff);
	return;
      }
    }
    if(!ciao_is_empty_list(option_list)) {
      report_error(EINVAL, "option_list_error", socket_atom);
      if(buff != NULL) free(buff);
      return;
    }
  }

  // .. Allocate message ...........................................
  zmq_msg_t msg;
  if(zmq_msg_init_size(&msg, size)) {
    report_error(errno, "message_init_error", socket_atom);
    if(buff != NULL) free(buff);
    return;
  }

  // .. Fill up the message data ...................................
  if(size>0) {
    memcpy((char *)zmq_msg_data(&msg), buff, size);
    free(buff);
  }

  // .. Send message ...............................................
  if(zmq_send(assoc->zmq_socket, &msg, flags)) {
    report_error(errno, "send_error", socket_atom);
  }
}

// == Receiving a message ==========================================

ciao_zmq_atom_option recv_options[] = {
  {"noblock", ZMQ_NOBLOCK},
  {NULL, 0}
};

extern ciao_term
ciao_zmq_recv(char *socket_atom, int *size, 
		 char **byte_list, ciao_term option_list) {
  
  *byte_list= NULL;
  *size= 0;

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc == NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return ciao_atom("none");
  }

  // .. Parse receive options ......................................
  int flags=0;
  if(!ciao_is_variable(option_list)) {
    ciao_term bp= option_list;
    while( ciao_is_list(bp)) {
      ciao_term h= ciao_list_head(bp);
      bp= ciao_list_tail(bp);
      if(ciao_is_atom(h)) {
	char *atom= (char *)ciao_atom_name(h);
	ciao_zmq_atom_option *opt= find_option(recv_options, atom);
	if(opt == NULL) {
	  report_error(EINVAL, "invalid_rcv_option", socket_atom);
	  return ciao_atom("none");
	}
	flags |= opt->value;
      } else {
	report_error(EINVAL, "unknown_rcv_option", socket_atom);
	return ciao_atom("none");
      }
    }
    if(!ciao_is_empty_list(bp)) {
      report_error(EINVAL, "option_list_error", socket_atom);
      return ciao_atom("none");
    }
  }

  // .. Initialize message .........................................
  zmq_msg_t msg;
  if(zmq_msg_init(&msg)) {
    report_error(errno, "messge_init_error", socket_atom);
    return ciao_atom("none");
  }

  // .. Perform reception ..........................................
  int ret= zmq_recv(assoc->zmq_socket, &msg, flags);
  if(ret) {
    if(!((flags & ZMQ_NOBLOCK) && (errno==EAGAIN))) {
      report_error(errno, "recv_error", socket_atom);
    }
    return ciao_atom("none");
  }

  // .. Get the message (part) size ................................
  size_t msg_size= zmq_msg_size(&msg);
  /* if(msg_size > CIAO_ZMQ_MAX_MESSAGE_SIZE) { */
  /*   msg_size= CIAO_ZMQ_MAX_MESSAGE_SIZE; */
  /* } */

  // .. Build the byte list from the message .......................

  *size= msg_size;
  if(msg_size>0) {
    *byte_list = (char *)ciao_malloc(msg_size);
    memcpy(*byte_list, zmq_msg_data(&msg), msg_size);
  } else {
    *byte_list = NULL;
  }
   
  // .. Close the message structure ................................
  zmq_msg_close(&msg);

  // .. Return the byte list ....................................
  return ciao_atom("some");
}

// == Check if a  multipart message is pending =====================

ciao_term
ciao_zmq_multipart_pending(char *socket_atom) {

  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(socket_atom == NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    return ciao_atom("none");
  }

  // .. Get the value of the ZMQ_RCVMORE flag ......................
  int64_t more;
  size_t more_size= sizeof(more);
  if(zmq_getsockopt(assoc->zmq_socket, ZMQ_RCVMORE, &more, &more_size)) {
    report_error(errno, "error_checking_multipart", socket_atom);
    return ciao_atom("none");
  }
  
  // .. Return the indicator .......................................
  return ciao_atom((more? "some" : "none"));
}

// == (Un)Subscribe socket =============================================

static void
x_subscribe(char *socket_atom, ciao_term byte_prefix, 
	    int indicative_size, int action) {

  char *buff;
  size_t size= collect_bytes(indicative_size, byte_prefix, &buff);
  
  // .. Find the socket ............................................
  ciao_zmq_socket_assoc *assoc= find_socket(socket_atom);
  if(assoc == NULL) {
    report_error(EINVAL, "socket_not_found", socket_atom);
    if(buff != NULL) free(buff);
    return;
  }

  // .. Invoke the operation .......................................
  int op= (action? ZMQ_SUBSCRIBE : ZMQ_UNSUBSCRIBE);
  if(zmq_setsockopt(assoc->zmq_socket, op, buff, size))
    report_error(errno, "subscription_error", socket_atom);
  if(buff != NULL) free(buff);
}

void
ciao_zmq_subscribe(char *socket_atom, int size, ciao_term byte_prefix) {
  x_subscribe(socket_atom, byte_prefix, size, 1);
}

void
ciao_zmq_unsubscribe(char *socket_atom, int size, ciao_term byte_prefix) {
  x_subscribe(socket_atom, byte_prefix, size, 0);
}

// == Poll sockets for incoming messages ===========================

ciao_term
ciao_zmq_poll(ciao_term socket_list, int timeout) {
  
  // .. Check the socket list ......................................
  ciao_term sl= socket_list;
  int nsock= 0;
  while(ciao_is_list(sl)) {
    nsock++;
    sl= ciao_list_tail(sl);
  }
  if(!ciao_is_empty_list(sl)) {
    report_error(EINVAL, "invalid_socket_list", "");
    return;
  }

  if(nsock<1) return ciao_empty_list();

  // .. Create polling item array ..................................

  zmq_pollitem_t *poll_items= 
    (zmq_pollitem_t *)malloc(nsock*sizeof(zmq_pollitem_t));
  sl= socket_list;
  int i;
  for(i=0; i<nsock; i++) {
    ciao_term h= ciao_list_head(sl);
    sl= ciao_list_tail(sl);
    if(!ciao_is_atom(h)) {
      report_error(EINVAL, "not_a_socket_list", "");
      free(poll_items);
      return;
    }
    ciao_zmq_socket_assoc *assoc= find_socket((char *)ciao_atom_name(h));
    if(assoc == NULL) {
      report_error(EINVAL, "socket_not_found", (char *)ciao_atom_name(h));
      free(poll_items);
      return;
    }
    poll_items[i].socket= assoc->zmq_socket;
    poll_items[i].events= ZMQ_POLLIN;
  }

  // .. Perform polling ............................................
  int res= zmq_poll(poll_items, nsock, (long)timeout);
  if(res<0) {
    report_error(errno, "polling_error", "");
    free(poll_items);
    return ciao_empty_list();
  }

  // .. Construct the list of sockets with data ....................
  ciao_term rlist= ciao_empty_list();
  sl= socket_list;
  for(i=0; i<nsock; i++) {
    if(poll_items[i].revents & ZMQ_POLLIN) {
      rlist= ciao_list(ciao_list_head(sl), rlist);
    }
    sl= ciao_list_tail(sl);
  }
  free(poll_items);

  return rlist;
 }

// == Run a device =================================================

static ciao_zmq_atom_option device_options[] = {
  {"queue",    ZMQ_QUEUE},
  {"forwarder",ZMQ_FORWARDER},
  {"streamer", ZMQ_STREAMER},
  {NULL, 0}
};


void
ciao_zmq_device(char *type_atom, char *frontend, char *backend) {
  
  ciao_zmq_socket_assoc *front_assoc= find_socket(frontend);
  if(front_assoc == NULL) {
    report_error(EINVAL, "socket_not_found", frontend);
    return;
  }

  ciao_zmq_socket_assoc *back_assoc= find_socket(backend);
  if(back_assoc==NULL) {
    report_error(EINVAL, "socket_not_found", backend);
    return;
  }

  ciao_zmq_atom_option *opt= find_option(device_options, type_atom);
  if(opt == NULL) {
    report_error(EINVAL, "invalid_device", type_atom);
    return;
  }

  zmq_device(opt->value, 
	     front_assoc->zmq_socket, 
	     back_assoc->zmq_socket);
}

// == Check for errors =============================================

ciao_term
ciao_zmq_error_check(void) {
  ciao_zmq_state *state= get_state();
  return ciao_atom((state->error_list == NULL? "none": "some"));
}


// == Return error list ============================================

static ciao_term
error_collect( ciao_zmq_error_record *rec) {
  if(rec == NULL)
    return ciao_empty_list();

  rec->padding= 0;
  ciao_term trec= ciao_structure("error", 3, 
				 ciao_integer(rec->error_code),
				 ciao_atom(rec->error_functor),
				 ciao_atom(rec->socket_atom_chars));
  ciao_zmq_error_record *next= rec->next;
  free(rec);
  return ciao_list(trec, error_collect(next));
}

ciao_term
ciao_zmq_errors(void) {
  ciao_zmq_state *state= get_state();
  ciao_term err_list= error_collect(state->error_list);
  state->error_list= NULL;
  return err_list;
}


				 
