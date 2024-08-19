/*
 *  stream_basic.c
 *
 *  Stream handling primitives (see engine(stream_basic))
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 The Ciao Development Team
 */

/* TODO: This code should implement particular kind of streams (file,
   sockets, etc.) */

#include <ciao/eng.h>
#include <ciao/io_basic.h>
#include <ciao/stream_basic.h>

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <sys/param.h>
#include <errno.h>

#define ENG_NOFILES 20

#include <ciao/eng_registry.h>
#include <ciao/eng_interrupt.h> /* control_c_normal */
#include <ciao/rt_exp.h>

/* ------------------------------------------------------------------------- */
/* The root stream pointer (all streams are liked) */

stream_node_t *root_stream_ptr; /* Shared and _locked_ */

/* ------------------------------------------------------------------------- */
/* The table of stream aliases */

stream_node_t *stream_user_input  = NULL; /* Shared */
stream_node_t *stream_user_output = NULL; /* Shared */
stream_node_t *stream_user_error  = NULL; /* Shared */

/* ------------------------------------------------------------------------- */

tagged_t atom_user_input; /* "user_input" */ 
tagged_t atom_user_output; /* "user_output" */
tagged_t atom_user_error; /* "user_error" */

/* ------------------------------------------------------------------------- */
/* Initialize the streams library (only once) */

void init_streams(void)
{
  root_stream_ptr = checkalloc_TYPE(stream_node_t); 
  root_stream_ptr->label=ERRORTAG;
  root_stream_ptr->streamname=ERRORTAG;
  root_stream_ptr->forward=root_stream_ptr;
  root_stream_ptr->backward=root_stream_ptr;
  root_stream_ptr->last_nl_pos = 0;               /* used for tty streams */
  root_stream_ptr->nl_count = 0;
  root_stream_ptr->rune_count = 0;

  stream_user_input = new_stream(ERRORTAG, "r", stdin);
  stream_user_output = new_stream(ERRORTAG, "a", stdout);
  stream_user_error = new_stream(ERRORTAG, "a", stderr);

  /* initialize the table of streams aliases */
  atom_user_input=GET_ATOM("user_input");
  atom_user_output=GET_ATOM("user_output");
  atom_user_error=GET_ATOM("user_error");
}


/* --------------------------------------------------------*/

/* Protect the creation of streams: several threads might want to create
   streams at once. */

stream_node_t *insert_new_stream(stream_node_t *new_stream){

  Wait_Acquire_lock(stream_list_l);
  new_stream->forward = root_stream_ptr;
  new_stream->backward = root_stream_ptr->backward;
  root_stream_ptr->backward->forward = new_stream;
  root_stream_ptr->backward = new_stream;
  Release_lock(stream_list_l);

  return new_stream;
}

stream_node_t *new_stream(tagged_t streamname,
                          char *streammode,
                          FILE *streamfile)
{
  stream_node_t *s;

  s = checkalloc_TYPE(stream_node_t);
  s->streamname = streamname;
  s->streammode = streammode[0];
  s->pending_rune = RUNE_VOID;
  s->previous_rune = RUNE_VOID;
  s->socket_eof = FALSE;
  update_stream(s,streamfile);

  return insert_new_stream(s);
}

static int file_is_tty(FILE *file) {
  return (isatty(fileno(file)) ||
          (interactive_flag_bool && fileno(file)<3));
}

void update_stream(stream_node_t *s, FILE *file) {
  s->label = MakeSmall(fileno(file));
  s->streamfile = file;
  if ((s->isatty = file_is_tty(file)))
    s = root_stream_ptr;
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->rune_count = 0; /* less than perfect */
}

/* ------------------------------------------------------------------------- */
/* Functions to check types */
/* (useful to find when exceptions should be raised) */

/* ISO Prolog does not allow stream aliases to be used instead of
   stream terms in many cases.  We are relaxing this here. */

CBOOL__PROTO(is_var_or_alias_or_stream, tagged_t Cell)
{
  if (IsVar(Cell)) {
    /* a variable */
    return TRUE;
  } else if (TaggedIsATM(Cell)) {
    /* a stream alias */
    return (Cell == atom_user_input ||
            Cell == atom_user_output ||
            Cell == atom_user_error);
  } else {
    /* a Dstream functor */
    return (TaggedIsSTR(Cell) && TaggedToHeadfunctor(Cell) == functor_Dstream);
  }

}

/* ------------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) <-- (stream_node_t *) */

CFUN__PROTO(ptr_to_stream_noalias, tagged_t, stream_node_t *n) {
  // printf("(int)n is %ud\n", (int)n);
  // printf("n->label is %ud\n", n->label);
  tagged_t *pt1 = w->heap_top;
  HeapPush(pt1,functor_Dstream);
  HeapPush(pt1,PointerToTerm(n));
  HeapPush(pt1,n->label);
  w->heap_top = pt1;
  return Tagp(STR,HeapOffset(pt1,-3));
}

/* ------------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) <-- (stream_node_t *) */
/* or */
/* <stream_alias> <-- (stream_node_t *) */
CFUN__PROTO(ptr_to_stream, tagged_t, stream_node_t *n)
{
  if (n==stream_user_input)
    return atom_user_input;
  if (n==stream_user_output)
    return atom_user_output;
  if (n==stream_user_error)
    return atom_user_error;

  return ptr_to_stream_noalias(Arg, n);
}

/* ------------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) --> (stream_node_t *)
                          --> NULL, if invalid
   'r' - read mode,  streammode=[rs]
   'w' - write mode, streammode=[was]
   'x' - any mode,   streammode=[rwas]
   'y' - any mode but no standard streams  */
stream_node_t *stream_to_ptr(tagged_t t,
                             int mode)
{
  stream_node_t *n = NULL;
  tagged_t x1, x2;

  DerefSw_HVAorCVAorSVA_Other(t,;,{});

  if (TaggedIsATM(t))
    {
      if (mode=='y')
        n = NULL;
      else if (t==atom_user)
        n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
        n = stream_user_input;
      else if (t==atom_user_output)
        n = stream_user_output;
      else if (t==atom_user_error)
        n = stream_user_error;
    }
  else if (TaggedIsSTR(t) && (TaggedToHeadfunctor(t) == functor_Dstream))
    {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TaggedIsSmall(x1) || !TaggedIsSmall(x2) ||
          (n=TaggedToStream(x1), n->label != x2))
        n = NULL;
    }

  if (((mode=='r') && (n!=NULL) && (n->streammode=='w'||n->streammode=='a')) ||
      ((mode=='w') && (n!=NULL) && (n->streammode=='r')))
    return NULL;
  else
    return n;
}

/* ------------------------------------------------------------------------- */
/* Similar to stream_to_ptr(), but giving an error code */

stream_node_t *stream_to_ptr_check(tagged_t t,
                                   int mode, /* not 'y' */
                                   int *errcode)
{
  stream_node_t *n = NULL;
  tagged_t x1, x2;

  DerefSw_HVAorCVAorSVA_Other(t,{*errcode = ERR_instantiation_error; return NULL;},{});

  if (TaggedIsATM(t))
    {
      if (t==atom_user)
        n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
        n = stream_user_input;
      else if (t==atom_user_output)
        n = stream_user_output;
      else if (t==atom_user_error)
        n = stream_user_error;
      else {
            *errcode = ERR_domain_error(stream_or_alias);
            return NULL;
      }
    }
  else if (TaggedIsSTR(t) && (TaggedToHeadfunctor(t) == functor_Dstream)) {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TaggedIsSmall(x1) || !TaggedIsSmall(x2) ||
          (n = TaggedToStream(x1), n->label != x2)) {
            *errcode = ERR_existence_error(stream);
            return NULL;
      }
    } else {
        *errcode = ERR_domain_error(stream_or_alias);
        return NULL;
    }

  if (mode=='r') {
    if (n->streammode=='w'||n->streammode=='a') {
      *errcode = ERR_permission_error(access,stream);  
      return NULL;
    }
  } else if (mode=='w') {
    if (n->streammode=='r') {
      *errcode = ERR_permission_error(modify,stream); 
      return NULL;
    }
  }

  return n;
}

/* ------------------------------------------------------------------------- */
/* BUILTIN C PREDICATES */

/* ------------------------------------------------------------------------- */
/* USAGE: open(+,+,-) only */

CBOOL__PROTO(prolog_open)
{
  ERR__FUNCTOR("stream_basic:$open", 3);
  struct stat statbuf;
  FILE *fileptr;
  char *modecodif;
  char modespec[3];

  enum {
    STRANGE_SOURCE_SINK,
    INEXISTENT_SOURCE_SINK,
    CANNOT_OPEN,
    SYS_ERROR,
    FINISHED_RESOURCES
  } what_happened;


  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  modecodif = GetString(X(1));

#if defined(_WIN32) || defined(_WIN64)
#warning "TODO(MinGW): assume binary streams"
  /* TODO(MinGW): Otherwise we may have big problems with magic \r\n <-> \n translations. */
#endif
  modespec[0] = modecodif[0];
  modespec[1] = 'b';
  modespec[2] = 0;

  fileptr = (TaggedIsATM(X(0)) ? fopen(GetString(X(0)), modespec) :
             TaggedIsSmall(X(0)) ? fdopen(GetSmall(X(0)), modespec) :
             NULL);

  if (fileptr==NULL) {
    what_happened = SYS_ERROR;                            /* Just in case */
    if (errno==ENOENT || errno==ENOTDIR || errno==ENXIO ||errno==EBADF)   {
      what_happened = INEXISTENT_SOURCE_SINK;
    } else if (errno==EEXIST || errno==EISDIR || 
               errno==EBADF || errno==EROFS) {
      what_happened = CANNOT_OPEN;
    } else if (errno==ENOMEM || errno==EMFILE || errno==ENFILE) {
      what_happened = FINISHED_RESOURCES;
    }
    goto bombit;
  } else {
    if (fstat(fileno(fileptr), &statbuf) || 
        (statbuf.st_mode & S_IFMT) == S_IFDIR) {
      fclose(fileptr);
      what_happened = CANNOT_OPEN;
      goto bombit;
    }
  }

#if !defined(_WIN32) && !defined(_WIN64)
  /* TODO(MinGW): file locks not available in MinGW */
  {
    char locking = modecodif[1];

    if (locking == 'l' || locking == 'b') /* file locking */ {
      struct flock sflo;
      int cmd = (locking == 'l' ? F_SETLK : F_SETLKW);

      sflo.l_whence = 0; sflo.l_start = 0; sflo.l_len = 0;
      sflo.l_type = 
        (modecodif[2] == 'r' || 
         (modecodif[2] =='\0' && modecodif[0] == 'r') ? F_RDLCK
         : F_WRLCK);
      if (fcntl(fileno(fileptr), cmd, &sflo) < 0) {
        fclose(fileptr);
        return FALSE;
      }
    }
  }
#endif

  return
    cunify(Arg, ptr_to_stream(Arg,new_stream(X(0),modespec,fileptr)),X(2));

 bombit:
  if (current_ferror_flag == atom_on) {
    switch (what_happened) {
    case STRANGE_SOURCE_SINK:
      BUILTIN_ERROR(ERR_domain_error(source_sink), X(0), 1); break;
    case INEXISTENT_SOURCE_SINK:
      BUILTIN_ERROR(ERR_existence_error(source_sink), X(0), 1); break;
    case CANNOT_OPEN:
      BUILTIN_ERROR(ERR_permission_error(open, source_sink),X(0),1); break;
   case FINISHED_RESOURCES:
      BUILTIN_ERROR(ERR_resource_error(r_undefined),X(0),1); break;
    case SYS_ERROR:
    default:
      BUILTIN_ERROR(ERR_system_error,X(0),1); break;
    } 
  } else {
    return FALSE;
  }
}



/* ------------------------------------------------------------------------- */

/* as Quintus closing a stream object referring to user_input,user_output */
/*   or user_error will succeed but cause no changes */

CBOOL__PROTO(prolog_close)
{
  ERR__FUNCTOR("stream_basic:close", 1);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  } else if (stream==Input_Stream_Ptr) {
    Input_Stream_Ptr = stream_user_input;
  } else if (stream==Output_Stream_Ptr) {
    Output_Stream_Ptr = stream_user_output;
  } else if (stream==Error_Stream_Ptr) {
    Error_Stream_Ptr = stream_user_error;
  }

  if ((stream!=stream_user_input) &&
      (stream!=stream_user_output) &&
      (stream!=stream_user_error))
    {
      if (stream->streammode != 's')        /* Not a socket -- has FILE * */
        fclose(stream->streamfile);  /* Releases file locks automatically */
      else
        close(TaggedToIntmach(stream->label));            /* Needs a lock here */


 /* We are twiggling with a shared structure: lock the access to it */

      Wait_Acquire_lock(stream_list_l);

      stream->label = ERRORTAG;
      stream->backward->forward = stream->forward;
      stream->forward->backward = stream->backward;

      /* now ensure that no choicepoints point at the stream */
      {
        choice_t *B;
        tagged_t t1, t2;

        t1 = PointerToTerm(stream);
        t2 = PointerToTerm(stream->forward);
        
        for (B = w->choice;
             ChoiceYounger(B,Choice_Start);
             B = ChoiceCont(B))
          if (B->next_alt==address_nd_current_stream && B->x[3]==t1)
            B->x[3] = t2;
      }

      checkdealloc_TYPE(stream_node_t, stream);
      Release_lock(stream_list_l);
    }
  return TRUE;
}

/* ------------------------------------------------------------------------- */

#if defined(_WIN32) || defined(_WIN64) /* MinGW */
int pipe(int filedes[2]);
#endif

CBOOL__PROTO(prolog_pipe)
{
  ERR__FUNCTOR("stream_basic:pipe", 2);
  FILE *in;
  FILE *out;
  int fd[2];

  if (pipe(fd)) goto bombit;
  if (!(in  = fdopen(fd[0], "r"))) goto bombit;
  if (!(out = fdopen(fd[1], "w"))) goto bombit;

  return (cunify(Arg,ptr_to_stream(Arg, new_stream((tagged_t)0, "r", in)), X(0)) &&
          cunify(Arg,ptr_to_stream(Arg, new_stream((tagged_t)0, "w", out)), X(1)));

 bombit:
  BUILTIN_ERROR(ERR_resource_error(r_undefined),X(0),1) ;
}

/* ------------------------------------------------------------------------- */

/* ISO Behavior (MCL): current_input and current_output shall unify its
   argument with the current input (re. output) stream, _not_ stream_alias.
   This is a bit relaxed here: we allow also for stream aliases to be passed
   in and out without raising an exception.  Same goes for current_output */ 

CBOOL__PROTO(prolog_current_input)
{
  ERR__FUNCTOR("stream_basic:current_input", 1);
  DEREF(X(0), X(0));

  if (is_var_or_alias_or_stream(Arg, X(0))) {
    CBOOL__LASTUNIFY(ptr_to_stream(Arg,Input_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(ERR_domain_error(stream_or_alias), X(0), 1);
  }
}


CBOOL__PROTO(prolog_set_input)
{
  ERR__FUNCTOR("stream_basic:set_input", 1);
  int errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));

  stream = stream_to_ptr_check(X(0), 'r', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  Input_Stream_Ptr = stream;
  return TRUE;
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_current_output)
{
  ERR__FUNCTOR("stream_basic:current_output", 1);
  DEREF(X(0), X(0));

  if (is_var_or_alias_or_stream(Arg, X(0))) {
    CBOOL__LASTUNIFY(ptr_to_stream(Arg,Output_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(ERR_domain_error(stream_or_alias), X(0), 1);
  }
}


CBOOL__PROTO(prolog_set_output)
{
  ERR__FUNCTOR("stream_basic:set_output", 1);
  int errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode, X(0), 1);
  }
  Output_Stream_Ptr = stream;
  return TRUE;
}

/* ------------------------------------------------------------------------- */

/* Replacing the stream aliases pointer */

/* replace_stream(StreamAlias, NewStream) */

CBOOL__PROTO(prolog_replace_stream) {
  ERR__FUNCTOR("io_alias_redirection:replace_stream", 2);
  tagged_t which_stream;
  tagged_t which_atom;
  stream_node_t *node;
  int errcode;

  DEREF(which_atom, X(0));
  DEREF(which_stream, X(1));

  if ((which_atom == atom_user_error) ||
      (which_atom == atom_user_output)) {
    node = stream_to_ptr_check(which_stream, 'w', &errcode);    
  } else if (which_atom == atom_user_input) {
    node = stream_to_ptr_check(which_stream, 'r', &errcode);    
  } else {
    /* Not exactly: should be "alias"*/
    BUILTIN_ERROR(ERR_domain_error(stream_or_alias),X(0),1);
  }
  
  if (node == NULL) BUILTIN_ERROR(errcode,X(0),1);

  if (which_atom == atom_user_input) {
    stream_user_input = node;
  } else if (which_atom == atom_user_output) {
    stream_user_output = node;
  } else if (which_atom == atom_user_error) {
    stream_user_error = node;
  }

  CBOOL__PROCEED;
}

/* get_stream(StreamAlias, CurrentStream) */

CBOOL__PROTO(prolog_get_stream) {
  ERR__FUNCTOR("io_alias_redirection:get_stream", 2);
  tagged_t which_atom;
  stream_node_t *node;

  DEREF(which_atom, X(0));
  if (which_atom == atom_user_input) {
    node = stream_user_input;
  } else if (which_atom == atom_user_output) {
    node = stream_user_output;
  } else if (which_atom == atom_user_error) {
    node = stream_user_error;
  } else {
    BUILTIN_ERROR(ERR_domain_error(stream_or_alias),X(0),1);
  }
   
  CBOOL__LASTUNIFY(X(1), CFUN__EVAL(ptr_to_stream_noalias, node));
}

/* ------------------------------------------------------------------------- */

CBOOL__PROTO(prolog_current_error)
{
  CBOOL__LASTUNIFY(ptr_to_stream(Arg,Error_Stream_Ptr),X(0));
}

/* ------------------------------------------------------------------------- */

/* prolog_stream_code(?Stream,?Stream_code)
 * stream Stream        A prolog Stream
 * integer Stream_code  A unique number associated with Stream
 *
 * Description: Stream can be used by prolog predicates to perform IO
 * Stream_code can be used by C functions to somehow perform IO.
 * There is a problem due to the ambiguity of 'user':
 *      stream_code(user,X)
 * will return the stream code associated with user_output.
 */

CBOOL__PROTO(prolog_stream_code)
{
  ERR__FUNCTOR("stream_basic:stream_code", 2);
  int errcode;
  stream_node_t *s;

  DEREF(X(1), X(1));
  if (IsVar(X(1)))
    {
      s = stream_to_ptr_check(X(0), 'x', &errcode);
      if (!s) {
        BUILTIN_ERROR(errcode,X(0),1);
      }

      if (s->streammode != 's'){                            /* Not a socket */
        CBOOL__UnifyCons(MakeSmall(fileno(s->streamfile)),X(1));
      } else {                                                  /* DCG, MCL */
        CBOOL__UnifyCons(s->label,X(1)); /* Can't be this done above as well? */
      }
      return TRUE;
    }
  else if (X(1) >= TaggedZero && X(1) < MakeSmall(ENG_NOFILES))
    {
      for (s = root_stream_ptr->backward;
           s != root_stream_ptr && s->label != X(1);
           s = s->backward)
        ;
      if (s != root_stream_ptr && s->label == X(1))
        CBOOL__LASTUNIFY(ptr_to_stream(Arg,s),X(0));
      else
        return FALSE;
    }
  else if (IsInteger(X(1)))
    return FALSE;
  else
    BUILTIN_ERROR(ERR_type_error(integer),X(1),2);
}


CBOOL__PROTO(character_count)
{
  ERR__FUNCTOR("stream_basic:character_count", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->rune_count),X(1));
}


CBOOL__PROTO(line_position)
{
  ERR__FUNCTOR("stream_basic:line_position", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->rune_count-stream->last_nl_pos),X(1));
}


CBOOL__PROTO(line_count)
{
  ERR__FUNCTOR("stream_basic:line_count", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  CBOOL__LASTUNIFY(IntmachToTagged(stream->nl_count),X(1));
}

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_STREAM/3
   -----------------------------------------------------------------------*/

static CBOOL__PROTO(current_stream_data, stream_node_t *streamptr)
{
  CBOOL__UnifyCons(streamptr->streamname,X(0));
  switch (streamptr->streammode)
    {
    case 'a':
      CBOOL__UnifyCons(atom_append,X(1));
      break;
    case 'r':
      CBOOL__UnifyCons(atom_read,X(1));
      break;
    case 'w':
      CBOOL__UnifyCons(atom_write,X(1));
      break;
    case 's':
      CBOOL__UnifyCons(atom_socket,X(1));
      break;
    }
  return TRUE;
}


CBOOL__PROTO(current_stream)
{
  stream_node_t *streamptr;

  DEREF(X(2),X(2));
  if (!IsVar(X(2)) && (streamptr=stream_to_ptr(X(2),'y')))
    return current_stream_data(Arg,streamptr);

  streamptr = root_stream_ptr->forward;
  while (streamptr!=root_stream_ptr &&
         streamptr->streamname==ERRORTAG) /* skip over system streams */
    streamptr = streamptr->forward;
  if (streamptr==root_stream_ptr)
    return FALSE;
  else if (streamptr->forward!=root_stream_ptr)
    {
      X(3) = PointerToTerm(streamptr->forward);
      push_choicept(Arg,address_nd_current_stream);
    }

  return (cunify(Arg,ptr_to_stream(Arg,streamptr),X(2)) &&
          current_stream_data(Arg,streamptr));
}

CBOOL__PROTO(nd_current_stream)
{
  stream_node_t *streamptr = TaggedToStream(X(3));

  if (streamptr==root_stream_ptr)
    {                           /* zero alts due to close */
      pop_choicept(Arg);
      return FALSE;
    }
  else if (streamptr->forward==root_stream_ptr) /* last alt */
    pop_choicept(Arg);
  else
    w->choice->x[3]=PointerToTerm(streamptr->forward);
  return (cunify(Arg,ptr_to_stream(Arg,streamptr),X(2)) &&
          current_stream_data(Arg,streamptr));
}

/* ------------------------------------------------------------------------- */

/* Force interactive mode (used by toplevel -i option) */
CBOOL__PROTO(prolog_force_interactive)
{
  interactive_flag_bool = TRUE;
  /* Update streams so that isatty is enabled */
  update_stream(stream_user_input,stream_user_input->streamfile);
  update_stream(stream_user_output,stream_user_output->streamfile);
  update_stream(stream_user_error,stream_user_error->streamfile);
  /* This will (re)install SIGINT handler (since it is ignored for non interactive) */
  CVOID__CALL(control_c_normal);
  return TRUE;
}
