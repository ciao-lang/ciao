:- module(io_alias_redirection, 
      [set_stream/3, get_stream/2], 
      [assertions, basicmodes]).

:- doc(title,"Accessing and redirecting the stream aliases").

:- doc(author, "Manuel Carro").

:- doc(module, "This library allows the redefinition of the files
   to which the special streams @tt{user_input}, @tt{user_output}, and
   @tt{user_error} point to.  On startup they point to the standard
   input, standard output, and standard error, in Unix style (Windows
   users may find that standard error stream does not work properly).
   Changing the file pointed to is useful for, e.g., redirecting the
   place to which the Prolog's standard error stream goes from within
   Prolog (e.g., to start a log file).").

:- use_module(engine(stream_basic), [stream_alias/1, stream/1]).

:- pred set_stream(+StreamAlias, +NewStream, ?OldStream)
   : stream_alias * stream * stream 
   # "Associate @var{StreamAlias} with an open stream @var{newStream}.
   Returns in @var{OldStream} the stream previously associated with the
   alias.  The mode of @var{NewStream} must match the intended use of
   @var{StreamAlias}.".

set_stream(StreamAlias, NewStream, OldStream) :-
    get_stream(StreamAlias, OldStream),
    replace_stream(StreamAlias, NewStream).

:- pred get_stream(+StreamAlias, ?Stream) : stream_alias * stream 
   # "Return in @var{Stream} the stream associated with
   @var{StreamAlias}.".
:- if(defined(optim_comp)).
:- '$props'(get_stream/2, [impnat=cbool(prolog_get_stream)]). % in stream_basic.c
:- else.
:- impl_defined(get_stream/2).
:- endif.

:- pred replace_stream(+StreamAlias, +NewStream) : stream_alias * stream 
   # "Replace the stream associated to @var{StreamAlias} by
   @var{NewStream}.".
:- if(defined(optim_comp)).
:- '$props'(replace_stream/2, [impnat=cbool(prolog_replace_stream)]). % in stream_basic.c
:- else.
:- impl_defined(replace_stream/2).
:- endif.

