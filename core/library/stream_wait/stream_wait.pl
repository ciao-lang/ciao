:- module(stream_wait, [], [modes, assertions]).

:- doc(title, "Wait for streams").
:- doc(author, "Remy Haemmerle").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module make possible to wait for stream to be
   ready for performing I/O.").

:- doc(bug, "Waits is only supported for input streams").
:- doc(bug, "It should not be required to set the stream to be unbuffered").  

:- use_module(library(streams), [stream/1]).
:- import(io_basic, ['$input_wait'/3, '$set_unbuf'/1]).

:- export(input_wait/2).
:- pred input_wait(+InputStream, +TimeOut) : stream * int # "Waits at
   most @var{TimeOut} microseconds for the stream @var{InputStream} to
   be ready for reading.  The predicate succeeds as soon
   @var{InputStream} is ready, but fails if the time limit expires. To
   work properly @var{InputStream} must be made unbuffered using
   @pred{input_set_unbuf/1} before any call.".

input_wait(_, TimeOut) :- var(TimeOut), !, 
    throw(error(instantiation_error, 'stream_wait:input_wait'/2-2)).
input_wait(Stream, TimeOut):- integer(TimeOut), !, 
    Sec  is TimeOut // 1000000, 
    USec is TimeOut mod 1000000,
    '$input_wait'(Stream, Sec, USec).
input_wait(_, TimeOut) :-
    throw(error(type_error(integer, TimeOut), 'stream_wait:input_wait'/2-2)).

:- export(input_set_unbuf/1).
:- pred input_set_unbuf(+InputStream) : stream # "Set the input stream
   @var{InputStream} unbuffered. Portable applications should call it
   only once on any given stream, and before any I/O is performed.".

input_set_unbuf(Stream):-
    '$set_unbuf'(Stream).

