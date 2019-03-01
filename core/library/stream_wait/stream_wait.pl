:- module(stream_wait, [input_wait/1, input_wait/2, 
	                input_set_unbuf/0, input_set_unbuf/1], 
	[foreign_interface, assertions]).

:- doc(title, "Wait for streams").
:- doc(author, "Remy Haemmerle").

:- doc(module, "This module make possible to wait for stream to be
   ready for performing I/O.").

:- doc(bug, "Waits is only supported for input streams").
:- doc(bug, "It should not be required to set the stream to be unbuffered").  

:- use_module(engine(stream_basic)).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).

:- trust pred input_wait__c(+int, +int) + (foreign_low). 

:- pred input_wait(+InputStream, +TimeOut) : stream * int # "Waits at
   most @var{TimeOut} microseconds for the stream @var{InputStream} to
   be ready for reading.  The predicate succeeds as soon
   @var{InputStream} is ready, but fails if the time limit expires. To
   work properly @var{InputStream} must be made unbuffered using
   @pred{input_set_unbuf/1} before any call.".

input_wait(TimeOut):- var(TimeOut), !, 
	throw(error(instantiation_error, 'stream_wait:input_wait'/1-1)).
input_wait(TimeOut):- integer(TimeOut), !, 
	Sec  is TimeOut // 1000000, 
	USec is TimeOut mod 1000000,
	input_wait__c(Sec, USec).
input_wait(TimeOut):-
	throw(error(type_error(integer, TimeOut), 'stream_wait:input_wait'/1-1)).

input_wait(Stream, TimeOut):-
	current_input(CurIn),
	set_input(Stream),
	once_port_reify(input_wait(TimeOut), Port),
	set_input(CurIn),
	port_call(Port).

:- pred input_wait(+TimeOut) : int(TimeOut)
   # "Like @pred{input_wait/2}, but waits for the @concept{current input}.".

:- use_foreign_source(stream_wait).

:- pred input_set_unbuf(+InputStream) : stream # "Set the input stream
   @var{InputStream} unbuffered. Portable applications should call it
   only once on any given stream, and before any I/O is performed.".

input_set_unbuf(Stream):-
	current_input(CurIn),
	set_input(Stream),
	( input_set_unbuf, fail
	; set_input(CurIn)
	).

:- pred input_set_unbuf # "Like @pred{input_wait/1}, but set the
   @concept{current input} to be unbuffered.".

:- trust pred input_set_unbuf + (foreign_low(input_set_unbuf__c)). 
