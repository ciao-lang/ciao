:- module(http_ll, [http_transaction/5], [assertions,isomodes]).

:- use_module(library(sockets)).
:- use_module(library(strings), [write_string/2]).
:- use_module(library(file_utils), [stream_to_string/2]).

:- pred http_transaction(+Host, +Port, +Request, +Timeout, -Response)
   :: atm * int * string * int * string
   # "Sends an HTTP Request to an HTTP server and returns the resultant
      message.  Fails on timeout (@var{Timeout} in seconds).".

http_transaction(Host, Port, Request, Timeout, Response) :-
        connect_to_socket(Host, Port, Stream),
        write_string(Stream, Request),
        flush_output(Stream),
	Timeout_ms is Timeout*1000,
        select_socket(_,_,Timeout_ms,[Stream],R),
        R \== [],  % Fail if timeout
        stream_to_string(Stream,Response).
