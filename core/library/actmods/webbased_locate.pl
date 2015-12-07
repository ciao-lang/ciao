
:- module(webbased_locate,[module_address/2],[]).

:- use_module(library(actmods/actmodrt)).
:- use_module(library(actmods/webbased_common)).
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(pillow/http)).

:- data address_db/2.
:- data server/2.

module_address(M, Address) :-
	current_fact(address_db(M, Address)), !.
module_address(M, Address) :-
	server_address(Server),
        ( remote_call(Server,module_address(M,Address)) 
	-> asserta_fact(address_db(M, Address))
	 ; throw(unable_to_connect(Server,module_address(M)))
	).

server_address(Address):-
	current_fact(server(Address,_Pid)), !.
server_address(Address):-
	common_url(URL),
	( fetch_url(URL,[],Response), ! ; Response = [] ),
	( member(content(String),Response), ! ; String = Response ),
	( append(String0,[_],String), ! ; String0 = String ),
	( read_from_string_atmvars(String0,server(Address,Pid))
	-> asserta_fact(server(Address,Pid))
	 ; name(Address,String0),
	   throw(unable_to_connect(Address,webserver))
	).
