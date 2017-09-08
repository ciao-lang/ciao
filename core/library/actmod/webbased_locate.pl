
:- module(webbased_locate,['actmod.address'/2],[]).

:- use_module(library(actmod/actmod_rt)).
:- use_module(library(actmod/webbased_common)).
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(pillow/http)).

:- data address_db/2.
:- data server/2.

'actmod.address'(M, Address) :-
	current_fact(address_db(M, Address)), !.
'actmod.address'(M, Address) :-
	server_address(Server),
        ( 'actmod.call'(Server,'actmod.address'(M,Address)) 
	-> asserta_fact(address_db(M, Address))
	 ; throw(unable_to_connect(Server,'actmod.address'(M)))
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
