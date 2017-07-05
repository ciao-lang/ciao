:- module(webbased_publish, [], []).

:- use_module(library(actmods/actmod_rt)).
:- use_module(library(actmods/webbased_common)).
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(pathnames)).
:- use_module(library(lists), [append/3]).
:- use_module(library(pillow/http)).
:- use_module(library(system)).

:- include(library(actmods/actmod_hooks)).

'actmod.save_addr'([Address]) :- !,
	current_executable(ExePath),
        path_basename(ExePath, ExeFile),
        ( path_splitext(ExeFile, Mod, _), ! ; Mod = ExeFile ),
        get_pid(Pid),
	common_url(URL),
	( fetch_url(URL,[],Response), ! ; Response = [] ),
	( member(content(String),Response), ! ; String = Response ),
	( append(String0,[_],String), ! ; String0 = String ),
	( read_from_string_atmvars(String0,server(Server,_)), ! ; name(Server,String0) ),
        ( 'actmod.call'(Server,'actmod.tell_address'(Mod,Address,Pid))
	; throw(unable_to_connect(Server,'actmod.address'(Mod,Address,Pid)))
	).
'actmod.save_addr'(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).
