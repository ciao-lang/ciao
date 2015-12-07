:- module(webbased_publish, [], []).

:- use_module(library(actmods/actmodrt)).
:- use_module(library(actmods/webbased_common)).
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(library(pathnames)).
:- use_module(library(lists), [append/3]).
:- use_module(library(pillow/http)).
:- use_module(library(system)).

:- multifile save_addr_actmod/1.

save_addr_actmod([Address]) :- !,
	current_executable(ExePath),
        path_basename(ExePath, ExeFile),
        ( path_splitext(ExeFile, Mod, _), ! ; Mod = ExeFile ),
        get_pid(Pid),
	common_url(URL),
	( fetch_url(URL,[],Response), ! ; Response = [] ),
	( member(content(String),Response), ! ; String = Response ),
	( append(String0,[_],String), ! ; String0 = String ),
	( read_from_string_atmvars(String0,server(Server,_)), ! ; name(Server,String0) ),
        ( remote_call(Server,my_module_address(Mod,Address,Pid))
	; throw(unable_to_connect(Server,module_address(Mod,Address,Pid)))
	).
save_addr_actmod(_) :-
        inform_user(['Bad number of arguments: either none or port number']),
 	halt(1).
