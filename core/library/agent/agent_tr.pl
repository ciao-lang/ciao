:- module(agent_tr,[agent_s/3],[]).

:- use_module(library(compiler/c_itf), [defines_module/2, exports/5]).
:- use_module(library(system),          [mktemp/2]).

:- data tmp_file/1.

agent_s( 0, 0, _):-
    retractall_fact(tmp_file(_)).
agent_s( (:- protocol(P)), [], _):-
    current_fact(tmp_file(_)), !,
    warning(['protocol ',P,' ignored: there is one already declared']).
agent_s( (:- protocol(P)), [(:- use_module(library(Pub))),
	                    (:- ensure_loaded(User))],
         Mod):-
    atom_concat(P,'_publish',Pub),
    atom_concat(P,'_locate',Loc),
    write_user_file(Mod,Loc,User),
    asserta_fact(tmp_file(User)).
agent_s( end_of_file, end_of_file, _):-
    delete_temp.

write_user_file(Mod,Loc,File):-
    mktemp('tmpciaoXXXXXX', File),
    open(File,write,S),
    displayq(S,(:- use_module(library(Loc),[module_address/2]))),
    display(S,'.'), nl(S),
    displayq(S,('$agent$address'(Agent,Add):- module_address(Agent,Add))),
    display(S,'.'), nl(S),
    display(S,(:- meta_predicate(exe(?,fact)))),
    display(S,'.'), nl(S),
    display(about), nl,
    defines_module(Base,Mod),
    display(togo), nl,
    actmod_serves(S,Base),
    display(gone), nl,
    close(S).

actmod_serves(S,Base) :-
    exports(Base, F, A, _, _),
    functor(Pred, F, A),
    display(Pred), nl,
    displayq(S,(exe(Pred,Pred))),
    display(S,'.'), nl(S),
    fail.
actmod_serves(_,_Base).

%% delete_temp :-
%%     retract_fact(tmp_file(File)),
%%     delete_file(File),
%%     fail.
delete_temp.
