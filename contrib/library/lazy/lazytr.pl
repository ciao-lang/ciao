:- module(lazytr, [lazy_sentence_translation/3], []).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(aggregates)).

% ---------------------------------------------------------------------------

:- data lazy/5.
:- data newp/3.
:- data done/2.

clean_db(Module) :-
        retractall_fact(lazy(Module, _, _, _, _)),
	retractall_fact(newp(Module, _, _)),
	retractall_fact(done(Module, _)).

make_newp(Module, Head, Body) :-
        asserta_fact(newp(Module, Head, Body)).

make_proc_lazy(Module, Name, Args, ArgRes) :-
	functor(Pred, Name, Args),
	atom_concat([Name, '_$$lazy$$'], LazyName),
	functor(NewHead,LazyName,Args),
	functor(Head, Name, Args),
	make_lazy(Module, Pred, Head, NewHead, ArgRes).

make_lazy(Module, Predicate, Freeze, New, ArgRes) :-
        asserta_fact(lazy(Module, Predicate, Freeze, New, ArgRes)).

% ---------------------------------------------------------------------------

lazy_sentence_translation(0, _, Module) :-
	!,
	clean_db(Module).
lazy_sentence_translation(end_of_file, List, Module) :- !,
	findall(
		   (Predicate :- Body),
		   newp(Module, Predicate, Body),
		   List1
	       ),
	reverse([end_of_file|List1], List),
	clean_db(Module).
lazy_sentence_translation((?- _), _, _) :- !,
	fail.
lazy_sentence_translation((:- lazy(Specification)), _, Mod) :- !,
	treat_lazy_decl(Mod, Specification).
lazy_sentence_translation((:- _), _, _) :- !,
	fail.
lazy_sentence_translation((Head :- Body), (NewHead :- NewBody), Module) :-
	!,
        lazy_sentence_translation_proc(Module, Head, Body, NewHead, NewBody).
lazy_sentence_translation(Head, (NewHead :- NewBody), Module) :-
        lazy_sentence_translation_proc(Module, Head, true, NewHead, NewBody).

treat_lazy_decl(Mod, Specification) :-
	( Specification = N/Args ->
	    make_proc_lazy(Mod, N, Args, Args)
	; Specification = -(N/Args,Res) ->
	    make_proc_lazy(Mod, N, Args, Res)
	; inform_user(['Invalid lazy specification: ', Specification])
	).

lazy_sentence_translation_proc(Module, Head, Body, NewHead, NewBody) :-
	lazy(Module, Head, Freeze, New, ArgRes),
	!,
	Head =.. [H|As],
%	arg(ArgRes, Head, Value),
 	( % ground
	  % TODO: disabled, this generated incorrect code (JFMC)
%	  ground(Value) ->
%	      New  =.. [He|_],
%	      New2 =.. [He|As],
%	      make_newp(Module, New2, Body)
%	; 
	  % write the wrapper predicate (calls freeze)
	  % Note: this is done once per predicate
	  ( done(Module, Freeze) ->
	      true
	  ; functor(New,LazyName,_),
	    Freeze =.. [_|Arguments],
	    FreezePart =.. [LazyName|Arguments],
	    arg(ArgRes, Freeze, Result),
	    NewHead = Freeze,
	    NewBody = (freeze(Result, (Module:FreezePart))),
	    asserta_fact(done(Module, Freeze))
	  ),
	  % write the actual clause
	  atom_concat([H, '_$$lazy$$'], LH),
	  LHead =.. [LH|As],
	  make_newp(Module, LHead, Body)
	).

