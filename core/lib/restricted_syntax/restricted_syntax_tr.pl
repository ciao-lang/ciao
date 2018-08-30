:- module(restricted_syntax_tr, [tr_sentence/3], []).

:- use_module(engine(data_facts)).
:- use_module(library(messages), [error_message/3]).
:- use_module(library(compiler/c_itf_internal), [location/3, module_error/0]).

tr_sentence((:- Decl), [], _Mod) :- forbidden_decl(Decl), !,
	set_fact(module_error),
	functor(Decl, F, A),
	current_location(Loc),
	% TODO: hide "from module" in error_message? (it can be confusing for users)
	error_message(Loc, "declaration not allowed in restricted mode (using module/2): ~w", [F/A]).

current_location(Loc) :-
	location(S, L0, L1),
	Loc = loc(S, L0, L1),
	!.
current_location(_).

% Just a few
forbidden_decl(use_package(_)).
forbidden_decl(load_compilation_module(_)).

