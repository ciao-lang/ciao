:- package(debug).

:- use_module(library(debugger/embedded_rt)).

:- new_declaration(spy/1).
:- op(900, fx, [(spy)]).

:- load_compilation_module(library(debugger/embedded_tr)).
:- add_clause_trans(embedded_tr:srcdbg_expand/4, 8510). % TODO: Right priority?
:- add_sentence_trans(embedded_tr:srcdbg_expand_decl/3, 8510). % TODO: Right priority?

% TODO: Do not use initialization here
:- initialization(debugger_init).

debugger_init :-
	debug,
	this_module(M),
	debug_module(M).
