:- package(trace).
:- use_module(library(debugger/embedded_rt)).

% TODO: Not very nice, avoid this use of initialization!
:- initialization((this_module(M), debug_module(M), trace)).
 
:- new_declaration(spy/1).
:- op(900, fx, [(spy)]).

:- load_compilation_module(library(debugger/embedded_tr)).
:- add_clause_trans(embedded_tr:srcdbg_expand/4, 8510). % TODO: Right priority?
:- add_sentence_trans(embedded_tr:srcdbg_expand_decl/3, 8510). % TODO: Right priority?
