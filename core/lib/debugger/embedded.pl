:- package(embedded).
% Internal package for the embedded debugger.
% Use one of `debug`, `trace`, or `nodebug` packages instead.

:- use_module(library(debugger/embedded_rt)).
:- use_module(engine(hiord_rt), [this_module/1]).

:- new_declaration(spy/1).
:- op(900, fx, [(spy)]).

:- load_compilation_module(library(debugger/embedded_tr)).
:- add_clause_trans(embedded_tr:srcdbg_expand/4, 8510). % TODO: Right priority?
:- add_sentence_trans(embedded_tr:srcdbg_expand_decl/3, 8510). % TODO: Right priority?
