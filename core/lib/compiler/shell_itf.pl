:- module(_, [], [assertions, nortchecks, datafacts]).

:- doc(title, "Compiler frontend for toplevels").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides a reduced version of @lib{c_itf}
   for interactive toplevels.").

% TODO: reuse more code from c_itf.pl

% ---------------------------------------------------------------------------
% TODO: merge with core_OC/compiler/read_source.pl

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(read), [read_term/3]).
:- use_module(engine(messages_basic), [message_lns/4]).

read_query(Stream, Query, Dict, VarNames) :-
	repeat,
	Opts = [dictionary(Dict), variable_names(VarNames)],
	catch(read_term(Stream, Query, Opts),
	      error(syntax_error([L0, L1, Msg, ErrorLoc]), _),
	      handle_syntax_error(L0, L1, Msg, ErrorLoc)),
	!,
	Query \== end_of_file.

% TODO: share duplicated code with c_itf.pl
read_sentence(Stream, Sentence) :-
	repeat,
	catch(do_read_sentence(Stream, Sentence),
	      error(syntax_error([L0, L1, Msg, ErrorLoc]), _),
	      handle_syntax_error(L0, L1, Msg, ErrorLoc)).

do_read_sentence(Stream, Sentence) :-
	Opts = [ variable_names(VarNames),
		 singletons(Singletons),
		 lines(Ln0, Ln1) ],
	read_term(Stream, Data, Opts),
	( Data = end_of_file ->
	    Sentence = end_of_file(Ln0, Ln1)
	; Sentence = sentence(Data, VarNames, Singletons, Ln0, Ln1)
	),
	!.

handle_syntax_error(L0, L1, Msg, ErrorLoc) :-
	display(user_error, '{SYNTAX '),
	message_lns(error, L0, L1, [[](Msg), '\n', [](ErrorLoc), '\n}']),
	fail.

% ---------------------------------------------------------------------------
% Query reader (including term and sentence expansion)

:- export(get_query/4).
get_query(Stream, Query, Dict, VarNames) :-
	read_query(Stream, Query, Dict, VarNames).

% ---------------------------------------------------------------------------
% Including files (source or packages) in shell

% TODO: move to compiler/dynsyntax.pl? (dynamically change syntax); use add_module instead of shell_module?

:- use_module(engine(internals), ['$open'/3]).
:- use_module(library(lists), [member/2]).

:- data new_decl/2.
:- data package/2.

% TODO: Share the duplicated logic with compiler/c_itf.pl
do_include(Type, File, ShMod) :-
	absolute_file_name(File, '_opt', '.pl', '.', SourceFile, SourceBase, _),
	%
	now_doing_include(Type, SourceFile),
	'$open'(SourceFile, r, Stream),
	( read_sentence(Stream, Sentence) -> true ; fail ), % (once)
	check_include_decl(Type, SourceBase, Sentence, Rest),
	%
	( member(Sentence2, Rest) 
	; read_sentence(Stream, Sentence2) 
	),
	( Sentence2 = end_of_file(_,_), ! % end loop
	; process_sentence(Sentence2, ShMod),
	  fail % loop
	),
	%
	close(Stream),
	end_doing.

process_sentence(Sentence2, ShMod) :-
	Sentence2 = sentence(RawData, VarNames, _, L0, L1), % TODO: missing singleton check
	expand_term_to_list(RawData, ShMod, VarNames, Data0),
	process_expanded_data_list(Data0, ShMod, L0, L1).

% (partially duplicated in c_itf.pl)
expand_term_to_list(Data0, M, VNs, Data) :-
	expand_term(Data0, M, VNs, Data1),
	expand_list_tail(Data1, Data).

% (duplicated in c_itf.pl)
expand_list_tail(Data1, Data) :-
	( var(Data1) ->
	    Data = []
	; Data1 = [X|Data3] ->
	    Data = [X|Data2],
	    expand_list_tail(Data3, Data2)
	; Data1 = [] ->
	    Data = Data1
	; Data = [Data1]
	).

process_expanded_data_list(Data0, ShMod, Ln0, Ln1) :-
	( member(Data, Data0),
	  process_expanded_data(Data, ShMod, Ln0, Ln1),
	  fail
	; true
	).

now_doing_include(source, SourceFile) :- now_doing(['Including ', SourceFile]).
now_doing_include(package, SourceFile) :- now_doing(['Using package ', SourceFile]).

now_doing(M) :- message(inform, ['{'| M]).

end_doing :- message(inform, '}').

:- use_module(library(compiler/c_itf), [module_from_base/2]).

% Check that packages contains the right declarations. Nothing is
% required for included source.
check_include_decl(source, _, Sentence, [Sentence]).
check_include_decl(package, Base, Sentence, Sentences) :-
	( Sentence = sentence(Data, _, _, Ln0, Ln1),
	  Data = (:- package(M)) ->
	    Sentences = [],
	    module_from_base(Base, SM),
	    ( SM = M -> % Allow vars in package declarations
	        true
	    ; shell_error_at(bad_module_name(package, M), Ln0, Ln1)
	    )
	; % Do not consume the sentence, it is not a valid package declaration
          Sentences = [Sentence],
	  sentence_lines(Sentence, Ln0, Ln1),
	  shell_error_at(bad_module_decl_kind(package, unknown), Ln0, Ln1)
	).

sentence_lines(sentence(_,_,_,Ln0,Ln1), Ln0, Ln1).
sentence_lines(end_of_file(Ln0,Ln1), Ln0, Ln1).
	
% ---------------------------------------------------------------------------

:- doc(subsection, "Translation hooks (term and sentence)").

add_trans_hook(M, sentence, P, Prior) :- !, translation:add_sentence_trans_and_init(M, P, Prior).
add_trans_hook(M, term, P, Prior) :- !, translation:add_term_trans(M, P, Prior).

:- use_module(library(compiler/translation),
	    [expand_term/4, add_sentence_trans_and_init/3, add_term_trans/3]).

:- export(shell_expand/4).
shell_expand(V, _ShMod, _, Query) :- var(V), !, Query = call(V).
shell_expand((:- Decl), ShMod, VarNames, Query) :- !,
	expand_term((:- Decl), ShMod, VarNames, Query),
	(Query = true -> true ; true). % unify Query if a var
shell_expand(RawQuery, ShMod, VarNames, Query) :-
	expand_term(('SHELL':-RawQuery), ShMod, VarNames, Expansion),
	( Expansion = ('SHELL':-Query), !
	; Query = fail,
	  message(error, ['unexpected answer from expansion: ', Expansion])
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Translation hooks (goal)").

:- use_module(library(goal_trans), [add_goal_trans/3]).

do_add_goal_trans(P, Prior, ShMod) :-
	( goal_trans:add_goal_trans(ShMod, P, Prior) ->
	    true
	; shell_error(declaration_failed(add_goal_trans(P, Prior)))
	).

% ---------------------------------------------------------------------------

:- multifile '$shell_call_in_mod'/2.

process_expanded_data((?- Goal), ShMod, _, _) :- !,
	'$shell_call_in_mod'(ShMod, Goal), !. % TODO: Deprecate (or make it optional with a flag)
process_expanded_data((:- Decl), ShMod, L0, L1) :- !,
	( current_fact(new_decl(Decl, ShMod)) ->
	    true
	; is_known_decl(Decl) ->
	    process_decl(Decl, ShMod)
	; bad_shell_directive(Decl, L0, L1)
	).
process_expanded_data(Clause, ShMod, _, _) :-
	'$shell_call_in_mod'(ShMod, assertz(Clause)).

bad_shell_directive(Decl, L0, L1) :-
	functor(Decl, F, A),
	shell_error_at(directive_not_allowed_in_toplevel(F, A), L0, L1).

is_known_decl(use_module(_)).
is_known_decl(use_module(_, _)).
is_known_decl(ensure_loaded(_)).
is_known_decl(include(_)).
is_known_decl(use_package(_)).
is_known_decl(set_prolog_flag(_, _)).
is_known_decl(push_prolog_flag(_, _)).
is_known_decl(pop_prolog_flag(_)).
is_known_decl(op(_, _, _)).
is_known_decl(new_declaration(_, _)).
is_known_decl(new_declaration(_)).
is_known_decl(load_compilation_module(_)).
is_known_decl(add_sentence_trans(_, _)).
is_known_decl(add_term_trans(_, _)).
is_known_decl(add_goal_trans(_, _)).
is_known_decl(multifile(_)).

:- export(process_decl/2).
process_decl(use_module(A), ShMod) :- use_module(A, all, ShMod).
process_decl(use_module(A, B), ShMod) :- use_module(A, B, ShMod).
process_decl(ensure_loaded(A), ShMod) :- do_ensure_loaded(A, ShMod).
process_decl(include(A), ShMod) :- do_include(source, A, ShMod).
process_decl(use_package(A), ShMod) :- do_use_package(A, ShMod).
process_decl(set_prolog_flag(A, B), ShMod) :- do_set_pl_flag(A, B, ShMod).
process_decl(push_prolog_flag(A, B), ShMod) :- do_push_pl_flag(A, B, ShMod).
process_decl(pop_prolog_flag(A), ShMod) :- do_pop_pl_flag(A, ShMod).
process_decl(op(A, B, C), ShMod) :- do_op(A, B, C, ShMod).
process_decl(new_declaration(A, B), ShMod) :- do_new_decl(A, B, ShMod).
process_decl(new_declaration(A), ShMod) :- do_new_decl(A, off, ShMod).
process_decl(load_compilation_module(A), ShMod) :- do_load_compilation_module(A, ShMod).
process_decl(add_sentence_trans(A, B), ShMod) :- do_add_sentence_trans(A, B, ShMod).
process_decl(add_term_trans(A, B), ShMod) :- do_add_term_trans(A, B, ShMod).
process_decl(add_goal_trans(A, B), ShMod) :- do_add_goal_trans(A, B, ShMod).
process_decl(multifile(A), ShMod) :- '$shell_call_in_mod'(ShMod, multifile(A)).

% ---------------------------------------------------------------------------

% Dynamic module loading
:- use_module(library(compiler), [use_module/3, ensure_loaded/2]).

% ---------------------------------------------------------------------------
:- doc(section, "'ensure_loaded' declaration").

do_ensure_loaded([], _) :- !.
do_ensure_loaded([File|Files], M) :- !,
	do_ensure_loaded__2(File, M),
	do_ensure_loaded(Files, M).
do_ensure_loaded(File, M) :-
	do_ensure_loaded__2(File, M).

:- if(defined(optim_comp)).
do_ensure_loaded__2(File, Module) :- dynload:ensure_loaded(File, Module).
:- else.
do_ensure_loaded__2(File, Module) :- compiler:ensure_loaded(File, Module).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "'use_package' declaration").

do_use_package([], _ShMod) :- !.
do_use_package([F|Fs], ShMod) :- !,
	do_use_package(F, ShMod),
	do_use_package(Fs, ShMod).
do_use_package(F, ShMod) :-
	package_file(F, P), !,
	( current_fact(package(ShMod,P)) ->
	    shell_error(package_already_loaded_in_toplevel(F))
	; assertz_fact(package(ShMod,P)),
	  do_include(package, P, ShMod)
	).
do_use_package(F, _ShMod) :-
        shell_error(bad_package_file(F)).

package_file(F, P) :-
	( atom(F) -> P = library(F)
	; functor(F,_,1) -> P = F
	).

% ---------------------------------------------------------------------------
:- doc(section, "new_declaration declaration").

do_new_decl(S, _ITF, ShMod) :-
	( S = F/A, functor(D, F, A) ->
	    ( current_fact(new_decl(D, ShMod)) -> true
	    ; asserta_fact(new_decl(D, ShMod))
	    )
        ; shell_error(badly_formed(new_declaration, S))
	).

% ---------------------------------------------------------------------------
:- doc(section, "'load_compilation_module' declaration").

:- use_module(engine(hiord_rt), [this_module/1]).

do_load_compilation_module(File, ShM) :-
	% TODO: only once?
	this_module(M),
	use_module(File, all, M), % Here for sentence/term expansions
	use_module(File, all, ShM). % In toplevel_scope for goal expansions

% ---------------------------------------------------------------------------
:- doc(section, "op/3 declaration").
% TODO: Not module-local!

:- use_module(library(operators), [op/3]).

do_op(A, B, C, _ShMod) :-
	op(A, B, C).

% ---------------------------------------------------------------------------
:- doc(section, "prolog_flag declarations").
% TODO: Not module-local!

:- use_module(engine(runtime_control), [
	set_prolog_flag/2, push_prolog_flag/2, pop_prolog_flag/1]).

do_set_pl_flag(A, B, _ShMod) :-
	set_prolog_flag(A, B).

do_push_pl_flag(A, B, _ShMod) :-
	push_prolog_flag(A, B).

do_pop_pl_flag(A, _ShMod) :-
	pop_prolog_flag(A).

% ---------------------------------------------------------------------------
:- doc(section, "sentence and term translations declarations").

do_add_sentence_trans(P, Prior, ShMod) :-
	( add_trans_hook(ShMod, sentence, P, Prior) ->
	    true
	; shell_error(declaration_failed(add_sentence_trans(P, Prior)))
	).

do_add_term_trans(P, Prior, ShMod) :-
	( add_trans_hook(ShMod, term, P, Prior) ->
	    true
	; shell_error(declaration_failed(add_term_trans(P, Prior)))
	).

% ---------------------------------------------------------------------------
:- doc(section, "Error messages").

:- use_module(engine(messages_basic), [message/2, message_lns/4]).

shell_error(bad_package_file(F)) :- !,
	message(error, ['Bad package file ', ~~(F)]).
shell_error(badly_formed(new_decl(_), S)) :- !,
	message(error, ['Bad predicate specifier ', S,
		        'in new_declaration directive']).
shell_error(declaration_failed(Decl)) :- !,
	message(warning, [Decl, ' - declaration failed']).
shell_error(package_already_loaded_in_toplevel(F)) :- !,
	message(note, ['Package ', ~~(F), ' already loaded in shell']).

shell_error_at(bad_module_name(package, M), Ln0, Ln1) :- !,
	message_lns(error, Ln0, Ln1, ['Bad package ',M,' in package declaration']).
shell_error_at(bad_module_decl_kind(package, unknown), Ln0, Ln1) :- !,
	message_lns(warning, Ln0, Ln1,
	             ['Source used as package without package declaration']).
shell_error_at(directive_not_allowed_in_toplevel(F, A), Ln0, Ln1) :- !,
	message_lns(error, Ln0, Ln1,
	    [~~(F/A), ' directive not allowed in shell']).

