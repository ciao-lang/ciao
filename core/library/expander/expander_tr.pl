:- module(expander_tr, [expand_sentence/4, expand_clause/4, expand_goal/3], [assertions]).

:- doc(author, "The Ciao Development Team").
:- doc(author, "David Trallero").
:- doc(author, "Edison Mera").

:- doc(module, "Generates a file with the result of applying the
	clause and sentence expansions in a module.").

:- doc(bug, "Goal and term expansions are not applied yet at the
   time this expansion intercepts the compiler results. It would be
   easier to base this package on the same dump code shared by static
   analysis tools.").

% ---------------------------------------------------------------------------

:- use_module(engine(data_facts)).
:- use_module(library(aggregates)).

:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(write)).
:- use_module(library(varnames/pretty_names)).

:- if(defined(optim_comp)).
% TODO: fix
defines_module(M,M).
package(_,_) :- fail.
exports_pred(_,_,_) :- fail.
:- else.
:- use_module(library(compiler/c_itf),
	    [defines_module/2, package/2, exports_pred/3]).
:- endif.

% ---------------------------------------------------------------------------

:- use_module(engine(runtime_control), [push_prolog_flag/2, pop_prolog_flag/1]). % TODO: do in a better way

:- data output_s/2.

pretty_write(OutS, Term) :-
	push_prolog_flag(write_strings, on),
	write_term(OutS, Term, [numbervars(true), quoted(true)]),
	pop_prolog_flag(write_strings).

expand_sentence(0, 0, Module, Dict) :-
	defines_module(Base, Module),
	atom_concat(Base, '_co.pl', F),
	open(F, write, OutS),
	retractall_fact(output_s(Module, _)),
	asserta_fact(output_s(Module, OutS)),
	findall(Package, ( package(Base, P),
		(P = library(Package) -> true ; P = Package) ), Packages),
	atom_concat(Module, '_co', Module_co),
	( findall(Func/Arity, exports_pred(Base, Func, Arity), Exports),
	    Exports \= [all/ all] -> true ; true ),
	(
	    ModuleDecl = (:- module(Module_co, Exports, [])),
	    pretty_names(Dict, ModuleDecl, ModuleDeclN),
	    pretty_write(OutS, ModuleDeclN),
	    write(OutS, '.\n'),
	    pretty_write(OutS, '$applied_packages'(Packages)),
	    write(OutS, '.\n'),
	    !,
	    fail
	;
	    true
	).
expand_sentence((:- S), (:- S), M, Dict) :-
	write_sentence((:- S), M, Dict).
expand_sentence(end_of_file, end_of_file, Module, _) :-
	% (Do not close it yet, do on clause/goal translation)
	output_s(Module, OutS),
	write(OutS, '\n').
% expand_sentence(S, S, _, _).

write_sentence(S, M, Dict) :-
	output_s(M, OutS),
	(
	    pretty_names(Dict, S, SN),
	    pretty_write(OutS, SN),
	    write(OutS, '.\n'),
	    !,
	    fail
	;
	    true
	).

expand_clause(clause(0, 0), clause(0, 0), Module, _) :- % TODO: missing cut
	defines_module(Base, Module),
	atom_concat(Base, '_co.pl', F),
	display('{'), message(note, [Module, ' expanded in ', F, '}']),
	( output_s(Module, OutS0) ->
	    OutS = OutS0
	; open(F, append, OutS),
	  asserta_fact(output_s(Module, OutS))
	).
expand_clause(clause(Head, Body), clause(Head, Body), Module, Dict) :-
	output_s(Module, OutS),
	(
	    pretty_names(Dict, Head-Body, HeadN-BodyN),
	    pretty_write(OutS, HeadN),
	    write_body_ini(BodyN, OutS, 8),
	    write(OutS, '.\n'),
	    !,
	    fail
	;
	    true
	).

% (called when clause/goal translations finish)
expand_goal(end_of_file, _, M) :-
	( retract_fact(output_s(M, OutS)) ->
	    close(OutS)
	; true
	).

write_body_ini(true, _,  _) :- !.
write_body_ini(Body, OutS, L) :-
	write(OutS, ' :-\n'),
	write_body(Body, OutS, L).

write_body_or(A, B, OutS, L0) :-
	L is L0 + 4,
	tab(OutS, L0), write(OutS, '(\n'),
	write_body(A, OutS, L), nl(OutS),
	tab(OutS, L0), write(OutS, ';\n'),
	write_body(B, OutS, L), nl(OutS),
	tab(OutS, L0), write(OutS, ')').

write_body_implies(A, B, OutS, L) :-
	write_body(A, OutS, L), write(OutS, ' ->\n'),
	write_body(B, OutS, L).

write_body_and(A, B, OutS, L) :-
	write_body_par(A, OutS, L), write(OutS, ',\n'),
	write_body(B, OutS, L).

write_body_sep(';'(A, B), OutS, L) :-
	write_body_or(A, B, OutS, L).
write_body_sep('->'(A, B), OutS, L) :-
	write_body_implies(A, B, OutS, L).
write_body_sep(','(A, B), OutS, L) :-
	write_body_and(A, B, OutS, L).

write_body_sep_par(';'(A, B), OutS, L) :-
	write_body_or(A, B, OutS, L).
write_body_sep_par('->'(A, B), OutS, L) :-
	write_body_implies(A, B, OutS, L).
write_body_sep_par(','(A, B), OutS, L0) :-
	L is L0 + 4,
	tab(OutS, L0), write(OutS, '(\n'),
	write_body_and(A, B, OutS, L), nl(OutS),
	tab(OutS, L0), write(OutS, ')').

write_body_par(B, OutS, L0) :-
	write_body_sep_par(B, OutS, L0),
	!.
write_body_par(B, OutS, L0) :-
	tab(OutS, L0), pretty_write(OutS, B).

write_body(B, OutS, L0) :-
	write_body_sep(B, OutS, L0),
	!.
write_body(B, OutS, L0) :-
	tab(OutS, L0), pretty_write(OutS, B).
