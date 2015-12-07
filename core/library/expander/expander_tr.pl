:- module(expander_tr, [expand_sentence/4, expand_clause/4], [assertions]).

:- doc(author, "The CLIP Group").
:- doc(author, "Improved by Edison Mera.").

:- doc(module, "Generates a file with the result of applying the
	clause and sentence expansions in a module.").

:- doc(bug, "Goal and term expansions are not applied yet at the
   time this expansion intercepts the compiler results. It would be
   easier to base this package in the same dump code shared by static
   analysis tools.").

% ---------------------------------------------------------------------------

:- use_module(library(aggregates)).
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

:- data io_output/1.

pretty_write(IO, Term) :-
	push_prolog_flag(write_strings, on),
	write_term(IO, Term, [numbervars(true), quoted(true)]),
	pop_prolog_flag(write_strings).

expand_sentence(0, 0, Module, Dict) :-
	defines_module(Base, Module),
	atom_concat(Base, '_co.pl', F),
	open(F, write, IO),
	asserta_fact(io_output(IO)),
	findall(Package, ( package(Base, P),
		(P = library(Package) -> true ; P = Package) ), Packages),
	atom_concat(Module, '_co', Module_co),
	( findall(Func/Arity, exports_pred(Base, Func, Arity), Exports),
	    Exports \= [all/ all] -> true ; true ),
	(
	    ModuleDecl = (:- module(Module_co, Exports, [])),
	    pretty_names(Dict, ModuleDecl, ModuleDeclN),
	    pretty_write(IO, ModuleDeclN),
	    write(IO, '.\n'),
	    pretty_write(IO, '$applied_packages'(Packages)),
	    write(IO, '.\n'),
	    !,
	    fail
	;
	    true
	).
expand_sentence((:- S), (:- S), _, Dict) :-
	write_sentence((:- S), Dict).
expand_sentence(end_of_file, end_of_file, _, _) :-
	io_output(IO),
	write(IO, '\n').
% expand_sentence(S, S, _, _).

write_sentence(S, Dict) :-
	io_output(IO),
	(
	    pretty_names(Dict, S, SN),
	    pretty_write(IO, SN),
	    write(IO, '.\n'),
	    !,
	    fail
	;
	    true
	).

expand_clause(clause(0, 0), clause(0, 0), Module, _) :-
	defines_module(Base, Module),
	atom_concat(Base, '_co.pl', F),
	display(user_error, '{'), note([Module, ' expanded in ', F, '}']),
	open(F, append, IO),
	asserta_fact(io_output(IO)).
expand_clause(clause(Head, Body), clause(Head, Body), _, Dict) :-
	io_output(IO),
	(
	    pretty_names(Dict, Head-Body, HeadN-BodyN),
	    pretty_write(IO, HeadN),
	    write_body_ini(BodyN, IO, 8),
	    write(IO, '.\n'),
	    !,
	    fail
	;
	    true
	).
expand_clause(A, A, _, _) :-
	io_output(IO),
	close(IO),
	display([A, 'user_output']).

write_body_ini(true, _,  _) :- !.
write_body_ini(Body, IO, L) :-
	write(IO, ' :-\n'),
	write_body(Body, IO, L).

write_body_or(A, B, IO, L0) :-
	L is L0 + 4,
	tab(IO, L0), write(IO, '(\n'),
	write_body(A, IO, L), nl(IO),
	tab(IO, L0), write(IO, ';\n'),
	write_body(B, IO, L), nl(IO),
	tab(IO, L0), write(IO, ')').

write_body_implies(A, B, IO, L) :-
	write_body(A, IO, L), write(IO, ' ->\n'),
	write_body(B, IO, L).

write_body_and(A, B, IO, L) :-
	write_body_par(A, IO, L), write(IO, ',\n'),
	write_body(B, IO, L).

write_body_sep(';'(A, B), IO, L) :-
	write_body_or(A, B, IO, L).
write_body_sep('->'(A, B), IO, L) :-
	write_body_implies(A, B, IO, L).
write_body_sep(','(A, B), IO, L) :-
	write_body_and(A, B, IO, L).

write_body_sep_par(';'(A, B), IO, L) :-
	write_body_or(A, B, IO, L).
write_body_sep_par('->'(A, B), IO, L) :-
	write_body_implies(A, B, IO, L).
write_body_sep_par(','(A, B), IO, L0) :-
	L is L0 + 4,
	tab(IO, L0), write(IO, '(\n'),
	write_body_and(A, B, IO, L), nl(IO),
	tab(IO, L0), write(IO, ')').

write_body_par(B, IO, L0) :-
	write_body_sep_par(B, IO, L0),
	!.
write_body_par(B, IO, L0) :-
	tab(IO, L0), pretty_write(IO, B).

write_body(B, IO, L0) :-
	write_body_sep(B, IO, L0),
	!.
write_body(B, IO, L0) :-
	tab(IO, L0), pretty_write(IO, B).
