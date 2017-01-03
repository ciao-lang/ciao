%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Originally written by:
%%   * Emilio Jesús Gallego Arias
%%
%% Modified by:
%%   * Remy Haemmerle
%%   * Jose F. Morales
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for \more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
%% ---------------------------------------------------------------------------

:- module(indexicals_tr, [translate/3], [assertions]).

:- doc(title, "Indexical Compiler").

:- use_package(library(clpfd/clpfd_options)).

:- include(library(clpfd/fd_ops)).

:- use_module(library(lists), [append/3, reverse/2, select/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms)).
:- use_module(library(formulae), [list_to_conj/2]).

:- use_module(library(clpfd/fd_utils), [member_ro/2]).

% TODO: Support compile-time range calculation?
% :- use_module(clpfd_range, 
% 	[fd_range_new/3,
% 	 fd_range_intersect
% 	]).

:- data c_number/2.
:- data c_pred/3.
:- data c_flag/2.

%% idx_translate predicate. Use with add_sentence_trans when in need
%% to translate indexicals contraints.

%% The general translation scheme is to compile an indexical to a new
%% predicate, which will be called whenever it should be
%% re-executed. Aside from this consideration, the translator has to
%% do two other tasks:
%% - Install the constraint callback in the update chains.
%% - Call the constraint for first time.

%% Initialize the handlers.
translate(0, _, M) :-
	assertz_fact(c_number(0, M)).
%
translate(end_of_file, NewP, Module) :-
	% Insert all the new generated constraint predicates stored in c_pred.
	findall(
		   (Head :- Body),
		   c_pred(Module, Head, Body),
		   CPreds
	       ),
	CPreds = CPreds2,
	reverse([end_of_file|CPreds2], NewP),
	show_sentences(CPreds2),
	retract_fact(c_number(_, Module)),
	retractall_fact(c_pred(Module, _, _)),
	retractall_fact(c_flag(_, Module)).
%
translate((:- clpfd_flag(Flag)), [], Module) :- !, % TODO: generalize
	assertz_fact(c_flag(Flag, Module)).

translate((Head +: IdxBody), (Head :- NewBody), Module) :-
	process_idx_list(Head, IdxBody, ConstraintBody, Module),
	NewBody = (!, ConstraintBody),
	copy_term((Head :- NewBody), R),
	show_clause(R).

%% Process each indexical
process_idx_list(Head, (Clause, Rest), (NewC, (!), NewRest), M) :- !,
	process_idx_clause(Head, Clause, NewC, M),
	process_idx_list(Head, Rest, NewRest, M).
process_idx_list(Head, Clause, NewC, M) :-
	process_idx_clause(Head, Clause, NewC, M).

%% This is the main translation predicate. It firsts generates the
%% constraint code, and then returns the needed install code and the call.
process_idx_clause(Head, X in R, IdxCode, M) :-
	generate_c_predicate(Head, X in R, Pred, M, InstallInfo),
	kill_duplicates(InstallInfo, IF2),
 	install_codegen(IF2, M, Pred, InstallCode),
	gen_call_code(M:Pred, CallCode),
	IdxCode = (InstallCode, CallCode).

kill_duplicates([], []).
kill_duplicates([E|El], S) :- member_ro(E,El),!,
	kill_duplicates(El, S).
kill_duplicates([E|El], [E|S]) :-
	kill_duplicates(El, S).

gen_call_code(M:Pred, M:Pred).

install_codegen([], _, _, true).
install_codegen([I], M, Pred, R) :- !,
	install_codegen_(I, M, Pred, R).
install_codegen([I|IL], M, Pred, (R,RC)) :-
	install_codegen_(I, M, Pred, R),
	install_codegen(IL, M, Pred, RC).

install_codegen_(if(Var, Chain), M, Pred, R) :-
	goal_to_closure(M, Pred, Closure),
	R = fd_term:add_propag(Var, Chain, Closure).

generate_c_predicate(Head, Constraint, Pred, M, Install) :-
	c_number(N, M),
	atom_number(Num, N),
	atom_concat([cstr_,Num], PredName),
	inc_c_number(M),
	Head =.. [_|Args],
	Pred =.. [PredName|Args],
	!,
	idx_compile(Constraint, ConjCode, Install),
 	gen_precondition(Install, ConjCode, PreCode),
%       kill_duplicates(Install, InstallNop),
%	avoid_reexec(Constraint, ConjCode2, ConjCode3),
	assertz_fact(c_pred(M, Pred, PreCode)).


% Unimplemented optimization.
% avoid_reexec(X in P, ConjCode2, CC) :-
% 	CC = ( var(X) ->
% 	       ConjCode2
% 	     ;
% 	       write(avoiding(X in P)),nl
% 	     ).

gen_precondition(InstallInfo, Code, PreCode) :- !,
	select_vals(InstallInfo, Vals),
	(
	    Vals = [] ->
	    PreCode = Code
	;
	    gen_singleton_check(Vals, CheckCode),
	    PreCode = ((CheckCode) -> Code ; true)
	).

select_vals(InstallInfo, [check(A)|Vals]) :-
	select(if(A, val), InstallInfo, InstallInfo2),
	!,
	select_vals(InstallInfo2, Vals).
select_vals(_InstallInfo, []) :- !.

gen_singleton_check([check(A)], fd_term:is_singleton(A)).
gen_singleton_check([check(A)|Xs], (fd_term:is_singleton(A),Code)) :- !,
	gen_singleton_check(Xs, Code).

%% The main indexical compiler. It receives an indexical expression
%% and it return a tuple containing the code and the variable that
%% holds the results once the code is executed. (I guess this is the
%% scheme of a typical expression compiler)
idx_compile(X in Range, Code, Install) :-
	idx_compile_range(Range, (RCode, TellRange), Install),
	list_to_conj(RCode, RConjCode),
	Code = (RConjCode, (!), fd_term:tell_range(X, TellRange)).
%
%% Interval range:
idx_compile_range((TMin .. TMax), (Code, Res), Install) :-
	idx_compile_term(TMin, MayFail1, (CMin_, MinR), Install1),
	(
	    var(MayFail1) -> CMin = CMin_;
	    list_to_conj(CMin_, CMin__), 
	    CMin = [(CMin__ -> true; MinR = inf)]
	),
	idx_compile_term(TMax, MayFail2, (CMax_, MaxR), Install2),
	(
	    var(MayFail2) -> CMax = CMax_;
	    list_to_conj(CMax_, CMax__), 
	    CMax = [(CMax__ -> true; MaxR = sup)]
	),
	append(Install1, Install2, Install),
	append(CMin, CMax, RCode),
	append(RCode, [fd_range:new(MinR, MaxR, Res)], Code).
%
idx_compile_range(-(A), (Code, Res), Install) :-
	idx_compile_range(A, (ACode, ARes), Install),
	append(ACode, [fd_range:complement(ARes, Res)], Code).
%
idx_compile_range({T,TR}, (Code, Res), Install) :-
	idx_compile_range({TR}, (RCode, RRes), RInstall),
	idx_compile_term(T, exception, (TCode, TRes), TInstall),
	append(RInstall, TInstall, Install),
	append(RCode, TCode, PCode),
	append(PCode, [fd_range:new(TRes, TRes, MRes),
	               fd_range:union(RRes, MRes, Res)
		      ], Code).
%
idx_compile_range({T}, (Code, Res), Install) :-
	idx_compile_term(T, exception, (TCode, TRes), Install),
	append(TCode, [fd_range:new(TRes, TRes, Res)], Code).
%
idx_compile_range(R+T, (Code, Res), Install) :-
	idx_compile_range(R, (RCode, RRes), Install1),
	idx_compile_term(T, exception, (TCode, TRes), Install2),
	append(Install1, Install2, Install),
	append(RCode, TCode, MCode),
	append(MCode, [fd_range:range_add(RRes, TRes, Res)], Code).
%
idx_compile_range(R-T, (Code, Res), Install) :-
	idx_compile_range(R, (RCode, RRes), Install1),
	idx_compile_term(T, exception, (TCode, TRes),  Install2),
	append(Install1, Install2, Install),
	append(RCode, TCode, MCode),
	append(MCode, [fd_range:range_sub(RRes, TRes, Res)], Code).
%
idx_compile_range(R*T, (Code, Res), Install) :-
	idx_compile_range(R, (RCode, RRes), Install1),
	idx_compile_term(T, exception, (TCode, TRes), Install2),
	append(Install1, Install2, Install),
	append(RCode, TCode, MCode),
	append(MCode, [fd_range:range_mul(RRes, TRes, Res)], Code).

%% This is broken.
idx_compile_range(dom(A), (Code, Res), [if(A, dom)]) :-
	Code = [fd_term:range(A, Res)].

% A possible idea:
% idx_compile_term(min(A), ([indexicals_rt:min(A,B)], B)) :-
% 	\+var(A),
% 	!,
% 	report_error(A must be a var).

idx_compile_term(min(A), _, ([fd_term:min(A,B)], B), [if(A,min)]).
idx_compile_term(max(A), _, ([fd_term:max(A,B)], B), [if(A,max)]).
%
idx_compile_term(c(A),   _,  ([fd_range:bound_const(A,B)], B), []).
% Here we profit from the fact that the val chain will only be called
% when A is a singleton.
idx_compile_term(val(A), _, ([fd_term:integerize(A,B)], B), [if(A,val)]).
%
idx_compile_term(A + B, MayFail, (Code, Res), Install) :-
	idx_compile_term(A, MayFail, (ACode, ARes), Install1),
	idx_compile_term(B, MayFail, (BCode, BRes), Install2),
	append(Install1, Install2, Install),
	append(ACode, BCode, ABCode),
	append(ABCode, [fd_range:bound_add(ARes, BRes, Res)], Code).
%
idx_compile_term(A - B, MayFail, (Code, Res), Install) :-
	idx_compile_term(A, MayFail, (ACode, ARes), Install1),
	idx_compile_term(B, MayFail, (BCode, BRes), Install2),
	append(Install1, Install2, Install),
	append(ACode, BCode, ABCode),
	append(ABCode, [fd_range:bound_sub(ARes, BRes, Res)], Code).
%
idx_compile_term(A * B, MayFail, (Code, Res), Install) :-
	idx_compile_term(A, MayFail, (ACode, ARes), Install1),
	idx_compile_term(B, MayFail, (BCode, BRes), Install2),
	append(Install1, Install2, Install),
	append(ACode, BCode, ABCode),
	append(ABCode, [fd_range:bound_mul(ARes, BRes, Res)], Code).
%
idx_compile_term(A / B, MayFail, (Code, Res), Install) :-
	idx_compile_term(A, MayFail, (ACode, ARes), Install1),
	idx_compile_term(B, MayFail, (BCode, BRes), Install2),
 	append(Install1, Install2, Install),
 	append(ACode, BCode, ABCode),
	(
	    MayFail = true ->
	    append(ABCode, [fd_range:bound_div(ARes, BRes, Res)], Code)
	;
	    MayFail = execption ->
	    append(ABCode, [(fd_range:bound_div(ARes, BRes, Res) -> true, throw(evaluation_error(zero_divisor), -1))], Code)
	).
%% Compile time range execution.

%% Dead code ?
% idx_range_execute(R, R). % Empty ATM, this is meant for making R = 3 + 2 .. 90 +100 into R =(5,190,[]).

%% Dead code ?
% %% FIXME!
% install_constraint(X, RTree, InstallCode) :-
% 	copy_term(RTree, Temp),
% 	prettyvars(Temp),
% 	write(r_parse_tree(X, Temp)),
% 	nl,
% 	InstallCode = true.

inc_c_number(M) :-
	retract_fact(c_number(N, M)),
	N1 is N + 1,
	assertz_fact(c_number(N1, M)).

:- if(defined(debug_translation)).
% Code for debugging this translation
:- use_module(library(write)).
portray_clause(C):-
        write(C), nl.

show_clause(R) :-
	portray_clause(R), nl.

show_sentences(CPreds2) :-
        copy_term(CPreds2, CP),
        prettyvars(CP),
        portray_clause_list1(CP), nl.
 
portray_clause_list1([]).
portray_clause_list1([C|Cl]) :-
	portray_clause(C), nl,
	portray_clause_list1(Cl).
:- else.
show_clause(_).
show_sentences(_).
:- endif.

%% Regtype for indexical ranges.
%% r := r .. r
%%    |

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), ['$module_concat'/3]).

goal_to_closure(M, G, MG) :- c_flag(atom_based, M), !,
	MG = G. % Note: only for js_backend, do not qualify
goal_to_closure(M, G, MG) :-
	G =.. [N|As],
	rt_exp:'$module_concat'(N, M, MN),
	MG =.. [MN|As].
:- else.
:- use_module(engine(internals), [module_concat/3]).

% goal_to_glosure(M:G, MG) generates a closure MG from a M:G goal.
% Closures can be passed as terms and called with low overhead.
%
% Note: Currently, we just put the low-level identifier that
%       must be called with hiord_rt:'$meta_call'/1.
%       It skips the module system, so use with caution!
%       Nevertheless, using them gave a x2 speedup in this CLP(fd)
%       implementation.
%
% (Jose F. Morales)
goal_to_closure(M, G, MG) :-
	internals:module_concat(M, G, MG).
:- endif.

% TODO: (only for JS-backend, see idx.pl)
:- export(idx_translate_fix/3).
idx_translate_fix(_:G, G, _M).

