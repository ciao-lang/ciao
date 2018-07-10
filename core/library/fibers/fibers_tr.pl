:- module(fibers_tr, [], [assertions, dcg]).

% Translation module for fibers

% NOTES:
%   The first pass extends the module interface (see mexpand_extra)
%   and marks bodies for post-processing, the second pass finish
%   processing the bodies using the collected extended interface
%   information.

% TODO: document:
%   '$ct_mexp_of_type'/3
%   fiberSusp/2
%   fiberSuspSpawn/2

:- export(mod_error/2).
:- export(is_exported_nd/3).
:- include(library(fibers/mexpand_extra)).

% ---------------------------------------------------------------------------
% Database to store suspendable declarations (first phase)

:- data susp_pred/2.

cleanup_db(M) :-
	reset_mexpand_extra(M),
	retractall_fact(susp_pred(_, M)).

set_susp_pred(Head, M) :-
	( current_fact(susp_pred(Head, M)) -> true
	; assertz_fact(susp_pred(Head, M))
	).

% ---------------------------------------------------------------------------

% TODO: Use common translation rules, check later
mexp_of_type(goal, G, M, G2) :- !,
	( functor_expansion(G, M, -, NG),
	  current_static_fact(NG, fnct, prop(async)),
	  unstub(NG, EM:_) ->
	    mod_concat_head(EM, G, G2)
	; G2 = M:G % TODO: assume it is here
	).
% Like 'goal', error if it is not suspendable
mexp_of_type(susp_goal, G, M, G2) :- !,
	( is_susp_goal(G, M) ->
	    mexp_of_type(goal, G, M, G2)
	; mod_error(M, ['Non-suspendable goal in continuation: ', G]),
	  G2 = 'fibers_blt.error' % TODO: something better?
	).

% ---------------------------------------------------------------------------

% (remove '.stub' if present in module)
unstub(RM0:G, RM:G) :-
	( is_stub_mod(RM0, RM1) ->
	    RM = RM1
	; RM = RM0
	).

% is_stub_mod(+StubM, ?M)
is_stub_mod(StubM, M) :-
	atom_concat(M, '.stub', StubM).

:- export(stub_base/2). % TODO: used in actmod_tr; make it more general?
% stub_base(+FileBase, -StubPath) :-
stub_base(FileBase, StubPath) :-
	atom_concat(FileBase, '.stub', StubPath).

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).

:- export(sentence_tr/3).
sentence_tr(0, 0, M) :- !,
	cleanup_db(M).
sentence_tr((:- Decl), Cs, M) :- !,
	decl_tr(Decl, Cs, M).
sentence_tr(end_of_file, Cs, M) :- !,
	Cs = ['$curr_mod'(M)|Cs0], % TODO: not for .stub modules! (it works because we have .stub in the name)
	Cs0 = [end_of_file],
	cleanup_db(M).
sentence_tr(Sent, Cs, M) :-
	is_susp_clause(Sent, Head, Body, M),
	!,
	susp_cl(Head, Body, Cs, []).

% Sent is a clause of an suspendable predicate
is_susp_clause(Sent, Head, Body, M) :-
	norm_clause(Sent, Head, Body),
	susp_pred(Head, M). % (only if it is suspendable)

norm_clause(Sent, Head, Body) :-
	( Sent = (Head :- Body) -> true
	; Head = Sent, Body = true
	).

% Head with continuation
headcont(Head, Cont, HeadCont) :-
	Head =.. [F|Args],
	append(Args, [Cont], Args2),
	atom_concat(F, '$cont', F2),
	HeadCont =.. [F2|Args2].

% Expanded clause (first step)
susp_cl(Head, Body) -->
	{ headcont(Head, Cont, HeadCont) },
	[(HeadCont :- '\6\AsyncBody'(Cont, Body))].

% ---------------------------------------------------------------------------

% TODO: Move to its own package (compile-time trait)
decl_tr(static_trait_spec(Trait, F, A), Cs, _M) :- !,
	decl_static_trait_spec(Trait, F, A, Cs).
% Extensions for transient data and suspendable predicates
decl_tr(transient(F/A), Cs, M) :- !, % TODO: Document
	functor(Head, F, A),
	meta_fact(M, Head, transient, decl, Cs, []).
decl_tr(suspendable(FArgs), Cs, M) :- !, % TODO: Document
	decl_suspendable(FArgs, [], M, Cs, []).
decl_tr(suspendable(FArgs, Props), Cs, M) :- !, % TODO: Document
	decl_suspendable(FArgs, Props, M, Cs, []).

decl_suspendable(FArgs, _Props, M) --> { is_stub_mod(M, Mod) }, !,
	% TODO: Props are ignored in stub mod (warning at least?)
	{ functor(FArgs, F, A) },
	{ FArgs =.. [_|FTypes] },
	{ functor(Head, F, A) },
	async_def(stub(Head, FTypes, Mod), M).
decl_suspendable(FArgs, Props, M) -->
	{ functor(FArgs, F, A) },
	{ FArgs =.. [_|FTypes] },
	% TODO: this is similar to metatype addmodule; keep it or use fluids?
	{ get_positional_arity(FTypes, PosA) },
	{ PosA < A }, % we have some non-positional arguments
	!,
	{ functor(Head, F, A) },
	{ functor(PosHead, F, PosA) },
	{ set_susp_pred(Head, M) },
	async_def(pos(PosHead, FTypes), M),
	async_def(real(Head), M),
	async_prop_cls(Props, Head, M).
decl_suspendable(FArgs, Props, M) --> !,
	{ functor(FArgs, F, A) },
	{ FArgs =.. [_|FTypes] },
	{ functor(Head, F, A) },
	{ set_susp_pred(Head, M) },
	async_def(real_ty(Head, FTypes), M),
	async_prop_cls(Props, Head, M).

async_def(pos(PosHead, FTypes), M) --> !,
	ensure_export_functor(M, PosHead),
	meta_fact(M, PosHead, fnct, decl_at_mod(M)),
	meta_fact(M, PosHead, async, ftypes(FTypes)), % TODO: add ftypes and props to Head and add a prop to make the jump?
	meta_cl(M, PosHead, async, run(_), fail),
	% meta_cl(M, PosHead, fnct, prop(_), fail),
	meta_fact(M, PosHead, fnct, prop(async)).
async_def(real(Head), M) --> !,
	ensure_export_functor(M, Head),
        static_fact(M, Head, fnct, prop(async)),
	meta_fact(M, Head, fnct, decl_at_mod(M)),
	meta_cl(M, Head, async, ftypes(_), fail),
	{ headcont(Head, Cont, HeadCont) },
	meta_cl(M, Head, async, run(Cont), HeadCont),
	% meta_cl(M, Head, fnct, prop(_), fail),
	meta_fact(M, Head, fnct, prop(async)).
async_def(real_ty(Head, FTypes), M) --> !,
	ensure_export_functor(M, Head),
        static_fact(M, Head, fnct, prop(async)),
	meta_fact(M, Head, fnct, decl_at_mod(M)),
	meta_fact(M, Head, async, ftypes(FTypes)),
	{ headcont(Head, Cont, HeadCont) },
	meta_cl(M, Head, async, run(Cont), HeadCont),
	% meta_cl(M, Head, fnct, prop(_), fail).
	meta_fact(M, Head, fnct, prop(async)).
% Declare async at the stub module, with a connection via
% '$fnct_stub_rename'/2 multifile.
% TODO: '$fnct_stub_rename'/2 should be avoided once we have native
%   way to load module interfaces without module code
async_def(stub(Head, FTypes, M), StubM) --> !,
	{ mod_concat_head(M, Head, MHead) },
	{ mod_concat_head(StubM, Head, StubMHead) },
        [('$fnct_stub_rename'(MHead, StubMHead))],
	ensure_export_functor(StubM, Head),
        static_fact(StubM, Head, fnct, prop(async)),
	meta_fact(StubM, Head, fnct, decl_at_mod(M)),
	meta_fact(StubM, Head, async, ftypes(FTypes)),
	meta_cl(StubM, Head, async, run(_), fail),
	% meta_cl(StubM, Head, fnct, prop(_), fail),
	meta_fact(StubM, Head, fnct, prop(async)).

async_prop_cls(Props, Head, M) -->
	async_prop_cl(cached_step, Props, Head, M),
	async_prop_cl(nosideff_step, Props, Head, M).

async_prop_cl(Prop, Props, Head, M) -->
	( { member(Prop, Props) } ->
	    meta_fact(M, Head, fnct, prop(Prop))
	; []
	).

% ---------------------------------------------------------------------------
% TODO: needed for actmod; make it more general?

% Arity for positional args (others are added from data)
get_positional_arity(FTypes, N) :-
	get_positional_arity_(FTypes, 0, N).

get_positional_arity_([], N, N).
get_positional_arity_([Ty|Tys], N0, N) :-
	( Ty = data(_,_) -> N1 is N0
	; Ty = blob(_,_) -> N1 is N0
	; N1 is N0 + 1
	),
	get_positional_arity_(Tys, N1, N).

% ---------------------------------------------------------------------------

:- export(goal_tr/3).
goal_tr('\6\AsyncBody'(ContVar, Body), G, M) :- !,
	conj_to_list(Body, Gs),
	take_nonsusp(Gs, M, Gs2, Cont),
	mexp_cont(Cont, M, Cont2),
	list_to_conj(Gs2, Body2),
	G = (Body2, ContVar = Cont2).
goal_tr('$ct_mexp_of_type'(Ty, G, Gv), R, M) :-
	mexp_of_type(Ty, G, M, G1),
	R = (Gv = G1).
goal_tr(fiberSusp(Goal,V), G, M) :- !, % TODO: meta_predicate?
	mexp_of_type(susp_goal, Goal, M, Goal2),
	G = '$fiber_susp'(Goal2, V).
goal_tr(fiberSuspSpawn(Goal,V), G, M) :- !, % TODO: meta_predicate?
	mexp_of_type(susp_goal, Goal, M, Goal2),
	G = '$fiber_susp_spawn'(Goal2, V).
% TODO: transient is not module expanded but it is not easy to fix now
%   since there are calls with variables (search ":- transient" and
%   "\(t_current_fact\|t_defined_data\|t_set_fact\)([~A-Z]")
%  
% :- meta_predicate t_defined_data(transient).
% :- meta_predicate t_current_fact(transient).
% :- meta_predicate t_set_fact(transient).
goal_tr(t_defined_data(F), RG, _M) :- !,
 	RG = '$t_defined_data'(F).
goal_tr(t_current_fact(F), RG, _M) :- !,
 	RG = '$t_current_fact'(F).
goal_tr(t_set_fact(F), RG, _M) :- !,
 	RG = '$t_set_fact'(F).

mexp_cont([], _M, []).
mexp_cont([G|Gs], M, [G2|Gs2]) :-
	mexp_of_type(susp_goal, G, M, G2),
	mexp_cont(Gs, M, Gs2).

take_nonsusp([], _M, [], []).
take_nonsusp([G|Gs], M, [], Cont) :-
	is_susp_goal(G, M),
	!,
	Cont = [G|Gs].
take_nonsusp([G|Gs], M, [G|Gs2], Cont) :-
	take_nonsusp(Gs, M, Gs2, Cont).

is_susp_goal(G, M) :-
	functor_expansion(G, M, -, NG),
	current_static_fact(NG, fnct, prop(async)),
	!.

% Conjunction to list
conj_to_list(A, Xs) :-
	conj_to_list_(A, Xs, []).

conj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = Xs0.
conj_to_list_((A,B), Xs, Xs0) :- !,
	conj_to_list_(A, Xs, Xs1),
	conj_to_list_(B, Xs1, Xs0).
conj_to_list_(true, Xs, Xs0) :- !, Xs = Xs0.
conj_to_list_(A, [A|Xs], Xs).

% List to conjunction
list_to_conj([], A) :- !, A = true.
list_to_conj([A], A) :- !.
list_to_conj([A|As], (A,Bs)) :-
	list_to_conj(As, Bs).

