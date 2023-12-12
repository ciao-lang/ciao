:- module(_, [shpa_clause/3], []).

%! \title shpa translation module
%  \author Jose F. Morales
%
%  \module Use this package to enable support for PA with
%  shared-by-default variables (`-[X1...Xn] -> Head :- Body`),
%  where all common variables are shared except `X1...Xn`.

shpa_clause(clause(H, B), clause(H, B2), _M) :-
    anot_linking_vars(H, B, B2).

%-----------------------------------------------------------------------------
%! ## Annotate linking vars (for PA)
% (based on straight_clause from pl2wam.pl)

anot_linking_vars(Head, Body0, Body3) :-
    seq_to_list(Body0, Body1),
    mk_occurrences_list(Body1, 1, List),
    record_occurrences(Head, 0, List),
    straight_body(Body1, 1, List, Body2),
    list_to_seq(Body2, Body3).

% (backward traverse)
mk_occurrences_list([], _, _).
mk_occurrences_list([G|Gs], Gn, List) :-
    Gn1 is Gn+1,
    mk_occurrences_list(Gs, Gn1, List),
    record_occurrences(G, Gn, List). % TODO: bad complexity if G needs_linked

% (forward traverse)
straight_body([], _, _, []).
straight_body([S|Gs], Gn, List, [Goal|Gs1]) :-
    ( needs_linked(S) ->
        linking_vars(List, Gn, Shared),
        straight_body2(S, right, Shared, Goal)
    ; Goal = S
    ),
    Gn1 is Gn+1,
    straight_body(Gs, Gn1, List, Gs1).

% the subgoal requires linked vars? should it be annotated?
needs_linked(G) :- var(G), !, fail.
needs_linked('basiccontrol:;'(_,_)).
needs_linked('basiccontrol:->'(_,_)).
needs_linked('\6\shpa_val'(_,_)).

straight_body2(G, right, _, R) :- atom(G), !, R = G.
straight_body2('basiccontrol:;'(G1,G2), right, Shared, R) :- !,
    R = (G1b;G2b),
    straight_body2(G1, left, Shared, G1b),
    straight_body2(G2, right, Shared, G2b).
straight_body2('\6\shpa_val'(Head,Body), _, Shared, R) :- !,
    % go inside shpa, including Head in head
    R = '\6\shpa_val2'(Shared,Head,Body2),
    anot_linking_vars(Head-Shared, Body, Body2).
straight_body2(Goal0, _, Shared, Goal) :-
    anot_linking_vars(Shared, Goal0, Goal).

%-----------------------------------------------------------------------------
%! ## Ocurrences and linking vars

:- use_module(library(lists), [list_lookup/3,nonsingle/1,contains_ro/2,contains1/2]).

linking_vars(List, _, Xs) :- var(List), !, Xs = [].
linking_vars([V-Occs|List], Gn, Xs) :-
    nonsingle(Occs), contains_ro(Occs, Gn), !,
    Xs = [V|Xs0],
    linking_vars(List, Gn, Xs0).
linking_vars([_|List], Gn, Xs) :-
    linking_vars(List, Gn, Xs).

add_last(L, X) :- var(L), !, L = [X|_].
add_last([_|L], X) :- add_last(L, X).

record_occurrences(Var, Gn, D) :- var(Var), !,
    list_lookup(D, Var, Occs),
    contains1(Occs, Gn).
record_occurrences(T, Gn, D) :-
    T =.. [_|Args],
    record_occurrences_args(Args, Gn, D).

record_occurrences_args([], _, _).
record_occurrences_args([X|Xs], Gn, D) :-
    record_occurrences(X, Gn, D),
    record_occurrences_args(Xs, Gn, D).

%-----------------------------------------------------------------------------
%! ## Conj<->sequences
%
% Transform between conjunctions and lists. Expand into special forms
% for occurence recording and PA variable renaming.

:- use_module(library(lists), [append/3]).

% TODO: pending: local vars require a copy_term

% (encode -> as a list)
seq_to_list('basiccontrol:->'(A,B), Seq) :- !,
    conj_to_list(A,A2),
    conj_to_list(B,B2),
    append(A2, ['\6\->'|B2], Seq).
seq_to_list(A, Seq) :-
    conj_to_list(A, Seq).

% (decode -> as a list)
list_to_seq(Seq, G) :-
    append(A2, ['\6\->'|B2], Seq),
    !,
    list_to_conj(A2,A),
    list_to_conj(B2,B),
    G = (A->B).
list_to_seq(Seq, G) :-
    list_to_conj(Seq,G).

% conjunction to list (splitting shpa)
conj_to_list(A, Xs) :-
    conj_to_list_(A, Xs, []).

conj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = [A|Xs0].
conj_to_list_(A, Xs, Xs0) :- is_pa(A, V, Head, Body), !,
    Xs = ['\6\shpa_var'(V), '\6\shpa_val'(Head,Body)|Xs0]. % split PA for occurence record (support letrec)
conj_to_list_('basiccontrol:,'(A,B), Xs, Xs0) :- !,
    conj_to_list_(A, Xs, Xs1),
    conj_to_list_(B, Xs1, Xs0).
conj_to_list_('basiccontrol:true', Xs, Xs0) :- !, Xs = Xs0.
conj_to_list_(A, [A|Xs], Xs).

% list to conjunction (merging shpa)
list_to_conj([], 'basiccontrol:true') :- !.
list_to_conj([X], X) :- !.
list_to_conj(['\6\shpa_var'(V), '\6\shpa_val2'(Shared,Head,Body)|Xs], Y) :- !,
    new_pa(V, Shared, Head, Body, G),
    list_to_conj([G|Xs], Y).
list_to_conj([X|Xs], 'basiccontrol:,'(X,Y)) :- list_to_conj(Xs, Y).

% TODO: introduce as a new builtin? (rather than using =)

is_pa(G, V, Head2, Body2) :-
    nonvar(G), G = 'term_basic:='(A,V),
    nonvar(A), A = '$:'(A1),
    nonvar(A1), A1 = 'PAEnv'(ShVs, PA),
    nonvar(ShVs), ShVs = -(NonShVs),
    PA = 'PA'(_,Head,Body),
    rename_vars(NonShVs, Head-Body, Head2-Body2). % rename some vars

new_pa(V, ShVs, Head, Body, G) :-
    G = 'term_basic:='(A,V),
    A = '$:'(A1),
    A1 = 'PAEnv'(ShVs, PA),
    copy_term('PA'(ShVs,Head,Body),PA). % rename all vars

:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), [ord_subtract/3]).

% Copy X into Y, renaming variables Vs
rename_vars(Vs, X, Y) :-
    sort(Vs, Vs2),
    varset(X, XVs),
    ord_subtract(XVs, Vs2, Sh),
    copy_term(Sh-X, Sh-Y).

