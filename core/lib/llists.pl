:- module(llists, [
	           append/2,
                   flatten/2,
                   collect_singletons/2,
                   transpose/2
		  ],
		  [assertions, isomodes]).

:- use_module(library(lists), [append/3]).

:- pred append/2 :: list(list) * list
        # "Concatenates a list of lists into a list.".

append([],[]).
append([L|Ls], LR) :-
        append_all(Ls, L, LR).

append_all([], L, L).
append_all([L|Ls], L2, LR) :-
        append(L2, LR0, LR),
        append_all(Ls, L, LR0).

%% Was this, much more inefficient:
%
% append([],[]).
% append([L],L):- !.
% append([L0,L1|Ls],L):-
% 	append(L0,L1,L2),
% 	append([L2|Ls],L).

:- pred flatten(+term, ?list)
        # "Flattens out nested lists into a list.".

flatten(Xs, Ys) :-
	flatten_dif(Xs, Ys, []).

flatten_dif(X, [X|Ys], Ys) :-
	var(X),
	!.
flatten_dif([], Xs, Xs) :-
	!.
flatten_dif([X|Xs], Ys, Zs) :-
	!,
	flatten_dif(X, Ys, Ys1),
	flatten_dif(Xs, Ys1, Zs).
flatten_dif(X, [X|Xs], Xs).

:- pred collect_singletons(+list(list), ?list)
        # "Collects in a list the singletons lists appearing in a list
          of lists.".

collect_singletons([],[]).
collect_singletons([[X]|Xss],[X|Ys]):- !,
	collect_singletons(Xss,Ys).
collect_singletons([_|Xss],Ys):-
	collect_singletons(Xss,Ys).

:- test transpose(X, Y) : (X = [[aaa, bbb, ccc], [ddd, eee, fff]])
	=> (Y == [[aaa, ddd], [bbb, eee], [ccc, fff]])
        # "Transpose a non numeric matrix.".

:- test transpose(X, Y) : (X = [[1.0, 2.0]])
	=> (Y == [[1.0], [2.0]]) # "Transpose a 2x1 matrix.".

:- pred transpose(+list(list), ?list(list))
        # "Transposes a list of lists, that is, viewing it as a matrix
          changes rows by columns.".

transpose([], L) :-
        unify_nil(L).
transpose([C|Cs], L) :-
        deal_column(C, L, R),
        transpose(Cs, R).

deal_column([], [], []).
deal_column([E|Es], [[E|R1]|L], [R1|R]) :-
        deal_column(Es, L, R).

unify_nil([]).
unify_nil([[]|R]) :-
        unify_nil(R).

/*
%-------------------------------------------------------------------------
% combinations(+,-)                                                      |
% combinations(Xss,Css)                                                  |
% Css = {Xs | forall Xsi in Xss, exists X in Xsi, X in Xs}               |
%-------------------------------------------------------------------------

combinations([],[]).
combinations([Ls|Lss],Cs):-
	list_to_list_of_lists(Ls,Init),
	combinations(Lss,Init,Cs).

combinations([],Cs,Cs).
combinations([Ls|Lss],Cs0,Cs):-
	combinations2(Ls,Cs0,Cs1),
	sort(Cs1,Cs2),
	combinations(Lss,Cs2,Cs).

combinations2([],_Cs,[]).
combinations2([E|Es],Cs0,Cs1):-
	add_to_each(Cs0,E,Cs1,Tail),
	combinations2(Es,Cs0,Tail).

add_to_each([],_,L,L).
add_to_each([Ls|Lss],E,[NewLs|NewLss],Tail):-
	insert(Ls,E,NewLs),
	add_to_each(Lss,E,NewLss,Tail).
*/
