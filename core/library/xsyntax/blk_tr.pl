:- module(_,[sentence_tr/4],[assertions]).

% This translations merges the VN dictionary of sentences with the
% parent clause.

:- use_module(library(dict)).

:- export(sentence_tr/4).
sentence_tr((:- _),_,_,_) :- !, fail.
sentence_tr(Cl,Cl2,_M,D) :-
    add_dic(D,Dic),
    merge_dics(Cl, Dic),
    Cl2 = Cl.

add_dic([], _).
add_dic([Name=Var|VNs], Dic) :-
    dic_lookup(Dic, Name, Var),
    add_dic(VNs, Dic).

merge_dics(X, _Dic) :- var(X), !.
merge_dics(X, _Dic) :- atomic(X), !.
merge_dics('\6\curly_block'(Ss), Dic) :- !,
    merge_dics_sents(Ss, Dic).
merge_dics(X, Dic) :-
    functor(X, _, A),
    merge_dics_args(1, A, X, Dic).

merge_dics_args(I, N, _, _Dic) :- I > N, !.
merge_dics_args(I, N, X, Dic) :-
    arg(I, X, Arg),
    merge_dics(Arg, Dic),
    I1 is I + 1,
    merge_dics_args(I1, N, X, Dic).

merge_dics_sents([], _).
merge_dics_sents([X|Xs], Dic) :-
    X = sentence(_,VNs,_,_,_),
    add_dic(VNs, Dic),
    merge_dics_sents(Xs, Dic).
