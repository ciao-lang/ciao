:- module(_, [win_reg/3], [dcg]).

:- use_module(library(strings)).
:- use_module(library(lists), [length/2]).

win_reg(Registry) -->
        "REGEDIT4", nl, nl,
        win_reg_items(Registry).

win_reg_items([]) --> "".
win_reg_items([R|Rs]) -->
        win_reg_item(R),
        nl,
        win_reg_items(Rs).

win_reg_item([Name|Assigns]) -->
        win_reg_node(Name),
        win_reg_assigns(Assigns).

win_reg_node([HKey|Path]) -->
        {atom_codes(HKey,HkeyS)},
        "[", string(HkeyS), win_reg_node_elems(Path), "]", nl.

win_reg_node_elems([]) --> "".
win_reg_node_elems([E|Es]) -->
        {atom_codes(E,S)},
        "\\", string(S),
        win_reg_node_elems(Es).

win_reg_assigns([]) --> "".
win_reg_assigns([K=V|As]) -->
        win_reg_key(K), "=", win_reg_value(V), nl,
        win_reg_assigns(As).

win_reg_key(@) --> !, "@".
win_reg_key(K) -->
        {atom_codes(K,S)},
        win_reg_string(S).

win_reg_value(S) --> win_reg_string(S), !.
win_reg_value(Num) --> {integer(Num)}, !, win_reg_hexbin(Num).
win_reg_value(hexstring(S)) --> win_reg_hexstring(S).
win_reg_value(dword(Num))   --> win_reg_dword(Num).

win_reg_string(S) --> """", win_reg_strchars(S), """".

win_reg_strchars([]) --> "".
win_reg_strchars([C|Cs]) -->
        win_reg_strchar(C),
        win_reg_strchars(Cs).

win_reg_strchar(0'") --> !, "\\""". %"
win_reg_strchar(0'\\) --> !, "\\\\".
win_reg_strchar(C) --> [C].

win_reg_hexstring([C|Cs]) --> "hex(2):", win_reg_hexstr(23,C,Cs).

win_reg_hexstr(0, C, S) --> !, "\\", nl, "  ", win_reg_hexstr(25,C,S).
win_reg_hexstr(_, C, []) --> !, win_reg_hex(C).
win_reg_hexstr(N, C, [X|Xs]) -->
        win_reg_hex(C), ",",
        {N1 is N-1},
        win_reg_hexstr(N1, X, Xs).

win_reg_hex(N) -->
        {number_codes(N,16,S), (S = [_] -> H = "0"||S ; H = S)},
        string(H).

win_reg_hexbin(Num) --> "hex:",
        {number_codes(Num,16,S), length(S,L),
         R is L mod 2, addceros(R, S, Bytes), Bytes = [B1,B2|Bs]},
        win_reg_hexbytes(Bs, B1, B2).

win_reg_hexbytes([], A, B) --> [A,B].
win_reg_hexbytes([B1,B2|Bs], A, B) -->
        [A,B,0',],
        win_reg_hexbytes(Bs, B1, B2).

win_reg_dword(Num) -->
        "dword:",
        { number_codes(Num,16,S),
          length(S,L), R is 8-L, addceros(R, S, NS) },
        string(NS).

addceros(0, S, S) :- !.
addceros(N, S, NS) :-
        N1 is N-1,
        addceros(N1,[0'0|S],NS).

%% Already added by cygwin
% nl --> [13,10].
nl --> [10].
