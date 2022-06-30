:- module(sudoku_clpfd, [], [clpfd]).

% Original author: MCL (mcarro_at_fi_dot_upm_dot_es)

:- export(sudoku/1).
sudoku(Sol) :-
    do([], Sol).

% ---------------------------------------------------------------------------

setup(T, Tt, Squares):-
    % % From The Times online: "very hard"
    %
    C1= [_,4,3,_,8,_,2,5,_],
    C2= [6,_,_,_,_,_,_,_,_],
    C3= [_,_,_,_,_,1,_,9,4],
    C4= [9,_,_,_,_,4,_,7,_],
    C5= [_,_,_,6,_,8,_,_,_],
    C6= [_,1,_,2,_,_,_,_,3],
    C7= [8,2,_,5,_,_,_,_,_],
    C8= [_,_,_,_,_,_,_,_,5],
    C9= [_,3,4,_,9,_,7,1,_],
    %
    T = [C1,C2,C3,C4,C5,C6,C7,C8,C9],
    C1 = [V11, V12, V13, V14, V15, V16, V17, V18, V19],
    C2 = [V21, V22, V23, V24, V25, V26, V27, V28, V29],
    C3 = [V31, V32, V33, V34, V35, V36, V37, V38, V39],
    C4 = [V41, V42, V43, V44, V45, V46, V47, V48, V49],
    C5 = [V51, V52, V53, V54, V55, V56, V57, V58, V59],
    C6 = [V61, V62, V63, V64, V65, V66, V67, V68, V69],
    C7 = [V71, V72, V73, V74, V75, V76, V77, V78, V79],
    C8 = [V81, V82, V83, V84, V85, V86, V87, V88, V89],
    C9 = [V91, V92, V93, V94, V95, V96, V97, V98, V99],
    %
    S1 = [V11, V12, V13, V21, V22, V23, V31, V32, V33],
    S2 = [V14, V15, V16, V24, V25, V26, V34, V35, V36],
    S3 = [V17, V18, V19, V27, V28, V29, V37, V38, V39],
    %
    S4 = [V41, V42, V43, V51, V52, V53, V61, V62, V63],
    S5 = [V44, V45, V46, V54, V55, V56, V64, V65, V66],
    S6 = [V47, V48, V49, V57, V58, V59, V67, V68, V69],
    %
    S7 = [V71, V72, V73, V81, V82, V83, V91, V92, V93],
    S8 = [V74, V75, V76, V84, V85, V86, V94, V95, V96],
    S9 = [V77, V78, V79, V87, V88, V89, V97, V98, V99],
    %
    Squares = [S1,S2,S3,S4,S5,S6,S7,S8,S9],
    trans_m(T, Tt).

do(Opts, Sol) :-
%   fd_chain_stats(_),
    setup(T, Tt, Squares),
    all_vars(T, V),
    every_list_diff(T),
    every_list_diff(Tt),
    every_list_diff(Squares),
%    labeling(V),
    labeling(Opts, V),
    T = Sol,
    !. % just one solution

trans_m([[]|_],[]).
trans_m(M,[C1|Cn]):- trans_v(M,C1,R), trans_m(R,Cn).

trans_v([],[],[]).
trans_v([[C11|C1n]|C],[C11|X],[C1n|Y]):- trans_v(C,X,Y).

every_list_diff([]).
every_list_diff([L|Ls]):-
    all_different(L),
    every_list_diff(Ls).

all_vars([], []).
all_vars([N|R], V):- nonvar(N), N = [], !, all_vars(R, V).
all_vars([[X|Xs]|Rs], [X|Ys]):-
    X in 1..9,
    all_vars([Xs|Rs], Ys).

