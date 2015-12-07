 %% FORMA DE USO!
 %% use_module('EXAMPLES/gen_matrix').
 %% N = 10, gen_matrix(N,N,L1), gen_matrix(N,N,L2), sim(mmatrix_par_ndet(L1,L2,L),2,[all]).

%:- module(mmatrix, [mmatrix_test/5,mmatrix_test_seq/4]).

%:- use_package(fsyntax).
:- use_package(and_sim_plan).
%:- use_module(library(lists), [append/3]).
 %% :- use_module(gen_list).

:- op(950, xfy, [&]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% mmatrix_test(N, M, L, Procs, Sched) :-
 %% 	gen_list(N, M, L1),
 %% 	gen_list(N, M, L2),
 %%         sim(mmatrix_par_ndet(L1, L2, L), Procs, Sched).
 %% mmatrix_test_seq(N, M, L) :-
 %% 	gen_list(N, M, L1),
 %% 	gen_list(N, M, L2),
 %%         mmatrix_par_det(L1, L2, L).
 %%         sim(mmatrix_par_ndet(L1, L2, _Result), 1, Sched).


mmatrix_par_ndet([],_,[]).
mmatrix_par_ndet([R|RR],X,[Res|RRes]):-
        mmatrix_par_ndet(RR,X,RRes) '&'
        multiply_ndet(X,R,Res).

multiply_ndet([],_,[]).
multiply_ndet([V0|Rest], V1, [Result|Others]):-  
        multiply_ndet(Rest, V1, Others) '&'
        vmul(V0,V1,Result).

mmatrix_par_det([],_,[]).
mmatrix_par_det([R|RR],X,[Res|RRes]):-
        mmatrix_par_det(RR,X,RRes),
        multiply_det(X,R,Res).

multiply_det([],_,[]).
multiply_det([V0|Rest], V1, [Result|Others]):-
        multiply_det(Rest, V1, Others),
        vmul(V0,V1,Result).

mmatrix_seq([],_,[]).
mmatrix_seq([R|RR],X,[Res|RRes]):-
        mmatrix_seq(RR,X,RRes) ,
        multiply_seq(X,R,Res).

multiply_seq([],_,[]).
multiply_seq([V0|Rest], V1, [Result|Others]):-
        multiply_seq(Rest, V1, Others),
        vmul(V0,V1,Result).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):-
        vmul(T1,T2, Newresult),
        Product is H1*H2,
        Result is Product+Newresult.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% :- fun_eval arith(true).
 %% 
 %% :- fun_eval gen_list/2.
