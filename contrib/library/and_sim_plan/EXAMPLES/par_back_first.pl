 %% FORMA de USO
 %% use_module('EXAMPLES/get_board').
 %% get_board(bin,3,8,8,R), display(R), nl, sim(find_ind(R,3,L),8,[all]).
:- module(par_back_first,
        [
            find_dep/3,
            find_ind/3
	],
	[]).

:- use_package(and_sim_plan).


find_dep(Board,D,R) :-
	D2 is D * -1,
	each_colum_det(Board,R,D,D2).

each_colum_det([],[],_,_) :- !.
each_colum_det([C|RC],[Ind|IndR],D,PrevInd) :- 
	get_pos(C,1,PrevInd,D,Ind),
	each_colum_det(RC,IndR,D,Ind).

get_pos([],_,_PrevInd,_D,_Ind) :- fail.
get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
	sleep(100),
	N == 1,
	Diff is Pos - PrevInd,
	Diff >= D, 
	Ind = Pos.

get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
	N == 1,
	Diff is PrevInd - Pos,
	Diff >= D, 
	Ind = Pos.

get_pos([_|LR],Pos,PrevInd,D,Ind) :-
	Pos1 is Pos + 1,
	get_pos(LR,Pos1,PrevInd,D,Ind).
	
	
find_ind(Board,D,R) :-
	throw_colum_ndet(Board,R,D).
 %% 	check_dist(R,D).

throw_colum_ndet([C],[R],_) :- 	
	get_colum(C,1,R).

throw_colum_ndet([C|RC],[R1,R2|RR],D) :- 
	get_colum(C,1,R1) '&'
	throw_colum_ndet(RC,[R2|RR],D),
	check_value(R1,R2,D).

get_colum([],_,_) :- !, fail.
get_colum([C|_],Ind,R) :- 
	sleep(100),
	C == 1,
	Ind = R.
get_colum([_|RC],Ind,R) :- 
	Ind1 is Ind + 1,
	get_colum(RC,Ind1,R).

 %% check_dist([_],_) :- !.
 %% check_dist([R1,R2|RR],D) :-
 %% 	check_value(R1,R2,D) '&'
 %% 	check_dist([R2|RR],D).

check_value(R1,R2,D) :-
	R1 > R2,
	R1 - R2 >= D.

check_value(R1,R2,D) :-
	R2 - R1 >= D.




	

