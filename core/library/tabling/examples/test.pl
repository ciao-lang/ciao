 %% 800 MHz - Traduccion especial - Managing empty spaces
 %% tcl tarda: 122.511224473
 %% tcr tarda: 384.40223356
 %% tcn tarda: 544.461182162
 %% sgm tarda: 6300.331450057456
 %% atr2 tarda: 276.706717089
 %% pg tarda: 23.336484062
 %% kalah tarda: 32.832968177
 %% gabriel tarda: 33.763808106
 %% disj tarda: 26.77159212
 %% cs_o tarda: 49.438288196
 %% cs_r tarda: 91.85145235700001
 %% peep tarda: 95.435900371

 %% 800 MHz - Traduccion NO especial
 %% sgm tarda: 7457.611246188962
 %% atr2 tarda: 372.142361444
 %% pg tarda: 36.00426812600001
 %% kalah tarda: 37.876480132
 %% gabriel tarda: 36.324096114
 %% disj tarda: 29.795976132
 %% cs_o tarda: 53.28691222400001
 %% cs_r tarda: 96.331956371
 %% peep tarda: 106.269940456

:- module(test, 
	[
	    test/0,
	    time_spend/1	    
	]).

:- use_module(library(between)).
:- use_module(library(prolog_sys)).
:- use_module(library(lists)).

:- use_module(kalah, [test/1,result/1,spend_time/2]).
:- use_module(atr2, [test/1,result/1,spend_time/2]).
:- use_module(cs_o, [test/1,result/1,spend_time/2]).
:- use_module(pg, [test/1,result/1,spend_time/2]).
:- use_module(tcn, [test/1,result/1,spend_time/2]).
:- use_module(cs_r, [test/1,result/1,spend_time/2]).
:- use_module(gabriel, [test/1,result/1,spend_time/2]).
:- use_module(sgm, [test/1,result/1,spend_time/2]).
:- use_module(tcr, [test/1,result/1,spend_time/2]).
:- use_module(disj, [test/1,result/1,spend_time/2]).
:- use_module(tcl, [test/1,result/1,spend_time/2]).
:- use_module(peep, [test/1,result/1,spend_time/2]).

:- include(tabling_type).

programs([tcl,tcr,tcn,sgm,atr2,pg,disj,kalah,gabriel,cs_r,cs_o,peep]).

time_spend(Nt) :-
	N is Nt * 1000,
	programs(Ps),
	member(P,Ps),
	P:spend_time(N,T),
	display(P), display(' takes: '), display(T), nl,
	fail.

time_spend(_).

%Gets all the benchmarks
test :-
	display('=== Result is ==='), nl,
	programs(Ps),
	member(P,Ps),
 	P:test(L1),
 	P:result(L2),
 	check(P,L1,L2),
	fail.
test :- abolish_all_tables.

check(Name,L1,L2) :- 
	check_outputs(L1,L2), !,
	display(Name), display(' working OK'), nl.

check(Name,_,_) :- 
	display(Name), display(' NOT working'), nl.

check_outputs(L1,L2) :- 
	length(L1,N1),
	length(L2,N2),
 %% 	display(N1), nl,
 %% 	display(N2), nl,
	N1 = N2,
	!, check_outputs_(L1,L1,L2).

check_outputs_([],_,_) :- !.
check_outputs_([H|R],L1,L2) :-
	count_atom(H,L1,N1),
	count_atom(H,L2,N2),
	N1 = N2,
	check_outputs_(R,L1,L2).

count_atom(_,[],0) :- !.
count_atom(H,[H|R],N1) :- !,
	count_atom(H,R,N2),
	N1 is N2 + 1.
count_atom(H,[_|R],N) :- !,
	count_atom(H,R,N).



%%%%%%%%% PERFORMANCE %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%  800MHz     %%%%%%%%%%%%%%%%%%%%
 %% 
 %%       TOT    TRAILING
 %% tcl   104.8     0
 %% tcr   171.3    5.82
 %% tcn   229.6   15.41
 %% sgm   16318    5.35
 %% atr2  270.8    5.78
 %% disj  29.31    0.13
 %% pg    24.86    0.11
 %% kalah 38.35    0.11
 %% gabri 35.86    0.16
 %% cs_r  104.9    0.07
 %% cs_o  52.88    0.04
 %% peep  108.0    0.17

