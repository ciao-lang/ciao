:- module( ta4 , _ , _ ).

:- use_module(library(time_analyzer)).

gen_n_list( 0 , [] ) :- !.
gen_n_list( I , [1|A] ) :-
	I1 is I -1 ,
	gen_n_list( I1 , A ).

gen_nn_list( 0 , _ ,  [] ) :- !.
gen_nn_list( I , N , [L|A] ) :-
	gen_n_list( N , L ),
	I1 is I - 1,
	gen_nn_list( I1 , N , A ).

test( N ) :- gen_nn_list( N , N , _ ).

main :-
	benchmark( test , [
			   (100,100),
			   (200,200),
			   (500,500),
			   (700,700),
	                   (1000,1000),
	                   (2000,3000)
% 	                   (6000,6000),
% 	                   (9000,9000),
% 			   (10000,10000)
			  ] , average , 3 , runtime , L ),
	generate_plot( 'output' , [(L,[with(lines)])] ).
