:- module( tester_test2 , _ , _ ).


:- use_module('..'(tester)).
%:- use_module( library(tester) ).
:- use_module(library(lists)).
:- use_module(library(write)).

init_func :-
	write( 'Starting the test\n' ).

tester_func( (X,X,_) ) :-
	write( 'The argument is correct ' ),
	write( X ) , nl.

checker_func( (_,X,X) ) :-
	write( 'check is fine\n\n' ).
	

end_func :-
	write( 'Test ended\n' ).


main :-
	L = [ (1,1,1),   % CORRECT
	      (2,2,1),   % Test CORRECT , CHECK FALSE
	      (1,2,2)    % Test FALSE
	    ],
	      
 	run_tester(
		      'test.log',
		      'result.log',
		      init_func ,
		      tester_func ,
		      L,
		      checker_func,
		      L,
		      end_func,
		      Res,
		      slider( 'Tester2: ' )
		  ),

	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).
