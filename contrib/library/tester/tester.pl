:- module( tester , [run_tester/10] , [assertions,hiord]).

:- use_module(library(lists), [length/2]).
:- use_module(library(write), [write/2]).
:- use_module(library(io_alias_redirection)).


:- doc(title,"Automatic tester").  
:- doc(author, "David Trallero Mena").

:- doc(module, "This module have been created to automate the test
 that a predicate should pass hopefully. With that intention we have to 
 provide a set of test and its correct answers. The predicate
 @pred{run_tester/10} will execute every test and compare it with its answer,
 generating two traces, one with detailed information, and another with the
 summary of executions of the tests.").



gen_bar( 0 , _ , S , S ) :- !.
gen_bar( N , A , [A|R] , S ) :-
	N1 is N - 1 ,
	gen_bar( N1 , A , R , S ).


print_slider( A , Slider ) :-
	Chars is floor( A*0.25 ),
	EmptyChars is 25 - Chars ,

	Bar = [ 0'[ | CBar ],
	gen_bar( Chars , 0'# , CBar , EBar ),
	gen_bar( EmptyChars , 32 , EBar , "]\r" ),
	atom_codes( Slider , Bar ).
	
	

show_slider(  no  , _ , _ , _ ).
show_slider(  Title , Out , Pos , Len ) :-
	SliderPos is ceiling(((Len-Pos)/Len)*100),
 	print_slider( SliderPos  , Slider ), 
	display( Out , Title  ),
	display( Out , Slider ).


:- meta_predicate safe_call( pred(1) , ? , pred(1) , ? , ? , ? , ? ).

safe_call( P , [A|B] , C , [LC1 | LCR] , ResStream , Good , 
	   slider( Slider , Title , Pos , Len )
         ) :-
        Pos1 is Pos + 1,
	safe_call( P , B , C , LCR , ResStream , RGood ,
		   slider( Slider , Title , Pos1 , Len ) ),
	show_slider( Slider , Title, Pos , Len ) ,
	(
	    P(A)
	->
	    % Run the checker,
	    (
		C( LC1 )
	    ->
		% Write in file => TRUE
	        write( ResStream , '   OK   ' ), 
		write( ResStream , A ) , 
		write( ResStream , ' <==> ' ) , 
		write( ResStream , LC1 ),
		Good is RGood + 1
	    ;
		% Write in file => FALSE
	        write( ResStream , ' FAIL   ' ), 
		write( ResStream , A ) , 
		write( ResStream , ' <==> ' ) , 
		write( ResStream , LC1 ),
		Good = RGood
	    ),
	    write( ResStream , '\n' )
	;	
          % Write in file => FALSE
	  write( ResStream , ' FAIL   EXECUTING TEST OF ' ), 
	  write( ResStream , A ) , 
	  write( ResStream , '\n' ),
	  Good = RGood
	).

safe_call( _P , [] , _C , _CL , _Stream , 0 , _ ).

:- pred run_tester( LogFile , ResultFile , 
	  Begin , Test , TestList , Check , CheckList , End , GoodExamples ,
	  Slider ) 

     : string * string * 
       callable * callable * list * callable * list * callable * var * 
       term

# "run_tester is a predicate for automatizate testers. It get 2 file names
	as entry (@var{LogFile} and @var{ResultFile}) for saving the trace
	and the short result scheme respectevely. @var{Being} and @var{End}
	are called at the beginning and at the end of the test. @var{Test}
	is called which each element of @var{TestList} and after,
	@var{Check} is called with the corresponding element in
	@var{CheckList} for checking the results of @var{Test}
	predicate. @var{GoodExample} is ground(int) at the exit and tells
	the number of examples that passed the test correctly. @var{Slider}
	can take the values slider(no) or slider(Title) and slider will be 
        shown everytime a new test is called ".

:- meta_predicate run_tester( ? , ? , pred(0), 
	                      pred(1), ? ,
			      pred(1), ? ,
                              pred(0), ? , ? ).

:- data tester_last_output/1.

run_tester( LogFile , ResultFile , I , T , L , C , L2 , E , GoodExamples , 
	    Slider ) :-
        (
	     Slider=slider( SlideTitle ) 
	->
	     true 
	; 
	    message( error , ['Last argument ' , Slider , 
	                      ' has to unify with slider( _ )'] ),
	    fail
	),
	open( LogFile , write , Stream ),
	open( ResultFile , write , ResStream ),
        current_output(OldUserOutput),
        set_output(Stream),
%	set_stream( user_output , Stream , OldUserOutput ),
	set_stream( user_error  , Stream , OldUserError  ),

	assertz_fact( tester_last_output( OldUserOutput ) ),
	current_fact( tester_last_output( RealOutput ) ),

	length( L , Len ),
	SliderArgs = slider( SlideTitle , RealOutput , 0 , Len ),
	( call( I ),
	  safe_call( T , L , C , L2 , ResStream , GoodExamples , SliderArgs ),
	  call( E ) -> true ; GoodExamples=0 ),
	
	set_stream( user_error , OldUserError  , _ ),
%	set_stream( user_output , OldUserOutput , _ ),
	set_output(OldUserOutput),
	( 
	    SlideTitle == no
	-> 
	    true 
	;
	    display( RealOutput , SlideTitle ) , 
	    display( RealOutput , ' done.\n'  )
	),

	retract_fact( tester_last_output( OldUserOutput ) ),

	close(Stream),
	close( ResStream ).


:- doc(appendix,"

   Two simple examples of the use of the run_tester are provided.

    @subsection{Understanding run_test predicate}

@noindent
@begin{verbatim}
@includeverbatim{tester/test/tester_test2.pl}
@end{verbatim}

    @subsection{More complex example}

In this example we just want to test if the output of Ciaopp is readable by CIAO.

Tester function succeds if it is able to write the output file. 

Checker function succeds if it is able to load the written file.

@noindent
@begin{verbatim}
@includeverbatim{tester/test/tester_test1.pl}
@end{verbatim}

   ").
