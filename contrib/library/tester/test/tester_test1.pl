:- module( tester_test1 , _ , [] ).

%:- use_module( library(tester) , [run_tester/10] ).
:- use_module('..'(tester), [run_tester/10]).


:- use_module(library(ciaopp)).
:- use_module(library(compiler)).

:- use_module(library(pathnames)).

:- use_module(library(write)).

:- use_module(library(lists)).

init_func.
	

test_files( '/home/dtm/Ciaopp/Benchmarks/ciaopp/modes/' ).

tester_func( FileArg ) :-
	test_files( Path ),
	atom_concat( Path , FileArg , File0 ),

	message( note ,
         [ '+++++++++++++++++++++++++++++++++++++++++++++++\n' ] ),
	(unload( File0 )->true;true),
	module( File0 ),

	atom_concat( TFile , '.pl', File0 ),
	atom_concat( TFile , '_test.pl' , TestFile ),

	output( TestFile ).


get_module( Path , Module ) :-
	path_basename( Path , File ),
	(atom_concat( Module , '.pl' , File )
	-> true ; Module = File ).

checker_func( FileArg ) :-
	get_module( FileArg , Module ),
	(unload( Module )->true;true),

	atom_concat(RawFile, '.pl'     , FileArg ),	 
	atom_concat(RawFile, '_test.pl' , OptFile ),

	test_files( Path ),
	atom_concat( Path , OptFile, OptFilePath ),

	message( note , [ 'Cargando ' , OptFilePath ] ),
	use_module( OptFilePath ).

end_func.





main :-
	L = [
		'aiakl.pl',
		'query.pl',
		'mmatrix.pl',
		'ann.pl',
		'bid.pl',
		'rdtok.pl',
		'myread.pl',
		'boyer.pl',
		'read.pl',
		'occur.pl',
		'serialize.pl',
		'browse.pl',
		'peephole.pl',
		'tak.pl',
		'deriv.pl',
		'progeom.pl',
		'warplan.pl',
		'fib.pl',
		'qplan.pl',
		'witt.pl',
		'grammar.pl',
		'zebra.pl',
		'qsortapp.pl',
		'hanoiapp.pl'
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
		      slider( 'Tester1: ' )
		  ),
	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).
