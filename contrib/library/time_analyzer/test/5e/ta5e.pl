%% Time Analizer 5 special.
%% It compares if it is faster load a dump file from CiaoPP 
%% analisis than load and do the analisis.

:- module( ta5e , _ , [] ).



:- use_module(library(ciaopp)).

:- use_module(library(time_analyzer)).

:- use_module(library(prolog_sys)).

:- use_module(library(system)).

dir( '/home/dtm/Ciaopp/Systems/benchmarks/modes/PLAI/' ).

dump_restore( (ArgFile,_) , Time ) :-
	dir( DIR ),
	atom_concat( DIR , ArgFile , File ),
	atom_concat( File , '.dump' , DFile ),
	module( File ),
	statistics( runtime , [Start , _ ] ),
	restore( DFile ),
	statistics( runtime , [End , _ ] ),
	Time is (End - Start).


load_analize( (ArgFile,Ana) , Time ) :-
	dir( DIR ),
	atom_concat( DIR , ArgFile , File ),

	module( File ),
	statistics( runtime , [Start , _ ] ),
	apply_ana( Ana ),
	statistics( runtime , [End , _ ] ),
	Time is (End - Start ).

apply_ana( [A|R] ) :-
	analyze( A ),!,
	apply_ana( R ).

apply_ana( [_|R] ) :- !,
	apply_ana( R ).

apply_ana( [] ).



generate_dump_files( [( ( File , Ana ) , _ ) | R ] ) :-
	\+ \+ gen_one_dump( File , Ana ),
	!,
	generate_dump_files( R ).
	
generate_dump_files( [] ) :- !.

generate_dump_files( [( (_ ,_ ) , _ ) | R ] ) :-
	generate_dump_files( R ).



gen_one_dump( ArgFile , Ana ) :-
	dir( DIR ),
	atom_concat( DIR , ArgFile , File ),
	atom_concat( File , '.dump' , DFile ),
	( 
	    file_exists( DFile )
	->
	    true 
	;
	    module( File ),
	    apply_ana( Ana ),
	    dump( DFile )
	).
	

main :-
	Files =	[
		(('aiakl.pl',    [shfr]),  1),
		(('query.pl',    [shfr]),  2),
		(('mmatrix.pl',  [shfr]),  3),
		(('ann.pl',      [shfr]),  4),
		(('bid.pl',      [shfr]),  5),
%		(('rdtok.pl',    [shfr]),  6),
		(('myread.pl',   [shfr]),  7),
		(('boyer.pl',    [shfr]),  8),
		(('read.pl',     [shfr]),  9),
		(('occur.pl',    [shfr]), 10),
		(('serialize.pl',[shfr]), 11),
		(('browse.pl',   [shfr]), 12),
		(('peephole.pl', [shfr]), 13),
		(('tak.pl',      [shfr]), 14),
		(('deriv.pl',    [shfr]), 15),
		(('progeom.pl',  [shfr]), 16),
		(('warplan.pl',  [shfr]), 17),
		(('fib.pl',      [shfr]), 18),
		(('qplan.pl',    [shfr]), 19),
		(('witt.pl',     [shfr]), 20),
		(('grammar.pl',  [shfr]), 21),
		(('zebra.pl',    [shfr]), 22),
		(('qsortapp.pl', [shfr]), 23),
		(('hanoiapp.pl', [shfr]), 24)
	    ],
	generate_dump_files( Files ),
	get_general_options( A ),
	compare_benchmark2( 
			     [
			       (load_analize , [title("module&analize")  ]) ,
			       (dump_restore , [title("dump_and_restore")])
			     ] , 
			     Files,
			     average,
			     1,
			     'restore_vs_analize',
			     runtime,
	    [
		title("Re-Analizing vs. restore"),
		xlabel("Benchmark number"),
		ylabel("Time in ms")|A 
	    ] ).
