% This module is used for analize between 2 analisys: nf and nfg
:- module( ta6e , [main/0] , [hiord] ).


:- use_module(library(ciaopp)).
:- use_module(library(write)).
:- use_module(library(time_analyzer)).

:- use_module(library(format)).

load_ana( ArgFile , T ) :-
	atom_concat( '/home/pedro/ciaopp/Benchmarks/ciaopp/types/' ,
	              ArgFile , File ),
	module( File ),
        analyze(shfr, time(_,Tshfr)),
        analyze(eterms, time(_, Teterms)),
        analyze(nfg, nfinfo(Tnfg,_Num_Pred,_Num_NF_Pred,_NCov)),
        T is Tshfr + Teterms + Tnfg,
	display( ended ).

load_ana_nf( ArgFile , T ) :-
	atom_concat( '/home/pedro/ciaopp/Benchmarks/ciaopp/types/' ,
	              ArgFile , File ),
	module( File ),
        analyze(nf , time( _ ,T ) ).

main :-
	Files = [
		    (hanoi,1),
		    (fib,2),
		    (tak,3),
		    (subst,4),
		    (reverse,5),
		    (mv,6),
		    (qsort,7),
		    (qsort2,8),
		    (pv_queen,9),
		    (pv_pg,10)
		],

        benchmark2( load_ana , Files , min , 1 , time , AnaTimes ),

        benchmark2( load_ana_nf , Files , min , 1 , time , AnaTimes2 ),


	write( 'Files&Order(nf): ' ) , write( Files ) , nl ,
	write( 'Analisys times(nf): ' ) , write( AnaTimes2 ) , nl ,
	write( 'Analisys times(nfg): ' ) , write( AnaTimes ) , nl ,
	
	print_nice_table( Files, AnaTimes2 , AnaTimes ),
	
	get_general_options( A ),
	generate_plot( 'nfg_types_times' , 
	[ (AnaTimes2,[with(impulses)]) ] , 
	   [ xrange(0,11) , title( "NF Analisys times" )|A ]  ),

	get_general_options( A ),
	generate_plot( 'nf_types_times' , 
	[ (AnaTimes,[with(impulses)]) ] , 
	   [ xrange(0,11) , title( "NFG Analisys times" )|A ]  ),

	generate_plot( 'nf_compare' , 
	[ (AnaTimes,[with(lines)]),(AnaTimes2,[with(lines)]) ] , 
	   [ xrange(0,11) , title( "NF vs NFG" )|A ]  ).


write_spaces( 0 ) :-!.
write_spaces( N ) :-
	N1 is N - 1,
	write( ' ' ),
	write_spaces( N1 ).

print_nice_table( [] , [] , [] ) :- !, nl.

print_nice_table( [ (File,_)|A1] , [(_,NF)|A2] , [(_,NFG)|A3] ) :-
	write( File ) , 
	atom_length( File , L ),
	write_spaces( 10 - L ),
	format( " ~d & ~d~n" , [NF , NFG] ),
	print_nice_table( A1 , A2 , A3 ).
