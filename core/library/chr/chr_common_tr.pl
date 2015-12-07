:- data module_cls/2.

:- use_module(library(chr/aggregates_nat)).
:- use_module(library(pretty_print)).
:- use_module(library(lists), [append/3]).
:- use_module(library(messages), [note_message/1]).

chr_compile_module( 0 , _ , M ) :-
	retractall_fact( module_cls( M , _ ) ),
	!.
chr_compile_module( end_of_file , CodeWithEnd , M ) :- 
	!,
	findall( A , retract_fact( module_cls( M , A ) ) , Code ),
	chr_translate( [(:-module(M))|Code] , [_|CodeT] ),
	append( CodeT , [end_of_file] , CodeWithEnd ).
chr_compile_module((:- chr_compiler_message(_Message)), [], _):-!,
%	note_message(Message).
	true.
% To see the generated CHR code
%	display( 'CHR code goes here:\n' ),
%	pretty_print( CodeT , []  ), nl, nl,
%	display( 'CHR end of code\n' ).
chr_compile_module( (:- D)  , _ , M ) :-
	!,
	functor( D , F , 1 ),
	member( F , [constraints, chr_constraint, handler, rules] ),
	assertz_fact( module_cls( M , (:- D) ) ).	
chr_compile_module( A , _ , M ) :-
	!,
	assertz_fact( module_cls( M , A ) ).
chr_compile_module( G , _ , M ) :-
	display( not_processed( M , G ) ), nl,
	fail.
