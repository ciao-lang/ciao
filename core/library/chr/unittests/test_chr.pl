:- module(test_chr, [main/0], [assertions]).

:- use_module(engine(stream_basic)).
:- use_module(library(compiler)).
:- use_module(library(messages)).
:- use_module(library(iso_misc)).
:- use_package(fsyntax).
:- use_package(hiord).
:- use_module(library(hiordlib), [maplist/2]).

test_chr(Base-Name) :-
	absolute_file_name(library(chr/'Tests'/Base), '', '.chr', '.', Source, _, _),
	use_module(Source),
	simple_message("Executing ~a test~n", [Name]),
	(_:Base, ! ; error_message("Test ~a failed!!!~n", [Name])).

:- test main.

main:-
	maplist(test_chr,
	        [fibonacci-'Fibonacci',
		 leq-'Less or Equal',
		 primes-'Primes',
		 zebra-'Zebra',
		 %typesmodes-'Typesmodes',
		 passive_check-'Passive Check',
		 passive_check2-'Passive Check2',
		 trigger_no_active_occurrence-'Trigger']),
	!.
