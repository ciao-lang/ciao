:- module(object_test, [main/0], [assertions, hlc, objects]).

:- use_module(library(read)).
:- use_module(library(system), [pause/1]).

:- use_module(.(queens/nqueens)).
:- use_class(.(multiple_inh/rogue_sorcerer)).
:- use_class(.(conc/concur)).
:- use_class(.(conc/coconcur)).
:- use_module(.(conc/mess), [mess/1]). 

:- test main.

main:-
        display('Solving N-Queens using objects'), nl,
        nqueens:all(8), !,
        display('Testing a rogue-sorcerer'), nl,
	X new rogue_sorcerer,
	X:steal(_),
	X:cast_spell(_),
	nl,
	display('An object with concurrency'), nl,
	open(file,write,O),
	Y new concur,
	Y:main(O) &&,
	pause(2),
	close(O),
	open(file,read,I),
	read(I,a),
	read(I,end_of_file),
	close(I),
	display(ok),nl,
	display('Two concurrent objects'), nl,
	W1 new coconcur,
	W2 new coconcur,
	W1:main(A) &&> HA,
	W2:main(B) &&> HB,
	asserta_fact(mess(a)),
	HA <&&,
	HB <&&,
	A=B,
	display(ok),nl.
