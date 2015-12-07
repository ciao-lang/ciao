:- module(test_all, [main/0, test/1], []).

:- use_module(library(format)).

file(addresses,'addresses/objects').
file(any_term,'any_term/any_term').
file(bignums,'bignums/bigints').
file(byte_lists,'byte_lists/byte_lists').
file(exceptions,'exceptions/exceptions_example').
file(foreign_init,'foreign_init/foreign_init').
file(int_lists,'int_lists/int_lists').
file(math,'math/math').
file(foreign_low,'foreign_low/random').
file(strings_and_atoms,'strings_and_atoms/strings_and_atoms').

test(addresses) :-
	format("Testing addresses:~n~n",[]),
	member(X,[1,2,3,4,5,6,7,8,9]),
	object(X,O),
	format("Object ~w has address ~w.~n",[X,O]),
	format("Showing object (from C)...~n",[]),
	show_object(O),
	fail.
test(addresses):- nl, nl, nl.

test(any_term):- 
	format("Testing any_term:~n~n",[]),
        custom_create_term(7, T),
        custom_display_term(T),
        fail.
test(any_term):- nl, nl, nl.

test(bignums):- 
	format("Testing bignums:~n~n",[]),
        ToTry = [0, -0, 1, 1.0e45, 123414123512353245234524523452345],
        member(Number, ToTry),
        make_smart_conversion(Number, Smart, How),
        force_string_conversion(Number, String),
        format("Original is ~w, smart is ~w (~w), string is ~w~n",
               [Number, Smart, How, String]),
        fail.
test(bignums):- nl, nl, nl.

test(exceptions):- 
	format("Testing exceptions:~n~n",[]),
        safe_codes_to_number("1234", _),
        safe_codes_to_number("123a", _),
        fail.
test(exceptions):- nl, nl, nl.

test(foreign_init):- 
	format("Testing foreign_init:~n~n",[]),
        print_time,
        fail.
test(foreign_init):- nl, nl, nl.

test(byte_lists) :-
	format("Testing lists of bytes:~n~n",[]),
	member(X,[0,1,2,3,4,5,6,7,8,9,100]),
	byte_lists:obtain_list(X,L,List),
	format("~w is a list of length ~w (= ~w).~n",[List,X,L]),
	format("Showing list (from C)...~n",[]),
	byte_lists:show_list(L,List),
	fail.
test(byte_lists):- nl, nl, nl.

test(int_lists) :-
	format("Testing lists of integers:~n~n",[]),
	member(X,[0,10,20,30,40,512]),
        int_lists:obtain_list(X,L,List),
	format("~w is a list of length ~w (= ~w).~n",[List,X,L]),
	format("Showing list (from C)...~n",[]),
	int_lists:show_list(L,List),
	fail.
test(int_lists):- nl, nl, nl.

test(math) :-
	format("Testing numbers:~n~n",[]),
	sin(0,X),
	sin(1,Y),
	sin(3.1415,Z),
	format("sin(0)=~w, sin(1)=~w, sin(3.1415)=~w.~n",[X,Y,Z]),
        fail.
test(math):- nl, nl, nl.


test(strings_and_atoms) :-
	format("Testing strings and atoms:~n~n",[]),
	lookup_string(1,S1),
	lookup_string(2,S2),
	lookup_atom(1,A1),
	lookup_atom(2,A2),
	a_string(S),
	format("The following two lines should be identical.~n",[]),
	format("~w ~w ~w ~w ~w~n",
	       ["bcdefg",bcdefg,"cdefghi",cdefghi,
	        "this is a string Prolog should not free"]),
	format("~w ~w ~w ~w ~w~n",[S1,A1,S2,A2,S]),
	show_string("hello message (Prolog string) from C."),
	show_atom('hello message (Prolog atom) from C.'),
        fail.
test(strings_and_atoms):- nl, nl, nl.

test(foreign_low) :-
	format("Testing random:~n~n",[]),
	format("set random seed to ~w~n", 10),
	srandom(10),
	random(X),
	random(1,10,Y),
	format("~w is a random number, ~w is a random number between 1 and 10~n", [X, Y]),
	format("set random seed to ~w~n", 10),
	srandom(10),
	random(A),
	random(1,10,B),
	format("~w is a random number, ~w is a random number between 1 and 10~n", [A, B]),
        fail.
test(foreign_low):- nl, nl, nl.

:- use_package(foreign_interface).

:- use_module(library(foreign_interface/examples/addresses/objects)).
:- use_module(library(foreign_interface/examples/any_term)).
:- use_module(library(foreign_interface/examples/bignums/bigints)).
:- use_module(library(foreign_interface/examples/byte_lists)).
:- use_module(library(foreign_interface/examples/exceptions/exceptions_example)).
:- use_module(library(foreign_interface/examples/foreign_init)).
:- use_module(library(foreign_interface/examples/int_lists)).
:- use_module(library(foreign_interface/examples/math)).
:- use_module(library(foreign_interface/examples/foreign_low/random)).
:- use_module(library(foreign_interface/examples/strings_and_atoms)).

 %% rebuild_all :-
 %% 	member(X,[addresses,byte_lists,math,strings_and_atoms]),
 %% 	file(X,F),
 %% 	(
 %% 	    rebuild_foreign_interface(F,_) ->
 %% 	    true
 %% 	;
 %% 	    format("failed rebuilding foreign interface for ~w~n",[X])
 %% 	),
 %% 	fail.
 %% rebuild_all.

main:-
        file(X, _),
        test(X),
        fail.
main.
