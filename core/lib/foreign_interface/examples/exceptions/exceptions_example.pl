:- module(exceptions_example,
	[codes_to_number_c/2,
	 safe_codes_to_number/2
	],
	[foreign_interface]).

:- use_module(library(format)).

% If the string is not a number raises an exception.
:- true pred codes_to_number_c(in(X), go(Y)) :: string * int + (foreign, returns(Y)).

safe_codes_to_number(X, Y) :-
        catch(codes_to_number_c(X, Y), Error, handle_exception(Error)).

handle_exception(Error) :- format("Exception caught ~w~n", [Error]).

:- use_foreign_source(exceptions_c).
:- extra_compiler_opts('-O2').
