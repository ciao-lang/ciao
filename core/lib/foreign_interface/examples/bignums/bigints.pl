:- module(bigints,
	[ 
          make_smart_conversion/3, % Checks and uses convenient format
          force_string_conversion/2  % Passes around using strings
	],
	[foreign_interface]).

:- true pred make_smart_conversion_c(in(X), go(Y), go(How)):: 
        any_term * any_term * any_term + foreign # 
"Given a number @var{X}, it is unified with @var{Y} by using the most
specific internal representation (short integer, float, or long
integer).  @var{How} returns how the conversion was done.
It behaves unpredictably if @var{X} is not a number.".

:- true pred force_string_conversion_c(in(X), go(Y)):: 
        any_term * any_term + foreign #
"Given a number @var{X}, it is unified with @var{Y} by using the most
general internal representation (a string of characters). It behaves
unpredictably if @var{X} is not a number.".

:- use_foreign_source(bigints_c).

make_smart_conversion(A, B, C):-
        number(A),                              % Safety test
        make_smart_conversion_c(A, B, C).

force_string_conversion(A, B):-
        number(A),                              % Safety test
        force_string_conversion_c(A, B).
