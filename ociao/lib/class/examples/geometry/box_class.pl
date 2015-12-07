%%---------------------------------------------------------------------
%%
%% BOX CLASS
%%
%%---------------------------------------------------------------------

:- class(box_class).

:- inherit_class(library(class/examples/geometry/rectangle_class)).

%%---------------------------------------------------------------------
%% NEW IMPLEMENTATION
%%---------------------------------------------------------------------

:- export(set_coordinates/2).

set_coordinates((X,Y),Size) :-
	number(Size),
	Size >= 0,
	XX is X+Size,
	YY is Y+Size,
	inherited set_coordinates((X,Y),(XX,YY)).

%%---------------------------------------------------------------------
%% CONSTRUCTORS
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

box_class.

box_class(Canvas) :-
	rectangle_class(Canvas).

box_class((X,Y),Size) :-
	rectangle_class,
	set_coordinates((X,Y),Size).

:- set_prolog_flag(multi_arity_warnings,on).
