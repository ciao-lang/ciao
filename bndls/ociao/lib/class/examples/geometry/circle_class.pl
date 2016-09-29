%%---------------------------------------------------------------------
%%
%% CIRCLE CLASS
%%
%%---------------------------------------------------------------------

:- class(circle_class).

:- inherit_class(library(class/examples/geometry/oval_class)).


%%---------------------------------------------------------------------
%% COORDINATES
%%---------------------------------------------------------------------

:- export([set_width/1,set_height/1]).

set_width(W) :-
	inherited set_width(W),
	inherited set_height(W).

set_height(H) :-
	inherited set_height(H),
	inherited set_width(H).

%%---------------------------------------------------------------------
%% CONSTRUCTOR
%%---------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings,off).

circle_class.

circle_class(Owner) :-
	oval_class(Owner).

circle_class((X,Y),Diameter,Owner) :-
	oval_class(Owner),
	set_width(Diameter),
	set_center(X,Y).

circle_class((X,Y),Diameter) :-
	set_height(Diameter),
	set_center(X,Y).

:- set_prolog_flag(multi_arity_warnings,on).
